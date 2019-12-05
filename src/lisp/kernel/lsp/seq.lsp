;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;                           sequence routines

(in-package "SYSTEM")


(defun error-not-a-sequence (value)
  (signal-type-error value 'sequence))

(defun error-sequence-index (sequence index)
  (error 'simple-type-error
         :datum index
         :expected-type 'unsigned-byte
         :format-control "Not a valid index ~A into sequence ~A"
         :format-arguments (list index sequence)))

(defun error-sequence-type (type)
  (error 'simple-type-error
	 :datum (vector) ;; Any sequence object will do, because it does not belong to TYPE
	 :expected-type type
	 :format-control "~S does not specify a sequence type"
	 :format-arguments (list type)))

(defun error-sequence-length (object type size)
  (error 'simple-type-error
	 :format-control
	 "Cannot create a sequence of size ~S which matches type ~S."
	 :format-arguments (list size type)
	 :expected-type type
	 :datum object))

;;; Given a type specifier, returns information about it in its capacity as a sequence.
;;; Returns three values: An upgraded element type or 'list, a length or '*, and a boolean.
;;; The third value is T if the function could determine the first two.
(defun closest-sequence-type (type &optional env)
  (let (elt-type length name args)
    (cond ((consp type)
	   (setq name (first type) args (cdr type)))
	  ((clos::classp type)
	   (setf name (class-name (the class type)) args nil))
	  (t
	   (setq name type args nil)))
    (case name
      ((LIST)
       ;; This is the only descriptor that does not match a real
       ;; array type.
       (values 'list '* t))
      ((VECTOR)
       (values (if (endp args) 't (upgraded-array-element-type (first args) env))
               (if (endp (rest args)) '* (second args))
               t))
      ((SIMPLE-VECTOR) (values 't (if (endp args) '* (first args)) t))
      #-unicode
      ((STRING SIMPLE-STRING) (values 'base-char (if (endp args) '* (first args)) t))
      #+(or unicode clasp)
      ((BASE-STRING SIMPLE-BASE-STRING)
       (values 'base-char (if (endp args) '* (first args)) t))
      #+(or unicode clasp)
      ((STRING SIMPLE-STRING) (values 'character (if (endp args) '* (first args)) t))
      ((BIT-VECTOR SIMPLE-BIT-VECTOR)
       (values 'bit (if (endp args) '* (first args)) t))
      ((ARRAY SIMPLE-ARRAY)
       ;; It's only a sequence if a rank is specified.
       (unless (= (length args) 2) (values nil nil nil))
       (let ((element-type (upgraded-array-element-type (first args)))
             (dimension-spec (second args)))
         (cond ((eql dimension-spec 1) (values element-type '* t))
               ((and (consp dimension-spec)
                     (null (cdr dimension-spec)))
                (values element-type (car dimension-spec) t))
               (t (values nil nil nil)))))
      ((null) (values 'list 0 t))
      ;; This is pretty dumb, but we are required to signal a length mismatch
      ;; error in safe code, and this case will probably not come up often.
      ((cons)
       (if (and (consp args) (consp (cdr args))) ; we have (cons x y
           (multiple-value-bind (et len success)
               (closest-sequence-type (second args) env)
             (cond ((or (not success) ; can't figure out what cdr is
                        (not (eq et 'list))) ; (a . #(...)) is not a sequence
                    (values nil nil nil))
                   ((eq len '*) (values 'list '* t))
                   (t (values 'list (1+ len) t))))
           (values 'list '* t)))
      (t
       ;; We arrive here when the sequence type is not easy to parse.
       ;; We give up trying to guess the length of the sequence.
       ;; Furthermore, we also give up trying to find if the element
       ;; type is *. Instead we just compare with some specialized
       ;; types and otherwise fail.
       (dolist (i '((NIL . NIL)
                    (LIST . LIST)
                    (STRING . CHARACTER)
                    . #.(mapcar #'(lambda (i) `((VECTOR ,i) . ,i))
                         sys::+upgraded-array-element-types+)) ;; clasp change
                  (values nil nil nil))
         (when (subtypep type (car i) env)
           (return (values (cdr i) '* t))))))))

(defun make-sequence (type size	&key (initial-element nil iesp))
  "Args: (type length &key initial-element)
Creates and returns a sequence of the given TYPE and LENGTH.  If INITIAL-
ELEMENT is given, then it becomes the elements of the created sequence.  The
default value of INITIAL-ELEMENT depends on TYPE."
  (if (and (integerp size) (>= size 0))
      (multiple-value-bind (element-type length success)
          (closest-sequence-type type)
        (cond ((not success)
               (if (subtypep type 'sequence)
                   (error "Could not determine element type in ~a" type)
                   (error-sequence-type type)))
              ((eq element-type 'LIST)
               (let ((result (make-list size :initial-element initial-element)))
                 (if (or (eq length '*) (eql length size))
                     ;;; ecl  tests instead for (or (and (subtypep type 'NULL) (plusp size))
                     ;;;                            (and (subtypep type 'CONS) (zerop size)))
                     (if (subtypep 'LIST type)
                         result
                         (if (and (subtypep type 'CONS) (zerop size))
                             (error-sequence-length result type 0)
                             result))
                     (error-sequence-length result type size))))
              (t
               (let ((result (sys:make-vector (if (eq element-type '*) 't element-type)
                                              size nil nil nil 0)))
                 ;; Don't know why we don't just pass make-vector the initial element.
                 (when iesp
                   (si::fill-array-with-elt result initial-element 0 nil))
                 (unless (or (eql length '*) (eql length size))
                   (error-sequence-length result type size))
                 result))))
      (error 'type-error :datum size :expected-type '(integer 0 *))))

;;; Sequence iterators.

;;; Magic terminator for list :from-end t
(defvar *exhausted* (list nil))

(defun list-iterator-next (seq it from-end)
  (declare (ignore seq from-end)
           (optimize (safety 0))
           (type cons it))
  (cdr it))
(defun list-iterator-prev (seq it from-end)
  (declare (ignore from-end)
           (optimize (safety 0))
           (type list seq it))
  (if (eq it seq)
      *exhausted*
      (do ((cdr seq (cdr seq)))
          ((eq (cdr cdr) it) cdr))))
(defun list-iterator-endp (seq it limit from-end)
  (declare (ignore seq from-end))
  (eq it limit))
(defun list-iterator-elt (seq it)
  (declare (ignore seq)
           (optimize (safety 0))
           (type cons it))
  (car it))
(defun (setf list-iterator-elt) (new seq it)
  (declare (ignore seq)
           (optimize (safety 0))
           (type cons it))
  (rplaca it new)
  new)
(defun list-iterator-index (seq it)
  (do ((i 0 (1+ i))
       (cdr seq (cdr cdr)))
      ((eq cdr it) i)))
(defun list-iterator-copy (seq it)
  (declare (ignore seq))
  it)
(defun list-iterator-step (seq it from-end)
  (if from-end
      (list-iterator-prev seq it from-end)
      (list-iterator-next seq it from-end)))

(defun make-simple-list-iterator (list from-end start end)
  (cond (from-end
         (let* ((termination
                  (if (= start 0)
                      *exhausted*
                      (nthcdr (1- start) list)))
                (init (if (<= (or end (length list)) start)
                          termination
                          (if end
                              (last list (- (length list) (1- end)))
                              (last list)))))
           (values init termination t)))
        ((not end) (values (nthcdr start list) nil nil))
        (t (let ((st (nthcdr start list)))
             (values st (nthcdr (- end start) st) nil)))))

(defun make-list-iterator (list from-end start end)
  (multiple-value-bind (iterator limit from-end)
      (make-simple-list-iterator list from-end start end)
    (values iterator limit from-end
            (if from-end #'list-iterator-prev #'list-iterator-next)
            #'list-iterator-endp
            #'list-iterator-elt #'(setf list-iterator-elt)
            #'list-iterator-index #'list-iterator-copy)))

(defun vec-iterator-next (seq it from-end)
  (declare (ignore seq from-end)
           (optimize (safety 0))
           (type fixnum it))
  (1+ it))
(defun vec-iterator-prev (seq it from-end)
  (declare (ignore seq from-end)
           (optimize (safety 0))
           (type fixnum it))
  (1- it))
(defun vec-iterator-endp (seq it limit from-end)
  (declare (ignore seq from-end)
           (optimize (safety 0))
           (type fixnum it limit))
  (= it limit))
(defun vec-iterator-elt (seq it)
  (declare (optimize (safety 0)))
  (aref (the vector seq) it))
(defun (setf vec-iterator-elt) (new seq it)
  (declare (optimize (safety 0)))
  (setf (aref (the vector seq) it) new))
(defun vec-iterator-index (seq it)
  (declare (ignore seq))
  it)
(defun vec-iterator-copy (seq it)
  (declare (ignore seq))
  it)
(defun vec-iterator-step (seq it from-end)
  (if from-end
      (vec-iterator-prev seq it from-end)
      (vec-iterator-next seq it from-end)))

(defun make-vector-iterator (sequence from-end start end)
  (let* ((end (or end (length sequence)))
         (iterator (if from-end (1- end) start))
         (limit (if from-end (1- start) end)))
    (values iterator limit from-end
            (if from-end #'vec-iterator-prev #'vec-iterator-next)
            #'vec-iterator-endp
            #'vec-iterator-elt #'(setf vec-iterator-elt)
            #'vec-iterator-index #'vec-iterator-copy)))

(macrolet ((def (generic (&rest params) list vec)
             `(defun ,generic (seq it ,@params)
                (cond ((listp seq) (,list seq it ,@params))
                      ((vectorp seq) (,vec seq it ,@params))
                      (t (error-not-a-sequence seq))))))
  (def sequence:iterator-step (from-end)
    list-iterator-step vec-iterator-step)
  (def sequence:iterator-endp (limit from-end)
    list-iterator-endp vec-iterator-endp)
  (def sequence:iterator-elt () list-iterator-elt vec-iterator-elt)
  (def sequence:iterator-index () list-iterator-index vec-iterator-index)
  (def sequence:iterator-copy () list-iterator-copy vec-iterator-copy))
(defun (setf sequence:iterator-elt) (new seq it)
  (cond ((listp seq) (funcall #'(setf list-iterator-elt) new seq it))
        ((vectorp seq) (funcall #'(setf vec-iterator-elt) new seq it))
        (t (error-not-a-sequence seq))))

(defun %make-sequence-iterator (sequence from-end start end)
  (cond ((listp sequence)
         (make-list-iterator sequence from-end start end))
        ((vectorp sequence)
         (make-vector-iterator sequence from-end start end))
        (t (error-not-a-sequence sequence))))

(defun sequence:make-sequence-iterator (sequence &key from-end start end)
  (%make-sequence-iterator sequence from-end start end))

;;; If a SEQUENCE:EMPTYP symbol exists, alexandria needs it to be fbound.
(defun sequence:emptyp (sequence) (zerop (length sequence)))

;;; Given a list of sequences, return three lists of the same length:
;;; one with irrelevant elements, one with iterators of the sequences,
;;; and one with limits of the sequences.
(defun lists-for-do-sequence-list (sequences)
  (declare (optimize speed (safety 0)))
  (let* ((elts-head (cons nil nil)) (elts-tail elts-head)
         (its-head (cons nil nil)) (its-tail its-head)
         (limits-head (cons nil nil)) (limits-tail limits-head))
    (declare (type cons elts-head elts-tail)
             (type cons its-head its-tail)
             (type cons limits-head limits-tail))
    (dolist (seq sequences)
      (let ((new-elts-tail (cons nil nil)))
        (rplacd elts-tail new-elts-tail)
        (setq elts-tail new-elts-tail))
      (sequence:with-sequence-iterator (iterator limit) (seq)
        (let ((new-its-tail (cons iterator nil)))
          (rplacd its-tail new-its-tail)
          (setq its-tail new-its-tail))
        (let ((new-limits-tail (cons limit nil)))
          (rplacd limits-tail new-limits-tail)
          (setq limits-tail new-limits-tail))))
    (values (cdr elts-head) (cdr its-head) (cdr limits-head))))

;;; Given four lists of the same length, with elements:
;;; 1) irrelevant 2) sequences 3) iterators 4) limits
;;; If any of the iterations are complete, return NIL.
;;; Otherwise, mutate 1 to contain the current elements of the
;;; iterations, mutate 3 to contain the next iterations,
;;; and return true.
(defun seq-iterator-list-pop
    (values-list seq-list iterator-list limit-list)
  (declare (optimize speed (safety 0)))
  (do ((v-list values-list (cdr v-list))
       (s-list seq-list (cdr s-list))
       (i-list iterator-list (cdr i-list))
       (l-list limit-list (cdr l-list)))
      ((null v-list) t)
    (let ((sequence (cons-car s-list))
          (it (cons-car i-list))
          (limit (cons-car l-list)))
      (when (sequence:iterator-endp sequence it limit nil)
        (return nil))
      (rplaca (the cons v-list)
              (sequence:iterator-elt sequence it))
      (rplaca (the cons i-list)
              (sequence:iterator-step sequence it nil)))))

(defun coerce-to-list (object)
  (if (listp object)
      object
      (let* ((head (list nil)) (tail head))
        (declare (type cons head tail)
                 (optimize (safety 0) (speed 3)))
        (sequence:dosequence (elt object (cdr head))
          (let ((new-tail (list elt)))
            (rplacd tail new-tail)
            (setq tail new-tail))))))

;;; separate because it's easier to not make the whole list at once.
;;; Only used in a compiler macroexpansion, for now
(defun concatenate-to-list (core:&va-rest sequences)
  (let* ((head (list nil)) (tail head))
    (declare (type cons head tail)
             (optimize (safety 0) (speed 3)))
    (dovaslist (sequence sequences (cdr head))
      (sequence:dosequence (elt sequence)
        (let ((new-tail (list elt)))
          (rplacd tail new-tail)
          (setq tail new-tail))))))

(defun concatenate-into-vector (vector core:&va-rest sequences)
  ;; vector is assumed to be non complex and have the correct length.
  (let ((index 0))
    (dovaslist (sequence sequences vector)
      (sequence:dosequence (elt sequence)
        (setf (vref vector index) elt)
        (incf index)))))

(defun concatenate (result-type &rest sequences)
  (let* ((lengths-list (mapcar #'length sequences))
         (result (make-sequence result-type (apply #'+ lengths-list))))
    (if (listp result)
        (let ((cons result))
          (do* ((sequences sequences (rest sequences))
                (sequence (first sequences) (first sequences)))
               ((null sequences) result)
            (sequence:dosequence (elt sequence)
              (rplaca cons elt)
              (setq cons (cdr cons)))))
        (with-array-data ((vec result) index)
          (do* ((sequences sequences (rest sequences))
                (sequence (first sequences) (first sequences)))
               ((null sequences) result)
            (sequence:dosequence (elt sequence)
              (setf (vref vec index) elt)
              (incf index)))))))

(defun map-for-effect (function sequence &rest more-sequences)
  "Does (map nil ...)"
  (let ((sequences (list* sequence more-sequences))
        (function (coerce-fdesignator function)))
    (declare (type function function)
             (optimize (safety 0) (speed 3)))
    (do-sequence-list (elt-list sequences)
      (apply function elt-list)))
  nil)

(defun map (result-type function sequence &rest more-sequences)
  "Args: (type function sequence &rest more-sequences)
Creates and returns a sequence of TYPE with K elements, with the N-th element
being the value of applying FUNCTION to the N-th elements of the given
SEQUENCEs, where K is the minimum length of the given SEQUENCEs."
  (if result-type
      ;; FIXME: Could avoid this in map-to-list case.
      (let ((length
              (reduce #'min more-sequences
                      :initial-value (length sequence)
                      :key #'length)))
        (apply #'map-into (make-sequence result-type length)
               function sequence more-sequences))
      (apply #'map-for-effect function sequence more-sequences)))

(defun some (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if at least one of the elements in SEQUENCEs satisfies PREDICATE;
NIL otherwise."
  (reckless
   (do-sequence-list (elt-list (cons sequence more-sequences))
     (let ((x (apply predicate elt-list)))
       (when x (return x))))))

(defun every (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if every elements of SEQUENCEs satisfy PREDICATE; NIL otherwise."
  (reckless
   (do-sequence-list (elt-list (cons sequence more-sequences) t)
     (unless (apply predicate elt-list)
       (return nil)))))

(defun every* (predicate &rest sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if every elements of SEQUENCEs satisfy PREDICATE and all sequences
have the same length; NIL otherwise."
  (and (apply #'= (mapcar #'length sequences))
       (apply #'every predicate sequences)))

(defun notany (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if none of the elements in SEQUENCEs satisfies PREDICATE; NIL
otherwise."
  (not (apply #'some predicate sequence more-sequences)))

(defun notevery (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if at least one of the elements in SEQUENCEs does not satisfy
PREDICATE; NIL otherwise."
  (not (apply #'every predicate sequence more-sequences)))

(defun map-into (result-sequence function &rest sequences)
"Fills the output sequence with the values returned by applying FUNCTION to the
elements of the given sequences. The i-th element of RESULT-SEQUENCE is the output
of applying FUNCTION to the i-th element of each of the sequences. The map routine
stops when it reaches the end of one of the given sequences."
  ;; Set the fill pointer to the number of iterations
  ;; NOTE: We have to do this beforehand, barring more cleverness, because the
  ;; sequence iterator for the output uses LENGTH (i.e. the fill pointer if there
  ;; is one) to decide when to stop, and we have to ignore the fill pointer when
  ;; computing the number of iterations.
  (when (and (vectorp result-sequence)
             (array-has-fill-pointer-p result-sequence))
    (setf (fill-pointer result-sequence)
          (reduce #'min sequences
                  :initial-value (if (vectorp result-sequence)
                                     (array-dimension result-sequence 0)
                                     (length result-sequence))
                  :key #'length)))
  ;; Perform mapping
  (let ((function (coerce-fdesignator function)))
    (declare (type function function) (optimize (safety 0) (speed 3)))
    (sequence:with-sequence-iterator (out-it out-limit nil
                                             out-step out-endp nil out-set)
        (result-sequence)
      (do-sequence-list (elt-list sequences result-sequence)
        (when (funcall out-endp result-sequence out-it out-limit nil)
          (return-from map-into result-sequence))
        (funcall out-set (apply function elt-list) result-sequence out-it)
        (setf out-it (funcall out-step result-sequence out-it nil))))))
