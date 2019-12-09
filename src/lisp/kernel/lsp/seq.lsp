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
;;; Returns three values: A "kind", a length or '*, and a boolean.
;;; The third value is T if the function could determine the first two.
;;; A "kind" is either the symbol LIST, the symbol NIL,
;;; a list (VECTOR symbol) where symbol is an upgraded array element type,
;;; or a class (a user-defined sequence class).
(defun sequence-type-maker-info (type &optional env)
  (let (elt-type length name args)
    (cond ((consp type)
	   (setq name (first type) args (cdr type)))
	  ((clos::classp type)
	   (setf name (class-name (the class type)) args nil))
	  (t
	   (setq name type args nil)))
    (case name
      ((LIST) (values 'list '* t))
      ((VECTOR)
       (values (list 'vector
                     (if (or (endp args) (eq (first args) '*))
                         't
                         (upgraded-array-element-type (first args) env)))
               (if (endp (rest args)) '* (second args))
               t))
      ((SIMPLE-VECTOR) (values '(vector t) (if (endp args) '* (first args)) t))
      #-unicode
      ((STRING SIMPLE-STRING)
       (values '(vector base-char) (if (endp args) '* (first args)) t))
      #+(or unicode clasp)
      ((BASE-STRING SIMPLE-BASE-STRING)
       (values '(vector base-char) (if (endp args) '* (first args)) t))
      #+(or unicode clasp)
      ((STRING SIMPLE-STRING)
       (values '(vector character) (if (endp args) '* (first args)) t))
      ((BIT-VECTOR SIMPLE-BIT-VECTOR)
       (values '(vector bit) (if (endp args) '* (first args)) t))
      ((ARRAY SIMPLE-ARRAY)
       ;; It's only a sequence if a rank is specified.
       (let ((uaet (if (or (endp args) (eq (first args) '*))
                       't
                       (upgraded-array-element-type (first args) env)))
             (dimension-spec (second args)))
         (cond ((eql dimension-spec 1) (values `(vector ,uaet) '* t))
               ((and (consp dimension-spec)
                     (null (cdr dimension-spec)))
                (values `(vector ,uaet) (car dimension-spec) t))
               (t (values nil nil nil)))))
      ((null) (values 'list 0 t))
      ;; This is pretty dumb, but we are required to signal a length mismatch
      ;; error in safe code, and this case will probably not come up often.
      ((cons)
       (if (and (consp args) (consp (cdr args))) ; we have (cons x y
           (multiple-value-bind (et len success)
               (sequence-type-maker-info (second args) env)
             (cond ((or (not success) ; can't figure out what cdr is
                        (not (eq et 'list))) ; (a . #(...)) is not a sequence
                    (values nil nil nil))
                   ((eq len '*) (values 'list '* t))
                   (t (values 'list (1+ len) t))))
           (values 'list '* t)))
      ((nil) (values nil '* t))
      (t
       ;; Might have a weird to parse vector or list type,
       ;; e.g. incorporating OR/AND/NOT.
       ;; If we make it this far we give up on determining a length.
       (dolist (i '(nil list
                    . #.(mapcar #'(lambda (i) `(VECTOR ,i))
                         sys::+upgraded-array-element-types+)))
         (when (subtypep type i env)
           (return-from sequence-type-maker-info (values i '* t))))
       ;; Might be a user sequence type.
       (when (symbolp type)
         (let ((class (find-class type env)))
           (when class (return-from sequence-type-maker-info
                         (values class '* t)))))
       ;; Dunno.
       (values nil nil nil)))))

(defun make-sequence (type size	&key (initial-element nil iesp))
  "Args: (type length &key initial-element)
Creates and returns a sequence of the given TYPE and LENGTH.  If INITIAL-
ELEMENT is given, then it becomes the elements of the created sequence.  The
default value of INITIAL-ELEMENT depends on TYPE."
  (if (and (integerp size) (>= size 0))
      (multiple-value-bind (kind length success)
          (sequence-type-maker-info type)
        (cond ((not success)
               (if (subtypep type 'sequence)
                   (error "Could not determine how to construct a sequence of type ~a"
                          type)
                   (error-sequence-type type)))
              ((eq kind 'nil)
               (error "Cannot construct sequence of bottom type (NIL)"))
              ((eq kind 'LIST)
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
              ((consp kind) ; (vector uaet)
               (let* ((uaet (second kind))
                      (result (sys:make-vector uaet size)))
                 ;; Don't know why we don't just pass make-vector the initial element.
                 (when iesp
                   (si::fill-array-with-elt result initial-element 0 nil))
                 (unless (or (eql length '*) (eql length size))
                   (error-sequence-length result type size))
                 result))
              ;; Must be a user sequence type.
              (t
               (if iesp
                   (sequence:make-sequence kind size :initial-element initial-element)
                   (sequence:make-sequence kind size)))))
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

;;; the random-access-iterator- functions are also made
;;; available through sequence:define-random-access-iterator,
;;; defined in clos/sequences.lsp.
(defun random-access-iterator-next (seq it from-end)
  (declare (ignore seq from-end)
           (optimize speed (safety 0))
           (type fixnum it))
  (1+ it))
(defun random-access-iterator-prev (seq it from-end)
  (declare (ignore seq from-end)
           (optimize speed (safety 0))
           (type fixnum it))
  (1- it))
(defun random-access-iterator-endp (seq it limit from-end)
  (declare (ignore seq from-end)
           (optimize speed (safety 0))
           (type fixnum it limit))
  (= it limit))
(defun vec-iterator-elt (seq it)
  (declare (optimize speed (safety 0)))
  (aref (the vector seq) it))
(defun (setf vec-iterator-elt) (new seq it)
  (declare (optimize speed (safety 0)))
  (setf (aref (the vector seq) it) new))
(defun random-access-iterator-index (seq it)
  (declare (ignore seq) (optimize speed (safety 0)))
  it)
(defun random-access-iterator-copy (seq it)
  (declare (ignore seq) (optimize speed (safety 0)))
  it)

(defun make-vector-iterator (sequence from-end start end)
  (let* ((end (or end (length sequence)))
         (iterator (if from-end (1- end) start))
         (limit (if from-end (1- start) end)))
    (values iterator limit from-end
            (if from-end
                #'random-access-iterator-prev
                #'random-access-iterator-next)
            #'random-access-iterator-endp
            #'vec-iterator-elt #'(setf vec-iterator-elt)
            #'random-access-iterator-index
            #'random-access-iterator-copy)))

(defun %make-sequence-iterator (sequence from-end start end)
  (cond ((listp sequence)
         (make-list-iterator sequence from-end start end))
        ((vectorp sequence)
         (make-vector-iterator sequence from-end start end))
        (t
         (sequence:make-sequence-iterator
          sequence :from-end from-end :start start :end end))))

;;; Given a list of sequences, return two lists of the same length:
;;; one with irrelevant elements, and one with "iterators" for the sequences.
;;; These "iterators" are single objects,
;;; lists of (iterator limit step endp elt) as returned from
;;; make-sequence-iterator.
;;; Obviously this conses, and a fair bit.
;;; FIXME: We could reduce the impact by special casing lists and vectors.
;;; But it'll be uglier code and I'm hoping we can usually just avoid going
;;; through do-sequence-list entirely.
(defun lists-for-do-sequence-list (sequences)
  (declare (optimize speed (safety 0)))
  (let* ((elts-head (cons nil nil)) (elts-tail elts-head)
         (its-head (cons nil nil)) (its-tail its-head))
    (declare (type cons elts-head elts-tail)
             (type cons its-head its-tail))
    (dolist (seq sequences)
      (let ((new-elts-tail (cons nil nil)))
        (rplacd elts-tail new-elts-tail)
        (setq elts-tail new-elts-tail))
      (sequence:with-sequence-iterator
          (it limit from-end step endp elt)
          (seq)
        (let* ((iterator (list it limit from-end step endp elt))
               (new-its-tail (cons iterator nil)))
          (rplacd its-tail new-its-tail)
          (setq its-tail new-its-tail))))
    (values (cdr elts-head) (cdr its-head))))

;;; Given three lists of the same length, with elements:
;;; 1) irrelevant 2) sequences 3) iterators
;;; If any of the iterations are complete, return NIL.
;;; Otherwise, mutate 1 to contain the current elements of the
;;; iterations, mutate 3 to contain the next iterations,
;;; and return true.
(defun seq-iterator-list-pop (values-list seq-list iterator-list)
  (declare (optimize speed (safety 0)))
  (do ((v-list values-list (cdr v-list))
       (s-list seq-list (cdr s-list))
       (i-list iterator-list (cdr i-list)))
      ((null v-list) t)
    (let ((sequence (cons-car s-list))
          (iterator (cons-car i-list)))
      (destructuring-bind (it limit from-end step endp elt)
          iterator
        (declare (type function step endp elt))
        (when (funcall endp sequence it limit from-end)
          (return nil))
        (rplaca (the cons v-list)
                (funcall elt sequence it))
        (let ((next-it (funcall step sequence it from-end)))
          (rplaca (the cons iterator) next-it))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Some sequence functions
;;;
;;; Most of the nonstandard functions below are intended for compiler
;;; macroexpansions from the standard functions. See cmp/opt-sequence.lsp.
;;;

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

(defun concatenate-into-sequence (seq core:&va-rest sequences)
  ;; vector is assumed to be non complex and have the correct length.
  (reckless
   (sequence:with-sequence-iterator (it limit from-end step nil nil setelt)
       (seq)
     (dovaslist (sequence sequences seq)
       (sequence:dosequence (elt sequence)
         (funcall setelt elt seq it)
         (setq it (funcall step seq it from-end)))))))

(defun concatenate (result-type &rest sequences)
  (declare (dynamic-extent sequences))
  ;; SUBTYPEP is slow, but if you're here you already failed to optimize.
  ;; See compiler macro in cmp/opt-sequence.lsp.
  (if (subtypep result-type 'list)
      (apply #'concatenate-to-list sequences)
      (let* ((lengths-list (mapcar #'length sequences))
             (result (make-sequence result-type (apply #'+ lengths-list))))
        (apply #'concatenate-into-sequence result sequences))))

(defun map-for-effect (function &rest sequences)
  "Does (map nil ...), but the function is already a function."
  (declare (type function function))
  (do-sequence-list (elt-list sequences)
    (reckless (apply function elt-list)))
  nil)

(defun map-for-effect/1 (function sequence)
  "Does (map nil function sequence), but the function is already a function."
  (sequence:dosequence (e sequence nil)
    (reckless (funcall function e))))

(defun map (result-type function sequence &rest more-sequences)
  "Args: (type function sequence &rest more-sequences)
Creates and returns a sequence of TYPE with K elements, with the N-th element
being the value of applying FUNCTION to the N-th elements of the given
SEQUENCEs, where K is the minimum length of the given SEQUENCEs."
  (if result-type
      ;; FIXME: Could use map-to-list and avoid reduce, if appropriate.
      (let ((length
              (reduce #'min more-sequences
                      :initial-value (length sequence)
                      :key #'length)))
        (apply #'map-into (make-sequence result-type length)
               function sequence more-sequences))
      (apply #'map-for-effect function sequence more-sequences)))

(defun map-to-list (function &rest sequences)
  (declare (type function function))
  (reckless
   (let* ((head (cons nil nil)) (tail head))
     (declare (type cons head tail))
     (do-sequence-list (elt-list sequences (cdr head))
       (let ((new (cons (apply function elt-list) nil)))
         (rplacd tail new)
         (setq tail new))))))

(defun map-to-list/1 (function sequence)
  (declare (type function function))
  (reckless
   (let* ((head (cons nil nil)) (tail head))
     (declare (type cons head tail))
     (sequence:dosequence (e sequence (cdr head))
       (let ((new (cons (funcall function e) nil)))
         (rplacd tail new)
         (setq tail new))))))

(defun some (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if at least one of the elements in SEQUENCEs satisfies PREDICATE;
NIL otherwise."
  (reckless
   (do-sequence-list (elt-list (cons sequence more-sequences) nil)
     (let ((x (apply predicate elt-list)))
       (when x (return x))))))

(defun some/1 (predicate sequence)
  (declare (type function predicate))
  (reckless
   (sequence:dosequence (e sequence nil)
     (let ((x (funcall predicate e)))
       (when x (return x))))))

(defun every (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if every elements of SEQUENCEs satisfy PREDICATE; NIL otherwise."
  (reckless
   (do-sequence-list (elt-list (cons sequence more-sequences) t)
     (unless (apply predicate elt-list)
       (return nil)))))

(defun every/1 (predicate sequence)
  (declare (type function predicate))
  (reckless
   (sequence:dosequence (e sequence t)
     (unless (funcall predicate e)
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
    (sequence:with-sequence-iterator (out-it out-limit out-fe
                                             out-step out-endp nil out-set)
        (result-sequence)
      (do-sequence-list (elt-list sequences result-sequence)
        (when (funcall out-endp result-sequence out-it out-limit out-fe)
          (return-from map-into result-sequence))
        (funcall out-set (apply function elt-list) result-sequence out-it)
        (setf out-it (funcall out-step result-sequence out-it out-fe))))))

;;; This is like MAP-INTO with the following specialization:
;;; 1) FUNCTION is actually a function.
;;; 2) If RESULT is a vector,
;;;     we don't need to worry about setting its fill pointer.
;;; 3) RESULT is at least as long as the shortest input sequence.
(defun map-into-sequence (result-sequence function &rest sequences)
  (declare (type function function))
  (reckless
    (sequence:with-sequence-iterator (out-it out-limit out-fe
                                             out-step nil nil out-set)
        (result-sequence)
      (do-sequence-list (elt-list sequences result-sequence)
        (funcall out-set (apply function elt-list) result-sequence out-it)
        (setf out-it (funcall out-step result-sequence out-it out-fe)))))
  result-sequence)

(defun map-into-sequence/1 (result-sequence function sequence)
  (declare (type function function))
  (reckless
    (sequence:with-sequence-iterator (out-it out-limit out-fe
                                             out-step nil nil out-set)
        (result-sequence)
      (sequence:dosequence (e sequence)
        (funcall out-set (funcall function e) result-sequence out-it)
        (setf out-it (funcall out-step result-sequence out-it out-fe)))))
  result-sequence)
