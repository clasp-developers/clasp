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
  (multiple-value-bind (element-type length success)
      (closest-sequence-type type)
    (cond ((not success)
           (if (subtypep type 'sequence)
               (error "Could not determine element type in ~a" type)
               (error-sequence-type type)))
          ((eq element-type 'LIST)
           (let ((result (make-list size :initial-element initial-element)))
             (if (or (eq length '*) (eql length size))
                 result
                 (error-sequence-length result type size))))
          (t
           (let ((result (sys:make-vector (if (eq element-type '*) 't element-type)
                                          size nil nil nil 0)))
             ;; Don't know why we don't just pass make-vector the initial element.
             (when iesp
               (si::fill-array-with-elt result initial-element 0 nil))
             (unless (or (eql length '*) (eql length size))
               (error-sequence-length result type size))
             result)))))

(defun make-seq-iterator (sequence &optional (start 0))
  (declare (optimize (safety 0)))
  (cond ((fixnump start)
         (let ((aux start))
           (declare (fixnum aux))
           (cond ((minusp aux)
                  (error-sequence-index sequence start))
                 ((listp sequence)
                  (nthcdr aux sequence))
                 ((vectorp sequence)
                  (and (< start (length (the vector sequence)))
                       start))
                 (t
                  (error-not-a-sequence sequence)))))
        ((not (or (listp sequence) (vectorp sequence)))
         (error-not-a-sequence sequence))
        ((integerp start)
         nil)
        (t
         (error-sequence-index sequence start))))

(defun seq-iterator-ref (sequence iterator)
  (declare (optimize (safety 0)))
  (if (si::fixnump iterator)
      (aref (the vector sequence) iterator)
      (car (the cons iterator))))

(defun seq-iterator-set (sequence iterator value)
  (declare (optimize (safety 0)))
  (if (si::fixnump iterator)
      (setf (aref (the vector sequence) iterator) value)
      (setf (car (the cons iterator)) value)))

(defun seq-iterator-next (sequence iterator)
  (declare (optimize (safety 0)))
  (cond ((fixnump iterator)
         (let ((aux (1+ iterator)))
           (declare (fixnum aux))
           (and (< aux (length (the vector sequence)))
                aux)))
        ((atom iterator)
         (error-not-a-sequence iterator))
        (t
         (setf iterator (cdr (the cons iterator)))
         (unless (listp iterator)
           (error-not-a-sequence iterator))
         iterator)))

(declaim (inline seq-iterator-endp))
(defun seq-iterator-endp (sequence iterator)
  (declare (ignore sequence))
  (null iterator))

(defun seq-iterator-list-pop (values-list seq-list iterator-list)
  (declare (optimize (safety 0)))
  (do* ((it-list iterator-list)
        (v-list values-list))
       ((null v-list)
        values-list)
    (let* ((it (cons-car it-list))
           (sequence (cons-car seq-list)))
      (cond ((seq-iterator-endp sequence it)
             (return nil))
            ((fixnump it)
             (let* ((n it) (s sequence))
               (declare (fixnum n) (vector s))
               (rplaca v-list (aref s n))
               (rplaca it-list (and (< (incf n) (length s)) n))))
            ((atom it)
             (error-not-a-sequence it))
            (t
             (rplaca v-list (cons-car it))
             (unless (listp (setf it (cons-cdr it)))
               (error-not-a-sequence it))
             (rplaca it-list it)))
      (setf v-list (cons-cdr v-list)
            it-list (cons-cdr it-list)
            seq-list (cons-cdr seq-list)))))

;;; FIXME: This should be moved. But right now this file is the earliest
;;; use. Also we don't have typecase yet.
;;; This is different from coerce-to-function, which implements the behavior
;;; of (coerce x 'function). If we get a lambda expression here, that's a
;;; type error, not a command to implicitly evaluate it.
(declaim (inline coerce-fdesignator))
(defun coerce-fdesignator (object)
  (cond ((functionp object) object)
        ((symbolp object) (fdefinition object))
        (t (error 'type-error :datum object :expected-type '(or symbol function)))))

(defun coerce-to-list (object)
  (if (listp object)
      object
      (do ((it (make-seq-iterator object) (seq-iterator-next object it))
	   (output nil))
	  ((null it) (nreverse output))
	(push (seq-iterator-ref object it) output))))

(defun coerce-to-vector (object elt-type length simple-array-p)
  (let ((output object))
    (unless (and (vectorp object)
                 (or (null simple-array-p) (simple-array-p object))
		 (eq (array-element-type object) elt-type))
      (let* ((final-length (if (eq length '*) (length object) length)))
	(setf output (make-vector elt-type final-length nil nil nil 0))
	(do ((i (make-seq-iterator object) (seq-iterator-next output i))
	     (j 0 (the index (1+ j))))
	    ((= j final-length)
	     (setf object output))
	  (declare (index j))
	  (setf (aref output j) (seq-iterator-ref object i)))))
    (unless (eq length '*)
      (unless (= length (length output))
	(check-type output `(vector ,elt-type (,length)) "coerced object")))
    output))

(defun concatenate (result-type &rest sequences)
  "Args: (type &rest sequences)
Returns a new sequence of the specified type, consisting of all elements of
SEQUENCEs."
  (do* ((length-list (mapcar #'length sequences) (rest length-list))
	(output (make-sequence result-type (apply #'+ length-list)))
        (sequences sequences (rest sequences))
        (i (make-seq-iterator output)))
      ((null sequences) output)
    (do* ((s (first sequences))
	  (j (make-seq-iterator s) (seq-iterator-next s j)))
	 ((seq-iterator-endp s j))
      (seq-iterator-set output i (seq-iterator-ref s j))
      (setq i (seq-iterator-next output i)))))

;;; This is not called anywhere yet (just used for a compiler macro)
;;; but it might be useful to have.
(defun map-for-effect (function sequence &rest more-sequences)
  "Does (map nil ...)"
  (let ((sequences (list* sequence more-sequences))
        (function (coerce-fdesignator function))
        it)
    (do-sequences (elt-list sequences)
      (apply function elt-list)))
  nil)

(defun map (result-type function sequence &rest more-sequences)
  "Args: (type function sequence &rest more-sequences)
Creates and returns a sequence of TYPE with K elements, with the N-th element
being the value of applying FUNCTION to the N-th elements of the given
SEQUENCEs, where K is the minimum length of the given SEQUENCEs."
  (let* ((sequences (list* sequence more-sequences))
         (function (coerce-fdesignator function))
         output
         it)
    (when result-type
      (let ((l (length sequence)))
        (when more-sequences
          (setf l (reduce #'min more-sequences
                          :initial-value l
                          :key #'length)))
        (setf output (make-sequence result-type l)
              it (make-seq-iterator output))))
    (do-sequences (elt-list sequences :output output)
      (let ((value (apply function elt-list)))
        (when result-type
          (seq-iterator-set output it value)
          (setf it (seq-iterator-next output it)))))))

(defun some (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if at least one of the elements in SEQUENCEs satisfies PREDICATE;
NIL otherwise."
  (reckless
   (do-sequences (elt-list (cons sequence more-sequences) :output nil)
     (let ((x (apply predicate elt-list)))
       (when x (return x))))))

(defun every (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if every elements of SEQUENCEs satisfy PREDICATE; NIL otherwise."
  (reckless
   (do-sequences (elt-list (cons sequence more-sequences) :output t)
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
  (let ((nel (apply #'min (if (vectorp result-sequence)
			      (array-dimension result-sequence 0)
			      (length result-sequence))
		    (mapcar #'length sequences))))
    (declare (fixnum nel))
    ;; Set the fill pointer to the number of iterations
    (when (and (vectorp result-sequence)
	       (array-has-fill-pointer-p result-sequence))
      (setf (fill-pointer result-sequence) nel))
    ;; Perform mapping
    (do ((ir (make-seq-iterator result-sequence) (seq-iterator-next result-sequence ir))
         (it (mapcar #'make-seq-iterator sequences))
         (val (make-sequence 'list (length sequences))))
        ((seq-iterator-endp result-sequence ir) result-sequence)
      (do ((i it (cdr i))
	   (v val (cdr v))
           (s sequences (cdr s)))
	  ((null i))
	(unless (car i) (return-from map-into result-sequence))
	(rplaca v (seq-iterator-ref (car s) (car i)))
	(rplaca i (seq-iterator-next (car s) (car i))))
      (seq-iterator-set result-sequence ir (apply function val)))))
