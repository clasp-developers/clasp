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

#+ecl-min
(eval-when (:execute)
  (load (merge-pathnames "seqmacros.lsp" *load-truename*)))

(defun error-not-a-sequence (value)
  (declare (si::c-local))
  (signal-type-error value 'sequence))

(defun error-sequence-index (sequence index)
  (declare (si::c-local))
  (error 'simple-type-error
         :datum index
         :expected-type 'unsigned-byte
         :format-control "Not a valid index ~A into sequence ~A"
         :format-arguments (list index sequence)))

(defun error-sequence-type (type)
  (declare (si::c-local))
  (error 'simple-type-error
	 :datum (vector) ;; Any sequence object will do, because it does not belong to TYPE
	 :expected-type type
	 :format-control "~S does not specify a sequence type"
	 :format-arguments (list type)))

(defun error-sequence-length (object type size)
  (declare (si::c-local))
  (error 'simple-type-error
	 :format-control
	 "Cannot create a sequence of size ~S which matches type ~S."
	 :format-arguments (list size type)
	 :expected-type type
	 :datum object))

(defun closest-sequence-type (type)
  (let (elt-type length name args)
    (cond ((consp type)
	   (setq name (first type) args (cdr type)))
	  ((si::instancep type)
	   (setf name (class-name (truly-the class type)) args nil))
	  (t
	   (setq name type args nil)))
    (case name
      ((LIST)
       ;; This is the only descriptor that does not match a real
       ;; array type.
       (setq elt-type 'LIST length '*))
      ((VECTOR)
       (setq elt-type (if (endp args) 'T (first args))
	     length (if (endp (rest args)) '* (second args))))
      ((SIMPLE-VECTOR)
       (setq elt-type 'T
	     length (if (endp args) '* (first args))))
      #-unicode
      ((STRING SIMPLE-STRING)
       (setq elt-type 'BASE-CHAR
	     length (if (endp args) '* (first args))))
      #+unicode
      ((BASE-STRING SIMPLE-BASE-STRING)
       (setq elt-type 'BASE-CHAR
	     length (if (endp args) '* (first args))))
      #+unicode
      ((STRING SIMPLE-STRING)
       (setq elt-type 'CHARACTER
	     length (if (endp args) '* (first args))))
      ((BIT-VECTOR SIMPLE-BIT-VECTOR)
       (setq elt-type 'BIT
	     length (if (endp args) '* (first args))))
      ((ARRAY SIMPLE-ARRAY)
       (when (or (endp (rest args))
		 (atom (setq length (second args)))
		 (endp length)
		 (not (endp (rest length))))
	 (error-sequence-type type))
       (setq elt-type (upgraded-array-element-type (first args))
	     length (first (second args))))
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
                         +upgraded-array-element-types+))
		(if (subtypep type 'vector)
		    ;; Does this have to be a type-error?
		    ;; 17.3 for MAKE-SEQUENCE says it should be an error,
		    ;; but does not specialize what kind.
		    (error "Cannot find the element type in vector type ~S" type)
		    (error-sequence-type type)))
	  (when (subtypep type (car i))
	    (setq elt-type (cdr i) length '*)
	    ;; The (NIL . NIL) case above
	    (unless elt-type
	      (error-sequence-type type))
	    (return)))))
    (values elt-type length)))

(defun make-sequence (type size	&key (initial-element nil iesp) &aux sequence)
  "Args: (type length &key initial-element)
Creates and returns a sequence of the given TYPE and LENGTH.  If INITIAL-
ELEMENT is given, then it becomes the elements of the created sequence.  The
default value of INITIAL-ELEMENT depends on TYPE."
  (multiple-value-bind (element-type length)
      (closest-sequence-type type)
    (cond ((eq element-type 'LIST)
	   (setq sequence (make-list size :initial-element initial-element))
	   (unless (subtypep 'LIST type)
	     (when (or (and (subtypep type 'NULL) (plusp size))
		       (and (subtypep type 'CONS) (zerop size)))
	       (error-sequence-length (make-list size :initial-element initial-element) type 0))))
	  (t
	   (setq sequence (sys:make-vector (if (eq element-type '*) T element-type)
					   size nil nil nil nil))
	   (when iesp
	     (si::fill-array-with-elt sequence initial-element 0 nil))
	   (unless (or (eql length '*) (eql length size))
	     (error-sequence-length sequence type size))))
    sequence))

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
                  (and (< start (length (truly-the vector sequence)))
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
      (aref (truly-the vector sequence) iterator)
      (car (truly-the cons iterator))))

(defun seq-iterator-set (sequence iterator value)
  (declare (optimize (safety 0)))
  (if (si::fixnump iterator)
      (setf (aref (truly-the vector sequence) iterator) value)
      (setf (car (truly-the cons iterator)) value)))

(defun seq-iterator-next (sequence iterator)
  (declare (optimize (safety 0)))
  (cond ((fixnump iterator)
         (let ((aux (1+ iterator)))
           (declare (fixnum aux))
           (and (< aux (length (truly-the vector sequence)))
                aux)))
        ((atom iterator)
         (error-not-a-sequence iterator))
        (t
         (setf iterator (cdr (truly-the cons iterator)))
         (unless (listp iterator)
           (error-not-a-sequence iterator))
         iterator)))

(defun seq-iterator-list-pop (values-list seq-list iterator-list)
  (declare (optimize (safety 0)))
  (do* ((it-list iterator-list)
        (v-list values-list))
       ((null v-list)
        values-list)
    (let* ((it (cons-car it-list))
           (sequence (cons-car seq-list)))
      (cond ((null it)
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
	     (j 0 (truly-the index (1+ j))))
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
	 ((null j))
      (seq-iterator-set output i (seq-iterator-ref s j))
      (setq i (seq-iterator-next output i)))))


(defun map (result-type function sequence &rest more-sequences)
  "Args: (type function sequence &rest more-sequences)
Creates and returns a sequence of TYPE with K elements, with the N-th element
being the value of applying FUNCTION to the N-th elements of the given
SEQUENCEs, where K is the minimum length of the given SEQUENCEs."
  (let* ((sequences (list* sequence more-sequences))
         (function (si::coerce-to-function function))
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

#|
(def-seq-bool-parser notany
  "Args: (predicate sequence &rest more-sequences)
Returns T if none of the elements in SEQUENCEs satisfies PREDICATE; NIL
otherwise."
  (when that-value (return nil))
  t)

(def-seq-bool-parser notevery
  "Args: (predicate sequence &rest more-sequences)
Returns T if at least one of the elements in SEQUENCEs does not satisfy
PREDICATE; NIL otherwise."
  (unless that-value (return t))
  nil)
|#

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
        ((null ir) result-sequence)
      (do ((i it (cdr i))
	   (v val (cdr v))
           (s sequences (cdr s)))
	  ((null i))
	(unless (car i) (return-from map-into result-sequence))
	(rplaca v (seq-iterator-ref (car s) (car i)))
	(rplaca i (seq-iterator-next (car s) (car i))))
      (seq-iterator-set result-sequence ir (apply function val)))))
