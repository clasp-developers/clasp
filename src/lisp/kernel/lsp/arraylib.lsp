;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;    arraylib.lsp
;;;;
;;;;                            array routines

(in-package "SYSTEM")

(defun make-array (dimensions
                   &key (element-type t)
                     (initial-element nil initial-element-supplied-p)
                     (initial-contents nil initial-contents-supplied-p)
                     adjustable fill-pointer
                     displaced-to
                     (displaced-index-offset 0))
  "Args: (dimensions &key (element-type t) initial-element (initial-contents nil)
		    (adjustable nil) (fill-pointer nil) (displaced-to nil)
		    (displaced-index-offset 0) (static nil))
Creates an array of the specified DIMENSIONS.  DIMENSIONS is a list of
non-negative integers each representing the length of the corresponding
dimension.  It may be an integer for vectors, i.e., one-dimensional arrays.
ELEMENT-TYPE specifies the type of array elements.  INITIAL-ELEMENT specifies
the initial value for all elements.  Its default value depends on ELEMENT-
TYPE.  INITIAL-CONTENTS specifies each element in terms of sequences.
ADJUSTABLE specifies whether or not the array is adjustable (see ADJUST-
ARRAY).  FILL-POINTER is meaningful only for vectors.  It specifies whether
the vector has fill-pointer or not, and if it has, the initial value of the
fill-pointer.  Possible values are NIL (no fill-pointer), T (the length of the
vector), or an integer.  See VECTOR-PUSH and VECTOR-POP.  DISPLACED-TO, if
non-NIL, must be an array and specifies that the new array is displaced to the
given array.  DISPLACED-INDEX-OFFSET is meaningful only when DISPLACED-TO is
non-NIL and specifies that the reference to the I-th element of the new array
in raw-major indexing is actually the reference to the (I + DISPLACED-INDEX-
OFFSET)th element of the given array.If the STATIC argument is supplied
with a non-nil value, then the body of the array is allocated as a
contiguous block."
  (let ((x (make-array-with-initial-element dimensions
                                            (upgraded-array-element-type element-type)
                                            initial-element
                                            initial-element-supplied-p
                                            adjustable
                                            fill-pointer
                                            displaced-to
                                            displaced-index-offset)))
    (declare (array x))
    (cond (initial-element-supplied-p
           (when initial-contents-supplied-p
             (error "MAKE-ARRAY: Cannot supply both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))
           #| initial element was filled on construction|#)
          (initial-contents-supplied-p
           (fill-array-with-seq x initial-contents)))
    x))

(defun make-array-with-initial-element (dimensions
                                        upgraded-element-type
                                        initial-element
                                        initial-element-supplied-p
                                        adjustable
                                        fill-pointer
                                        displaced-to
                                        &optional
                                          (displaced-index-offset 0))
  (when displaced-to
    (if (let ((array-element-type (array-element-type displaced-to)))
          (not (and (subtypep array-element-type upgraded-element-type)
                    (subtypep upgraded-element-type array-element-type))))
        (error "Cannot displace the array, because the element types don't match"))
    (when (< (array-total-size displaced-to) displaced-index-offset)
      (error "Cannot displace the array, because the total size of the displaced-to-array ~s is too small for displaced-index-offset ~a."
             displaced-to displaced-index-offset)))
  (cond
    ((null dimensions)
     (make-mdarray dimensions upgraded-element-type adjustable displaced-to displaced-index-offset
                   initial-element initial-element-supplied-p))
    ((or (fixnump dimensions) (and (consp dimensions) (eql 1 (length dimensions))))
     (let ((dim (if (fixnump dimensions)
		    dimensions
		    (car dimensions))))
       (let ((x (make-vector upgraded-element-type dim adjustable fill-pointer displaced-to displaced-index-offset
                             initial-element initial-element-supplied-p)))
         (when (and displaced-to initial-element-supplied-p)
           (fill-array-with-elt x initial-element 0 nil))
         x)))
    ((listp dimensions)
     (when fill-pointer (error "Multi-dimensional arrays don't allow fill-pointer"))
     (let ((x (make-mdarray dimensions
                            upgraded-element-type
                            adjustable
                            displaced-to
                            displaced-index-offset
                            initial-element
                            initial-element-supplied-p)))
       (when (and displaced-to initial-element-supplied-p)
         (fill-array-with-elt x initial-element 0 nil))
       x))
    (t (error "Illegal dimensions ~a for make-array" dimensions ))))

(defun fill-array-with-seq (array initial-contents)
  (declare (array array)
           (sequence initial-contents)
           (optimize (safety 0)))
  (labels ((iterate-over-contents (array contents dims written)
	     (declare (fixnum written)
		      (array array)
		      (optimize (safety 0)))
	     (when (/= (length contents) (first dims))
	       (error "In MAKE-ARRAY: the elements in :INITIAL-CONTENTS do not match the array dimensions"))
	     (if (= (length dims) 1)
		 (do* ((it (make-seq-iterator contents) (seq-iterator-next contents it)))
		      ((null it))
		   (sys:row-major-aset array written (seq-iterator-ref contents it))
		   (incf written))
		 (do* ((it (make-seq-iterator contents) (seq-iterator-next contents it)))
		      ((null it))
		   (setf written (iterate-over-contents array
							(seq-iterator-ref contents it)
							(rest dims)
							written))))
	     written))
    (let ((dims (array-dimensions array)))
      (if dims
	  (iterate-over-contents array initial-contents dims 0)
	  (setf (aref array) initial-contents))))
  array)

(defun array-in-bounds-p (array &rest indices)
  "Args: (array &rest indexes)
Returns T if INDEXes are valid indexes of ARRAY; NIL otherwise.  The number of
INDEXes must be equal to the rank of ARRAY."
  (declare (type array array)
           (optimize (safety 0))
           #+(or)(ext:check-arguments-type))
  (do* ((indices indices (cons-cdr indices))
        (r (array-rank array))
        (i 0 (1+ i)))
       ((>= i r) t)
    (declare (type index r i))
    (if indices
	(let* ((index (cons-car indices)))
	  (when (or (not (si::fixnump index))
		    (minusp (the fixnum index))
		    (>= (the fixnum index) (array-dimension array i)))
	    (return nil)))
	(error "The rank of the array is ~R,~%~
               ~7@Tbut ~R ~:*~[indices are~;index is~:;indices are~] ~
               supplied."
                 r i))))

(defun row-major-index-inner (array indices)
  (declare (optimize speed)
           (array array))
  (flet ((indexing-error (array indices)
           (error "Not valid index or indices~%~A~%into array~%~A" indices array)))
    (do* ((r (array-rank array))
          (i 0 (1+ i))
          (j 0)
          (s indices (cons-cdr s)))
         ((null s)
          (when (< i r)
            (indexing-error array indices))
          j)
      (declare (ext:array-index j)
               (fixnum i r))
      (let* ((d (array-dimension array i))
             (o (cons-car s))
             ndx)
        (declare (ext:array-index ndx))
        (unless (and (typep o 'fixnum)
                     (<= 0 (setf ndx o))
                     (< ndx (array-dimension array i)))
          (indexing-error array indices))
        (setf j (* j d)
              j (+ j ndx))))))

(defun bit (bit-array &rest indices)
  "Args: (bit-array &rest indexes)
Returns the bit of BIT-ARRAY specified by INDEXes."
  (declare #+ecl (array bit-array) ;; FIXME! Should be (simple-array bit)
           #+clasp (type (simple-array bit) bit-array)
           (ext:check-arguments-type))
  #+(not clasp-min)
  (check-type bit-array (array bit))
  (row-major-aref bit-array (row-major-index-inner bit-array indices)))

(defun sbit (bit-array &rest indices)
  "Args: (simple-bit-array &rest subscripts)
Returns the specified bit in SIMPLE-BIT-ARRAY."
  (declare #+ecl (array bit-array) ;; FIXME! Should be (simple-array bit)
           #+clasp (type (simple-array bit) bit-array)
           (ext:check-arguments-type))
  #+(not clasp-min)
  (check-type bit-array (simple-array bit))
  (row-major-aref bit-array (row-major-index-inner bit-array indices)))

(defun handle-empty-bitarrray (empty-bitarray-1 empty-bitarray-2 result-bit-array operation)
  (let* ((bit1 (row-major-aref empty-bitarray-1 0))
         (bit2 (if empty-bitarray-2
                   (row-major-aref empty-bitarray-2 0)
                   nil))
         (initial-element
          (ecase operation
            (bit-not (if (= bit1 0)
                         1
                         0))
            (bit-and (if (or (= bit1 0)(= bit2 0))
                         0
                         1))
            (bit-ior (if (and (= bit1 0)(= bit2 0))
                         0
                         1))
            (bit-xor (if (or (and (= bit1 0)(= bit2 0))
                             (and (= bit1 1)(= bit2 1)))
                         0
                         1))
            (bit-eqv (if (or (and (= bit1 0)(= bit2 0))
                             (and (= bit1 1)(= bit2 1)))
                         1
                         0))
            (bit-nand (if (and (= bit1 1)(= bit2 1))
                          0
                          1))
            (bit-nor (if (and (= bit1 0)(= bit2 0))
                         1
                         0))
            (bit-andc1 (if (and (= bit1 0)(= bit2 1))
                           1
                           0))
            (bit-andc2 (if (and (= bit1 1)(= bit2 0))
                           1
                           0))
            (bit-orc1 (if (and (= bit1 1)(= bit2 0))
                          0
                          1))
            (bit-orc2 (if (and (= bit1 0)(= bit2 1))
                          0
                          1))
            )))
    (cond ((null result-bit-array)
           (make-array  nil :element-type 'bit :initial-element initial-element))
          ((eql result-bit-array t)
           (setf (row-major-aref empty-bitarray-1 0) initial-element)
           empty-bitarray-1)
          ((and (typep result-bit-array '(SIMPLE-ARRAY BIT NIL))
                (zerop (array-rank result-bit-array)))
           (setf (row-major-aref result-bit-array 0) initial-element)
           result-bit-array)
          (t (error 'SIMPLE-TYPE-ERROR
                    :DATUM result-bit-array
                    :EXPECTED-TYPE '(SIMPLE-ARRAY BIT NIL)
                    :FORMAT-CONTROL "Bad argument ~S to bit operation ~S"
                    :FORMAT-ARGUMENTS (list result-bit-array operation))))))

(defmacro generate-bit-array-function (bit-array1 bit-array2 bit-operation boole-operation &optional result-bit-array)
  `(if (and (zerop (array-rank ,bit-array1)) (zerop (array-rank ,bit-array2)))
      (handle-empty-bitarrray ,bit-array1 ,bit-array2 ,result-bit-array (quote ,bit-operation))
      (bit-array-op ,boole-operation ,bit-array1 ,bit-array2 ,result-bit-array)))

(defun bit-and (bit-array1 bit-array2 &optional result-bit-array)
  "Args: (bit-array1 bit-array2 &optional (result nil))
Returns the element-wise AND of BIT-ARRAY1 and BIT-ARRAY2.  Puts the results
into a new bit-array if RESULT is NIL, into BIT-ARRAY1 if RESULT is T, or into
RESULT if RESULT is a bit-array."
  (generate-bit-array-function bit-array1 bit-array2 bit-and boole-and result-bit-array))

(defun bit-ior (bit-array1 bit-array2 &optional result-bit-array)
  "Args: (bit-array1 bit-array2 &optional (result nil))
Returns the element-wise INCLUSIVE OR of BIT-ARRAY1 and BIT-ARRAY2.  Puts the
results into a new bit-array if RESULT is NIL, into BIT-ARRAY1 if RESULT is T,
or into RESULT if RESULT is a bit-array."
  (generate-bit-array-function bit-array1 bit-array2 bit-ior boole-ior result-bit-array))

(defun bit-xor (bit-array1 bit-array2 &optional result-bit-array)
  "Args: (bit-array1 bit-array2 &optional (result nil))
Returns the element-wise EXCLUSIVE OR of BIT-ARRAY1 and BIT-ARRAY2.  Puts the
results into a new bit-array if RESULT is NIL, into BIT-ARRAY1 if RESULT is T,
or into RESULT if RESULT is a bit-array."
  (generate-bit-array-function bit-array1 bit-array2 bit-xor boole-xor result-bit-array))

(defun bit-eqv (bit-array1 bit-array2 &optional result-bit-array)
  "Args: (bit-array1 bit-array2 &optional (result nil))
Returns the element-wise EQUIVALENCE of BIT-ARRAY1 and BIT-ARRAY2.  Puts the
results into a new bit-array if RESULT is NIL, into BIT-ARRAY1 if RESULT is T,
or into RESULT if RESULT is a bit-array."
  (generate-bit-array-function bit-array1 bit-array2 bit-eqv boole-eqv result-bit-array))

(defun bit-nand (bit-array1 bit-array2 &optional result-bit-array)
  "Args: (bit-array1 bit-array2 &optional (result nil))
Returns the element-wise NOT of {the element-wise AND of BIT-ARRAY1 and BIT-
ARRAY2}.  Puts the results into a new bit-array if RESULT is NIL, into BIT-
ARRAY1 if RESULT is T, or into RESULT if RESULT is a bit-array."
  (generate-bit-array-function bit-array1 bit-array2 bit-nand boole-nand result-bit-array))

(defun bit-nor (bit-array1 bit-array2 &optional result-bit-array)
  "Args: (bit-array1 bit-array2 &optional (result nil))
Returns the element-wise NOT of {the element-wise INCLUSIVE OR of BIT-ARRAY1
and BIT-ARRAY2}.  Puts the results into a new bit-array if RESULT is NIL, into
BIT-ARRAY1 if RESULT is T, or into RESULT if RESULT is a bit-array."
  (generate-bit-array-function bit-array1 bit-array2 bit-nor boole-nor result-bit-array))

(defun bit-andc1 (bit-array1 bit-array2 &optional result-bit-array)
  "Args: (bit-array1 bit-array2 &optional (result nil))
Returns the element-wise AND of {the element-wise NOT of BIT-ARRAY1} and BIT-
ARRAY2.  Puts the results into a new bit-array if RESULT is NIL, into BIT-
ARRAY1 if RESULT is T, or into RESULT if RESULT is a bit-array."
  (generate-bit-array-function bit-array1 bit-array2 bit-andc1 boole-andc1 result-bit-array))

(defun bit-andc2 (bit-array1 bit-array2 &optional result-bit-array)
  "Args: (bit-array1 bit-array2 &optional (result nil))
Returns the element-wise AND of BIT-ARRAY1 and {the element-wise NOT of BIT-
ARRAY2}.  Puts the results into a new bit-array if RESULT is NIL, into BIT-
ARRAY1 if RESULT is T, or into RESULT if RESULT is a bit-array."
  (generate-bit-array-function bit-array1 bit-array2 bit-andc2 boole-andc2 result-bit-array))

(defun bit-orc1 (bit-array1 bit-array2 &optional result-bit-array)
  "Args: (bit-array1 bit-array2 &optional (result nil))
Returns the element-wise INCLUSIVE OR of {the element-wise NOT of BIT-ARRAY1}
and BIT-ARRAY2.  Puts the results into a new bit-array if RESULT is NIL, into
BIT-ARRAY1 if RESULT is T, or into RESULT if RESULT is a bit-array."
  (generate-bit-array-function bit-array1 bit-array2 bit-orc1 boole-orc1 result-bit-array))

(defun bit-orc2 (bit-array1 bit-array2 &optional result-bit-array)
  "Args: (bit-array1 bit-array2 &optional (result nil))
Returns the element-wise INCLUSIVE OR of BIT-ARRAY1 and {the element-wise NOT
of BIT-ARRAY2}.  Puts the results into a new bit-array if RESULT is NIL, into
BIT-ARRAY1 if RESULT is T, or into RESULT if RESULT is a bit-array."
  (generate-bit-array-function bit-array1 bit-array2 bit-orc2 boole-orc2 result-bit-array))

(defun bit-not (bit-array &optional result-bit-array)
  "Args: (bit-array &optional (result nil))
Returns the element-wise NOT of BIT-ARRAY.  Puts the results into a new bit-
array if RESULT is NIL, into BIT-ARRAY if RESULT is T, or into RESULT if
RESULT is a bit-array."
  (if (zerop (array-rank bit-array))
      (handle-empty-bitarrray bit-array nil result-bit-array 'bit-not)
      (bit-array-op boole-c1 bit-array bit-array result-bit-array)))

(defun vector-pop (vector)
  "Args: (vector)
Decrements the fill-pointer of VECTOR by one and returns the element pointed
to by the new fill-pointer.  Signals an error if the old value of the fill-
pointer is 0 already."
  ;; FILL-POINTER asserts vector is a vector and has fill pointer
  (let* ((fp (fill-pointer vector))
         (vector (the vector vector)))
    (declare (ext:array-index fp)
             (optimize (safety 0)))
    (when (zerop fp)
      (error "The fill pointer of the vector ~S zero." vector))
    (sys:fill-pointer-set vector (decf fp))
    (aref vector fp)))

(defun copy-array-contents (dest orig)
  (declare (array dest orig)
	   (optimize (safety 0)))
  (labels
      ((do-copy (dest orig dims1 dims2 start1 start2)
	 (declare (array dest orig)
		  (list dims1 dims2)
		  (ext:array-index start1 start2))
	 (let* ((d1 (pop dims1))
		(d2 (pop dims2))
		(l (min d1 d2))
		(i1 start1)
		(i2 start2))
	   (declare (ext:array-index d1 d2 l i1 i2))
	   (if (null dims1)
	       (copy-subarray dest i1 orig i2 l)
	       (let ((step1 (apply #'* dims1))
		     (step2 (apply #'* dims2)))
		 (declare (ext:array-index step1 step2))
		 (dotimes (i l)
		   (declare (ext:array-index i))
		   (do-copy dest orig dims1 dims2 i1 i2)
		   (incf i1 step1)
		   (incf i2 step2)))))))
    ;; We have to lie to DO-COPY reshaping the zero-dimensional array
    ;; as a one-dimensional array of one element.
    (do-copy dest orig (or (array-dimensions dest) '(1))
	               (or (array-dimensions orig) '(1))
		       0 0)))

(defun adjust-array (array new-dimensions
                     &rest r
		     &key (element-type (array-element-type array))
			  initial-element
			  initial-contents
			  fill-pointer
			  displaced-to
			  displaced-index-offset)
  "Args: (array dimensions
       &key (element-type (array-element-type array))
            initial-element (initial-contents nil) (fill-pointer nil)
            (displaced-to nil) (displaced-index-offset 0))
Adjusts the dimensions of ARRAY to the given DIMENSIONS.  ARRAY -- kpoeck -- may be an
adjustable array."
  (declare (ignore initial-element
                   displaced-index-offset))
  (when (integerp new-dimensions)
        (setq new-dimensions (list new-dimensions)))
  ;; FILL-POINTER = NIL means use the old value of the fill pointer
  ;; Cannot set a fill pointer for an array that does not have any.
  (if (array-has-fill-pointer-p array)
      (unless fill-pointer
	(setf r (list* :fill-pointer (fill-pointer array) r)))
      (when fill-pointer
	(error 'simple-type-error
	       :datum array
	       :expected-type '(satisfies array-has-fill-pointer-p)
	       :format-control "You supplied a fill pointer for an array without it.")))
  (let ((x (apply #'make-array new-dimensions :adjustable t :element-type element-type r)))
    (declare (array x))
    (unless (or displaced-to initial-contents)
      (copy-array-contents x array))
    ;Kpoeck
    (if (adjustable-array-p array)
        (sys:replace-array array x)
        x)))

;;; Copied from cmuci-compat.lisp of CLSQL by Kevin M. Rosenberg (LLGPL-licensed)
(defun shrink-vector (vec len)
  "Shrinks a vector."
  (cond ((adjustable-array-p vec)
	 (adjust-array vec len))
	((typep vec 'simple-array)
	 (let ((new-vec (make-array len :element-type (array-element-type vec))))
	   (copy-subarray new-vec 0 vec 0 len)))
	((typep vec 'vector)
	 (setf (fill-pointer vec) len)
	 vec)
	(t
	 (error "Unable to shrink vector ~S which is type-of ~S" vec (type-of vec)))
	))
