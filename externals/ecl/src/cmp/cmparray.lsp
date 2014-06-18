;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPARRAY. Optimizations related to arrays

;;;;  Copyright (c) 2008. Juan Jose Garcia-Ripol
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

(defun valid-array-index-p (x)
  (typep x 'ext:array-index))

;;;
;;; MAKE-ARRAY
;;;

(defun guess-array-element-type (element-type)
  (if (and (setf element-type (extract-constant-value element-type))
           (known-type-p element-type))
      (upgraded-array-element-type element-type)
      '*))

(defun guess-array-dimensions-type (orig-dimensions &aux dimensions)
  (and (consp orig-dimensions)
       (eq (first dimensions) 'LIST)
       (let ((l (list-length orig-dimensions)))
         (when (and l (< -1 l array-rank-limit))
           (return-from guess-array-dimensions-type
             (make-list (1- l) :initial-element '*)))))
  (let ((dimensions (extract-constant-value orig-dimensions :failed)))
    (cond ((eq dimensions ':failed)
           '*)
          ((valid-array-index-p dimensions)
           (list dimensions))
          ((and (listp dimensions)
                (let ((rank (list-length dimensions)))
                  (or (numberp rank)
                      (< -1 rank array-rank-limit)
                      (every #'valid-array-index dimensions))))
           dimensions)
          (t
           (cmpwarn "The first argument to MAKE-ARRAY~%~A~%is not a valid set of dimensions" orig-dimensions)
           '*))))

(define-compiler-macro make-array (&whole form dimensions &key (element-type t)
					  (initial-element nil initial-element-supplied-p)
					  (initial-contents nil initial-contents-supplied-p)
					  adjustable fill-pointer
					  displaced-to (displaced-index-offset 0)
					  &environment env)
  ;; This optimization is always done unless we provide content. There
  ;; is no speed, debug or space reason not to do it, unless the user
  ;; specifies not to inline MAKE-ARRAY, but in that case the compiler
  ;; macro should not be used.
  (let* ((dimensions-type (guess-array-dimensions-type dimensions))
         (guessed-element-type (guess-array-element-type element-type)))
    (unless initial-contents-supplied-p
      ;; If the type is known and we can assume it will not change, we
      ;; replace it with the upgraded form.
      (unless (eq guessed-element-type '*)
        (setf element-type `',guessed-element-type))
      ;; Now we choose between making a vector or making a general array.
      ;; It only saves some time, since MAKE-PURE-ARRAY will call MAKE-VECTOR
      ;; if a one-dimensional array is to be created.
      (let ((function 'si::make-pure-array))
        (when (and (listp dimensions-type)
                   (null (rest dimensions-type))
                   (integerp (first dimensions-type)))
          (setf function 'si::make-vector
                dimensions (first dimensions-type)))
        (setf form
              `(,function ,element-type ,dimensions ,adjustable ,fill-pointer
                          ,displaced-to ,displaced-index-offset)))
      ;; Then we may fill the array with a given value
      (when initial-element-supplied-p
        (setf form `(si::fill-array-with-elt ,form ,initial-element 0 nil)))
      (setf form `(truly-the (array ,guessed-element-type ,dimensions-type)
                    ,form))))
  form)

;;;
;;; VECTOR-PUSH and VECTOR-PUSH-EXTEND
;;;

(defun expand-vector-push (whole env extend)
  (declare (si::c-local))
  (let* ((args (rest whole)))
    (with-clean-symbols (value vector index dimension)
      (unless (or (eq (first args) 'value) ; No infinite recursion
                  (not (policy-open-code-aref/aset)))
        (setf whole
              `(let* ((value ,(car args))
                      (vector ,(second args)))
                 (declare (:read-only value vector)
                          (optimize (safety 0)))
		 (optional-type-assertion vector vector)
                 (let ((index (fill-pointer vector))
                       (dimension (array-total-size vector)))
                   (declare (fixnum index dimension)
                            (:read-only index dimension))
                   (cond ((< index dimension)
                          (sys::fill-pointer-set vector (truly-the fixnum (+ 1 index)))
                          (sys::aset vector index value)
                          index)
                         (t ,(if extend
                               `(vector-push-extend value vector ,@(cddr args))
                               nil)))))))))
  whole)

(define-compiler-macro vector-push (&whole whole &rest args &environment env)
  (expand-vector-push whole env nil))

(define-compiler-macro vector-push-extend (&whole whole &rest args &environment env)
  (expand-vector-push whole env t))

;;;
;;; AREF/ASET
;;;

(define-compiler-macro aref (&whole form array &rest indices &environment env)
  (if (policy-open-code-aref/aset env)
      (expand-aref array indices env)
      form))

(defun expand-aref (array indices env)
  (with-clean-symbols (%array)
    `(let ((%array ,array))
       (declare (:read-only %array)
                (optimize (safety 0)))
       (row-major-aref %array
                       ,(expand-row-major-index '%array indices env)))))

(define-compiler-macro si::aset (&whole form array &rest indices-and-value
                                        &environment env)
  (cond ((null indices-and-value)
	 (cmpwarn "Too few arguments to SI::ASET form~%~4I~A"
		  form)
	 form)
	((policy-open-code-aref/aset env)
	 (let* ((indices (butlast indices-and-value))
		(value (first (last indices-and-value))))
	   (expand-aset array indices value env)))
	(t
	 form)))

(defun expand-aset (array indices value env)
  (ext:with-unique-names (%array)
    `(let* ((,%array ,array))
       (declare (:read-only ,%array)
                (optimize (safety 0)))
       (si::row-major-aset ,%array ,(expand-row-major-index %array indices env) ,value))))

(define-compiler-macro array-row-major-index (&whole form array &rest indices &environment env)
  (if (policy-open-code-aref/aset env)
      (with-clean-symbols (%array)
	`(let ((%array ,array))
	   (declare (:read-only %array)
		    (optimize (safety 0)))
	   ,(expand-row-major-index '%array indices env)))
      form))

(defun expand-zero-dim-index-check (a env)
  (if (policy-type-assertions env)
      `(progn
         (optional-type-assertion ,a array)
         (check-expected-rank ,a 0)
         0)
      0))

(defun expand-vector-index-check (a index env)
  (flet ((expansion (a index)
           `(progn
              (optional-type-assertion ,a vector)
              (check-vector-in-bounds ,a ,index)
              ,index)))
    (if (policy-type-assertions env)
	(with-clean-symbols (%array-index)
	  `(let ((%array-index ,index))
	     (declare (:read-only %array-index))
	     ,(expansion a '%array-index)))
        index)))

(defun expand-row-major-index (a indices env)
  (when (null indices)
    (return-from expand-row-major-index
      (expand-zero-dim-index-check a env)))
  (when (null (rest indices))
    (return-from expand-row-major-index
      (expand-vector-index-check a (first indices) env)))
  (let* ((expected-rank (length indices))
         (check (policy-array-bounds-check env))
	 (dims (loop for i from 0
		  for index in indices
		  collect `(,(gentemp "DIM") (array-dimension-fast ,a ,i))))
	 (dim-names (mapcar #'first dims)))
    (with-clean-symbols (%ndx-var %output-var %dim-var)
      `(let* (,@dims
	      (%output-var 0))
         (declare (type ext:array-index %output-var ,@dim-names)
		  (ignorable ,@dim-names))
         ,@(when (policy-type-assertions env)
                 `((optional-type-assertion ,a array)
                   (check-expected-rank ,a ,expected-rank)))
	 ,@(loop for i from 0
	      for l in indices
	      for index in indices
	      for dim-var in dim-names
	      when (plusp i)
	      collect `(setf %output-var
			     (truly-the ext:array-index (* %output-var ,dim-var)))
	      collect `(let ((%ndx-var ,index))
			 (declare (ext:array-index %ndx-var))
			 ,(and check `(check-index-in-bounds ,a %ndx-var ,dim-var))
			 (setf %output-var
			       (truly-the ext:array-index (+ %output-var %ndx-var)))))
         %output-var))))

;(trace c::expand-row-major-index c::expand-aset c::expand-aref)

(defmacro check-expected-rank (a expected-rank)
  `(c-inline
    (,a ,expected-rank) (:object :fixnum) :void
    "if (ecl_unlikely((#0)->array.rank != (#1)))
            FEwrong_dimensions(#0,#1);"
    :one-liner nil))

(defmacro check-index-in-bounds (array index limit)
  `(c-inline
    (,array ,index ,limit) (:object :fixnum :fixnum) :void
    "if (ecl_unlikely((#1)>=(#2)))
           FEwrong_index(ECL_NIL,#0,-1,ecl_make_fixnum(#1),#2);"
    :one-liner nil))

(defmacro check-vector-in-bounds (vector index)
  `(c-inline
    (,vector ,index) (:object :fixnum) :void
    "if (ecl_unlikely((#1)>=(#0)->vector.dim))
           FEwrong_index(ECL_NIL,#0,-1,ecl_make_fixnum(#1),(#0)->vector.dim);"
    :one-liner nil))

(defconstant +array-dimension-accessor+
  '#.(loop for i from 0 below array-rank-limit
       collect (format nil "(#0)->array.dims[~D]" i)))

(defun array-dimension-accessor (array n)
  (let ((tails #.(apply 'vector
                   (loop for i from 0 below array-rank-limit
                      for c-code = (format nil "(#0)->array.dims[~D]" i)
                      collect `((:object) :fixnum ,c-code :one-liner t
                                :side-effects nil)))))
    `(c-inline (,array) ,@(aref tails n))))

(defmacro array-dimension-fast (array n)
  (if (typep n '(integer 0 #.(1- array-rank-limit)))
      (array-dimension-accessor array n)
      (error "In macro ARRAY-DIMENSION-FAST, the index is not a constant integer: ~A"
             n)))
