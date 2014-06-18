;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;;  CMPOPT-BITS  -- Optimize operations acting on bits
;;;;

(in-package "COMPILER")

;;;
;;; LDB
;;; Look for inline expansion of LDB1 in sysfun.lsp
;;;

(defun inline-bytespec (bytespec)
  (declare (si::c-local))
  (and (consp bytespec)
       (eq 'BYTE (car bytespec))
       (= (length bytespec) 3)
       (policy-inline-bit-operations)))

(define-compiler-macro ldb (&whole whole bytespec integer)
  (if (inline-bytespec bytespec)
      (with-clean-symbols (%pos %size)
	`(with-let*-type-check ((%size ,(second bytespec) unsigned-byte)
				(%pos ,(third bytespec) unsigned-byte))
	   (logand (lognot (ash -1 %size)) (ash ,integer (- %pos)))))
      whole))

(define-compiler-macro ldb-test (&whole whole bytespec integer)
  (if (inline-bytespec bytespec)
      `(not (zerop (mask-field ,bytespec ,integer)))
      whole))

(define-compiler-macro mask-field (&whole whole bytespec integer)
  (if (inline-bytespec bytespec)
      (with-clean-symbols (%pos %size)
	`(with-let*-type-check ((%size ,(second bytespec) unsigned-byte)
				(%pos ,(third bytespec) unsigned-byte))
	   (logand (ash (lognot (ash -1 %size)) %pos)
		   ,integer)))
      whole))

(define-compiler-macro dpb (&whole whole newbyte bytespec integer)
  (if (inline-bytespec bytespec)
      (with-clean-symbols (%pos %size %mask)
	`(with-let*-type-check ((%size ,(second bytespec) unsigned-byte)
				(%pos ,(third bytespec) unsigned-byte)
				(%mask (ash (lognot (ash -1 %size)) %pos) t))
	     (logior (logand (ash ,newbyte %pos) %mask)
		     (logandc2 ,integer %mask))))
      whole))

(define-compiler-macro deposit-field (&whole whole newbyte bytespec integer)
  (if (inline-bytespec bytespec)
      (with-clean-symbols (%pos %size %mask)
	`(with-let*-type-check ((%size ,(second bytespec) unsigned-byte)
				(%pos ,(third bytespec) unsigned-byte)
				(%mask (ash (lognot (ash -1 %size)) %pos) t))
	     (logior (logand ,newbyte %mask)
		     (logandc2 ,integer %mask)
		     )))
      whole))

(define-setf-expander ldb (&environment env bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion int env)
    (if (inline-bytespec bytespec)
	(let* ((bpos (gensym))
	       (bsize (gensym))
	       (store (gensym))
	       (btemp `(byte ,bpos ,bsize))
	       (stemp (first stores)))
	  (values `(,bpos ,bsize ,@temps)
		  `(,(second bytespec) ,(third bytespec) ,@vals)
		  `(,store)
		  `(let ((,stemp (dpb ,store ,btemp ,access-form)))
		     ,store-form ,store)
		  `(ldb ,btemp ,access-form)))
	(let* ((btemp (gensym))
	       (store (gensym))
	       (stemp (first stores)))
	  (values `(,btemp ,@temps)
		  `(,bytespec ,@vals)
		  `(,store)
		  `(let ((,stemp (dpb ,store ,btemp ,access-form)))
		     ,store-form ,store)
		  `(ldb ,btemp ,access-form))))))

(define-setf-expander mask-field (&environment env bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion int env)
    (if (inline-bytespec bytespec)
	(let* ((bpos (gensym))
	       (bsize (gensym))
	       (store (gensym))
	       (btemp `(byte ,bpos ,bsize))
	       (stemp (first stores)))
	  (values `(,bpos ,bsize ,@temps)
		  `(,(second bytespec) ,(third bytespec) ,@vals)
		  `(,store)
		  `(let ((,stemp (deposit-field ,store ,btemp ,access-form)))
		     ,store-form ,store)
		  `(mask-field ,btemp ,access-form)))
	(let* ((btemp (gensym))
	       (store (gensym))
	       (stemp (first stores)))
	  (values `(,btemp ,@temps)
		  `(,bytespec ,@vals)
		  `(,store)
		  `(let ((,stemp (deposit-field ,store ,btemp ,access-form)))
		     ,store-form ,store)
		  `(mask-field ,btemp ,access-form))))))

;;;
;;; ASH
;;; Bit fiddling. It is a bit tricky because C does not allow
;;; shifts in << or >> which exceed the integer size. In those
;;; cases the compiler may do whatever it wants (and gcc does!)
;;;

(define-compiler-macro ash (&whole whole argument shift)
  (cond ((and (integerp argument)
	      (integerp shift))
	 (ash argument shift))
	((and (policy-assume-right-type)
	      (integerp shift))
	 (if (zerop shift)
	     argument
	     `(shift ,argument ,shift)))
	(t
	 whole)))

(define-c-inliner shift (return-type argument orig-shift)
  (let* ((arg-type (inlined-arg-type argument))
         (arg-c-type (lisp-type->rep-type arg-type))
	 (return-c-type (lisp-type->rep-type return-type))
         (shift (loc-immediate-value (inlined-arg-loc orig-shift))))
    (if (or (not (c-integer-rep-type-p arg-c-type))
            (not (c-integer-rep-type-p return-c-type)))
        (produce-inline-loc (list argument orig-shift) '(:object :fixnum) '(:object)
                            "ecl_ash(#0,#1)" nil t)
        (let* ((arg-bits (c-integer-rep-type-bits arg-c-type))
	       (return-bits (c-integer-rep-type-bits return-c-type))
	       (max-type (if (and (plusp shift)
				  (< arg-bits return-bits))
			   return-c-type
			   arg-c-type)))
	  (produce-inline-loc (list argument) (list max-type) (list return-type)
			      (format nil
				      (if (minusp shift)
					  "((#0) >> (~D))"
					  "((#0) << (~D))")
				      (abs shift))
			      nil t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TYPE PROPAGATION
;;;

(def-type-propagator logand (fname &rest args)
  (values args
	  (if args
	      (dolist (int-type '((UNSIGNED-BYTE 8) FIXNUM) 'integer)
		(when (loop for value in args
			 always (subtypep value int-type))
		  (return int-type)))
	      'fixnum)))
