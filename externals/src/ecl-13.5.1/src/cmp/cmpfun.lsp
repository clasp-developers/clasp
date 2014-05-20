;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPFUN  Library functions.

;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi and William F. Schelter.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.


(in-package "COMPILER")

(defun c1apply (args)
  (check-args-number 'APPLY args 2)
  (let* ((fun (first args))
	 (arguments (rest args)))
    (cond ((and (consp fun)
		(eq (first fun) 'LAMBDA))
	   (optimize-funcall/apply-lambda (cdr fun) arguments t))
	  ((and (consp fun)
		(eq (first fun) 'EXT::LAMBDA-BLOCK))
	   (setf fun (macroexpand-1 fun))
	   (optimize-funcall/apply-lambda (cdr fun) arguments t))
	  ((and (consp fun)
		(eq (first fun) 'FUNCTION)
		(consp (second fun))
		(member (caadr fun) '(LAMBDA EXT::LAMBDA-BLOCK)))
	   (c1apply (list* (second fun) arguments)))
	  (t
	   (let ((form (c1funcall (list* '#'APPLY args))))
	     (when (and (consp fun) (eq (first fun) 'FUNCTION))
	       (let* ((fname (second fun))
		      (type (get-return-type fname)))
		 (when type
		   (setf (c1form-type form) type))))
	     form)))))

;;----------------------------------------------------------------------
;; We transform BOOLE into the individual operations, which have
;; inliners
;;

(define-compiler-macro boole (&whole form op-code op1 op2)
  (or (and (constantp op-code *cmp-env*)
	   (case (ext:constant-form-value op-code *cmp-env*)
	     (#. boole-clr `(progn ,op1 ,op2 0))
	     (#. boole-set `(progn ,op1 ,op2 -1))
	     (#. boole-1 `(prog1 ,op1 ,op2))
	     (#. boole-2 `(progn ,op1 ,op2))
	     (#. boole-c1 `(prog1 (lognot ,op1) ,op2))
	     (#. boole-c2 `(progn ,op1 (lognot ,op2)))
	     (#. boole-and `(logand ,op1 ,op2))
	     (#. boole-ior `(logior ,op1 ,op2))
	     (#. boole-xor `(logxor ,op1 ,op2))
	     (#. boole-eqv `(logeqv ,op1 ,op2))
	     (#. boole-nand `(lognand ,op1 ,op2))
	     (#. boole-nor `(lognor ,op1 ,op2))
	     (#. boole-andc1 `(logandc1 ,op1 ,op2))
	     (#. boole-andc2 `(logandc2 ,op1 ,op2))
	     (#. boole-orc1 `(logorc1 ,op1 ,op2))
	     (#. boole-orc2 `(logorc2 ,op1 ,op2))))
      form))

;----------------------------------------------------------------------

;; Return the most particular type we can EASILY obtain from x.  
(defun result-type (x)
  (cond ((symbolp x)
	 (c1form-primary-type (c1expr x)))
	((constantp x)
	 (type-of x))
	((and (consp x) (eq (car x) 'the))
	 (second x))
	(t t)))

