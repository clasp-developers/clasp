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
;;;;                    Exporting external symbols of LISP package

(eval-when (eval compile load)
  (si::select-package "SI"))

;;; ----------------------------------------------------------------------
;;;
(*make-special '*dump-defun-definitions*)
(setq *dump-defun-definitions* nil)
(*make-special '*dump-defmacro-definitions*)
(setq *dump-defmacro-definitions* *dump-defun-definitions*)

;; This is needed only when bootstrapping ECL using ECL-MIN
(eval-when (eval)
  (si::fset 'ext:register-with-pde
	  #'(ext::lambda-block ext:register-with-pde (whole env)
	       (let* ((definition (second whole))
		      (output-form (third whole)))
		 `(if ext:*register-with-pde-hook*
		      (funcall ext:*register-with-pde-hook*
			       (copy-tree *source-location*)
			       ,definition
			       ,output-form)
		      ,output-form)))
	  t)
  (si::fset 'defun
	  #'(ext::lambda-block defun (def env)
	      (let* ((name (second def))
		     (function `#'(ext::lambda-block ,@(cdr def))))
		(when *dump-defun-definitions*
		  (print function)
		  (setq function `(si::bc-disassemble ,function)))
		(ext:register-with-pde def `(si::fset ',name ,function))))
	  t)
 (si::fset 'in-package
 	  #'(ext::lambda-block in-package (def env)
	      `(eval-when (eval compile load)
		(si::select-package ,(string (second def)))))
 	  t)
)

;;
;; This is also needed for booting ECL. In particular it is required in
;; defmacro.lsp.
;;
(let ((f #'(ext::lambda-block dolist (whole env)
           (declare (ignore env))
	   (let (body pop finished control var expr exit)
	     (setq body (rest whole))
	     (when (endp body)
	       (simple-program-error "Syntax error in ~A:~%~A" 'DOLIST whole))
	     (setq control (first body) body (rest body))
	     (when (endp control)
	       (simple-program-error "Syntax error in ~A:~%~A" 'DOLIST whole))
	     (setq var (first control) control (rest control))
	     (if (<= 1 (length control) 2)
		 (setq expr (first control) exit (rest control))
		 (simple-program-error "Syntax error in ~A:~%~A" 'DOLIST whole))
	     (multiple-value-bind (declarations body)
		 (process-declarations body nil)
	       `(block nil
		 (let* ((%dolist-var ,expr)
			,var)
		   (declare ,@declarations)
		   (si::while %dolist-var
		      (setq ,var (first %dolist-var))
		      ,@body
		      (setq %dolist-var (cons-cdr %dolist-var)))
		   ,(when exit `(setq ,var nil))
		   ,@exit)))))))
  (si::fset 'dolist f t))

(let ((f #'(ext::lambda-block dotimes (whole env)
           (declare (ignore env))
	   (let (body pop finished control var expr exit)
	     (setq body (rest whole))
	     (when (endp body)
	       (simple-program-error "Syntax error in ~A:~%~A" 'DOTIMES whole))
	     (setq control (first body) body (rest body))
	     (when (endp control)
	       (simple-program-error "Syntax error in ~A:~%~A" 'DOTIMES whole))
	     (setq var (first control) control (rest control))
	     (if (<= 1 (length control) 2)
		 (setq expr (first control) exit (rest control))
		 (simple-program-error "Syntax error in ~A:~%~A" 'DOTIMES whole))
	     (multiple-value-bind (declarations body)
		 (process-declarations body nil)
               (when (integerp expr)
                 (setq declarations
                       (cons `(type (integer 0 ,expr) ,var) declarations)))
	       `(block nil
		 (let* ((%dotimes-var ,expr)
			(,var 0))
		   (declare ,@declarations)
		   (si::while (< ,var %dotimes-var)
		     ,@body
		     (setq ,var (1+ ,var)))
		   ,@exit)))))))
  (si::fset 'dotimes f t))

(let ((f #'(ext::lambda-block do/do*-expand (whole env)
           (declare (ignore env))
           (let (do/do* control test result vl step let psetq body)
	     (setq do/do* (first whole) body (rest whole))
	     (if (eq do/do* 'do)
		 (setq let 'LET psetq 'PSETQ)
		 (setq let 'LET* psetq 'SETQ))
	     (when (endp body)
	       (simple-program-error "Syntax error in ~A:~%~A" do/do* whole))
	     (setq control (first body) body (rest body))
	     (when (endp body)
	       (simple-program-error "Syntax error in ~A:~%~A" do/do* whole))
	     (setq test (first body) body (rest body))
	     (when (endp test)
	       (simple-program-error "Syntax error in ~A:~%~A" do/do* whole))
	     (setq result (rest test) test (first test))
	     (dolist (c control)
	       (when (symbolp c) (setq c (list c)))
	       (case (length c)
		 ((1 2)
		  (setq vl (cons c vl)))
		 (3
		  (setq vl (cons (butlast c) vl)
			step (list* (third c) (first c) step)))
		 (t
		  (simple-program-error "Syntax error in ~A:~%~A" do/do* whole))))
	     (multiple-value-bind (declarations real-body)
		 (process-declarations body nil)
	       `(BLOCK NIL
		 (,let ,(nreverse vl)
		   (declare ,@declarations)
		   (sys::until ,test
		      ,@real-body
		      ,@(when step (list (cons psetq (nreverse step)))))
		   ,@(or result '(nil)))))))))
  (si::fset 'do f t)
  (si::fset 'do* f t))

(defun eval-feature (x &aux operator)
  (declare (si::c-local))
  (cond ((symbolp x)
	 (and (member x *features* :test #'eq) t))
	((atom x) (error "~ is not allowed as a feature" x))
	((not (symbolp (setq operator (first x))))
	 (error "~S is not a valid feature expression." x))
        ((eql operator :AND)
         (dolist (x (cdr x) t) (when (not (eval-feature x)) (return nil))))
        ((eql operator :OR)
         (dolist (x (cdr x) nil) (when (eval-feature x) (return t))))
        ((eql operator :NOT)
	 (not (eval-feature (second x))))
	(t (error "~S is not a valid feature expression." x))))

(defun do-read-feature (stream subchar arg test)
  (declare (si::c-local))
  (when arg
    (error "Reading from ~S: no number should appear between # and ~A"
	   stream subchar))
  (let ((feature (let ((*package* (find-package "KEYWORD")))
		   (read stream t nil t))))
    (if (and (not *read-suppress*) (eq (eval-feature feature) test))
	(read stream t nil t)
	(let ((*read-suppress* t)) (read stream t nil t) (values)))))

(defun sharp-+-reader (stream subchar arg)
  (do-read-feature stream subchar arg T))

(defun sharp---reader (stream subchar arg)
  (do-read-feature stream subchar arg NIL))

(si::readtable-lock (si::standard-readtable) nil)
(set-dispatch-macro-character #\# #\+ 'sharp-+-reader)
(set-dispatch-macro-character #\# #\+ 'sharp-+-reader (sys::standard-readtable))
(set-dispatch-macro-character #\# #\- 'sharp---reader)
(set-dispatch-macro-character #\# #\- 'sharp---reader (sys::standard-readtable))
(si::readtable-lock (si::standard-readtable) t)
