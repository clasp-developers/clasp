;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPCALL  Function call.

;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.


(in-package "COMPILER")

(defun unoptimized-long-call (destination fun arguments)
  (let ((frame (gensym)))
    (c1translate destination
                 `(with-stack ,frame
                    ,@(loop for i in arguments collect `(stack-push ,frame ,i))
                    (si::apply-from-stack-frame ,frame ,fun)))))

(defun unoptimized-funcall (destination fun arguments)
  (let ((l (length arguments)))
    (if (<= l si::c-arguments-limit)
	(c1with-saved-values (prefix postfix temps (list* fun arguments))
	   (nconc prefix
		  (c1funcall-op destination temps)
		  postfix))
	(unoptimized-long-call destination fun arguments))))

(defun c1funcall (destination args)
  (check-args-number 'FUNCALL args 1)
  (let ((fun (first args))
	(arguments (rest args))
	fd)
    (cond ;; (FUNCALL (LAMBDA ...) ...)
          ((and (consp fun)
		(eq (first fun) 'LAMBDA))
	   (c1translate destination
                        (optimize-funcall/apply-lambda (cdr fun) arguments nil)))
	  ;; (FUNCALL (EXT::LAMBDA-BLOCK ...) ...)
          ((and (consp fun)
		(eq (first fun) 'EXT::LAMBDA-BLOCK))
	   (setf fun (macroexpand-1 fun))
	   (c1translate destination
                        (optimize-funcall/apply-lambda (cdr fun) arguments nil)))
	  ;; (FUNCALL atomic-expression ...)
	  ((atom fun)
	   (unoptimized-funcall destination fun arguments))
	  ;; (FUNCALL macro-expression ...)
	  ((let ((name (first fun)))
	     (setq fd (and (symbolp name)
                           ;; We do not want to macroexpand 'THE
                           (not (eq name 'THE))
			   (cmp-macro-function name))))
	   (c1funcall destination (list* (cmp-expand-macro fd fun) arguments)))
	  ;; (FUNCALL lisp-expression ...)
	  ((not (eq (first fun) 'FUNCTION))
	   (unoptimized-funcall destination fun arguments))
	  ;; (FUNCALL #'GENERALIZED-FUNCTION-NAME ...)
	  ((si::valid-function-name-p (setq fun (second fun)))
	   (or (c1call-local destination fun arguments)
	       (c1call-global destination fun arguments)))
	  ;; (FUNCALL #'(LAMBDA ...) ...)
	  ((and (consp fun) (eq (first fun) 'LAMBDA))
	   (c1translate destination
                        (optimize-funcall/apply-lambda (rest fun) arguments nil)))
	  ;; (FUNCALL #'(EXT::LAMBDA-BLOCK ...) ...)
	  ((and (consp fun) (eq (first fun) 'EXT::LAMBDA-BLOCK))
	   (setf fun (macroexpand-1 fun))
	   (c1translate destination
                        (optimize-funcall/apply-lambda (rest fun) arguments nil)))
	  (t
	   (cmperr "Malformed function name: ~A" fun)))))
