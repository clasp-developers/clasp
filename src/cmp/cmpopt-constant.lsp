;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;; CMPOPT-CONSTANTS  Constant expressions.
;;;;

(in-package "COMPILER")

(defun constant-expression-p (form &optional (env *cmp-env*))
  (or (constantp form env)
      (and (consp form)
	   (let ((head (car form)))
             (or (member head '(IF OR AND NULL NOT PROGN))
                 (and (get-sysprop head 'pure)
                      (inline-possible head))))
           (loop for c in (rest form)
	      always (constant-expression-p c env)))))

(defun extract-constant-value (form &optional failure (env *cmp-env*))
  (if (constant-expression-p form env)
      (handler-case (cmp-eval form env)
        (error (c) failure))
      failure))

(defun constant-value-p (form &optional (env *cmp-env*))
  (if (constant-expression-p form env)
      (handler-case
	  (values t (cmp-eval form env))
	(error (c) (values nil form)))
      (values nil form)))
