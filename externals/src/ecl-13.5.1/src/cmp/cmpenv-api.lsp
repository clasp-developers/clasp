;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;; CMPENVAPI -- API for creating and manipulating environments
;;;;

(in-package #-new-cmp "COMPILER" #+new-cmp "C-ENV")

(defun cmp-env-root (&optional (env *cmp-env-root*))
  "Provide a root environment for toplevel forms storing all declarations
that are susceptible to be changed by PROCLAIM."
  (let* ((env (cmp-env-copy env)))
    (add-default-optimizations env)))

(defun cmp-env-copy (&optional (env *cmp-env*))
  (cons (car env) (cdr env)))

(defmacro cmp-env-variables (&optional (env '*cmp-env*))
  `(car ,env))

(defmacro cmp-env-functions (&optional (env '*cmp-env*))
  `(cdr ,env))

#-new-cmp
(defun cmp-env-cleanups (env)
  (loop with specials = '()
	with end = (cmp-env-variables env)
	with cleanup-forms = '()
	with aux
	for records-list on (cmp-env-variables *cmp-env*)
	until (eq records-list end)
	do (let ((record (first records-list)))
	     (cond ((atom record))
		   ((and (symbolp (first record))
			 (eq (second record) :special))
		    (push (fourth record) specials))
		   ((eq (first record) :cleanup)
		    (push (second record) cleanup-forms))))
	finally (progn
		  (unless (eq records-list end)
		    (error "Inconsistency in environment."))
		  (return (values specials
                                  (apply #'nconc (mapcar #'copy-list cleanup-forms)))))))

(defun cmp-env-register-var (var &optional (env *cmp-env*) (boundp t))
  (push (list (var-name var)
	      (if (member (var-kind var) '(special global))
		  :special
		  t)
	      boundp
	      var)
	(cmp-env-variables env))
  env)

(defun cmp-env-declare-special (name &optional (env *cmp-env*))
  (cmp-env-register-var (c::c1make-global-variable name :warn nil :kind 'SPECIAL)
			env nil)
  env)

(defun cmp-env-add-declaration (type arguments &optional (env *cmp-env*))
  (push (list* :declare type arguments) 
        (cmp-env-variables env))
  env)

(defun cmp-env-extend-declaration (type arguments &optional (env *cmp-env*))
  (let ((x (cmp-env-search-declaration type)))
    (cmp-env-add-declaration type (append arguments x) env)
    env))

(defun cmp-env-register-function (fun &optional (env *cmp-env*))
  (push (list (fun-name fun) 'function fun)
	(cmp-env-functions env))
  env)

(defun cmp-env-register-global-macro (name function)
  (cmp-env-register-macro name function *cmp-env*)
  (cmp-env-register-macro name function *cmp-env-root*)
  (values))

(defun cmp-env-register-macro (name function &optional (env *cmp-env*))
  (push (list name 'si::macro function)
	(cmp-env-functions env))
  env)

(defun cmp-env-register-ftype (name declaration &optional (env *cmp-env*))
  (push (list* :declare name declaration)
        (cmp-env-functions env))
  env)

(defun cmp-env-register-symbol-macro (name form &optional (env *cmp-env*))
  (push (list name 'si::symbol-macro
	      #'(lambda (whole env) (declare (ignore env whole)) form))
	(cmp-env-variables env))
  env)

(defun cmp-env-register-block (blk &optional (env *cmp-env*))
  (push (list :block (blk-name blk) blk)
	(cmp-env-variables env))
  env)

(defun cmp-env-register-tag (name tag &optional (env *cmp-env*))
  (push (list :tag (list name) tag)
	(cmp-env-variables env))
  env)

(defun cmp-env-register-cleanup (form &optional (env *cmp-env*))
  (push (list :cleanup (copy-list form)) (cmp-env-variables env))
  env)

(defun cmp-env-search-function (name &optional (env *cmp-env*))
  (let ((ccb nil)
	(clb nil)
	(unw nil)
	(found nil))
    (dolist (record (cmp-env-functions env))
      (cond ((eq record 'CB)
	     (setf ccb t))
	    ((eq record 'LB)
	     (setf clb t))
	    ((eq record 'UNWIND-PROTECT)
	     (setf unw t))
	    ((atom record)
	     (baboon :format-control "Uknown record found in environment~%~S"
                     :format-arguments (list record)))
	    ;; We have to use EQUAL because the name can be a list (SETF whatever)
	    ((equal (first record) name)
	     (setf found (first (last record)))
	     (return))))
    (values found ccb clb unw)))

(defun cmp-env-search-variables (type name env)
  (let ((ccb nil)
	(clb nil)
	(unw nil)
	(found nil))
    (dolist (record (cmp-env-variables env))
      (cond ((eq record 'CB)
	     (setf ccb t))
	    ((eq record 'LB)
	     (setf clb t))
	    ((eq record 'UNWIND-PROTECT)
	     (setf unw t))
	    ((atom record)
	     (baboon :format-control "Uknown record found in environment~%~S"
                     :format-arguments (list record)))
	    ((not (eq (first record) type)))
	    ((eq type :block)
	     (when (eq name (second record))
	       (setf found record)
	       (return)))
	    ((eq type :tag)
	     (when (member name (second record) :test #'eql)
	       (setf found record)
	       (return)))
	    ((eq (second record) 'si::symbol-macro)
	     (when (eq name 'si::symbol-macro)
	       (setf found record))
	     (return))
	    (t
	     (setf found record)
	     (return))))
    (values (first (last found)) ccb clb unw)))

(defun cmp-env-search-block (name &optional (env *cmp-env*))
  (cmp-env-search-variables :block name env))

(defun cmp-env-search-tag (name &optional (env *cmp-env*))
  (cmp-env-search-variables :tag name env))

(defun cmp-env-search-symbol-macro (name &optional (env *cmp-env*))
  (cmp-env-search-variables name 'si::symbol-macro env))

(defun cmp-env-search-var (name &optional (env *cmp-env*))
  (cmp-env-search-variables name t env))

(defun cmp-env-search-macro (name &optional (env *cmp-env*))
  (let ((f (cmp-env-search-function name env)))
    (if (functionp f) f nil)))

(defun cmp-env-search-ftype (name &optional (env *cmp-env*))
  (dolist (i env nil)
    (when (and (consp i)
               (eq (pop i) :declare)
               (same-fname-p (pop i) name))
      (return i))))

(defun cmp-env-mark (mark &optional (env *cmp-env*))
  (cons (cons mark (car env))
	(cons mark (cdr env))))

(defun cmp-env-new-variables (new-env old-env)
  (loop for i in (ldiff (cmp-env-variables new-env)
			(cmp-env-variables old-env))
	when (and (consp i) (var-p (fourth i)))
	collect (fourth i)))

(defun cmp-env-search-declaration (kind &optional (env *cmp-env*))
  (loop for i in (car env)
     when (and (consp i)
               (eq (first i) :declare)
               (eq (second i) kind))
     return (cddr i)))

