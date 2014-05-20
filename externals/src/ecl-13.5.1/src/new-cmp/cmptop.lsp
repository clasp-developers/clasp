;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPTOP  --  Compiler top-level.

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

(defmacro with-t1expr ((init-name) &rest body)
  `(t1loop #'(lambda ()
               (flet ((t1expr (form)
                        (let ((*current-toplevel-form* nil)
                              (*compile-toplevel* t)
                              (*compile-time-too* nil))
                          (setf *top-level-forms* (nreconc (t1expr* 'trash form)
                                                           *top-level-forms*)))))
                 ,@body))
           ,init-name))

(defun t1loop (body init-name)
  (let* ((only-argument (make-var :name (gensym "CBLOCK")
                                  :type T
                                  :kind :object
                                  :loc `(LCL 1)
                                  :ref 1))
         (fun (make-fun :name +init-function-name+
                        :cfun init-name
                        :minarg 1 :maxarg 1 :closure nil
                        :global t :no-entry t :exported t))
         (*current-function* fun)
         (*lcl* 1)
         (*last-label* 0)
         (*cmp-env* (cmp-env-register-var only-argument (cmp-env-new)))
         (*permanent-data* nil))
    (setf *top-level-forms* nil)
    (funcall body)
    (setf (fun-lambda fun) (nconc (c1function-prologue fun)
                                  (nreverse *top-level-forms*)
                                  (c1function-epilogue fun))
          (fun-last-lcl fun) *lcl*
          (fun-last-label fun) *last-label*
          *top-level-forms* fun)))

(defvar *toplevel-forms-to-print*
  '(defun defmacro defvar defparameter defclass defmethod defgeneric))

(defun t1expr* (destination form)
  ;(let ((*print-level* 3)) (print form))
  (catch *cmperr-tag*
    (when (consp form)
      (let ((*current-toplevel-form* (list form *current-toplevel-form*))
            (fun (car form))
            (args (cdr form)) fd)
	(when (member fun *toplevel-forms-to-print*)
	  (print-current-form))
	(cond
            ((consp fun) (t1ordinary destination form))
            ((not (symbolp fun))
	     (cmperr "~s is illegal function." fun))
	    ((eq fun 'QUOTE)
	     (t1ordinary destination 'NIL))
	    ((setq fd (get-sysprop fun 'T1))
	     (funcall fd destination args))
	    ((or (get-sysprop fun 'C1) (get-sysprop fun 'C1SPECIAL))
             (t1ordinary destination form))
	    ((and (setq fd (compiler-macro-function fun))
		  (inline-possible fun)
		  (let ((success nil))
		    (multiple-value-setq (fd success)
		      (cmp-expand-macro fd form))
		    success))
             (push 'macroexpand *current-toplevel-form*)
	     (t1expr* destination fd))
	    ((setq fd (cmp-macro-function fun))
             (push 'macroexpand *current-toplevel-form*)
	     (t1expr* destination (cmp-expand-macro fd form)))
	    (t (t1ordinary destination form))
	   )))))

(defun t1/c1expr (destination form)
  (cond ((not *compile-toplevel*)
	 (c1translate destination form))
	((atom form)
	 (t1ordinary destination form))
	(t
	 (t1expr* destination form))))	

(defun c1progn (destination forms)
  (or (loop for fl on forms
         append (t1/c1expr (if (rest fl) 'TRASH destination)
                           (first fl)))
      (t1/c1expr destination 'NIL)))

(defun c1decl-body (destination decls body)
  (if (null decls)
      (c1progn destination body)
      (let* ((*cmp-env* (add-declarations decls (cmp-env-copy *cmp-env*))))
	(c1progn destination body))))

(defun c1eval-when (destination args)
  (check-args-number 'EVAL-WHEN args 1)
  (let ((load-flag nil)
	(compile-flag nil)
	(execute-flag nil))
    (dolist (situation (car args))
      (case situation
	((LOAD :LOAD-TOPLEVEL) (setq load-flag t))
	((COMPILE :COMPILE-TOPLEVEL) (setq compile-flag t))
	((EVAL :EXECUTE)
	 (if *compile-toplevel*
	     (setq compile-flag (or *compile-time-too* compile-flag))
	     (setq execute-flag t)))
	(otherwise (cmperr "The EVAL-WHEN situation ~s is illegal."
			   situation))))
    (cond ((not *compile-toplevel*)
	   (c1progn destination (and execute-flag (rest args))))
	  (load-flag
	   (let ((*compile-time-too* compile-flag))
	     (c1progn destination (rest args))))
	  (compile-flag
	   (cmp-eval (cons 'PROGN (rest args)))
	   (c1progn destination 'NIL))
	  (t
	   (c1progn destination 'NIL)))))

(defun t1ordinary (destination form)
  (when *compile-time-too* (cmp-eval form))
  (let* ((*compile-toplevel* nil)
         (*compile-time-too* nil))
    (add-load-time-values (c1translate destination form))))

(defun add-load-time-values (form)
  (let ((previous (nconc *load-time-values* *make-forms*)))
    (setf *load-time-values* nil
          *make-forms* nil)
    (nconc previous form)))

(defun c1load-time-value (destination args)
  (check-args-number 'LOAD-TIME-VALUE args 1 2)
  (let ((form (first args))
	loc)
    (cond ((not (listp *load-time-values*))
	   ;; When using COMPILE, we set *load-time-values* to 'VALUES and
	   ;; thus signal that we do not want to compile these forms, but
	   ;; just to retain their value.
	   (return-from c1load-time-value (c1constant-value destination
                                                            (cmp-eval form) :always t)))
          ((typep form '(or list symbol))
	   (setf loc (data-empty-loc))
	   (setf *load-time-values* (nconc *load-time-values*
                                           (c1translate loc form))))
	  (t
	   (setf loc (add-object (cmp-eval form)))))
    (c1set-loc destination loc)))

(defun c1locally (destination args)
  (multiple-value-bind (body ss ts is other-decl)
      (c1body args t)
    (let ((*cmp-env* (cmp-env-copy)))
      (c1declare-specials ss)
      (check-vdecl nil ts is)
      (c1decl-body destination other-decl body))))

(defun c1macrolet (destination args)
  (check-args-number 'MACROLET args 1)
  (let ((*cmp-env* (cmp-env-copy)))
    (cmp-env-register-macrolet (first args) *cmp-env*)
    (c1locally destination (cdr args))))

(defun c1symbol-macrolet (destination args)
  (check-args-number 'SYMBOL-MACROLET args 1)
  (let ((*cmp-env* (cmp-env-copy)))
    (dolist (def (car args))
      (let ((name (first def)))	    
	(cmpck (or (endp def) (not (symbolp name)) (endp (cdr def)))
	     "The symbol-macro definition ~s is illegal." def)
	(cmp-env-register-symbol-macro name (second def))))
    (c1locally destination (cdr args))))

(defun t1defmacro (destination args)
  (check-args-number 'DEFMACRO args 2)
  (destructuring-bind (name lambda-list &rest body)
      args
    (multiple-value-bind (function pprint doc-string)
        (sys::expand-defmacro name lambda-list body)
      (let ((fn (cmp-eval function *cmp-env*)))
        (cmp-env-register-global-macro name fn))
      (c1locally destination (macroexpand `(DEFMACRO ,@args))))))

;;; ----------------------------------------------------------------------
;;; Optimizer for FSET. Removes the need for a special handling of DEFUN as a
;;; toplevel form and also allows optimizing calls to DEFUN or DEFMACRO which
;;; are not toplevel, but which create no closures.
;;;
;;; The idea is as follows: when the function or macro to be defined is not a
;;; closure, we can use the auxiliary C functions c_def_c_*() instead of
;;; creating a closure and invoking si_fset(). However until the C2 phase of
;;; the compiler we do not know whether a function is a closure, hence the need
;;; for a c2fset.
;;;
(defun c1fset (destination args)
  (destructuring-bind (fname def &optional (macro nil) (pprint nil))
      args
    (let* ((unoptimized (c1call-global destination 'SI:FSET
                                       (list fname def macro pprint)))
           (fun-form (c1translate 'VALUE0 def)))
      (if (and (eq destination 'TRASH)
               (= (length fun-form) 1)
               (setf fun-form (first fun-form))
               (eq (c1form-name fun-form) 'FUNCTION)
	       (not (eq (c1form-arg 0 fun-form) 'GLOBAL)))
	  (let ((fun-object (c1form-arg 2 fun-form)))
            (setf (fun-child-funs *current-function*)
                  (delete fun-object (fun-child-funs *current-function*)))
	    (cond ((fun-no-entry fun-object)
                   (when macro
                     (cmperr "Declaration C-LOCAL used in macro ~a" (fun-name fun)))
                   (make-c1form* 'SI:FSET :args fun-object nil nil nil nil))
                  ((and (typep macro 'boolean)
                        (typep pprint '(or integer null))
                        (consp fname)
                        (eq (first fname) 'quote))
                   (make-c1form* 'SI:FSET
                                 :args
                                 fun-object ;; Function object
                                 (add-object (second fname) :permanent t :duplicate t)
                                 macro
                                 pprint
                                 unoptimized))))
          unoptimized))))

;;; ----------------------------------------------------------------------

;;; Pass 1 top-levels.

(put-sysprop 'COMPILER-LET 'T1 'c1compiler-let)
(put-sysprop 'EVAL-WHEN 'T1 'c1eval-when)
(put-sysprop 'PROGN 'T1 'c1progn)
(put-sysprop 'MACROLET 'T1 'c1macrolet)
(put-sysprop 'LOCALLY 'T1 'c1locally)
(put-sysprop 'SYMBOL-MACROLET 'T1 'c1symbol-macrolet)
