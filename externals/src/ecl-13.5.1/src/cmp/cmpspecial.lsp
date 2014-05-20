;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPSPECIAL  Miscellaneous special forms.

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

(defun c1quote (args)
  (check-args-number 'QUOTE args 1 1)
  (c1constant-value (car args) :always t))

(defun c1declare (args)
  (cmperr "The declaration ~s was found in a bad place." (cons 'DECLARE args)))

(defun c1the (args)
  (check-args-number 'THE args 2 2)
  ;; FIXME: C1CHECKED-VALUE cannot check multiple values.
  (let ((type (first args)))
    (if (and (policy-the-is-checked)
	     (not (and (consp type)
		       (eq (first type) 'values))))
	(c1checked-value args)
	(c1truly-the args))))

(defun c1truly-the (args)
  (check-args-number 'TRULY-THE args 2 2)
  (let* ((form (c1expr (second args)))
	 (the-type (first args))
	 type)
    (setf type (values-type-and the-type (c1form-type form)))
    (if (values-type-primary-type type)
        (setf (c1form-type form) type)
	(cmpwarn "Type mismatch was found in ~s." (cons 'THE args)))
    form))

(defun c1compiler-let (args &aux (symbols nil) (values nil))
  (when (endp args) (too-few-args 'COMPILER-LET 1 0))
  (dolist (spec (car args))
    (cond ((consp spec)
           (cmpck (not (and (symbolp (car spec))
                            (or (endp (cdr spec))
                                (endp (cddr spec)))))
                  "The variable binding ~s is illegal." spec)
           (push (car spec) symbols)
           (push (if (endp (cdr spec)) nil (eval (second spec))) values))
          ((symbolp spec)
           (push spec symbols)
           (push nil values))
          (t (cmperr "The variable binding ~s is illegal." spec))))
  (setq symbols (nreverse symbols))
  (setq values (nreverse values))
  (setq args (progv symbols values (c1progn (cdr args))))
  (make-c1form 'COMPILER-LET args symbols values args))

(defun c2compiler-let (c1form symbols values body)
  (declare (ignore c1form))
  (progv symbols values (c2expr body)))

(defun c1function (args &aux fd)
  (check-args-number 'FUNCTION args 1 1)
  (let ((fun (car args)))
    (cond ((si::valid-function-name-p fun)
	   (let ((funob (local-function-ref fun t)))
	     (if funob
		 (let* ((var (fun-var funob)))
		   (add-to-read-nodes var (make-c1form* 'VAR :args var)))
		 (make-c1form* 'FUNCTION
                               :type 'FUNCTION
			       :sp-change (not (and (symbolp fun)
						    (get-sysprop fun 'NO-SP-CHANGE)))
			       :args 'GLOBAL nil fun))))
          ((and (consp fun) (member (car fun) '(LAMBDA EXT::LAMBDA-BLOCK)))
           (cmpck (endp (cdr fun))
                  "The lambda expression ~s is illegal." fun)
	   (let (name body)
	     (if (eq (first fun) 'EXT::LAMBDA)
		 (setf name (gensym) body (rest fun))
		 (setf name (second fun) body (cddr fun)))
	     (c1expr `(flet ((,name ,@body)) #',name))))
	  (t (cmperr "The function ~s is illegal." fun)))))

(defun c2function (c1form kind funob fun)
  (declare (ignore c1form))
  (case kind
    (GLOBAL
     (unwind-exit (list 'FDEFINITION fun)))
    (CLOSURE
     (baboon)
     (new-local fun)
     (unwind-exit `(MAKE-CCLOSURE ,fun)))))

;;; Mechanism for sharing code.
(defun new-local (fun)
  ;; returns the previous function or NIL.
  (declare (type fun fun))
  (case (fun-closure fun)
    (CLOSURE
     (setf (fun-level fun) 0 (fun-env fun) *env*))
    (LEXICAL
     (let ((parent (fun-parent fun)))
       ;; Only increase the lexical level if there have been some
       ;; new variables created. This way, the same lexical environment
       ;; can be propagated through nested FLET/LABELS.
       (setf (fun-level fun) (if (plusp *lex*) (1+ *level*) *level*)
	     (fun-env fun) 0)))
    (otherwise
     (setf (fun-env fun) 0 (fun-level fun) 0)))
  (let ((previous
         nil
         #+(or)
         (dolist (old *local-funs*)
           (when (similar fun old)
             (return old)))))
    (if previous
	(progn
          (if (eq (fun-closure fun) 'CLOSURE)
	      (cmpnote "Sharing code for closure")
	      (cmpnote "Sharing code for local function ~A" (fun-name fun)))
	  (setf (fun-cfun fun) (fun-cfun previous)
		(fun-lambda fun) nil)
	  previous)
	(push fun *local-funs*))))

(defun wt-fdefinition (fun-name)
  (let* ((name (si::function-block-name fun-name))
	 (package (symbol-package name))
	 (safe (or (not (safe-compile))
		   (and (or (eq package (find-package "CL"))
			    (eq package (find-package "CLOS"))
			    (eq package (find-package "SI")))
			(fboundp fun-name)
			(functionp (fdefinition fun-name))))))
    (if (eq name fun-name)
	;; #'symbol
	(let ((vv (add-symbol name)))
	  (if safe
	      (wt "(" vv "->symbol.gfdef)")
	      (wt "ecl_fdefinition(" vv ")")))
	;; #'(SETF symbol)
	(if safe
	    #+(or)
	    (let ((set-loc (assoc name *setf-definitions*)))
	      (unless set-loc
		(let* ((setf-vv (data-empty-loc))
		       (name-vv (add-symbol name))
		       (setf-form-vv (add-object fun-name)))
		  (setf set-loc (list name setf-vv name-vv setf-form-vv))
		  (push set-loc *setf-definitions*)))
	      (wt "ECL_SETF_DEFINITION(" (second set-loc) "," (fourth set-loc) ")"))
	    (let ((set-loc (assoc name *setf-definitions*)))
	      (unless set-loc
		(let* ((setf-vv (data-empty-loc))
		       (name-vv (add-symbol name)))
		  (setf set-loc (list name setf-vv name-vv))
		  (push set-loc *setf-definitions*)))
	      (wt "ECL_CONS_CAR(" (second set-loc) ")"))
	    (let ((vv (add-symbol fun-name)))
	      (wt "ecl_fdefinition(" vv ")"))))))

(defun environment-accessor (fun)
  (let* ((env-var (env-var-name *env-lvl*))
	 (expected-env-size (fun-env fun)))
    (if (< expected-env-size *env*)
	(format nil "ecl_nthcdr(~D,~A)" (- *env* expected-env-size) env-var)
	env-var)))

(defun wt-make-closure (fun &aux (cfun (fun-cfun fun)))
  (declare (type fun fun))
  (let* ((closure (fun-closure fun))
	 narg)
    (cond ((eq closure 'CLOSURE)
	   (wt "ecl_make_cclosure_va((cl_objectfn)" cfun ","
	       (environment-accessor fun)
	       ",Cblock)"))
	  ((eq closure 'LEXICAL)
	   (baboon))
	  ((setf narg (fun-fixed-narg fun)) ; empty environment fixed number of args
	   (wt "ecl_make_cfun((cl_objectfn_fixed)" cfun ",ECL_NIL,Cblock," narg ")"))
	  (t ; empty environment variable number of args
	   (wt "ecl_make_cfun_va((cl_objectfn)" cfun ",ECL_NIL,Cblock)")))))
