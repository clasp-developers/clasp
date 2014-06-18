;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;; CMPEVAL --  The Expression Dispatcher.

;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    ECoLisp is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

(defun c1expr-inner (form)
  (declare (si::c-local))
  (cond ((symbolp form)
	 (setq form (chk-symbol-macrolet form))
	 (cond ((not (symbolp form))
		form)
	       ((eq form nil) (c1nil))
	       ((eq form t) (c1t))
	       ((keywordp form)
		(make-c1form* 'LOCATION :type (object-type form)
			      :args (add-symbol form)))
	       ((constantp form)
		(or (c1constant-value (symbol-value form) :only-small-values t)
		    (c1var form)))
	       (t (c1var form))))
	((consp form)
	 (cmpck (not (si::proper-list-p form))
		"Improper list found in lisp form~%~A" form)
	 (let ((fun (car form)))
	   (cond ((let ((fd (gethash fun *c1-dispatch-table*)))
		    (and fd (setf fun fd)))
		  (funcall fun (rest form)))
		 ((symbolp fun)
		  (c1call fun (cdr form) t))
		 ((and (consp fun) (eq (car fun) 'LAMBDA))
		  (c1funcall form))
		 (t (cmperr "~s is not a legal function name." fun)))))
	(t (c1constant-value form :always t))))

(defun c1expr (form)
  (let ((*current-form* form))
    (loop
       (setf form (c1expr-inner form))
       (when (c1form-p form)
	 (return form)))))

(defvar *c1nil* (make-c1form* 'LOCATION :type (object-type nil) :args nil))
(defun c1nil () *c1nil*)
(defvar *c1t* (make-c1form* 'LOCATION :type (object-type t) :args t))
(defun c1t () *c1t*)

(defun c1call (fname args macros-allowed &aux fd success can-inline)
  (cond ((> (length args) si::c-arguments-limit)
	 (if (and macros-allowed
		  (setf fd (cmp-macro-function fname)))
	     (cmp-expand-macro fd (list* fname args))
	     ;; When it is a function and takes many arguments, we will
	     ;; need a special C form to call it. It takes extra code for
	     ;; handling the stack
	     (unoptimized-long-call `#',fname args)))
	((setq fd (local-function-ref fname))
	 (c1call-local fname fd args))
	((and (setq can-inline (inline-possible fname))
	      (setq fd (compiler-macro-function fname))
	      (progn
		(multiple-value-setq (fd success)
		  (cmp-expand-compiler-macro fd fname args))
		success))
	 fd)
	((and can-inline
	      (progn
		(multiple-value-setq (fd success)
		  (clos-compiler-macro-expand fname args))
		success))
	 fd)
	((and macros-allowed
	      (setq fd (cmp-macro-function fname)))
	 (cmp-expand-macro fd (list* fname args)))
	((and (setq can-inline (declared-inline-p fname))
	      (consp can-inline)
	      (eq (first can-inline) 'function)
	      (plusp *inline-max-depth*)
	      (<= (cmp-env-optimization 'space) 1))
	 (let ((*inline-max-depth* (1- *inline-max-depth*)))
	   (cmpnote "Inlining ~a" fname)
	   `(funcall ,can-inline ,@args)))
	(t (c1call-global fname args))))

(defun c1call-local (fname fun args)
  (declare (si::c-local))
  (let ((lambda (fun-lambda-expression fun)))
    (when (and lambda
	       (declared-inline-p fname)
	       (plusp *inline-max-depth*))
      (return-from c1call-local
	(let ((*inline-max-depth* (1- *inline-max-depth*)))
	  `(funcall #',lambda ,@args)))))
  (let* ((forms (c1args* args))
	 (return-type (or (get-local-return-type fun) 'T))
	 (arg-types (get-local-arg-types fun)))
    ;; Add type information to the arguments.
    (when arg-types
      (let ((fl nil))
	(dolist (form forms)
	  (cond ((endp arg-types) (push form fl))
		(t (push (and-form-type (car arg-types) form (car args)
					:safe "In a call to ~a" fname)
			 fl)
		   (pop arg-types)
		   (pop args))))
	(setq forms (nreverse fl))))
    (make-c1form* 'CALL-LOCAL
		  :sp-change t ; conservative estimate
		  :side-effects t ; conservative estimate
		  :type return-type
		  :args fun forms)))

(defun c1call-global (fname args)
  (let* ((forms (c1args* args)))
    ;; If all arguments are constants, try to precompute the function
    ;; value. We abort when the function signals an error or the value
    ;; is not printable.
    (let ((value (c1call-constant-fold fname forms)))
      (when value
	(return-from c1call-global value)))
    ;; Otherwise emit a global function call
    (make-c1form* 'CALL-GLOBAL
		  :sp-change (function-may-change-sp fname)
		  :side-effects (function-may-have-side-effects fname)
		  :type (propagate-types fname forms)
		  :args fname forms
		  ;; loc and type are filled by c2expr
		  )))

(defun c1call-constant-fold (fname forms)
  (when (and (get-sysprop fname 'pure)
	     (policy-evaluate-forms)
	     (inline-possible fname))
    (handler-case
	(loop with all-values = '()
	   with constant-p
	   with v
	   for form in forms
	   do (if (multiple-value-setq (constant-p v)
		    (c1form-constant-p form))
		  (push v all-values)
		  (return nil))
	   finally
	     (return (c1constant-value
		      (apply fname (nreverse all-values))
		      :only-small-values nil)))
      (error (c)))))

(defun c2expr (form)
  (with-c1form-env (form form)
    (let* ((name (c1form-name form))
           (args (c1form-args form))
           (dispatch (gethash name *c2-dispatch-table*)))
      (apply dispatch form args))))

(defun c2expr* (form)
  ;; C2EXPR* compiles the giving expression in a context in which
  ;; other expressions will follow this one. We must thus create
  ;; a possible label so that the compiled forms exit right at
  ;; the point where the next form will be compiled.
  (with-exit-label (label)
    (let* ((*exit* label)
	   (*unwind-exit* (cons *exit* *unwind-exit*))
	   ;;(*lex* *lex*)
	   (*lcl* *lcl*)
	   (*temp* *temp*))
      (c2expr form))))

(defun c1with-backend (forms)
  (c1progn (loop for tag = (pop forms)
              for form = (pop forms)
              while tag
              when (eq tag :c/c++)
              collect form)))

(defun c1progn (forms)
  (cond ((endp forms) (t1/c1expr 'NIL))
	((endp (cdr forms)) (t1/c1expr (car forms)))
	(t (let* ((fl (mapcar #'t1/c1expr forms))
		  (output-form (first (last fl)))
		  (output-type (and output-form (c1form-type output-form))))
	     (make-c1form* 'PROGN :type output-type :args fl)))))

(defun c2progn (c1form forms)
  (declare (ignore c1form))
  ;; c1progn ensures that the length of forms is not less than 1.
  (do ((l forms (cdr l))
       (lex *lex*))
      ((endp (cdr l))
       (c2expr (car l)))
    (let* ((this-form (first l))
	   (name (c1form-name this-form)))
      (let ((*destination* 'TRASH))
	(c2expr* (car l)))
      (setq *lex* lex)	; recycle lex locations
      ;; Since PROGN does not have tags, any transfer of control means
      ;; leaving the current PROGN statement.
      (when (or (eq name 'GO) (eq name 'RETURN-FROM))
	(return)))))

(defun c1args* (forms)
  (mapcar #'c1expr forms))

;;; ----------------------------------------------------------------------

(defvar *compiler-temps*
	'(tmp0 tmp1 tmp2 tmp3 tmp4 tmp5 tmp6 tmp7 tmp8 tmp9))

(defmacro sys::define-inline-function (name vars &body body)
  (let ((temps nil)
	(*compiler-temps* *compiler-temps*))
    (dolist (var vars)
      (if (and (symbolp var)
	       (not (member var '(&OPTIONAL &REST &KEY &AUX) :test #'eq)))
	(push (or (pop *compiler-temps*)
		  (gentemp "TMP" (find-package 'COMPILER)))
	      temps)
	(error "The parameter ~s for the inline function ~s is illegal."
	       var name)))
    (let ((binding (cons 'LIST (mapcar
				#'(lambda (var temp) `(list ',var ,temp))
				vars temps))))
      `(progn
	 (defun ,name ,vars ,@body)
	 (define-compiler-macro ,name ,temps (list* 'LET ,binding ',body))))))
