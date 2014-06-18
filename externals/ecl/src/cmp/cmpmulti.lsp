;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPMULT  Multiple-value-call and Multiple-value-prog1.

(in-package "COMPILER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
(defun c1multiple-value-call (args &aux forms)
  (check-args-number 'MULTIPLE-VALUE-CALL args 1)
  (cond
   ;; (M-V-C #'FUNCTION) => (FUNCALL #'FUNCTION)
   ((endp (rest args))
    (c1funcall args))
   ;; (M-V-C #'FUNCTION (VALUES A ... Z)) => (FUNCALL #'FUNCTION A ... Z)
   ((and (= (length args) 2)
	 (consp (setq forms (second args)))
	 (eq 'VALUES (first forms)))
    (c1funcall (list* (first args) (rest forms))))
   ;; More complicated case.
   (t
    (let ((function (gensym))
	  (frame (gensym)))
      `(with-stack ,frame
	 (let* ((,function ,(first args)))
	   ,@(loop for i in (rest args)
		collect `(stack-push-values ,frame ,i))
	   (si::apply-from-stack-frame ,frame ,function)))))))

(defun c1multiple-value-prog1 (args)
  (check-args-number 'MULTIPLE-VALUE-PROG1 args 1)
  (let ((frame (gensym)))
    `(with-stack ,frame
       (stack-push-values ,frame ,(first args))
       ,@(rest args)
       (stack-pop ,frame))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Beppe:
;;; this is the WRONG way to handle 1 value problem.
;;; should be done in c2values, so that (values (truncate a b)) can
;;; be used to restrict to one value, so we would not have to warn
;;; if this occurred in a proclaimed fun.

(defun c1values (args)
  (make-c1form* 'VALUES :args (c1args* args)))

(defun c2values (c1form forms)
  (declare (ignore c1form))
  (when (and (eq *destination* 'RETURN-OBJECT)
             (rest forms)
             (consp *current-form*)
             (eq 'DEFUN (first *current-form*)))
    (cmpwarn "Trying to return multiple values. ~
              ~%;But ~a was proclaimed to have single value.~
              ~%;Only first one will be assured."
             (second *current-form*)))
  (cond
   ;; When the values are not going to be used, then just
   ;; process each form separately.
   ((eq *destination* 'TRASH)
    (mapc #'c2expr* forms)
    ;; We really pass no value, but we need UNWIND-EXIT to trigger all the
    ;; frame-pop, stack-pop and all other exit forms.
    (unwind-exit 'VALUE0)
    )
   ;; For (VALUES) we can replace the output with either NIL (if the value
   ;; is actually used) and set only NVALUES when the value is the output
   ;; of a function.
   ((endp forms)
    (cond ((eq *destination* 'RETURN)
	   (wt-nl "value0 = ECL_NIL;")
	   (wt-nl "cl_env_copy->nvalues = 0;")
	   (unwind-exit 'RETURN))
	  ((eq *destination* 'VALUES)
	   (wt-nl "cl_env_copy->values[0] = ECL_NIL;")
	   (wt-nl "cl_env_copy->nvalues = 0;")
	   (unwind-exit 'VALUES))
	  (t
	   (unwind-exit 'NIL))))
   ;; For a single form, we must simply ensure that we only take a single
   ;; value of those that the function may output.
   ((endp (rest forms))
    (let ((form (first forms)))
      (if (or (not (member *destination* '(RETURN VALUES)))
              (c1form-single-valued-p form))
          (c2expr form)
          (progn
            (let ((*destination* 'VALUE0)) (c2expr* form))
            (unwind-exit 'VALUE0)))))
   ;; In all other cases, we store the values in the VALUES vector,
   ;; and force the compiler to retrieve anything out of it.
   (t
    (let* ((nv (length forms))
	   (*inline-blocks* 0)
           (*temp* *temp*)
	   (forms (nreverse (coerce-locs (inline-args forms)))))
      ;; By inlining arguments we make sure that VL has no call to funct.
      ;; Reverse args to avoid clobbering VALUES(0)
      (wt-nl "cl_env_copy->nvalues = " nv ";")
      (do ((vl forms (rest vl))
	   (i (1- (length forms)) (1- i)))
	  ((null vl))
	(declare (fixnum i))
	(wt-nl "cl_env_copy->values[" i "] = " (first vl) ";"))
      (unwind-exit 'VALUES)
      (close-inline-blocks)))))

(defun c1multiple-value-setq (args &aux (vars nil) (temp-vars nil)
			      (late-bindings nil))
  (check-args-number 'MULTIPLE-VALUE-SETQ args 2 2)
  (dolist (var (reverse (first args)))
    (cmpck (not (symbolp var)) "The variable ~s is not a symbol." var)
    (let* ((var-or-form (chk-symbol-macrolet var))
	   (type t))
      (unless (when (symbolp var-or-form)
		(cmpck (constantp var-or-form)
		       "The constant ~s is being assigned a value." var-or-form)
		(when (or (not (policy-type-assertions))
			  (trivial-type-p
			   (setf type (variable-type-in-env var-or-form))))
		  (push var-or-form vars)
		  t))
	(let ((new-var (gensym)))
	  (push new-var vars)
	  (push new-var temp-vars)
	  (push `(setf ,var-or-form (checked-value ,type ,new-var)) late-bindings)))))
  (let ((value (second args)))
    (cond (temp-vars
	   `(let* (,@temp-vars)
	      (multiple-value-setq ,vars ,value)
	      ,@late-bindings))
	  ((endp vars)
	   `(values ,value))
	  ((= (length vars) 1)
	   `(setq ,(first vars) ,value))
	  (t
	   (setq value (c1expr value)
		 vars (mapcar #'c1vref vars))
	   (add-to-set-nodes-of-var-list
	    vars (make-c1form* 'MULTIPLE-VALUE-SETQ :args vars value))))))

(defun bind-or-set (loc v use-bind)
  (cond ((not use-bind)
	 (set-var loc v))
	((or (plusp (var-ref v))
	     (member (var-kind v) '(SPECIAL GLOBAL)))
	 (bind loc v))))

(defun values-loc-or-value0 (i)
  (if (plusp i) (values-loc i) 'VALUE0))

(defun do-m-v-setq (vars form use-bind &aux min-values max-values)
  ;; This routine moves values from the multiple-value stack into the
  ;; variables VARS. The amount of values is not known (or at least we only
  ;; know that there is some number between MIN-VALUES and MAX-VALUES).  If
  ;; the values are to be created with BIND, then USED-BIND=T.  The output of
  ;; this routine is a location containing the first value (typically, the
  ;; name of the first variable).
  ;;
  (when (= (length vars) 1)
    (let ((*destination* (first vars)))
      (c2expr* form)
      (return-from do-m-v-setq *destination*)))

  ;; Store the values in the values stack + value0. Try guessing how
  ;; many they are.
  (multiple-value-bind (min-values max-values)
      (c1form-values-number form)

    ;; We save the values in the value stack + value0
    (let ((*destination* 'RETURN))
      (c2expr* form))

    ;; At least we always have NIL value0
    (setf min-values (max 1 min-values))

    ;; We know that at least MIN-VALUES variables will get a value
    (dotimes (i min-values)
      (when vars
	(let ((v (pop vars))
	      (loc (values-loc-or-value0 i)))
	  (bind-or-set loc v use-bind))))

    (when (some #'useful-var-p vars)
      (let* ((*lcl* *lcl*)
	     (nr (make-lcl-var :type :int))
	     (tmp (make-lcl-var)))
	(wt-nl-open-brace)
	(wt-nl "const int " nr " = cl_env_copy->nvalues;")
	(wt-nl "cl_object " tmp ";")
	(loop for v in vars
	   for i from min-values
	   for loc = (values-loc-or-value0 i)
	   do (when (useful-var-p v)
		(wt-nl tmp " = (" nr "<=" i ")? ECL_NIL : " loc ";")
		(bind-or-set tmp v use-bind)))
	(wt-nl-close-brace)))
    'VALUE0))

(defun c2multiple-value-setq (c1form vars form)
  (declare (ignore c1form))
  (unwind-exit (do-m-v-setq vars form nil)))

(defun c1multiple-value-bind (args)
  (check-args-number 'MULTIPLE-VALUE-BIND args 2)
  (let* ((*cmp-env* (cmp-env-copy))
         (variables (pop args))
         (init-form (pop args)))
    (when (= (length variables) 1)
      (return-from c1multiple-value-bind
        `(let* ((,(first variables) ,init-form))
	   ,@args)))
    (multiple-value-bind (body ss ts is other-decls)
        (c1body args nil)
      (c1declare-specials ss)
      (let* ((vars (loop for name in variables
                      collect (c1make-var name ss is ts))))
        (setq init-form (c1expr init-form))
        (mapc #'push-vars vars)
        (check-vdecl variables ts is)
        (setq body (c1decl-body other-decls body))
        (mapc #'check-vref vars)
        (make-c1form* 'MULTIPLE-VALUE-BIND :type (c1form-type body)
                      :local-vars vars
                      :args vars init-form body)))))

(defun c2multiple-value-bind (c1form vars init-form body)
  (declare (ignore c1form))
  (let* ((*unwind-exit* *unwind-exit*)
	 (*env-lvl* *env-lvl*)
	 (*env* *env*)
	 (*lcl* *lcl*)
	 (labels nil)
	 (env-grows nil)
	 (nr (make-lcl-var :type :int))
	 (*inline-blocks* 0)
	 min-values max-values)
    ;; 1) Retrieve the number of output values
    (multiple-value-setq (min-values max-values)
      (c1form-values-number init-form))

    ;; 2) For all variables which are not special and do not belong to
    ;;    a closure, make a local C variable.
    (dolist (var vars)
      (declare (type var var))
      (let ((kind (local var)))
	(if kind
	    (when (useful-var-p var)
	      (maybe-open-inline-block)
	      (bind (next-lcl) var)
	      (wt-nl (rep-type->c-name kind) " " *volatile* var ";")
	      (wt-comment (var-name var)))
	    (unless env-grows (setq env-grows (var-ref-ccb var))))))

    ;; 3) If there are closure variables, set up an environment.
    (when (setq env-grows (env-grows env-grows))
      (let ((env-lvl *env-lvl*))
	(maybe-open-inline-block)
	(wt-nl "volatile cl_object env" (incf *env-lvl*)
	       " = env" env-lvl ";")))

    ;; 4) Assign the values to the variables, compiling the form
    ;;    and binding the variables in the process.
    (do-m-v-setq vars init-form t)

    ;; 5) Compile the body. If there are bindings of special variables,
    ;;    these bindings are undone here.
    (c2expr body)

    ;; 6) Close the C expression.
    (close-inline-blocks)))

