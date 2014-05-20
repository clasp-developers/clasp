;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPLET  Let and Let*.
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    ECL is free software; you can redistribute it and/or modify it
;;;;    under the terms of the GNU Library General Public License as
;;;;    published by the Free Software Foundation; either version 2 of
;;;;    the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

(defun c1let (args)
  (check-args-number 'LET args 1)
  (let ((bindings (pop args)))
    (cond ((null bindings)
           (c1locally args))
          ((atom bindings)
           (invalid-let-bindings 'LET bindings))
          ((null (rest bindings))
           (c1let/let* 'let* bindings args))
	  (t
	   (loop with temp
	      for b in bindings
	      if (atom b)
	      collect b into real-bindings
	      else collect (setf temp (gensym "LET")) into temp-names and
	      collect (cons temp (cdr b)) into temp-bindings and
	      collect (list (car b) temp) into real-bindings
	      finally
		(return (c1let/let* 'let*
				    (nconc temp-bindings real-bindings)
				    `((declare (ignorable ,@temp-names)
					       (:read-only ,@temp-names))
				      ,@args)))))
          (t
           (c1let/let* 'let bindings args)))))

(defun c1let* (args)
  (check-args-number 'LET* args 1)
  (let ((bindings (pop args)))
    (cond ((null bindings)
           (c1locally args))
          ((atom bindings)
           (invalid-let-bindings 'LET* bindings))
          (t
           (c1let/let* 'let* bindings args)))))

(defun c1let/let* (let/let* bindings body)
  (let* ((setjmps *setjmps*)
         (*cmp-env* (cmp-env-copy)))
    (multiple-value-bind (vars forms body)
        (process-let-bindings let/let* bindings body)
      ;; Try eliminating unused variables, replace constant ones, etc.
      (multiple-value-setq (vars forms)
        (c1let-optimize-read-only-vars vars forms body))
      ;; Verify that variables are referenced and assign final boxed / unboxed type
      (mapc #'check-vref vars)
      (let ((sp-change (some #'global-var-p vars)))
        (make-c1form* let/let*
                      :type (c1form-type body)
                      :volatile (not (eql setjmps *setjmps*))
                      :local-vars vars
                      :args vars forms body)))))

(defun invalid-let-bindings (let/let* bindings)
  (cmperr "Syntax error in ~A bindings:~%~4I~A"
          let/let* bindings))

(defun process-let-bindings (let/let* bindings body)
  (multiple-value-bind (body specials types ignoreds other-decls)
      (c1body body nil)
    (let ((vars '())
          (forms '()))
      (do ((b bindings)
           name form)
          ((atom b)
           (unless (null b)
             (invalid-let-bindings let/let* bindings)))
        (if (symbolp (setf form (pop b)))
            (setf name form form nil)
            (progn
              (check-args-number "LET/LET* binding" form 1 2)
              (setf name (first form) form (rest form))))
        (let* ((var (c1make-var name specials ignoreds types))
	       (type (var-type var))
               (init (cond ((null form)
			    (default-init var))
			   ((trivial-type-p type)
			    (c1expr (first form)))
			   (t
			    (c1expr `(checked-value ,type ,(first form)))))))
          ;; :read-only variable handling. Beppe
          (when (read-only-variable-p name other-decls)
	    (if (global-var-p var)
		(cmpwarn "Found :READ-ONLY declaration for global var ~A"
			 name)
		(setf (var-type var) (c1form-primary-type init)))
	    (multiple-value-bind (constantp value)
		(c1form-constant-p init)
	      (when constantp
		(cmp-env-register-symbol-macro name (si::maybe-quote value))
		(setf var nil))))
	  (when var
	    (push var vars)
	    (push init forms)
	    (when (eq let/let* 'LET*) (push-vars var)))))
      (setf vars (nreverse vars)
            forms (nreverse forms))
      (when (eq let/let* 'LET)
        (mapc #'push-vars vars))
      (check-vdecl (mapcar #'var-name vars) types ignoreds)
      (c1declare-specials specials)
      (values vars forms (c1decl-body other-decls body)))))

(defun c1let-optimize-read-only-vars (all-vars all-forms body)
  (loop with base = (list body)
     for vars on all-vars
     for forms on (nconc all-forms (list body))
     for var = (first vars)
     for form = (first forms)
     for rest-vars = (cdr vars)
     for rest-forms = (cdr forms)
     for read-only-p = (and (null (var-set-nodes var))
                            (null (var-functions-reading var))
                            (null (var-functions-setting var))
                            (not (global-var-p var)))
     when read-only-p
     do (fix-read-only-variable-type var form rest-forms)
     unless (and read-only-p
                (or (c1let-unused-variable-p var form)
                    (c1let-constant-value-p var form rest-vars rest-forms)
                    (c1let-constant-variable-p var form rest-vars rest-forms)
                    #+(or)
                    (c1let-can-move-variable-value-p var form rest-vars rest-forms)))
     collect var into used-vars and
     collect form into used-forms
     finally (return (values used-vars used-forms))))

(defun fix-read-only-variable-type (var form rest-forms)
  (and-form-type (var-type var) form (var-name var) :unsafe "In LET body")
  (let ((form-type (c1form-primary-type form)))
    (setf (var-type var) form-type)
    (update-variable-type var form-type)))

(defun c1let-unused-variable-p (var form)
  ;; * (let ((v2 e2)) e3 e4) => (let () e3 e4)
  ;;   provided
  ;;   - v2 does not appear in body
  ;;   - e2 produces no side effects
  (when (and (= 0 (var-ref var))
             (not (member (var-kind var) '(special global)))
             (not (form-causes-side-effect form)))
    (unless (var-ignorable var)
      (cmpdebug "Removing unused variable ~A" (var-name var)))
    (delete-c1forms form)
    t))

(defun c1let-constant-value-p (var form rest-vars rest-forms)
  ;;  (let ((v1 e1) (v2 e2) (v3 e3)) (expr e4 v2 e5))
  ;;  - v2 is a read only variable
  ;;  - the value of e2 is not modified in e3 nor in following expressions
  (when (and (eq (c1form-name form) 'LOCATION)
	     (loc-in-c1form-movable-p (c1form-arg 0 form)))
    (cmpdebug "Replacing variable ~A by its value ~A" (var-name var) form)
    (nsubst-var var form)
    t))

(defun c1let-constant-variable-p (var form rest-vars rest-forms)
  ;;  (let ((v1 e1) (v2 e2) (v3 e3)) (expr e4 v2 e5))
  ;;  - v2 is a read only variable
  ;;  - the value of e2 is not modified in e3 nor in following expressions
  (when (eq (c1form-name form) 'VAR)
    (let ((other-var (c1form-arg 0 form)))
      (unless (or (global-var-p other-var)
		  (member other-var rest-vars)
                  (var-changed-in-form-list other-var rest-forms))
        (cmpdebug "Replacing variable ~A by its value ~A" (var-name var) form)
        (nsubst-var var form)
        t))))

(defun c2let-replaceable-var-ref-p (var form rest-forms)
  (when (and (eq (c1form-name form) 'VAR)
	     (null (var-set-nodes var))
	     (local var))
    (let ((var1 (c1form-arg 0 form)))
      (declare (type var var1))
      (when (and ;; Fixme! We should be able to replace variable
	     ;; even if they are referenced across functions.
	     ;; We just need to keep track of their uses.
	     (local var1)
	     (eq (unboxed var) (unboxed var1))
	     (not (var-changed-in-form-list var1 rest-forms)))
	(cmpdebug "Replacing variable ~a by its value" (var-name var))
	(nsubst-var var form)
	t))))

(defun c1let-can-move-variable-value-p (var form rest-vars rest-forms)
  ;;  (let ((v1 e1) (v2 e2) (v3 e3)) (expr e4 v2 e5))
  ;;  can become
  ;;  (let ((v1 e1) (v3 e3)) (expr e4 e2 e5))
  ;;  provided
  ;;  - v2 appears only once
  ;;  - v2 appears only in body
  ;;  - e2 does not affect v1 nor e3, e3 does not affect e2
  ;;  - e4 does not affect e2
  (when (and (= 1 (var-ref var))
             (not (form-causes-side-effect form))
             ;; it does not refer to special variables which
             ;; are changed in the LET form
             (notany #'(lambda (v) (var-referenced-in-form v form)) rest-vars)
             (replaceable var rest-forms))
    (cmpdebug "Replacing variable ~A by its value ~A" (var-name var) form)
    (nsubst-var var form)
    t))

(defun read-only-variable-p (v other-decls)
  (dolist (i other-decls nil)
    (when (and (eq (car i) :READ-ONLY)
	       (member v (rest i)))
      (return t))))

(defun env-grows (possibily)
  ;; if additional closure variables are introduced and this is not
  ;; last form, we must use a new env.
  (and possibily
       (plusp *env*)
       (dolist (exit *unwind-exit*)
	 (case exit
	   (RETURN (return NIL))
	   (BDS-BIND)
	   (t (return T))))))

;; should check whether a form before var causes a side-effect
;; exactly one occurrence of var is present in forms
(defun replaceable (var form)
  (labels ((abort-on-side-effects (form)
             (if (eq (c1form-name form) 'VAR)
                 (when (eq var (first (c1form-args form)))
                   (return-from replaceable t))
                 (when (c1form-side-effects form)
                   (return-from replaceable nil)))))
    (traverse-c1form-tree form #'abort-on-side-effects)
    (baboon :format-control "In REPLACEABLE, variable ~A not found. Form:~%~A"
            :format-arguments (list (var-name var) *current-form*))))

(defun c2let* (c1form vars forms body
	       &aux
	       (*volatile* (c1form-volatile* c1form))
	       (*unwind-exit* *unwind-exit*)
	       (*env* *env*)
	       (*env-lvl* *env-lvl*)
	       (*inline-blocks* 0))
  (declare (type boolean block-p))

  ;; Replace read-only variables when it is worth doing it.
  (loop for var in vars
     for rest-forms on (append forms (list body))
     for form = (first rest-forms)
     unless (c2let-replaceable-var-ref-p var form rest-forms)
     collect var into used-vars and
     collect form into used-forms
     finally (setf vars used-vars forms used-forms))

  ;; Emit C definitions of local variables
  (loop for var in vars
     for kind = (local var)
     do (when kind
	  (maybe-open-inline-block)
	  (bind (next-lcl (var-name var)) var)
	  (wt-nl *volatile* (rep-type-name kind) " " var ";")))

  ;; Create closure bindings for closed-over variables
  (when (some #'var-ref-ccb vars)
    (maybe-open-inline-block)
    (let ((env-lvl *env-lvl*))
      (wt-nl *volatile* "cl_object env" (incf *env-lvl*) " = env" env-lvl ";")))

  ;; Assign values
  (loop for form in forms
     for var in vars
     do (case (var-kind var)
	  ((LEXICAL CLOSURE SPECIAL GLOBAL)
	   (case (c1form-name form)
	     (LOCATION (bind (c1form-arg 0 form) var))
	     (VAR (bind (c1form-arg 0 form) var))
	     (t (bind-init form var))))
	  (t ; local var
	   (let ((*destination* var)) ; nil (ccb)
	     (c2expr* form)))))
	  
  ;; Optionally register the variables with the IHS frame for debugging
  (if (policy-debug-variable-bindings)
      (let ((*unwind-exit* *unwind-exit*))
        (wt-nl-open-brace)
        (let* ((env (build-debug-lexical-env vars)))
          (when env (push 'IHS-ENV *unwind-exit*))
          (c2expr body)
          (wt-nl-close-brace)
          (when env (pop-debug-lexical-env))))
      (c2expr body))

  (close-inline-blocks :line))

(defun discarded (var form body &aux last)
  (labels ((last-form (x &aux (args (c1form-args x)))
	     (case (c1form-name x)
	       (PROGN
		 (last-form (car (last (first args)))))
	       ((LET LET* FLET LABELS BLOCK CATCH)
		(last-form (car (last args))))
	       (VAR (c1form-arg 0 x))
	       (t x))))
    (and (not (form-causes-side-effect form))
	 (or (< (var-ref var) 1)
	     (and (= (var-ref var) 1)
		  (eq var (last-form body))
		  (eq 'TRASH *destination*))))))

(defun nsubst-var (var form)
  (when (var-set-nodes var)
    (baboon :format-control "Cannot replace a variable that is to be changed"))
  (when (var-functions-reading var)
    (baboon :format-control "Cannot replace a variable that forms part of a closure"))
  (dolist (where (var-read-forms var))
    (unless (and (eql (c1form-name where) 'VAR)
                 (eql (c1form-arg 0 where) var))
      (baboon :format-control "VAR-READ-NODES are only C1FORMS of type VAR"))
    (delete-from-read-nodes var where)
    (c1form-replace-with where form))
  (setf (var-ignorable var) 0))

(defun member-var (var list)
  (let ((kind (var-kind var)))
    (if (member kind '(SPECIAL GLOBAL))
	(member var list :test
		#'(lambda (v1 v2)
		    (and (member (var-kind v2) '(SPECIAL GLOBAL))
			 (eql (var-name v1) (var-name v2)))))
	(member var list))))
