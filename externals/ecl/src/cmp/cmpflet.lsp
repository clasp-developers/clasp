;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPFLET  Flet, Labels, and Macrolet.

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

(defun c1labels (args) (c1labels/flet 'LABELS args))

(defun c1flet (args) (c1labels/flet 'FLET args))

(defun c1labels/flet (origin args)
  (check-args-number origin args 1)
  (let ((new-env (cmp-env-copy))
	(defs '())
	(local-funs '())
	(fnames '())
	body-c1form)
    ;; On a first round, we extract the definitions of the functions,
    ;; and build empty function objects that record the references to
    ;; this functions in the processed body. In the end
    ;;	DEFS = ( { ( fun-object  function-body ) }* ).
    (dolist (def (car args))
      (cmpck (or (endp def)
		 (not (si::valid-function-name-p (car def)))
		 (endp (cdr def)))
	     "The local function definition ~s is illegal." def)
      (cmpck (member (car def) fnames)
	     "The function ~s was already defined." (car def))
      (push (car def) fnames)
      (let* ((name (car def))
	     (var (make-var :name name :kind :object))
	     (fun (make-fun :name name :var var)))
	(cmp-env-register-function fun new-env)
	(push (cons fun (cdr def)) defs)))

    ;; Now we compile the functions, either in an empty environment
    ;; in which there are no new functions
    (let ((*cmp-env* (cmp-env-copy (if (eq origin 'FLET) *cmp-env* new-env))))
      (dolist (def (nreverse defs))
	(let ((fun (first def)))
	  ;; The closure type will be fixed later on by COMPUTE-...
	  (push (c1compile-function (rest def) :fun fun :CB/LB 'LB)
		local-funs))))

    ;; When we are in a LABELs form, we have to propagate the external
    ;; variables from one function to the other functions that use it.
    (when (eq origin 'LABELS)
      (loop for change = nil
	 do (loop for f1 in local-funs
	       for vars = (fun-referenced-vars f1)
	       for funs = (fun-referenced-funs f1)
	       do (loop for f2 in (fun-referencing-funs f1)
		     for c1 = (add-to-fun-referenced-vars f2 vars)
		     for c2 = (add-to-fun-referenced-funs f2 funs)
		     for c3 = (update-fun-closure-type f2)
		     when (or c1 c2 c3)
		     do (setf change t)))
	 do (unless change (return))))

    ;; Now we can compile the body itself.
    (let ((*cmp-env* new-env))
      (multiple-value-bind (body ss ts is other-decl)
	  (c1body (rest args) t)
	(c1declare-specials ss)
	(check-vdecl nil ts is)
	(setq body-c1form (c1decl-body other-decl body))))

    ;; Keep only functions that have been referenced at least once.
    ;; It is not possible to look at FUN-REF before because functions
    ;; in a LABELS can reference each other.
    (setf local-funs (remove-if-not #'plusp local-funs :key #'fun-ref))

    ;; Keep on inspecting the functions until the closure type does not
    ;; change.
    (loop while
	 (let ((x nil))
	   (loop for f in local-funs
	      when (update-fun-closure-type f)
	      do (setf x t))
	   x))

    (if local-funs
	(make-c1form* 'LOCALS :type (c1form-type body-c1form)
		      :args local-funs body-c1form (eq origin 'LABELS))
	body-c1form)))

(defun child-function-p (presumed-parent fun)
  (declare (optimize speed))
  (loop for real-parent = (fun-parent fun)
     while real-parent
     do (if (eq real-parent presumed-parent)
	    (return t)
	    (setf fun real-parent))))

(defun compute-closure-type (fun)
  (declare (si::c-local))
  (let ((lexical-closure-p nil))
    ;; it will have a full closure if it refers external non-global variables
    (dolist (var (fun-referenced-vars fun))
      (cond ((global-var-p var))
	    ;; ...across CB
	    ((ref-ref-ccb var)
	     (return-from compute-closure-type 'CLOSURE))
	    (t
	     (setf lexical-closure-p t))))
    ;; ...or if it directly calls a function
    (dolist (f (fun-referenced-funs fun))
      (unless (child-function-p fun f)
	;; .. which has a full closure
	(case (fun-closure f)
	  (CLOSURE (return-from compute-closure-type 'CLOSURE))
	  (LEXICAL (setf lexical-closure-p t)))))
    ;; ...or the function itself is referred across CB
    (when lexical-closure-p
      (if (or (fun-ref-ccb fun)
	      (and (fun-var fun)
		   (plusp (var-ref (fun-var fun)))))
	  'CLOSURE
	  'LEXICAL))))

(defun update-fun-closure-type-many (function-list)
  (do ((finish nil t)
       (recompute nil))
      (finish
       recompute)
    (dolist (f function-list)
      (when (update-fun-closure-type f)
	(setf recompute t finish nil)))))

(defun prepend-new (l1 l2)
  (loop for f in l1
     do (pushnew f l2))
  l2)

(defun update-fun-closure-type (fun)
  (let ((old-type (fun-closure fun)))
    (when (eq old-type 'closure)
      (return-from update-fun-closure-type nil))
    ;; This recursive algorithm is guaranteed to stop when functions
    ;; do not change.
    (let ((new-type (compute-closure-type fun))
	  (to-be-updated (fun-child-funs fun)))
      ;; Same type
      (when (eq new-type old-type)
	(return-from update-fun-closure-type nil))
      (when (fun-global fun)
	(cmpnote "Function ~A is global but is closed over some variables.~%~{~A ~}"
		 (fun-name fun) (mapcar #'var-name (fun-referenced-vars fun))))
      (setf (fun-closure fun) new-type)
      ;; All external, non-global variables become of type closure
      (when (eq new-type 'CLOSURE)
	(dolist (var (fun-referenced-vars fun))
	  (unless (or (global-var-p var)
		      (eq (var-kind var) new-type))
	    (setf (var-ref-clb var) nil
		  (var-ref-ccb var) t
		  (var-kind var) 'CLOSURE
		  (var-loc var) 'OBJECT
		  to-be-updated
		  (prepend-new (var-functions-reading var)
			       (prepend-new (var-functions-setting var)
					    to-be-updated)))))
	(dolist (f (fun-referenced-funs fun))
	  (setf (fun-ref-ccb f) t)))
      ;; If the status of some of the children changes, we have
      ;; to recompute the closure type.
      (when (update-fun-closure-type-many to-be-updated)
	(update-fun-closure-type fun))
      t)))

(defun c2locals (c1form funs body labels ;; labels is T when deriving from labels
		 &aux
		 (*env* *env*)
		 (*inline-blocks* 0)
		 (*env-lvl* *env-lvl*))
  (declare (ignore c1form))
  ;; create location for each function which is returned,
  ;; either in lexical:
  (loop with env-grows = nil
     with closed-vars = '()
     for fun in funs
     for var = (fun-var fun)
     when (plusp (var-ref var))
     do (case (var-kind var)
	  ((lexical closure)
	   (push var closed-vars)
	   (unless env-grows
	     (setq env-grows (var-ref-ccb var))))
	  (otherwise
	   (maybe-open-inline-block)
	   (bind (next-lcl) var)
	   (wt-nl "cl_object " *volatile* var ";")))
     finally
     ;; if we have closed variables
       (when (env-grows env-grows)
	 (maybe-open-inline-block)
	 (let ((env-lvl *env-lvl*))
	   (wt "cl_object " *volatile* "env" (incf *env-lvl*) " = env" env-lvl ";")))
     ;; bind closed locations because of possible circularities
       (loop for var in closed-vars
	  do (bind nil var)))
  ;; create the functions:
  (mapc #'new-local funs)
  ;; - then assign to it
  (loop for fun in funs
     for var = (fun-var fun)
     when (plusp (var-ref var))
     do (set-var (list 'MAKE-CCLOSURE fun) var))
  (c2expr body)
  (close-inline-blocks))


(defun c1decl-body (decls body)
  (if (null decls)
      (c1progn body)
      (let* ((*cmp-env* (reduce #'add-one-declaration decls
                                :initial-value (cmp-env-copy *cmp-env*))))
        (c1progn body))))

(defun c1locally (args)
  (multiple-value-bind (body ss ts is other-decl)
      (c1body args t)
    (if (or ss ts is other-decl)
        (let ((*cmp-env* (cmp-env-copy)))
          (c1declare-specials ss)
          (check-vdecl nil ts is)
          (c1decl-body other-decl body))
        (c1progn body))))

(defun c1macrolet (args)
  (check-args-number 'MACROLET args 1)
  (let ((*cmp-env* (cmp-env-register-macrolet (first args) (cmp-env-copy))))
    (c1locally (cdr args))))

(defun c1symbol-macrolet (args)
  (check-args-number 'SYMBOL-MACROLET args 1)
  (let ((*cmp-env* (cmp-env-copy)))
    (dolist (def (car args))
      (let ((name (first def)))	    
	(cmpck (or (endp def) (not (symbolp name)) (endp (cdr def)))
	     "The symbol-macro definition ~s is illegal." def)
	(cmp-env-register-symbol-macro name (second def))))
    (c1locally (cdr args))))

(defun local-function-ref (fname &optional build-object)
  (multiple-value-bind (fun ccb clb unw)
      (cmp-env-search-function fname)
    (when fun
      (when (functionp fun) 
	(when build-object
	  ;; Macro definition appears in #'.... This should not happen.
	  (cmperr "The name of a macro ~A was found in special form FUNCTION." fname))
	(return-from local-function-ref nil))
      (incf (fun-ref fun))
      (if build-object
	  (setf (fun-ref-ccb fun) t)
	  (let ((caller *current-function*))
	    (when (and caller
		       (not (member fun (fun-referenced-funs caller) :test #'eq)))
	      (push fun (fun-referenced-funs caller))
	      (push caller (fun-referencing-funs fun)))))
      ;; we introduce a variable to hold the funob
      (let ((var (fun-var fun)))
	(cond (ccb (when build-object
		     (setf (var-ref-ccb var) t
			   (var-kind var) 'CLOSURE))
		   (setf (fun-ref-ccb fun) t))
	      (clb (when build-object 
		     (setf (var-ref-clb var) t
			   (var-kind var) 'LEXICAL))))))
    fun))

(defun c2call-local (c1form fun args)
  (declare (type fun fun)
	   (ignore c1form))
  (unless (c2try-tail-recursive-call fun args)
    (let ((*inline-blocks* 0)
          (*temp* *temp*))
      (unwind-exit (call-loc (fun-name fun) fun (inline-args args)
			     (c1form-primary-type c1form)))
      (close-inline-blocks))))
