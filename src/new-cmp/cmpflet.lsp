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

(defun c1labels (destination args)
  (labels/flet-transform destination 'LABELS args))

(defun c1flet (destination args)
  (labels/flet-transform destination 'FLET args))

(defun labels/flet-transform (destination origin args)
  (check-args-number origin args 1)
  (let ((definitions (pop args))
        (fun-names '())
        (fun-bodies '())
        (local-funs '())
        (vars '())
        (setf-statements '())
        (output))
    ;; If there are no definitions, do as an ordinary body with
    ;; declarations.
    (unless definitions
      (return-from labels/flet-transform
        (c1translate destination `(LOCALLY ,@args))))

    ;; On a first round, we extract the definitions of the functions,
    ;; and build empty function objects that record the references to
    ;; this functions in the processed body.
    (dolist (def definitions)
      (cmpck (or (endp def)
		 (not (si::valid-function-name-p (car def)))
		 (endp (cdr def)))
	     "The local function definition ~s is illegal." def)
      (let ((name (pop def)))
        (cmpck (member name fun-names :test #'same-fname-p)
               "The function ~s was already defined." name)
        (push name fun-names)
        (let* ((var (gensym "CLOSURE-VAR"))
               (fun (make-fun :name name :var var)))
          (push fun local-funs)
          (push def fun-bodies)
          (push var vars)
          (push `(MAKE-FLET/LABELS-CLOSURE ,var ,fun) setf-statements))))

    ;; Compile an extended form in which we include the possible closure
    ;; variables associated to the functions.
    (setf local-funs (nreverse local-funs)
          fun-bodies (nreverse fun-bodies)
          output (c1translate destination 
                              `(LET ,vars
                                 (DO-FLET/LABELS ,origin ,local-funs ,fun-bodies)
                                 ,@setf-statements
                                 ,@args)))

    ;; Once we have everything compiled, we can inspect the nature of
    ;; the functions: whether they are closures or not, etc. We keep
    ;; on inspecting the functions until the closure type does not
    ;; change.
    (loop while
	 (let ((x nil))
	   (loop for f in local-funs
	      when (compute-fun-closure-type f)
	      do (setf x t))
	   x))

    output))

(defun c1do-flet/labels (destination args)
  (unless (eq destination 'TRASH)
    (error "Internal error in C1DO-FLET/LABELS: output value should not be used"))
  (let* ((origin (pop args))
         (local-funs (pop args))
         (definitions (first args))
         (new-env (cmp-env-copy *cmp-env*)))
    (loop for f in local-funs
       for v = (fun-var f)
       ;; We enlarge the environment
       do (cmp-env-register-function f new-env)
       ;; and assign to the function its variable. Previously
       ;; we only stored the names.
       do (setf (fun-var f) (cmp-env-search-var v *cmp-env*)))

    ;; We compile the functions, either in an empty environment in
    ;; which there are no new functions
    (let ((*cmp-env* (if (eq origin 'FLET) *cmp-env* new-env)))
      (loop for fun in local-funs
         for body in definitions
         ;; The closure type will be fixed later on by COMPUTE-...
         do (c1compile-function body :fun fun :CB/LB 'LB)))

    ;; When we are in a LABELs form, we have to propagate the external
    ;; variables from one function to the other functions that use it.
    (dolist (f1 local-funs)
      (let ((vars (fun-referred-vars f1)))
        (dolist (f2 local-funs)
          (when (and (not (eq f1 f2))
                     (member f1 (fun-referred-funs f2)))
            (add-referred-variables-to-function f2 vars)))))

    (setf *cmp-env* new-env)

    (c1do-flet/labels-op local-funs)))

(defun c1make-flet/labels-closure (destination args)
  (let* ((var (pop args))
         (funob (pop args)))
    (nconc (c1set-loc (c1vref var) (list 'MAKE-CCLOSURE funob))
           (c1set-loc destination nil))))

(defun fun-referred-local-vars (fun)
  (remove-if #'(lambda (v) (member (var-kind v) '(SPECIAL GLOBAL REPLACED DISCARDED)))
	     (fun-referred-vars fun)))

(defun compute-fun-closure-type (fun)
  (labels
      ((closure-type (fun &aux (lambda-form (fun-lambda fun)))
	 (let ((vars (fun-referred-local-vars fun))
	       (funs (remove fun (fun-referred-funs fun) :test #'child-p))
	       (closure nil))
	   ;; it will have a full closure if it refers external non-global variables
	   (dolist (var vars)
	     ;; ...across CB
	     (if (ref-ref-ccb var)
		 (setf closure 'CLOSURE)
		 (unless closure (setf closure 'LEXICAL))))
	   ;; ...or if it directly calls a function
	   (dolist (f funs)
	     ;; .. which has a full closure
	     (case (fun-closure f)
	       (CLOSURE (setf closure 'CLOSURE))
	       (LEXICAL (unless closure (setf closure 'LEXICAL)))))
	   ;; ...or the function itself is referred across CB, either
           ;; directly or through a second indirection to the function
           ;; variable.
	   (when closure
	     (when (or (fun-ref-ccb fun)
		       (and (fun-var fun)
                            (not (unused-variable-p (fun-var fun)))))
	       (setf closure 'CLOSURE)))
	   closure))
       (child-p (presumed-parent fun)
	 (let ((real-parent (fun-parent fun)))
	   (when real-parent
	     (or (eq real-parent presumed-parent)
		 (child-p real-parent presumed-parent))))))
    ;; This recursive algorithm is guaranteed to stop when functions
    ;; do not change.
    (let ((new-type (closure-type fun))
	  (old-type (fun-closure fun)))
;;       (format t "~%CLOSURE-TYPE: ~A ~A -> ~A, ~A" (fun-name fun)
;;        	      old-type new-type (fun-parent fun))
;;       (print (fun-referred-vars fun))
      ;; Same type
      (when (eq new-type old-type)
	(return-from compute-fun-closure-type nil))
      ;; {lexical,closure} -> no closure!
      ;; closure -> {lexical, no closure}
      (when (or (and (not new-type) old-type)
		(eq old-type 'CLOSURE))
	(baboon))
      (setf (fun-closure fun) new-type)
      ;; All external, non-global variables become of type closure
      (when (eq new-type 'CLOSURE)
	(when (fun-global fun)
	  (cmpnote "Function ~A is global but is closed over some variables.~%~{~A ~}"
                   (fun-name fun) (mapcar #'var-name (fun-referred-vars fun))))
	(dolist (var (fun-referred-local-vars fun))
	  (setf (var-ref-clb var) nil
		(var-ref-ccb var) t
		(var-kind var) 'CLOSURE
		(var-loc var) 'OBJECT))
	(dolist (f (fun-referred-funs fun))
	  (setf (fun-ref-ccb f) t)))
      ;; If the status of some of the children changes, we have
      ;; to recompute the closure type.
      (do ((finish nil t)
	   (recompute nil))
	(finish
	 (when recompute (compute-fun-closure-type fun)))
	(dolist (f (fun-child-funs fun))
	  (when (compute-fun-closure-type f)
	    (setf recompute t finish nil))))
      t)))

(defun local-function-ref (fname &optional build-object)
  (multiple-value-bind (fun ccb clb unw)
      (cmp-env-search-function fname)
    (when fun
      (when (functionp fun) 
	(when build-object
	  ;; Macro definition appears in #'.... This should not happen.
	  (cmperr "The name of a macro ~A was found in special form FUNCTION." name))
	(return-from local-function-ref nil))
      (incf (fun-ref fun))
      (cond (build-object
	     (setf (fun-ref-ccb fun) t))
	    (*current-function*
	     (push fun (fun-referred-funs *current-function*))))
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
