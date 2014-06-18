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

;;;; CMPLAM  Lambda expression.

(in-package "COMPILER")

;;; During Pass1, a lambda-list
;;;
;;; (	{ var }*
;;; 	[ &optional { var | ( var [ initform [ svar ] ] ) }* ]
;;; 	[ &rest var ]
;;; 	[ &key { var | ( { var | ( kwd var ) } [initform [ svar ]])}*
;;; 		[&allow-other-keys]]
;;; 	[ &aux {var | (var [initform])}*]
;;; )
;;;
;;; is transformed into
;;;
;;; (	( { var }* )				; required
;;; 	( { var initform svar }* )		; optional
;;; 	{ var | nil }				; rest
;;; 	allow-other-keys-flag
;;; 	( { kwd-vv-index var initform svar }* )	; key
;;; )
;;;
;;; where
;;; 	svar:	NIL	; means svar is not supplied
;;;	        | var
;;;
;;; &aux parameters will be embedded into LET*.
;;;
;;; c1lambda-expr receives
;;;	( lambda-list { doc | decl }* . body )
;;; and returns
;;;	( lambda info-object lambda-list' doc body' )
;;;
;;; Doc is NIL if no doc string is supplied.
;;; Body' is body possibly surrounded by a LET* (if &aux parameters are
;;; supplied) and an implicit block.

(defun c1lambda-doc (form)
  (second (c1form-args form)))

(defun c1lambda-body (form)
  (third (c1form-args form)))

(defun c1lambda-list (form)
  (first (c1form-args form)))

(defun add-referred-variables-to-function (fun var-list)
  (setf (fun-referred-vars fun)
	(set-difference (union (fun-referred-vars fun) var-list)
			(fun-local-vars fun)))
  fun)

(defun c1compile-function (lambda-list-and-body &key (fun (make-fun))
			   (name (fun-name fun)) (CB/LB 'CB))
  (setf (fun-name fun) name
	(fun-parent fun) *current-function*)
  (when *current-function*
    (push fun (fun-child-funs *current-function*)))
  (let* ((*lcl* 0)
         (*last-label* 0)
         (*current-function* fun))
    (c1lambda-expr fun lambda-list-and-body CB/LB)
    (c1set-function-closure-type fun)
    (setf (fun-last-lcl fun) *lcl*
          (fun-last-label fun) *last-label*))
  fun)

(defun cmp-process-lambda-list (list)
  (handler-case (si::process-lambda-list list 'function)
    (error (c) (cmperr "Illegal lambda list ~S" list))))

(defun c1set-function-properties (fun minargs maxargs declarations env)
  (let* ((name (fun-name fun))
         (global-p (and (assoc 'SI::C-GLOBAL declarations) t))
         (no-entry-p (and (or (assoc 'SI::C-LOCAL declarations)
                              #+ecl-min
                              (member name c-data::*in-all-symbols-functions*))
                          t))
         (debug (cmp-env-optimization 'debug env))
         cfun
         exported-p
         no-entry-p)
    (if global-p
        (multiple-value-setq (cfun exported-p) (exported-fname name))
        (setf cfun (next-cfun "LC~D~A" name) exported-p nil))
    (format t "~&;;; Function ~A is exported? ~A" name exported-p)
    (when (and no-entry-p (>= debug 2))
      (setf no-entry-p nil)
      (cmpnote "Ignoring SI::C-LOCAL declaration for ~A when DEBUG is ~D"
               name debug))
    ;; HACK! FIXME! This is for compatibility with old ECL.
    (when exported-p
      (multiple-value-setq (proclaimed-minargs maxargs found) (get-proclaimed-narg name))
      (if found
          (setf minargs proclaimed-minargs)
          (setf maxargs call-arguments-limit)))
    (setf (fun-name fun) name
          (fun-debug fun) debug
          (fun-minarg fun) minargs
          (fun-maxarg fun) maxargs
          (fun-global fun) global-p
          (fun-cfun fun) cfun
          (fun-exported fun) exported-p
          (fun-no-entry fun) no-entry-p
	  (fun-closure fun) nil
	  (fun-description fun) name)))

(defun c1set-function-closure-type (fun)
  (let ((children (fun-child-funs fun)))
    ;;
    ;; Ensure all variables referenced by children functions
    ;; are registered with this function...
    ;;
    (reduce #'add-referred-variables-to-function
	    (mapcar #'fun-referred-vars children)
	    :initial-value fun)
    (reduce #'add-referred-variables-to-function
	    (mapcar #'fun-referred-vars (fun-referred-funs fun))
	    :initial-value fun)
    ;;
    ;; ...and then compute closure type for function and children
    ;;
    (do ((finish nil))
	(finish)
      (setf finish t)
      (dolist (f children)
	(when (compute-fun-closure-type f)
	  (setf finish nil))))
    (compute-fun-closure-type fun)
    (when (fun-global fun)
      (if (fun-closure fun)
          (cmpnote "Function ~A is global but is closed over some variables.~%~{~A ~}"
                   (fun-name fun) (mapcar #'var-name (fun-referred-vars fun)))
          (new-defun fun (fun-no-entry fun))))))


(defun c1lambda-expr (fun lambda-expr CB/LB
                      &aux doc body ss is ts
                      other-decls
                      nargs
                      varargs
                      minargs
                      maxargs
                      compiled-body
                      (name (fun-name fun))
                      (block-name (si::function-block-name name))
                      (old-env *cmp-env*)
                      (*cmp-env* (cmp-env-mark CB/LB))
                      (*permanent-data* t))
  (declare (si::c-local))

  (cmpck (endp lambda-expr)
         "The lambda expression ~s is illegal." (cons 'LAMBDA lambda-expr))

  (multiple-value-setq (body ss ts is other-decls doc all-declarations)
    (c1body (cdr lambda-expr) t))

  (when block-name (setq body (list (cons 'BLOCK (cons block-name body)))))

  (multiple-value-bind (requireds optionals rest key-flag keywords
			allow-other-keys aux-vars)
      (cmp-process-lambda-list (car lambda-expr))

    ;; We need to add the declarations right here, because they should
    ;; affect _all_ statement in the function, including those in &optional,
    ;; &key arguments, etc.
    (setf *cmp-env* (add-declarations other-decls *cmp-env*))

    (setq minargs (pop requireds)
          maxargs (if (or rest (cdr keywords) allow-other-keys)
                      call-arguments-limit
                      (+ minargs (first optionals)))
          optionals (rest optionals))

    ;; At this point we know a lot about the function and can complete its
    ;; properties, including the C name, number of arguments, etc.
    (c1set-function-properties fun minargs maxargs other-decls *cmp-env*)

    ;;
    ;; Compile statements for processing required, optionals, rest and
    ;; keyword arguments.
    ;;
    (setf compiled-body (c1requireds requireds ss is ts))
    (let* ((nkeys (pop keywords)))
      (when (or optionals rest keywords)
        (setf nargs (make-var :name c-backend:+nargs-var+ :type 'FIXNUM
                              :loc '(VV "narg" 0)
                              :kind :fixnum)
              varargs (if (and (not (or rest keywords allow-other-keys))
                               (< maxargs 30))
                          c-backend:+simple-va-args+
                          c-backend:+cl-va-args+)
              varargs (make-var :name varargs :type 'T
                                :loc `(VV ,varargs 0)
                                :kind :OBJECT)
              rest (when rest
                     (let ((rest-var (c1make-var rest ss is ts)))
                       (cmp-env-register-var rest-var)
                       rest-var))
              compiled-body (nconc compiled-body
                                   (c1varargs-bind-op nargs varargs
                                                      minargs maxargs nkeys
                                                      (policy-check-nargs))
                                   (c1optionals optionals nargs varargs ss is ts)))
        (when (or rest keywords allow-other-keys)
          (setf compiled-body (nconc compiled-body
                                     (c1keywords rest keywords allow-other-keys
                                                 nargs varargs
                                                 ss is ts))))
        (setf compiled-body (nconc compiled-body
                                   (c1varargs-unbind-op nargs varargs
                                                        minargs maxargs nkeys)))))

    ;; Make other declarations take effect right now
    (setf *cmp-env* (reduce #'add-one-declaration other-decls
                            :initial-value *cmp-env*))

    ;; After creating all variables and processing the initalization
    ;; forms, we wil process the body. However, all free declarations,
    ;; that is declarations which do not refer to the function
    ;; arguments, have to be applied to the body. At the same time, we
    ;; replace &aux variables with a LET* form that defines them.
    (let* ((declarations other-decls)
           (type-checks (extract-lambda-type-checks requireds optionals
                                                    keywords ts other-decls))
           (type-check-forms (car type-checks))
           (aux-vars (nconc (cdr type-checks) aux-vars))
           (aux-var-names (loop for v in aux-vars by #'cddr
                             collect (first v)))
	   (new-variables (cmp-env-new-variables *cmp-env* old-env))
	   (already-declared-name (set-difference (mapcar #'var-name new-variables)
                                                  aux-var-names)))
      ;; Gather declarations for &aux variables, either special...
      (let ((specials (set-difference ss already-declared-names)))
        (when specials
          (push `(special ,@specials) declarations)))
      ;; ...ignorable...
      (let ((ignorables (loop for (var . expected-uses) in is
                           unless (member var already-declared-names)
                           collect var)))
        (when ignorables
          (push `(ignorable ,@ignorables) declarations)))
      ;; ...or type declarations
      (loop for (var . type) in ts
	    unless (member var already-declared-names)
	    do (push `(type ,type ,var) declarations))

      (let ((*cmp-env* (cmp-env-copy)))
        (when (policy-debug-variable-bindings)
          (cmp-env-register-cleanup (c1debug-env-pop-vars requireds) *cmp-env*))

        (setq body (c1lambda-body new-variables type-check-forms
                                  aux-vars declarations body))

        (when (policy-debug-ihs-frame)
          (setf body (nconc (c1debug-env-open block-name)
                            body
                            (c1debug-env-close block-name))))

        (let* ((bound-variables (set-difference new-variables requireds))
               (non-special-bound-variables (remove-if #'global-var-p bound-variables)))
          (setq compiled-body (nconc (c1bind non-special-bound-variables)
                                     compiled-body
                                     body
                                     (c1unbind new-variables)
                                     (c1set-loc 'ACTUAL-RETURN 'VALUES+VALUE0)))))

      (setf compiled-body (nconc (c1function-prologue fun)
                                 compiled-body
                                 (c1function-epilogue fun)))

      (setf (fun-lambda fun) compiled-body
            (fun-doc fun) doc
            (fun-lambda-list fun) (list requireds optionals rest
                                        key-flag keywords allow-other-keys)))))

(defun c1lambda-body (new-variables type-check-forms aux-vars declarations body)
  (cond ((and new-variables (policy-debug-variable-bindings))
         (let* ((cleanup (c1debug-env-pop-vars new-variables))
                (*cmp-env* (cmp-env-register-cleanup cleanup (cmp-env-copy))))
           (nconc (c1debug-env-push-vars new-variables)
                  (c1lambda-body nil aux-vars declarations body)
                  (c1debug-env-pop-vars new-variables t)))
         ((or aux-vars type-check-forms declarations)
          (c1translate 'VALUES+VALUE0
                       `(progn
                          ,@type-check-forms
                          `(let* ,(loop for specs on aux-vars by #'cddr
                                     for var = (first specs)
                                     for init = (second specs)
                                     collect (if init (list var init) var))
                             (declare ,@declarations)
                             ,@body))))
         (t
          (c1progn 'VALUES+VALUE0 body)))))

(defun c1requireds (requireds ss is ts)
  (c1bind-requireds
   (loop for i from 1
     for spec on requireds
     for name = (first spec)
     for var = (c1make-var name ss is ts)
     do (setf (first spec) var)
     do (push var (fun-local-vars *current-function*))
     do (cmp-env-register-var var)
     collect (cons var (next-lcl)))))

(defun c1keywords (rest-var keywords allow-other-keys nargs varargs ss is ts)
  (loop with keywords-list = '()
     with output = '()
     with nkeys = (/ (length keywords) 4)
     for spec on keywords by #'cddddr
     for i from 0
     for key = (first spec)
     for name = (second spec)
     for var = (c1make-var name ss is ts)
     for init = (third spec)
     for flag = (fourth spec)
     for flag-var = (and flag (c1make-var flag ss is ts))
     do (let* ((found-tag (make-tag :name (gensym "KEY-FOUND") :label (next-label)))
               (next-tag (make-tag :name (gensym "KEY-NEXT") :label (next-label))))
          (push key keywords-list)
          (setf output (nconc output
                              (c1jmp-true found-tag `(KEYVARS ,(+ nkeys i)))
                              (c1maybe-bind-special var init)
                              (and flag
                                   (setf flag (c1make-var flag ss is ts))
                                   (c1maybe-bind-special-op flag nil))
                              (c1jmp next-tag)
                              (list found-tag)
                              (c1maybe-bind-special-op var `(KEYVARS ,i))
                              (and flag
                                   (c1maybe-bind-special-op flag t))
                              (list next-tag)))
          (setf (first spec) var)
          (push var (fun-local-vars *current-function*))
          (cmp-env-register-var var)
          (when flag
            (setf (fourth spec) flag)
            (push flag (fun-local-vars *current-function*))
            (cmp-env-register-var flag)))
     finally (return (nconc (c1varargs-rest-op (or rest-var 'TRASH)
                                               nargs varargs (1+ i)
                                               (and keywords-list
                                                    (add-keywords (nreverse keywords-list)))
                                               allow-other-keys)
                            output))))

(defun c1optionals (optionals nargs varargs ss is ts)
  (unless optionals
    (return-from c1optionals nil))
  (loop with output = '()
     for spec on optionals by #'cdddr
     for name = (first spec)
     for init = (second spec)
     for var = (c1make-var name ss is ts)
     for flag = (third spec)
     do (let* ((found-tag (make-tag :name (gensym "OPT-FOUND") :label (next-label)))
               (next-tag (make-tag :name (gensym "OPT-NEXT") :label (next-label))))
          (setf output (nconc output
                              (c1jmp-nonzero found-tag nargs)
                              (c1maybe-bind-special var init)
                              (and flag
                                   (setf flag (c1make-var flag ss is ts))
                                   (c1maybe-bind-special-op flag nil))
                              (c1jmp next-tag)
                              (list found-tag)
                              (c1varargs-pop-op var nargs varargs)
                              (and flag
                                   (c1maybe-bind-special-op flag t))
                              (list next-tag))
                (first spec) var)
          (push var (fun-local-vars *current-function*))
          (cmp-env-register-var var)
          (when flag
            (setf (third spec) flag)
            (push flag (fun-local-vars *current-function*))
            (cmp-env-register-var flag)))
     finally (return output)))

(defun exported-fname (name)
  (let (cname)
    (if (and (symbolp name)
             (setf cname (get-sysprop name 'Lfun)))
        (values cname t)
        (values (next-cfun "L~D~A" name) nil))))

(defun new-defun (new &optional no-entry)
  (push new *global-funs*))

#| Steps:
 1. defun creates declarations for requireds + va_alist
 2. c2lambda-expr adds declarations for:
	unboxed requireds
	lexical optionals (+ supplied-p), rest, keywords (+ supplied-p)
    Lexical optionals and keywords can be unboxed if:
	a. there is more then one reference in the body
	b. they are not referenced in closures
 3. binding is performed for:
	special or unboxed requireds
	optionals, rest, keywords
|#

(defun optimize-funcall/apply-lambda (lambda-form arguments apply-p
				      &aux body apply-list apply-var
				      let-vars extra-stmts all-keys)
  (multiple-value-bind (requireds optionals rest key-flag keywords
				  allow-other-keys aux-vars)
      (cmp-process-lambda-list (car lambda-form))
    (when apply-p
      (setf apply-list (first (last arguments))
	    apply-var (gensym)
	    arguments (butlast arguments)))
    (setf arguments (copy-list arguments))
    (do ((scan arguments (cdr scan)))
	((endp scan))
      (let ((form (first scan)))
	(unless (constantp form)
	  (let ((aux-var (gensym)))
	    (push `(,aux-var ,form) let-vars)
	    (setf (car scan) aux-var)))))
    (when apply-var
      (push `(,apply-var ,apply-list) let-vars))
    (dolist (i (cdr requireds))
      (push (list i
		  (cond (arguments
			 (pop arguments))
			(apply-p
			 `(if ,apply-var
			   (pop ,apply-var)
			   (si::dm-too-few-arguments)))
			(t
			 (cmperr "Too few arguments for lambda form ~S"
                                 (cons 'LAMBDA lambda-form)))))
	    let-vars))
    (do ((scan (cdr optionals) (cdddr scan)))
	((endp scan))
      (let ((opt-var (first scan))
	    (opt-flag (third scan))
	    (opt-value (second scan)))
	(cond (arguments
	       (setf let-vars
		     (list* `(,opt-var ,(pop arguments))
			    `(,opt-flag t)
			    let-vars)))
	      (apply-p
	       (setf let-vars
		     (list* `(,opt-var (if ,apply-var
					   (pop ,apply-var)
					   ,opt-value))
			    `(,opt-flag ,apply-var)
			    let-vars)))
	      (t
	       (setf let-vars
		     (list* `(,opt-var ,opt-value)
			    `(,opt-flag nil)
			    let-vars))))))
    (when (or key-flag allow-other-keys)
      (unless rest
	(setf rest (gensym))))
    (when rest
      (push `(,rest ,(if arguments
			 (if apply-p
			     `(list* ,@arguments ,apply-var)
			     `(list ,@arguments))
			 (if apply-p apply-var nil)))
	    let-vars))
    (do ((scan (cdr keywords) (cddddr scan)))
	((endp scan))
      (let ((keyword (first scan))
	    (key-var (second scan))
	    (key-value (third scan))
	    (key-flag (or (fourth scan) (gensym))))
	(push keyword all-keys)
	(setf let-vars
	      (list*
	       `(,key-var (if (eq ,key-flag 'si::failed) ,key-value ,key-flag))
	       `(,key-flag (si::search-keyword ,rest ,keyword))
	       let-vars))
	(when (fourth scan)
	  (push `(setf ,key-flag (not (eq ,key-flag 'si::failed)))
		extra-stmts))))
    (when (and key-flag (not allow-other-keys))
      (push `(si::check-keyword ,rest ',all-keys) extra-stmts))
    `(let* ,(nreverse (delete-if-not #'first let-vars))
      ,@(multiple-value-bind (decl body)
	   (si::find-declarations (rest lambda-form))
	 (append decl extra-stmts body)))))
