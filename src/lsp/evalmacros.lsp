;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "SYSTEM")

(defmacro unless (pred &rest body)
  "Syntax: (unless test {form}*)
If TEST evaluates to NIL, then evaluates FORMs and returns all values of the
last FORM.  If not, simply returns NIL."
  `(IF (NOT ,pred) (PROGN ,@body)))

(defmacro defmacro (&whole whole name vl &body body &aux doc-string)
  ;; Documentation in help.lsp
  (multiple-value-bind (function pprint doc-string)
      (sys::expand-defmacro name vl body)
    (setq function `(function ,function))
    (when *dump-defun-definitions*
      (print function)
      (setq function `(si::bc-disassemble ,function)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,(ext:register-with-pde whole `(si::fset ',name ,function t ,pprint))
       ,@(si::expand-set-documentation name 'function doc-string)
       ',name)))

(defmacro defvar (&whole whole var &optional (form nil form-sp) doc-string)
  "Syntax: (defvar name [form [doc]])
Declares the variable named by NAME as a special variable.  If the variable
does not have a value, then evaluates FORM and assigns the value to the
variable.  FORM defaults to NIL.  The doc-string DOC, if supplied, is saved
as a VARIABLE doc and can be retrieved by (documentation 'NAME 'variable)."
  `(LOCALLY (DECLARE (SPECIAL ,var))
    (SYS:*MAKE-SPECIAL ',var)
    ,@(when form-sp
	  `((UNLESS (BOUNDP ',var)
	      (SETQ ,var ,form))))
    ,@(si::expand-set-documentation var 'variable doc-string)
    ,(ext:register-with-pde whole)
    ,(if *bytecodes-compiler*
         `(eval-when (:compile-toplevel)
            (sys:*make-special ',var))
         `(eval-when (:compile-toplevel)
            (si::register-global ',var)))
    ',var))

(defmacro defparameter (&whole whole var form &optional doc-string)
  "Syntax: (defparameter name form [doc])
Declares the global variable named by NAME as a special variable and assigns
the value of FORM to the variable.  The doc-string DOC, if supplied, is saved
as a VARIABLE doc and can be retrieved by (documentation 'NAME 'variable)."
  `(LOCALLY (DECLARE (SPECIAL ,var))
    (SYS:*MAKE-SPECIAL ',var)
    (SETQ ,var ,form)
    ,@(si::expand-set-documentation var 'variable doc-string)
    ,(ext:register-with-pde whole)
    ,(if *bytecodes-compiler*
         `(eval-when (:compile-toplevel)
            (sys:*make-special ',var))
         `(eval-when (:compile-toplevel)
            (si::register-global ',var)))
    ',var))

(defmacro defconstant (&whole whole var form &optional doc-string)
  "Syntax: (defconstant symbol form [doc])

Declares that the global variable named by SYMBOL is a constant with the value
of FORM as its constant value.  The doc-string DOC, if supplied, is saved as a
VARIABLE doc and can be retrieved by (DOCUMENTATION 'SYMBOL 'VARIABLE)."
  `(PROGN
     (SYS:*MAKE-CONSTANT ',var ,form)
    ,@(si::expand-set-documentation var 'variable doc-string)
    ,(ext:register-with-pde whole)
    ,(if *bytecodes-compiler*
         `(eval-when (:compile-toplevel)
            (sys:*make-constant ',var ,form))
         `(eval-when (:compile-toplevel)
            (sys:*make-constant ',var ,form)
            (si::register-global ',var)))
    ',var))

(defparameter *defun-inline-hook* nil)

(defmacro defun (&whole whole name vl &body body &environment env &aux doc-string)
  ;; Documentation in help.lsp
  (multiple-value-setq (body doc-string) (remove-documentation body))
  (let* ((function `#'(ext::lambda-block ,name ,vl ,@body))
	 (global-function `#'(ext::lambda-block ,name ,vl
                                                (declare (si::c-global))
                                                ,@body)))
    (when *dump-defun-definitions*
      (print function)
      (setq function `(si::bc-disassemble ,function)))
    `(progn
       ,(ext:register-with-pde whole `(si::fset ',name ,global-function))
       ,@(si::expand-set-documentation name 'function doc-string)
       ,(let ((hook *defun-inline-hook*))
	  (and hook (funcall hook name global-function env)))
       ',name)))

;;;
;;; This is a no-op unless the compiler is installed
;;;
(defmacro define-compiler-macro (&whole whole name vl &rest body)
  (multiple-value-bind (function pprint doc-string)
      (sys::expand-defmacro name vl body)
    (declare (ignore pprint))
    (setq function `(function ,function))
    (when *dump-defun-definitions*
      (print function)
      (setq function `(si::bc-disassemble ,function)))
    `(progn
       (put-sysprop ',name 'sys::compiler-macro ,function)
       ,@(si::expand-set-documentation name 'function doc-string)
       ,(ext:register-with-pde whole)
       ',name)))

(defun compiler-macro-function (name &optional env)
  (declare (ignorable env))
  (get-sysprop name 'sys::compiler-macro))

;;; Each of the following macros is also defined as a special form,
;;; as required by CLtL. Some of them are used by the compiler (e.g.
;;; dolist), some not at all (e.g. defun).
;;; Thus their names need not be exported.

(let ()
  ;; We enclose the macro in a LET form so that it is no longer
  ;; a toplevel form. This solves the problem of this simple LOOP
  ;; replacing the more complex form in loop2.lsp when evalmacros.lsp
  ;; gets compiled.
(defmacro loop (&rest body &aux (tag (gensym)))
  "Syntax: (loop {form}*)
Establishes a NIL block and executes FORMs repeatedly.  The loop is normally
terminated by a non-local exit."
  `(BLOCK NIL (TAGBODY ,tag (PROGN ,@body) (GO ,tag))))
)

(defmacro lambda (&rest body)
  `(function (lambda ,@body)))

(defmacro lambda-block (name lambda-list &rest lambda-body)
  (multiple-value-bind (decl body doc)
      (si::process-declarations lambda-body)
    (when decl (setq decl (list (cons 'declare decl))))
    `(lambda ,lambda-list ,@doc ,@decl
      (block ,(si::function-block-name name) ,@body))))

; assignment

(defmacro psetq (&rest args)
  "Syntax: (psetq {var form}*)
Similar to SETQ, but evaluates all FORMs first, and then assigns each value to
the corresponding VAR.  Returns NIL."
   (do ((l args (cddr l))
        (forms nil)
        (bindings nil))
       ((endp l) (list* 'LET* (nreverse bindings) (nreverse (cons nil forms))))
       (let ((sym (gensym)))
            (push (list sym (cadr l)) bindings)
            (push (list 'setq (car l) sym) forms)))
   )

; conditionals

(defmacro cond (&rest clauses &aux (form nil))
  "Syntax: (cond {(test {form}*)}*)
Evaluates TESTs in order until one evaluates to non-NIL.  Then evaluates FORMs
in order that follow the TEST and returns all values of the last FORM.  If no
forms follow the TEST, then returns the value of the TEST.  Returns NIL, if no
TESTs evaluates to non-NIL."
  (dolist (l (reverse clauses) form)	; don't use nreverse here
    (if (endp (cdr l))
	(if (eq (car l) 't)
	    (setq form 't)
	    (let ((sym (gensym)))
	      (setq form `(LET ((,sym ,(car l)))
;			   (DECLARE (:READ-ONLY ,sym)) ; Beppe
			   (IF ,sym ,sym ,form)))))
	(if (eq (car l) 't)
	    (setq form (if (endp (cddr l))
			   (cadr l)
			   `(PROGN ,@(cdr l))))
	    (setq form (if (endp (cddr l))
			   `(IF ,(car l) ,(cadr l) ,form)
			   `(IF ,(car l) (PROGN ,@(cdr l)) ,form))))))
  )

; program feature

(defmacro prog (vl &rest body &aux (decl nil))
  "Syntax: (prog ({var | (var [init])}*) {decl}* {tag | statement}*)
Establishes a NIL block, binds each VAR to the value of INIT (which defaults
to NIL) in parallel, and executes STATEMENTs.  Returns NIL."
  (multiple-value-setq (decl body)
    (find-declarations body))
  `(BLOCK NIL (LET ,vl ,@decl (TAGBODY ,@body)))
  )

(defmacro prog* (vl &rest body &aux (decl nil))
  "Syntax: (prog* ({var | (var [init])}*) {decl}* {tag | statement}*)
Establishes a NIL block, binds each VAR to the value of INIT (which defaults
to NIL) sequentially, and executes STATEMENTs.  Returns NIL."
  (multiple-value-setq (decl body)
    (find-declarations body))
  `(BLOCK NIL (LET* ,vl ,@decl (TAGBODY ,@body)))
  )

; sequencing

(defmacro prog1 (first &rest body &aux (sym (gensym)))
  "Syntax: (prog1 first-form {form}*)
Evaluates FIRST-FORM and FORMs in order.  Returns the value of FIRST-FORM."
  (if (null body) first
  `(LET ((,sym ,first))
;    (DECLARE (:READ-ONLY ,sym)) ; Beppe
    ,@body ,sym)))

(defmacro prog2 (first second &rest body &aux (sym (gensym)))
  "Syntax: (prog2 first-form second-form {forms}*)
Evaluates FIRST-FORM, SECOND-FORM, and FORMs in order.  Returns the value of
SECOND-FORM."
  `(PROGN ,first (LET ((,sym ,second))
;		       (DECLARE (:READ-ONLY ,sym)) ; Beppe
		       ,@body ,sym)))

; multiple values

(defmacro multiple-value-list (form)
  "Evaluates FORM and returns a list of all values FORM returns."
  `(MULTIPLE-VALUE-CALL 'LIST ,form))

(defmacro multiple-value-setq (vars form)
  "Syntax: (multiple-value-setq {var}* form)

Evaluates FORM and binds the N-th VAR to the N-th value of FORM or, if FORM
returns less than N values, to NIL.  Returns the first value of FORM or, if
FORM returns no value, NIL."
  (do ((vl vars (cdr vl))
       (sym (gensym))
       (forms nil)
       (n 0 (truly-the fixnum (1+ n))))
      ((endp vl) `(LET ((,sym (MULTIPLE-VALUE-LIST ,form))) ,@forms))
    (declare (fixnum n))
    (push `(SETQ ,(car vl) (NTH ,n ,sym)) forms)))

;; We do not use this macroexpanso, and thus we do not care whether
;; it is efficiently compiled by ECL or not.
(defmacro multiple-value-bind (vars form &rest body)
  "Syntax: (multiple-value-bind ({var}*) init {decl}* {form}*)

Evaluates INIT and binds the N-th VAR to the N-th value of INIT or, if INIT
returns less than N values, to NIL.  Then evaluates FORMs, and returns all
values of the last FORM.  If no FORM is given, returns NIL."
  (declare (notinline mapcar))
  `(multiple-value-call #'(lambda (&optional ,@(mapcar #'list vars)) ,@body) ,form))

(defun while-until (test body jmp-op)
  (declare (si::c-local))
  (let ((label (gensym))
	(exit (gensym)))
    `(TAGBODY
        (GO ,exit)
      ,label
        ,@body
      ,exit
	(,jmp-op ,test (GO ,label)))))

(defmacro sys::while (test &body body)
  (while-until test body 'when))

(defmacro sys::until (test &body body)
  (while-until test body 'unless))

(defmacro case (keyform &rest clauses)
  (let* ((last t)
	 (form nil)
	 (key (gensym)))
    (dolist (clause (reverse clauses)
	     `(LET ((,key ,keyform))
		;;(DECLARE (:READ-ONLY ,key)) ; Beppe
		,form))
      (let ((selector (car clause)))
	(cond ((or (eq selector T) (eq selector 'OTHERWISE))
	       (unless last
		 (si::signal-simple-error
		  'program-error nil
		  "CASE: The selector ~A can only appear at the last position."
		  (list selector)))
	       (setq form `(PROGN ,@(cdr clause))))
	      ((consp selector)
	       (setq form `(IF (MEMBER ,key ',selector)
			       (PROGN ,@(cdr clause))
			       ,form)))
	      (selector
	       (setq form `(IF (EQL ,key ',selector)
			       (PROGN ,@(cdr clause))
			       ,form))))
	(setq last nil)))))

(defmacro return (&optional (val nil)) `(RETURN-FROM NIL ,val))

;; Declarations
(let ()
(defmacro declaim (&rest decl-specs)
  (if (cdr decl-specs)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (mapcar #'proclaim ',decl-specs))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (proclaim ',(car decl-specs)))))
)

(let ()
(defmacro c-declaim (&rest decl-specs)
  (if (cdr decl-specs)
    `(eval-when (:compile-toplevel)
       (mapcar #'proclaim ',decl-specs))
    `(eval-when (:compile-toplevel)
       (proclaim ',(car decl-specs)))))
)

(defmacro in-package (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (si::select-package ,(string name))))

(defmacro define-symbol-macro (&whole whole symbol expansion)
  (cond ((not (symbolp symbol))
	 (error "DEFINE-SYMBOL-MACRO: ~A is not a symbol"
		symbol))
	((specialp symbol)
	 (error "DEFINE-SYMBOL-MACRO: cannot redefine a special variable, ~A"
		symbol))
	(t
	 `(eval-when (:compile-toplevel :load-toplevel :execute)
	   (put-sysprop ',symbol 'si::symbol-macro 
                        (lambda (form env) 
                          (declare (ignore form env))
                          ',expansion))
	   ,(ext:register-with-pde whole)
	   ',symbol))))

(defmacro nth-value (n expr)
  `(nth ,n (multiple-value-list ,expr)))

(defun maybe-unquote (form)
  (if (and (consp form) (eq (car form) 'quote))
      (second form)
      form))

(defun maybe-quote (form)
  ;; Quotes a form only if strictly required. This happens only when FORM is
  ;; either a symbol and not a keyword
  (if (if (atom form)
	  (typep form '(and symbol (not keyword) (not boolean)))
	  (not (eq (first form) 'quote)))
      (list 'quote form)
      form))

(defmacro ext:truly-the (&rest args)
  `(the ,@args))

(defmacro ext:checked-value (&rest args)
  `(the ,@args))
