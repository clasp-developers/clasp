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

(in-package :sys)

(defun (setf ext:symbol-macro) (expander name &optional env)
  (when env
    (error "Non-NIL environment passed to (setf ext:symbol-macro)"))
  (setf (get-sysprop name 'ext:symbol-macro) expander))

(defvar *defun-inline-hook* nil)

(defmacro when (condition &body forms)
  "Syntax: (when test {form}*)
If TEST evaluates to true, then evaluates FORMs and returns all values of the
last FORM.  If not (i.e. the TEST evaluates to NIL), simply returns NIL."
  `(if ,condition (progn ,@forms) nil))
(defmacro unless (pred &rest body)
  "Syntax: (unless test {form}*)
If TEST evaluates to NIL, then evaluates FORMs and returns all values of the
last FORM.  If not, simply returns NIL."
  `(IF (NOT ,pred) (PROGN ,@body)))

(defmacro and (&rest forms)
  (cond ((null forms) 't)
        ((null (cdr forms)) (car forms))
        (t `(if ,(car forms) (and ,@(cdr forms)) nil))))

(defmacro or (&rest forms)
  (cond ((null forms) 'nil)
        ((null (cdr forms)) (car forms))
        (t (let ((tmp (gensym)))
             `(let ((,tmp ,(car forms)))
                (if ,tmp
                    ,tmp
                    (or ,@(cdr forms))))))))

(defmacro defmacro (name lambda-list &body body &environment env)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (macro-function ',name) #',(ext:parse-macro name lambda-list body env))
     ',name))

(defmacro destructuring-bind (vl list &body body)
  (multiple-value-bind (decls body)
      (find-declarations body)
    (multiple-value-bind (whole dl arg-check ignorables)
        (destructure vl 'destructuring-bind)
      `(let* ((,whole ,list) ,@dl)
	 (declare (ignorable ,@ignorables))
         ,@decls
         ,@arg-check
         ,@body))))

(defmacro defvar (var &optional (form nil form-sp) doc-string)
  "Syntax: (defvar name [form [doc]])
Declares the variable named by NAME as a special variable.  If the variable
does not have a value, then evaluates FORM and assigns the value to the
variable.  FORM defaults to NIL.  The doc-string DOC, if supplied, is saved
as a VARIABLE doc and can be retrieved by (documentation 'NAME 'variable)."
  `(LOCALLY (DECLARE (SPECIAL ,var))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (SYS:*MAKE-SPECIAL ',var))
    ,@(when form-sp
	  `((UNLESS (BOUNDP ',var)
	      (SETQ ,var ,form))))
    ,@(when (ext:current-source-location)
        `((setf (core:variable-source-info ',var)
                ',(ext:current-source-location))))
    ,@(when doc-string
        `((ext:annotate ',var 'documentation 'variable ,doc-string)))
    ',var))

(defmacro defparameter (var form &optional doc-string)
  "Syntax: (defparameter name form [doc])
Declares the global variable named by NAME as a special variable and assigns
the value of FORM to the variable.  The doc-string DOC, if supplied, is saved
as a VARIABLE doc and can be retrieved by (documentation 'NAME 'variable)."
  `(LOCALLY (DECLARE (SPECIAL ,var))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (SYS:*MAKE-SPECIAL ',var))
     (SETQ ,var ,form)
     ,@(when (ext:current-source-location)
         `((setf (core:variable-source-info ',var)
                 ',(ext:current-source-location))))
    ,@(when doc-string
        `((ext:annotate ',var 'documentation 'variable ,doc-string)))
     ',var))

;; export as extension?

(defmacro defconstant-eqx (var form test &optional doc-string)
  "Like DEFCONSTANT, but doesn't fire if the form is equal under TEST to an
existing value."
  (let ((value (gensym)))
    `(PROGN
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (let ((,value ,form))
           (cond ((symbol-constantp ',var)
                  (unless (,test ,value (symbol-value ',var))
                    ;; This will just trigger the error in SET.
                    (set ',var ,value)))
                 ((ext:specialp ',var)
                  (error "Cannot redefine special variable ~a as constant" ',var))
                 (t (set ',var ,value)
                    (setf (symbol-constantp ',var) t)))))
       ,@(when (ext:current-source-location)
           `((setf (core:variable-source-info ',var)
                   ',(ext:current-source-location))))
       ,@(when doc-string
           `((ext:annotate ',var 'documentation 'variable ',doc-string)))
       ',var)))

(defmacro defconstant (var form &optional doc-string)
  "Syntax: (defconstant symbol form [doc])
Declares that the global variable named by SYMBOL is a constant with the value
of FORM as its constant value.  The doc-string DOC, if supplied, is saved as a
VARIABLE doc and can be retrieved by (DOCUMENTATION 'SYMBOL 'VARIABLE)."
  `(defconstant-eqx ,var ,form eql ,doc-string))

(defmacro defconstant-equal (var form &optional doc-string)
  `(defconstant-eqx ,var ,form equal ,doc-string))

(defmacro defun (name lambda-list &body body &environment env)
   ;; Documentation in help.lisp
   (multiple-value-bind (decls body doc-string) 
       (si:process-declarations body t)
     (let* ((doclist (when doc-string (list doc-string)))
            (sname (si::function-block-name name))
            (global-function
              `#'(lambda ,lambda-list
                   (declare (lambda-name ,name) ,@decls) 
                   ,@doclist
                   (block ,sname ,@body))))
       `(progn 
          (eval-when (:compile-toplevel)
            ;; this function won't be ready for a while, but it's okay as there's no
            ;; compiler to run :compile-toplevel forms anyway.
            (cmp::register-global-function-def 'defun ',name))
          (setf (fdefinition ',name) ,global-function)
          ,@(and *defun-inline-hook*
                 (list (funcall *defun-inline-hook* name global-function env)))
          ',name))))

(defmacro define-compiler-macro (name vl &rest body &environment env)
  ;; CLHS doesn't actually say d-c-m has compile time effects, but it's nice to match defmacro  
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (compiler-macro-function ',name)
           (function ,(ext:parse-compiler-macro name vl body env)))
     ',name))

(defmacro lambda (&rest body) `(function (lambda ,@body)))

;; Augmented by a compiler macro once cleavir is loaded.
(defmacro ext:with-current-source-form ((&rest forms) &body body)
  "Within BODY, the \"current source form\" will be the first element of FORMS
for which a source location can be ascertained, if any. This source form and
its source location will be used in condition reports from the compiler.
WITH-CURRENT-SOURCE-FORM is intended for use in macroexpansion and compiler
macroexpansion functions, to improve error reporting. For example, a SETF-like
macro could make each place the current source form while processing it; then
if that processing results in an error, the compiler report can localize that
to the place, not the SETF-like-form as a whole.
Outside of the compiler, this operator has no effect (i.e. evaluates the BODY
as a progn)."
  ;; Evaluate forms for their side effects. Otherwise, inoperative.
  `(progn ,@forms ,@body))

; conditionals

(defmacro cond (&rest clauses &aux (form nil))
  "Syntax: (cond {(test {form}*)}*)
Evaluates TESTs in order until one evaluates to non-NIL.  Then evaluates FORMs
in order that follow the TEST and returns all values of the last FORM.  If no
forms follow the TEST, then returns the value of the TEST.  Returns NIL, if no
TESTs evaluates to non-NIL."
  (dolist (l (reverse clauses) form)	; don't use nreverse here
    (ext:with-current-source-form (l)
      (if (endp (cdr l))
          (if (eq (car l) 't)
              (setq form 't)
              (let ((sym (gensym)))
                (setq form `(LET ((,sym ,(car l)))
                              (IF ,sym ,sym ,form)))))
          (if (eq (car l) 't)
              (setq form (if (endp (cddr l))
                             (cadr l)
                             `(PROGN ,@(cdr l))))
              (setq form (if (endp (cddr l))
                             `(IF ,(car l) ,(cadr l) ,form)
                             `(IF ,(car l) (PROGN ,@(cdr l)) ,form))))))))

; program feature

(defmacro prog (vl &rest body)
  "Syntax: (prog ({var | (var [init])}*) {decl}* {tag | statement}*)
Establishes a NIL block, binds each VAR to the value of INIT (which defaults
to NIL) in parallel, and executes STATEMENTs.  Returns NIL."
  (multiple-value-bind (decl body) (find-declarations body)
    `(BLOCK NIL (LET ,vl ,@decl (TAGBODY ,@body)))))

(defmacro prog* (vl &rest body)
  "Syntax: (prog* ({var | (var [init])}*) {decl}* {tag | statement}*)
Establishes a NIL block, binds each VAR to the value of INIT (which defaults
to NIL) sequentially, and executes STATEMENTs.  Returns NIL."
  (multiple-value-bind (decl body) (find-declarations body)
    `(BLOCK NIL (LET* ,vl ,@decl (TAGBODY ,@body)))))

; sequencing

(defmacro prog1 (first &rest body &aux (sym (gensym)))
  "Syntax: (prog1 first-form {form}*)
Evaluates FIRST-FORM and FORMs in order.  Returns the primary value of FIRST-FORM."
  (if (null body)
      `(values ,first)
      `(LET ((,sym ,first))
         ,@body ,sym)))


(defmacro prog2 (first second &rest body &aux (sym (gensym)))
  "Syntax: (prog2 first-form second-form {forms}*)
Evaluates FIRST-FORM, SECOND-FORM, and FORMs in order.  Returns the value of
SECOND-FORM."
  `(PROGN ,first
          (LET ((,sym ,second))
            ,@body ,sym)))

; multiple values

(defmacro multiple-value-list (form)
  "Evaluates FORM and returns a list of all values FORM returns."
  `(MULTIPLE-VALUE-CALL #'LIST ,form))

(defmacro multiple-value-bind (vars form &rest body)
  "Syntax: (multiple-value-bind ({var}*) init {decl}* {form}*)

Evaluates INIT and binds the N-th VAR to the N-th value of INIT or, if INIT
returns less than N values, to NIL.  Then evaluates FORMs, and returns all
values of the last FORM.  If no FORM is given, returns NIL."
  (declare (notinline mapcar))
  ;; Note that in cclasp, a compiler macro (in inline.lisp) takes over from this macro.
  (if (= (length vars) 1)
      ;; at the moment we don't handle multiple-value-call well, so this is probably
      ;; faster. Might be so in the future too.
      ;; Who would write m-v-b with one variable, you ask? Computers! (Mostly SETF.)
      `(let ((,(first vars) ,form)) ,@body)
      (let ((restvar (gensym)))
        `(multiple-value-call #'(lambda (&optional ,@(mapcar #'list vars) &rest ,restvar)
                                  (declare (ignore ,restvar))
                                  ,@body)
           ,form))))

(defmacro case (keyform &rest clauses)
  (let* ((last t)
	 (form nil)
	 (key (gensym)))
    (dolist (clause (reverse clauses)
	            `(LET ((,key ,keyform))
                       ;; in (case foo (default bar)) the key is unused.
                       (declare (ignorable ,key))
		       ,form))
      (ext:with-current-source-form (clause)
        (let ((selector (car clause)))
          (cond ((or (eq selector T) (eq selector 'OTHERWISE))
                 (unless last
                   (simple-program-error
                    "CASE: The selector ~A can only appear at the last position."
                    selector))
                 (setq form `(PROGN ,@(cdr clause))))
                ((listp selector)
                 (setq form `(IF (or ,@(ext:with-current-source-form (selector)
                                         (mapcar (lambda (obj)
                                                   `(eql ,key ',obj))
                                                 selector)))
                                 (PROGN ,@(cdr clause))
                                 ,form)))
                (selector
                 (setq form `(IF (EQL ,key ',selector)
                                 (PROGN ,@(cdr clause))
                                 ,form))))
          (setq last nil))))))

(defmacro return (&optional (val nil)) `(RETURN-FROM NIL ,val))

;; Declarations
(defmacro declaim (&rest decl-specs)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(mapcar #'(lambda (decl-spec)
                   `(proclaim ',decl-spec))
               decl-specs)))

(defmacro in-package (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (si::select-package ,(string name))
     *package*))

(defmacro define-symbol-macro (symbol expansion)
  (cond ((not (symbolp symbol))
	 (simple-program-error "DEFINE-SYMBOL-MACRO: ~A is not a symbol"
		               symbol))
	((ext:specialp symbol)
	 (error "DEFINE-SYMBOL-MACRO: cannot redefine a special variable, ~A"
		symbol))
	(t
	 `(progn
            (eval-when (:compile-toplevel :load-toplevel :execute)
              (setf (ext:symbol-macro ',symbol)
                    #'(lambda (form env)
                        (declare (ignore form env))
                        ',expansion)))
            ,@(when (ext:current-source-location)
                `((setf (core:variable-source-info ',symbol)
                        ',(ext:current-source-location))))
            ',symbol))))

(defmacro nth-value (n expr)
  `(nth ,n (multiple-value-list ,expr)))

;;; These are not needed by the bytecode compiler, and it in fact ignores them,
;;; but they are needed by clasp-cleavir.

(defmacro cl:catch (tag &rest forms)
  `(core:catch-function
    ,tag (lambda () (declare (core:lambda-name catch-lambda)) (progn ,@forms))))

(defmacro cl:throw (tag result-form)
  `(core:throw-function
    ,tag (lambda () (declare (core:lambda-name throw-lambda)) (progn ,result-form))))
