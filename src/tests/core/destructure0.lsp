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
;;  (declare (ignorable env))
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

#-cando
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

