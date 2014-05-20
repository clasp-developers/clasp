;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPPROP Type propagation.

(in-package "COMPILER")

(eval-when (:compile-toplevel :execute)
(defconstant +all-c1-forms+
  '((LOCATION		loc :pure :single-valued)
    (VAR		var :single-valued)
    (SETQ		var value-c1form :side-effects)
    (PSETQ		var-list value-c1form-list :side-effects)
    (BLOCK		blk-var progn-c1form :pure)
    (PROGN		body :pure)
    (PROGV		symbols values form :side-effects)
    (TAGBODY		tag-var tag-body :pure)
    (RETURN-FROM	blk-var return-type value variable-or-nil :side-effects)
    (FUNCALL		fun-value (arg-value*) :side-effects)
    (CALL-LOCAL		obj-fun (arg-value*) :side-effects)
    (CALL-GLOBAL	fun-name (arg-value*))
    (CATCH		catch-value body :side-effects)
    (UNWIND-PROTECT	protected-c1form body :side-effects)
    (THROW		catch-value output-value :side-effects)
    (GO			tag-var return-type :side-effects)
    (C-INLINE		(arg-c1form*)
			(arg-type-symbol*)
			output-rep-type
			c-expression-string
			side-effects-p
			one-liner-p)
    (LOCALS		local-fun-list body labels-p :pure)
    (IF			fmla-c1form true-c1form false-c1form :pure)
    (FMLA-NOT		fmla-c1form :pure)
    (FMLA-AND		* :pure)
    (FMLA-OR		* :pure)
    (LAMBDA		lambda-list doc body-c1form)
    (LET*		vars-list var-init-c1form-list decl-body-c1form :pure)
    (VALUES		values-c1form-list :pure)
    (MULTIPLE-VALUE-SETQ vars-list values-c1form-list :side-effects)
    (MULTIPLE-VALUE-BIND vars-list init-c1form body :pure)
    (COMPILER-LET	symbols values body)
    (FUNCTION		(GLOBAL/CLOSURE) lambda-form fun-object :single-valued)
    (RPLACD		(dest-c1form value-c1form) :side-effects)

    (SI:STRUCTURE-REF	struct-c1form type-name slot-index (:UNSAFE/NIL) :pure)
    (SI:STRUCTURE-SET	struct-c1form type-name slot-index value-c1form :side-effects)

    (WITH-STACK		body :side-effects)
    (STACK-PUSH-VALUES value-c1form push-statement-c1form :side-effects)

    (ORDINARY		c1form :pure)
    (LOAD-TIME-VALUE	dest-loc value-c1form :pure :single-valued)
    (SI:FSET		function-object vv-loc macro-p pprint-p lambda-form
			:side-effects)
    (MAKE-FORM		vv-loc value-c1form :side-effects)
    (INIT-FORM		vv-loc value-c1form :side-effects)
    (EXT:COMPILER-TYPECASE var expressions)
    (CHECKED-VALUE  type value-c1form let-form))))

(defconstant +c1-form-hash+
  #.(loop with hash = (make-hash-table :size 128 :test #'eq)
       for (name . rest) in +all-c1-forms+
       for length = (if (member '* rest) nil (length rest))
       for side-effects = (if (member :side-effects rest)
                              (progn (and length (decf length)) t)
                              nil)
       for movable = (if (member :pure rest)
                         (progn (and length (decf length)) t)
                         nil)
       for single-valued = (if (member :single-valued rest)
                               (progn (and length (decf length)) t)
                               nil)
       do (setf (gethash name hash) (list length side-effects movable single-valued))
       finally (return hash)))

(defconstant +c1-dispatch-alist+
  '((block . c1block) ; c1special
    (return-from . c1return-from) ; c1special
    (funcall . c1funcall) ; c1
    (catch . c1catch) ; c1special
    (unwind-protect . c1unwind-protect) ; c1special
    (throw . c1throw) ; c1special
    (ffi:defcallback . c1-defcallback) ; c1
    (progn . c1progn) ; c1special
    (ext:with-backend . c1with-backend) ; c1special
    (ffi:clines . c1clines) ; c1special
    (ffi:c-inline . c1c-inline) ; c1special
    (flet . c1flet) ; c1special
    (labels . c1labels) ; c1special
    (locally . c1locally) ; c1special
    (macrolet . c1macrolet) ; c1special
    (symbol-macrolet . c1symbol-macrolet) ; c1special

    (if . c1if) ; c1special
    (not . c1not) ; c1special
    (and . c1and) ; c1special
    (or . c1or) ; c1special

    (let . c1let) ; c1special
    (let* . c1let*) ; c1special

    (multiple-value-call . c1multiple-value-call) ; c1special
    (multiple-value-prog1 . c1multiple-value-prog1) ; c1special
    (values . c1values) ; c1
    (multiple-value-setq . c1multiple-value-setq) ; c1
    (multiple-value-bind . c1multiple-value-bind) ; c1

    (ext:compiler-typecase . c1compiler-typecase) ; c1special
    (checked-value . c1checked-value) ; c1special

    (quote . c1quote) ; c1special
    (function . c1function) ; c1special
    (the . c1the) ; c1special
    (ext:truly-the . c1truly-the) ; c1special
    (eval-when . c1eval-when) ; c1special
    (declare . c1declare) ; c1special
    (ext:compiler-let . c1compiler-let) ; c1special

    (with-stack . c1with-stack) ; c1
    (innermost-stack-frame . c1innermost-stack-frame) ; c1
    (stack-push . c1stack-push) ; c1
    (stack-push-values . c1stack-push-values) ; c1
    (stack-pop . c1stack-pop) ; c1
    (si::apply-from-stack-frame . c1apply-from-stack-frame) ; c1

    (tagbody . c1tagbody) ; c1special
    (go . c1go) ; c1special

    (setq . c1setq) ; c1special
    (progv . c1progv) ; c1special
    (psetq . c1psetq) ; c1special

    (load-time-value . c1load-time-value) ; c1
    (si:fset . c1fset) ; c1

    (apply . c1apply) ; c1
    ))

(defconstant +t1-dispatch-alist+
  '((ext:with-backend . c1with-backend) ; t1

    (defmacro . t1defmacro)
    (compiler-let . c1compiler-let)
    (eval-when . c1eval-when)
    (progn . c1progn)
    (macrolet . c1macrolet)
    (locally . c1locally)
    (symbol-macrolet . c1symbol-macrolet)
    ))

(defconstant +set-loc-dispatch-alist+
  '((bind . bind)
    (jump-true . set-jump-true)
    (jump-false . set-jump-false)

    (values . set-values-loc)
    (value0 . set-value0-loc)
    (return . set-return-loc)
    (trash . set-trash-loc)

    (the . set-the-loc)
    ))

(defconstant +wt-loc-dispatch-alist+
  '((call-normal . wt-call-normal)
    (call-indirect . wt-call-indirect)
    (ffi:c-inline . wt-c-inline-loc)
    (coerce-loc . wt-coerce-loc)

    (temp . wt-temp)
    (lcl . wt-lcl-loc)
    (fixnum-value . wt-number)
    (long-float-value . wt-number)
    (double-float-value . wt-number)
    (single-float-value . wt-number)
    (short-float-value . wt-number)
    (character-value . wt-character)
    (value . wt-value)
    (keyvars . wt-keyvars)
    (the . wt-the)

    (fdefinition . wt-fdefinition)
    (make-cclosure . wt-make-closure)

    (structure-ref . wt-structure-ref)

    (nil . "ECL_NIL")
    (t . "ECL_T")
    (return . "value0")
    (values . "cl_env_copy->values[0]")
    (va-arg . "va_arg(args,cl_object)")
    (cl-va-arg . "ecl_va_arg(args)")
    (value0 . "value0")
    ))

(defconstant +c2-dispatch-alist+
  '((block . c2block) ; c2
    (return-from . c2return-from) ; c2
    (funcall . c2funcall) ; c2
    (call-global . c2call-global) ; c2
    (catch . c2catch) ; c2
    (unwind-protect . c2unwind-protect) ; c2
    (throw . c2throw) ; c2
    (progn . c2progn) ; c2
    (ffi:c-inline . c2c-inline) ; c2
    (locals . c2locals) ; c2
    (call-local . c2call-local) ; c2

    (if . c2if)
    (fmla-not . c2fmla-not)
    (fmla-and . c2fmla-and)
    (fmla-or . c2fmla-or)

    (let* . c2let*)

    (values . c2values) ; c2
    (multiple-value-setq . c2multiple-value-setq) ; c2
    (multiple-value-bind . c2multiple-value-bind) ; c2

    (function . c2function) ; c2
    (ext:compiler-let . c2compiler-let) ; c2

    (with-stack . c2with-stack) ; c2
    (stack-push-values . c2stack-push-values) ; c2

    (tagbody . c2tagbody) ; c2
    (go . c2go) ; c2

    (var . c2var/location) ; c2
    (location . c2var/location) ; c2
    (setq . c2setq) ; c2
    (progv . c2progv) ; c2
    (psetq . c2psetq) ; c2

    (si:fset . c2fset) ; c2

    (ext:compiler-typecase . c2compiler-typecase)
    (checked-value . c2checked-value)
    ))

(defconstant +t2-dispatch-alist+
  '((compiler-let . t2compiler-let)
    (progn . t2progn)
    (ordinary . t2ordinary)
    (load-time-value . t2load-time-value)
    (make-form . t2make-form)
    (init-form . t2init-form)
    ))

(defconstant +p1-dispatch-alist+
  '((block . p1block)
    (return-from . p1return-from)
    (call-global . p1call-global)
    (call-local . p1call-local)
    (catch . p1catch)
    (throw . p1throw)
    (if . p1if)
    (fmla-not . p1fmla-not)
    (fmla-and . p1fmla-and)
    (fmla-or . p1fmla-or)
    (lambda . p1lambda)
    (let* . p1let*)
    (locals . p1locals)
    (multiple-value-bind . p1multiple-value-bind)
    (multiple-value-setq . p1multiple-value-setq)
    (progn . p1progn)
    (progv . p1progv)
    (setq . p1setq)
    (psetq . p1psetq)
    (tagbody . p1tagbody)
    (go . p1go)
    (unwind-protect . p1unwind-protect)
    (ordinary . p1ordinary)
    (sys::fset . p1fset)
    (var . p1var)
    (values . p1values)
    (location . p1trivial) ;; Some of these can be improved
    (ffi:c-inline . p1trivial)
    (function . p1trivial)
    (funcall . p1trivial)
    (load-time-value . p1trivial)
    (make-form . p1trivial)
    (init-form . p1trivial)
    (c::with-stack . p1with-stack)
    (c::stack-push-values . p1stack-push-values)
    (ext:compiler-typecase . p1compiler-typecase)
    (checked-value . p1checked-value)
    ))

(defun make-dispatch-table (alist)
  (loop with hash = (make-hash-table :size (max 128 (* 2 (length alist)))
				     :test #'eq)
     for (name . function) in alist
     do (setf (gethash name hash) function)
     finally (return hash)))

(defparameter *c1-dispatch-table* (make-dispatch-table +c1-dispatch-alist+))

(defparameter *t1-dispatch-table* (make-dispatch-table +t1-dispatch-alist+))

(defparameter *c2-dispatch-table* (make-dispatch-table +c2-dispatch-alist+))

(defparameter *set-loc-dispatch-table* (make-dispatch-table +set-loc-dispatch-alist+))

(defparameter *wt-loc-dispatch-table* (make-dispatch-table +wt-loc-dispatch-alist+))

(defparameter *t2-dispatch-table* (make-dispatch-table +t2-dispatch-alist+))

(defparameter *p1-dispatch-table* (make-dispatch-table +p1-dispatch-alist+)
  "Dispatch table for type propagators associated to C1FORMs.")

(defparameter *p0-dispatch-table* (make-dispatch-table '())
  "Type propagators for known functions.")

(defparameter *cinline-dispatch-table* (make-dispatch-table '()))
