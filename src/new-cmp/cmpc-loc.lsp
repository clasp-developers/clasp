;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPC-LOC	Write locations as C expressions

(in-package "C-BACKEND")

;;; Valid locations are:
;;;	NIL
;;;	T
;;;	fixnum
;;;	VALUE0
;;;	VALUES
;;;	var-object
;;;	( VALUE i )			VALUES(i)
;;;	( VV vv-index )
;;;	( VV-temp vv-index )
;;;	( LCL lcl [representation-type]) local variable, type unboxed
;;;	( TEMP temp )			local variable, type object
;;;	( CALL c-fun-name args fname )	locs are locations containing the arguments
;;;	( CALL-NORMAL fun locs)		similar as CALL, but number of arguments is fixed
;;;	( CALL-INDIRECT fun narg args)	similar as CALL, but unknown function
;;;	( C-INLINE output-type fun/string locs side-effects output-var )
;;;	( COERCE-LOC representation-type location)
;;;	( CAR lcl )
;;;	( CDR lcl )
;;;	( CADR lcl )
;;;	( FDEFINITION vv-index )
;;;	( MAKE-CCLOSURE cfun )
;;;	( FIXNUM-VALUE fixnum-value )
;;;	( CHARACTER-VALUE character-code )
;;;	( LONG-FLOAT-VALUE long-float-value vv )
;;;	( DOUBLE-FLOAT-VALUE double-float-value vv )
;;;	( SINGLE-FLOAT-VALUE single-float-value vv )
;;;	( STACK-POINTER index )	retrieve a value from the stack
;;;	( SYS:STRUCTURE-REF loc slot-name-vv slot-index )
;;;	( KEYVARS n )
;;;	( THE type loc )
;;;	VA-ARG
;;;	CL-VA-ARG

;;; Valid *DESTINATION* locations are:
;;;
;;;	VALUE0
;;;	RETURN				Object returned from current function.
;;;	TRASH				Value may be thrown away.
;;;	VALUES				Values vector.
;;;	var-object
;;;	( LCL lcl )
;;;	( LEX lex-address )
;;;	( BIND var alternative )	Alternative is optional
;;;	( JUMP-TRUE label )
;;;	( JUMP-FALSE label )
;;;	( JUMP-ZERO label )
;;;	( JUMP-NONZERO label )

(defun locative-type-from-var-kind (kind)
  (cdr (assoc kind
              '((:object . "_ecl_object_loc")
                (:fixnum . "_ecl_fixnum_loc")
                (:char . "_ecl_base_char_loc")
                (:float . "_ecl_float_loc")
                (:double . "_ecl_double_loc")
                ((special global closure replaced discarded lexical) . NIL)))))

(defun loc-has-side-effects (loc)
  (if (atom loc)
      ;; Needed to produce calls to ecl_symbol_value() for global
      ;; variables. These calls can detect whether a variable is
      ;; boudp and since the error is a side effect, they can not
      ;; be suppressed.
      (and (global-var-p loc)
           (policy-global-var-checking))
      (case (first loc)
        ((CALL CALL-NORMAL CALL-INDIRECT) T)
        (FFI:C-INLINE (fifth loc))
        (ACTUAL-RETURN T)
        (FDEFINITION (policy-global-function-checking))
        (otherwise nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; WRITING C/C++ REPRESENTATIONS LOCATIONS
;;;

(defun wt-loc (loc &aux fd)
  (cond ((consp loc)
         (unless (setq fd (gethash (first loc) +c2-wt-loc-table+))
           (error "Unknown or invalid location ~A" loc))
         (apply fd (rest loc)))
        ((var-p loc)
         (wt-var loc))
        ((setq fd (gethash loc +c2-wt-loc-table+))
         (funcall fd))
        (t
         (error "Unknown or invalid location ~A" loc))))

(defun wt-nil-loc () (wt "Cnil"))

(defun wt-t-loc () (wt "Ct"))

(defun wt-value0-loc () (wt "value0"))

(defun wt-values-loc () (wt "cl_env_copy->values[0]"))

(defun wt-va-arg-loc () (wt "(narg--,va_arg(args,cl_object))"))

(defun wt-cl-va-arg-loc () (wt "(narg--,cl_va_arg(cl_args))"))

(defun wt-car (loc) (wt "CAR(" loc ")"))

(defun wt-cdr (loc) (wt "CDR(" loc ")"))

(defun wt-cadr (loc) (wt "CADR(" loc ")"))

(defun lcl-name (lcl)
  (if (minusp lcl)
      (format nil "X~D" lcl)
      (format nil "V~D" lcl)))

(defun wt-lcl (lcl)
  (cond ((not (numberp lcl)) (baboon))
        ((minusp lcl) (wt "X" (- lcl)))
        (t (wt "V" lcl))))

(defun wt-vv (vv &optional value)
  (if (numberp vv)
    (wt "VV[" vv "]")
    (wt vv)))

(defun wt-vv-temp (vv &optional value)
  (if (numberp vv)
    (wt "VVtemp[" vv "]")
    (wt vv)))

(defun wt-lcl-loc (lcl &optional type)
  (wt-lcl lcl))

(defun wt-temp (temp)
  (wt "T" temp))

(defun wt-number (value &optional vv)
  (wt value))

(defun wt-character (value &optional vv)
  (wt (format nil "'\\~O'" value)))

(defun wt-value (i) (wt "cl_env_copy->values[" i "]"))

(defun wt-keyvars (i) (wt "keyvars[" i "]"))

(defun wt-the-loc (type loc)
  (wt-loc loc))

(defun wt-lex (lex)
  (if (consp lex)
    (wt "lex" (car lex) "[" (cdr lex) "]")
    (wt-lcl lex)))

;;; reference to variable of inner closure.
(defun wt-env (clv) (wt "ECL_CONS_CAR(CLV" clv ")"))


(defun wt-var (var &aux (var-loc (var-loc var))) ; ccb
  (declare (type var var))
  (case (var-kind var)
    (CLOSURE (wt-env var-loc))
    (LEXICAL (wt-lex var-loc))
    (REPLACED (wt var-loc))
    (DISCARDED (baboon))
    ((SPECIAL GLOBAL)
     (if (policy-global-var-checking)
	 (wt "ecl_symbol_value(" var-loc ")")
	 (wt "ECL_SYM_VAL(cl_env_copy," var-loc ")")))
    (t (wt var-loc))
    ))

(defun wt-fdefinition (fun-name)
  (let ((vv (add-object fun-name)))
    (if (and (symbolp fun-name)
	     (or (not (safe-compile))
		 (and (eql (symbol-package fun-name) (find-package "CL"))
		      (fboundp fun-name) (functionp (fdefinition fun-name)))))
	(wt "(" vv "->symbol.gfdef)")
	(wt "ecl_fdefinition(" vv ")"))))

(defun environment-accessor (fun)
  (let* ((env-var (env-var-name *env-lvl*))
	 (expected-env-size (fun-env fun)))
    (if (< expected-env-size *env*)
	(format nil "ecl_nthcdr(~D,~A)" (- *env* expected-env-size) env-var)
	env-var)))

(defun wt-make-closure (fun &aux (cfun (fun-cfun fun)))
  (declare (type fun fun))
  (let* ((closure (fun-closure fun))
	 narg)
    (cond ((eq closure 'CLOSURE)
	   (wt "ecl_make_cclosure_va((cl_objectfn)" cfun ","
	       (environment-accessor fun)
	       ",Cblock)"))
	  ((eq closure 'LEXICAL)
	   (baboon))
	  ((setf narg (fun-fixed-narg fun)) ; empty environment fixed number of args
	   (wt "ecl_make_cfun((cl_objectfn_fixed)" cfun ",Cnil,Cblock," narg ")"))
	  (t ; empty environment variable number of args
	   (wt "ecl_make_cfun_va((cl_objectfn)" cfun ",Cnil,Cblock)")))))
