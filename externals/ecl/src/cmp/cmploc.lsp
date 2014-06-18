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

;;;; CMPLOC  Set-loc and Wt-loc.

(in-package "COMPILER")

;;; Valid locations are:
;;;	NIL
;;;	T
;;;	fixnum
;;;	VALUE0
;;;	VALUES
;;;	var-object
;;;     a string                        designating a C expression
;;;	( VALUE i )			VALUES(i)
;;;	( VV vv-index )
;;;	( VV-temp vv-index )
;;;	( LCL lcl [representation-type]) local variable, type unboxed
;;;	( TEMP temp )			local variable, type object
;;;	( FRAME ndx )			variable in local frame stack
;;;	( CALL-NORMAL fun locs 1st-type ) similar as CALL, but number of arguments is fixed
;;;	( CALL-INDIRECT fun narg args)	similar as CALL, but unknown function
;;;	( C-INLINE output-type fun/string locs side-effects output-var )
;;;	( COERCE-LOC representation-type location)
;;;	( FDEFINITION vv-index )
;;;	( MAKE-CCLOSURE cfun )
;;;	( FIXNUM-VALUE fixnum-value )
;;;	( CHARACTER-VALUE character-code )
;;;	( LONG-FLOAT-VALUE long-float-value vv )
;;;	( DOUBLE-FLOAT-VALUE double-float-value vv )
;;;	( SINGLE-FLOAT-VALUE single-float-value vv )
;;;	( STACK-POINTER index )	retrieve a value from the stack
;;;	( SYS:STRUCTURE-REF loc slot-name-vv slot-index )
;;;     ( THE type location )
;;;	( KEYVARS n )
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

(defun tmp-destination (loc)
  (case loc
    (VALUES 'VALUES)
    (TRASH 'TRASH)
    (T 'RETURN)))

(defun precise-loc-type (loc new-type)
  (if (subtypep (loc-type loc) new-type)
      loc
      `(the ,new-type ,loc)))

(defun loc-in-c1form-movable-p (loc)
  "A location that is in a C1FORM and can be moved"
  (cond ((member loc '(t nil))
	 t)
	((numberp loc)
	 t)
	((stringp loc)
	 t)
        ((vv-p loc)
         t)
	((member loc '(value0 values va-arg cl-va-arg))
	 nil)
	((atom loc)
	 (baboon :format-control "Unknown location ~A found in C1FORM"
		 :format-arguments (list loc)))
	((eq (first loc) 'THE)
	 (loc-in-c1form-movable-p (third loc)))
	((member (setf loc (car loc))
		 '(VV VV-TEMP FIXNUM-VALUE CHARACTER-VALUE
		   DOUBLE-FLOAT-VALUE SINGLE-FLOAT-VALUE #+long-float LONG-FLOAT-VALUE
		   KEYVARS))
	 t)
	(t
	 (baboon :format-control "Unknown location ~A found in C1FORM"
		 :format-arguments (list loc)))))

(defun uses-values (loc)
  (and (consp loc)
       (or (member (car loc) '(CALL CALL-NORMAL CALL-INDIRECT) :test #'eq)
           (and (eq (car loc) 'C-INLINE)
                (eq (sixth loc) 'VALUES)))))

(defun loc-immediate-value-p (loc)
  (cond ((eq loc t)
         (values t t))
        ((eq loc nil)
         (values t nil))
        ((numberp loc)
         (values t loc))
        ((vv-p loc)
         (let ((value (vv-value loc)))
           (if (or (null value) (ext:fixnump value))
               (values nil nil)
               (values t value))))
        ((atom loc)
         (values nil nil))
	((eq (first loc) 'THE)
	 (loc-immediate-value-p (third loc)))
        ((member (first loc)
                 '(fixnum-value long-float-value
                   double-float-value single-float-value))
         (values t (second loc)))
	((eq (first loc) 'character-value)
	 (values t (code-char (second loc))))
        (t
         (values nil nil))))

(defun loc-immediate-value (loc)
  (nth-value 1 (loc-immediate-value-p loc)))

(defun unknown-location (where loc)
  (baboon :format-control "Unknown location found in ~A~%~S"
          :format-arguments (list where loc)))

(defun wt-loc (loc)
  (cond ((consp loc)
         (let ((fd (gethash (car loc) *wt-loc-dispatch-table*)))
           (if fd
               (apply fd (cdr loc))
               (unknown-location 'wt-loc loc))))
        ((symbolp loc)
         (let ((txt (gethash loc *wt-loc-dispatch-table* :not-found)))
           (when (eq txt :not-found)
             (unknown-location 'wt-loc loc))
           (wt txt)))
	((stringp loc)
	 (wt loc))
        ((var-p loc)
         (wt-var loc))
        ((vv-p loc)
         (wt-vv loc))
        (t
         (unknown-location 'wt-loc loc))))

(defun last-call-p ()
  (member *exit*
          '(RETURN RETURN-FIXNUM RETURN-CHARACTER RETURN-SINGLE-FLOAT
            RETURN-DOUBLE-FLOAT RETURN-LONG-FLOAT RETURN-OBJECT)))

(defun wt-lcl (lcl)
  (unless (numberp lcl) (baboon))
  (wt "v" lcl))

(defun wt-lcl-loc (lcl &optional type name)
  (unless (numberp lcl) (baboon))
  (wt "v" lcl name))

(defun wt-temp (temp)
  (wt "T" temp))

(defun wt-number (value &optional vv)
  (wt value))

(defun wt-character (value &optional vv)
  ;; We do not use the '...' format because this creates objects of type
  ;; 'char' which have sign problems
  (wt value))

(defun wt-value (i) (wt "cl_env_copy->values[" i "]"))

(defun wt-keyvars (i) (wt "keyvars[" i "]"))

(defun wt-the (type loc)
  (declare (ignore type))
  (wt-loc loc))

(defun loc-refers-to-special (loc)
  (cond ((var-p loc)
	 (member (var-kind loc) '(SPECIAL GLOBAL)))
	((atom loc)
	 nil)
	((eq (first loc) 'THE)
	 (loc-refers-to-special (third loc)))
	((eq (setf loc (first loc)) 'BIND)
	 t)
	((eq loc 'C-INLINE)
	 t) ; We do not know, so guess yes
	(t nil)))

(defun values-loc (n)
  (list 'VALUE n))

;;;
;;; SET-LOC
;;;

(defun set-unknown-loc (loc)
  (unknown-location 'set-loc *destination*))

(defun set-loc (loc &aux fd)
  (let ((destination *destination*))
    (cond ((eq destination loc))
          ((symbolp destination)
           (funcall (gethash destination *set-loc-dispatch-table*
                             'set-unknown-loc)
                    loc))
          ((var-p destination)
           (set-var loc destination))
          ((vv-p destination)
           (set-vv loc destination))
          ((atom destination)
           (unknown-location 'set-loc destination))
          (t
           (let ((fd (gethash (first destination) *set-loc-dispatch-table*)))
             (if fd
                 (apply fd loc (rest destination))
                 (progn
                   (wt-nl) (wt-loc destination) (wt " = ")
                   (wt-coerce-loc (loc-representation-type *destination*) loc)
                   (wt ";"))))))))

(defun set-the-loc (loc type orig-loc)
  (declare (ignore type))
  (let ((*destination* orig-loc))
    (set-loc loc)))
                 
(defun set-values-loc (loc)
  (cond ((eq loc 'VALUES))
        ((uses-values loc)
         (wt-nl "cl_env_copy->values[0] = ") (wt-coerce-loc :object loc) (wt ";"))
        (t
         (wt-nl "cl_env_copy->values[0] = ") (wt-coerce-loc :object loc)
	 (wt ";")
	 (wt-nl "cl_env_copy->nvalues = 1;"))))

(defun set-value0-loc (loc)
  (wt-nl "value0 = ") (wt-coerce-loc :object loc) (wt ";"))

(defun set-return-loc (loc)
  (cond ((or (eq loc 'VALUES) (uses-values loc))
         (wt-nl "value0 = ") (wt-coerce-loc :object loc) (wt ";"))
        ((eq loc 'VALUE0)
         (wt-nl "cl_env_copy->nvalues = 1;"))
        ((eq loc 'RETURN))
        (t
         (wt-nl "value0 = ") (wt-coerce-loc :object loc) (wt ";")
         (wt-nl "cl_env_copy->nvalues = 1;"))))

(defun loc-with-side-effects-p (loc &aux name)
  (cond ((var-p loc)
	 (and (global-var-p loc)
	      (policy-global-var-checking)))
	((atom loc)
	 nil)
	((member (setf name (first loc)) '(CALL CALL-NORMAL CALL-INDIRECT)
		 :test #'eq)
	 t)
	((eq name 'THE)
	 (loc-with-side-effects-p (third loc)))
	((eq name 'FDEFINITION)
	 (policy-global-function-checking))
	((eq name 'C-INLINE)
	 (or (eq (sixth loc) 'VALUES) ;; Uses VALUES
	     (fifth loc))))) ;; or side effects

(defun set-trash-loc (loc)
  (when (loc-with-side-effects-p loc)
    (wt-nl loc ";")
    t))
