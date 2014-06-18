;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;; CMPLOC -- Backend-independent functions for dealing with locations
;;;;

(in-package "C-BACKEND")

;;; Valid locations are:
;;;	NIL
;;;	T
;;;	fixnum
;;;	VALUE0
;;;	VALUES
;;;	VALUES+VALUE0
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

(in-package "C-DATA")

(defun location-type (loc)
  (cond ((eq loc NIL) 'NULL)
	((var-p loc) (var-type loc))
	((si::fixnump loc) 'fixnum)
	((atom loc) 'T)
	(t
	 (case (first loc)
	   (FIXNUM-VALUE 'FIXNUM)
	   (CHARACTER-VALUE (type-of (code-char (second loc))))
	   (DOUBLE-FLOAT-VALUE 'DOUBLE-FLOAT)
	   (SINGLE-FLOAT-VALUE 'SINGLE-FLOAT)
	   (LONG-FLOAT-VALUE 'LONG-FLOAT)
	   (C-INLINE (let ((type (first (second loc))))
                       (cond ((and (consp type) (eq (first type) 'VALUES)) T)
                             ((c-backend::lisp-type-p type) type)
                             (t (c-backend::rep-type->lisp-type type)))))
	   (BIND (var-type (second loc)))
	   (LCL (or (third loc) T))
           (MAKE-CCLOSURE 'FUNCTION)
           ((VV VV-TEMP)
            (if (cddr loc)
                (type-of (third loc))
                T))
	   (otherwise T)))))

(defun location-primary-type (loc)
  (location-type loc))

(defun (setf location-type) (value loc)
  (if (var-p loc)
      (setf (var-type loc) value)
      (error "Unable to change value of fixed location ~A" loc)))
