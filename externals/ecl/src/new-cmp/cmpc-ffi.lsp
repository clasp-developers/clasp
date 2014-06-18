;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPC-FFI --  Foreign types and expressions in C/C++ backend

;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "C-BACKEND")

;; ----------------------------------------------------------------------
;; REPRESENTATION TYPES
;;

(defconstant +representation-types+
  '(;; These types can be used by ECL to unbox data
    ;; They are sorted from the most specific, to the least specific one.
    :byte ((signed-byte 8) "int8_t")
    :unsigned-byte ((unsigned-byte 8) "uint8_t")
    :fixnum (fixnum "cl_fixnum")
    :int ((integer #.si:c-int-min #.si:c-int-max) "int")
    :unsigned-int ((integer 0 #.si:c-uint-max) "unsigned int")
    :long ((integer #.si:c-long-min #.si:c-long-max) "long")
    :unsigned-long ((integer 0 #.si:c-ulong-max) "unsigned long")
    :cl-index ((integer 0 #.most-positive-fixnum) "cl_index")
    :float (single-float "float")
    :double (double-float "double")
    #+:long-float :long-double #+:long-float (long-float "long double")
    :unsigned-char (base-char "char")
    :char (base-char "char")
    :wchar (character "ecl_character")
    :object (t "cl_object")
    :bool (t "bool")
    ;; These types are never selected to unbox data.
    ;; They are here, because we need to know how to print them.
    :void (nil "void")
    :pointer-void (si::foreign-data "void*")
    :cstring (string "char*")
    :char* (string "char*")
    :short ((integer #.si:c-short-min #.si:c-short-max) "short")
    :unsigned-short ((integer 0 #.si:c-ushort-max) "unsigned short")
    ))

(defun rep-type->lisp-type (rep-type)
  (let ((output (getf +representation-types+ rep-type)))
    (cond (output
           (if (eq rep-type :void) nil
	     (or (first output)
	         (cmperr "Representation type ~S cannot be coerced to lisp"
                         rep-type))))
	  ((lisp-type-p rep-type) rep-type)
	  (t (cmperr "Unknown representation type ~S" rep-type)))))

(defun lisp-type->rep-type (type)
  (cond
    ;; We expect type = NIL when we have no information. Should be fixed. FIXME!
    ((null type)
     :object)
    ((getf +representation-types+ type)
     type)
    (t
     (do ((l +representation-types+ (cddr l)))
	 ((endp l) :object)
       (when (subtypep type (first (second l)))
	 (return-from lisp-type->rep-type (first l)))))))

(defun rep-type-name (type)
  (or (second (getf +representation-types+ type))
      (cmperr "Not a valid type name ~S" type)))

(defun lisp-type-p (type)
  (subtypep type 'T))

(defun var-rep-type (var)
  (case (var-kind var)
    ((LEXICAL CLOSURE SPECIAL GLOBAL) :object)
    (REPLACED (loc-representation-type (var-loc var)))
    (DISCARDED :object)
    (t (var-kind var))))

;; ----------------------------------------------------------------------
;; LOCATIONS and representation types
;;
;; Locations are lisp expressions which represent actual C data. To each
;; location we can associate a representation type, which is the type of
;; the C data. The following routines help in determining these types,
;; and also in moving data from one location to another.

(defun loc-movable-p (loc)
  (if (atom loc)
      t
      (case (first loc)
	((CALL CALL-LOCAL) NIL)
	((C-INLINE) (not (fifth loc))) ; side effects?
	(otherwise t))))

(defun loc-representation-type (loc)
  (cond ((member loc '(NIL T)) :object)
	((var-p loc) (var-rep-type loc))
	((si::fixnump loc) :fixnum)
        ((eq loc 'TRASH) :void)
	((atom loc) :object)
	(t
	 (case (first loc)
	   (FIXNUM-VALUE :fixnum)
	   (CHARACTER-VALUE (if (<= (second loc) 255) :unsigned-char :wchar))
	   (DOUBLE-FLOAT-VALUE :double)
	   (SINGLE-FLOAT-VALUE :float)
	   (LONG-FLOAT-VALUE :long-double)
	   (C-INLINE (let ((type (first (second loc))))
                       (cond ((and (consp type) (eq (first type) 'VALUES)) :object)
                             ((lisp-type-p type) (lisp-type->rep-type type))
                             (t type))))
	   (BIND (var-rep-type (second loc)))
	   (LCL (lisp-type->rep-type (or (third loc) T)))
	   (otherwise :object)))))

(defun wt-coerce-loc (dest-rep-type loc)
  (setq dest-rep-type (lisp-type->rep-type dest-rep-type))
  ;(print dest-rep-type)
  ;(print loc)
  (let* ((dest-type (rep-type->lisp-type dest-rep-type))
	 (loc-type (location-type loc))
	 (loc-rep-type (loc-representation-type loc)))
    (labels ((coercion-error ()
	       (cmperr "Unable to coerce lisp object from type (~S,~S)~%~
			to C/C++ type (~S,~S)"
		       loc-type loc-rep-type dest-type dest-rep-type))
	     (ensure-valid-object-type (a-lisp-type)
	       (when (subtypep `(AND ,loc-type ,a-lisp-type) NIL)
		 (coercion-error))))
      (when (eq dest-rep-type loc-rep-type)
	(wt loc)
	(return-from wt-coerce-loc))
      (case dest-rep-type
	((:byte :unsigned-byte :short :unsigned-short :int :unsigned-int
	  :long :unsigned-long :fixnum :cl-index)
	 (case loc-rep-type
	   (#1=(:byte :unsigned-byte :short :unsigned-short :int :unsigned-int
		:long :unsigned-long :fixnum :cl-index
		:float :double :long-double) ; number types
	    (wt "((" (rep-type-name dest-rep-type) ")" loc ")"))
	   ((:object)
	    (ensure-valid-object-type dest-type)
	    (wt (cond ((or (subtypep (location-type loc) 'fixnum)
                           (policy-assume-no-errors))
		       "fix(")
		      ((member dest-rep-type '(:unsigned-short :unsigned-long :cl-index))
		       "ecl_to_unsigned_integer(")
		      (t
		       "ecl_to_fixnum("))
		loc ")"))
	   (otherwise
	    (coercion-error))))
	((:char :unsigned-char :wchar)
	 (case loc-rep-type
	   ((:char :unsigned-char :wchar)
	    (wt "((" (rep-type-name dest-rep-type) ")" loc ")"))
	   ((:object)
	    (ensure-valid-object-type dest-type)
	    (wt "ecl_char_code(" loc ")"))
	   (otherwise
	    (coercion-error))))
	((:float :double :long-double)
	 (case loc-rep-type
	   (#1# ; number type
	    (wt "((" (rep-type-name dest-rep-type) ")" loc ")"))
	   ((:object)
	    ;; We relax the check a bit, because it is valid in C to coerce
	    ;; between floats of different types.
	    (ensure-valid-object-type 'FLOAT)
	    (wt (ecase dest-rep-type
		  (:float "ecl_to_float(")
		  (:double "ecl_to_double(")
		  (:long-double "ecl_to_long_double("))
		loc ")"))
	   (otherwise
	    (coercion-error))))
	((:bool)
	 (case loc-rep-type
	   (#1# ; number type
	    (wt "1"))
	   ((:object)
	    (wt "(" loc ")!=Cnil"))
	   (otherwise
	    (coercion-error))))
	((:object)
	 (case loc-rep-type
	   ((:short :int :long)
	    (wt "ecl_make_integer(" loc ")"))
	   ((:unsigned-short :unsigned-int :unsigned-long)
	    (wt "ecl_make_unsigned_integer(" loc ")"))
	   ((:byte :unsigned-byte :fixnum)
	    (wt "MAKE_FIXNUM(" loc ")"))
	   ((:float)
	    (if (and (consp loc) (eq (first loc) 'SINGLE-FLOAT-VALUE))
		(wt (third loc)) ;; VV index
		(wt "ecl_make_singlefloat(" loc ")")))
	   ((:double)
	    (if (and (consp loc) (eq (first loc) 'DOUBLE-FLOAT-VALUE))
		(wt (third loc)) ;; VV index
		(wt "ecl_make_doublefloat(" loc ")")))
	   ((:long-double)
	    (if (and (consp loc) (eq (first loc) 'LONG-FLOAT-VALUE))
		(wt (third loc)) ;; VV index
		(wt "ecl_make_longfloat(" loc ")")))
	   ((:bool)
	    (wt "((" loc ")?Ct:Cnil)"))
	   ((:char :unsigned-char :wchar)
	    (wt "CODE_CHAR(" loc ")"))
	   ((:cstring)
	    (wt "ecl_cstring_to_base_string_or_nil(" loc ")"))
	   ((:pointer-void)
	    (wt "ecl_make_foreign_data(Cnil, 0, " loc ")"))
	   (otherwise
	    (coercion-error))))
	((:pointer-void)
	 (case loc-rep-type
	   ((:object)
	    ;; Only foreign data types can be coerced to a pointer
	    (wt "ecl_foreign_data_pointer_safe(" loc ")"))
	   ((:cstring)
	    (wt "(char *)(" loc ")"))
	   (otherwise
	    (coercion-error))))
	((:cstring)
	 (coercion-error))
	((:char*)
	 (case loc-rep-type
	   ((:object)
	    (wt "ecl_base_string_pointer_safe(" loc ")"))
	   ((:pointer-void)
	    (wt "(char *)(" loc ")"))
	   (otherwise
	    (coercion-error))))
        ((:void)
         (wt loc))
	(t
	 (coercion-error))))))


(defun produce-inline-loc (argument-locs arg-types output-rep-type
			   c-expression side-effects one-liner)
  (let* (args-to-be-saved
	 coerced-arguments)
    ;; If the expression begins with @[0-9a-z]*, this means we are
    ;; saving some variables.
    (when (and (> (length c-expression) 1)
	       (eq (char c-expression 0) #\@))
      (do ((ndx 1 (1+ ndx)))
	  ((>= ndx (length c-expression)))
	(let ((c (char c-expression ndx)))
	  (when (eq c #\;)
	    (setf c-expression (subseq c-expression (1+ ndx)))
	    (return))
	  (unless (alphanumericp c)
	    (setf args-to-be-saved nil)
	    (return))
	  (push (- (char-code c) (char-code #\0))
		args-to-be-saved))))

    (setf coerced-arguments (coerce-locations argument-locs arg-types args-to-be-saved))
    ;;(setf output-rep-type (lisp-type->rep-type output-rep-type))

    ;; If the form does not output any data, and there are no side
    ;; effects, try to omit it.
    (when (null output-rep-type)
      (if side-effects
	  (progn
	    (wt-nl)
	    (wt-c-inline-loc output-rep-type c-expression coerced-arguments t nil)
	    (when one-liner (wt ";")))
	  (cmpwarn "Ignoring form ~S" c-expression))
      (return-from produce-inline-loc NIL))

    ;; If the form is a one-liner, we can simply propagate this expression until the
    ;; place where the value is used.
    (when one-liner
      (return-from produce-inline-loc
	`(C-INLINE ,output-rep-type ,c-expression ,coerced-arguments ,side-effects
                   ,(if (equalp output-rep-type '((VALUES &REST T)))
                        'VALUES NIL))))

    ;; If the output is a in the VALUES vector, just write down the form and output
    ;; the location of the data.
    (when (equalp output-rep-type '((VALUES &REST T)))
      (wt-c-inline-loc output-rep-type c-expression coerced-arguments side-effects
                       'VALUES)
      (return-from produce-inline-loc 'VALUES))

    ;; Otherwise we have to set up variables for holding the output.
    (flet ((make-output-var (rep-type)
             (make-var :kind rep-type :type (rep-type->lisp-type rep-type)
                       :loc (next-lcl))))
      (open-c-block)
      (let ((output-vars (mapcar #'make-output-var output-rep-type)))
        (loop for v in output-vars
           do (wt (rep-type-name (var-kind v)) " " v ";"))
	(wt-c-inline-loc output-rep-type c-expression coerced-arguments
                         side-effects output-vars)
        (loop for v in output-vars
           for i from 0
           do (set-loc (coerce-one-location v :OBJECT)
                       `(VALUE ,i)))
        (wt "cl_env_copy->nvalues=" (length output-vars) ";"))
      (close-c-block)
      'VALUES+VALUE0)))

(defun c2c-inline (destination arguments &rest rest)
  (set-loc (apply #'produce-inline-loc arguments rest)
           destination))

(defun wt-c-inline-loc (output-rep-type c-expression coerced-arguments side-effects output-vars)
  (with-input-from-string (s c-expression)
    (when (and output-vars (not (eq output-vars 'VALUES)))
      (wt-nl))
    (do ((c (read-char s nil nil)
	    (read-char s nil nil)))
	((null c))
      (case c
	(#\@
	 (let ((object (read s)))
	   (cond ((and (consp object) (equal (first object) 'RETURN))
		  (if (eq output-vars 'VALUES)
		      (cmperr "User @(RETURN ...) in a C-INLINE form with no output values")
		      (let ((ndx (or (second object) 0))
			    (l (length output-vars)))
			(if (< ndx l)
			    (wt (nth ndx output-vars))
                            (cmperr "Used @(RETURN ~D) in a C-INLINE form with ~D output values"
                                    ndx l)))))
		 (t
		  (when (and (consp object) (eq (first object) 'QUOTE))
		    (setq object (second object)))
		  (wt (add-object object :permanent t))))))
	(#\#
	 (let* ((k (read-char s))
		(next-char (peek-char nil s nil nil))
		(index (digit-char-p k 36)))
	   (cond ((or (null index) (and next-char (alphanumericp next-char)))
		  (wt #\# k))
		 ((< index (length coerced-arguments))
		  (wt (nth index coerced-arguments)))
		 (t
		  (cmperr "C-INLINE: Variable code exceeds number of arguments")))))
	(otherwise
	 (write-char c *compiler-output1*))))))
