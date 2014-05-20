;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPFFI --  Foreign functions interface.

;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

;; ----------------------------------------------------------------------
;; REPRESENTATION TYPES
;;

;; All known integer C types, sorted by bit size.
(defconstant +all-integer-rep-type-pairs+
  '#.(stable-sort
      '((:byte . -8)
	(:unsigned-byte . 8)
	(:unsigned-short . #.(logcount ffi:c-ushort-max))
	(:short . #.(- (logcount ffi:c-ushort-max)))
	(:unsigned-int . #.(logcount ffi:c-uint-max))
	(:int . #.(logcount ffi:c-uint-max))
	(:unsigned-long . #.(logcount ffi:c-ulong-max))
	(:long . #.(logcount ffi:c-ulong-max))
	#+long-long
	(:unsigned-long-long . #.(logcount ffi:c-ulong-long-max))
	#+long-long
	(:long-long . #.(logcount ffi:c-ulong-long-max))
	(:cl-index . #.(logcount most-positive-fixnum))
	(:fixnum . #.(- (logcount most-positive-fixnum)))
	(:uint8-t . 8)
	(:int8-t . -8)
	(:uint16-t . 16)
	(:int16-t . -16)
	(:uint32-t . 32)
	(:int32-t . -32)
	(:uint64-t . 64)
	(:int64-t . -64))
      #'< :key #'(lambda (pair) (abs (cdr pair)))))

(defconstant +all-integer-rep-types+
  (mapcar #'car +all-integer-rep-type-pairs+))

(defconstant +all-number-rep-types+
  (append +all-integer-rep-types+ '(:float :double :long-double)))

(defconstant +representation-types+
  '(;; These types can be used by ECL to unbox data
    ;; They are sorted from the most specific, to the least specific one.
    :byte
    #1=((signed-byte 8) "int8_t" "ecl_make_int8_t" "ecl_to_int8_t" "ecl_fixnum")
    :unsigned-byte
    #2=((unsigned-byte 8) "uint8_t" "ecl_make_uint8_t" "ecl_to_uint8_t" "ecl_fixnum")
    :fixnum
    (fixnum "cl_fixnum" "ecl_make_fixnum" "ecl_to_fixnum" "ecl_fixnum")
    :int
    ((integer #.ffi:c-int-min #.ffi:c-int-max) "int"
     "ecl_make_int" "ecl_to_int" "ecl_to_int")
    :unsigned-int
    ((integer 0 #.ffi:c-uint-max) "unsigned int"
     "ecl_make_uint" "ecl_to_uint" "ecl_to_uint")
    :long
    ((integer #.ffi:c-long-min #.ffi:c-long-max) "long" "ecl_make_long" "ecl_to_long"
     #.(if (<= most-negative-fixnum ffi:c-long-min ffi:c-long-max most-positive-fixnum)
	   "ecl_fixnum"
	   "ecl_to_long"))
    :unsigned-long
    ((integer 0 #.ffi:c-ulong-max) "unsigned long"
     "ecl_make_ulong" "ecl_to_ulong"
     #.(if (<= ffi:c-long-max most-positive-fixnum) "ecl_fixnum" "ecl_to_ulong"))
    :cl-index
    ((integer 0 #.most-positive-fixnum) "cl_index"
     "ecl_make_unsigned_integer" "ecl_to_cl_index" "ecl_fixnum")
    #+long-long
    :long-long
    #+long-long
    ((signed-byte #.ffi:c-long-long-bit) "ecl_long_long_t" "ecl_make_long_long"
     "ecl_to_long_long" "ecl_to_long_long")
    #+long-long
    :unsigned-long-long
    #+long-long
    ((unsigned-byte #.ffi:c-long-long-bit) "ecl_ulong_long_t"
         "ecl_make_ulong_long"
         "ecl_to_ulong_long" "ecl_to_ulong_long")
    :float
    (single-float "float" "ecl_make_single_float" "ecl_to_float" "ecl_single_float")
    :double
    (double-float "double" "ecl_make_double_float" "ecl_to_double" "ecl_double_float")
    #+:long-float
    :long-double
    #+:long-float
    (long-float "long double" "ecl_make_long_float" "ecl_to_long_double" "ecl_long_float")
    :unsigned-char
    (base-char "unsigned char" "CODE_CHAR" "ecl_base_char_code" "CHAR_CODE")
    :char
    (base-char "char" "CODE_CHAR" "ecl_base_char_code" "CHAR_CODE")
    :wchar
    (character "ecl_character" "CODE_CHAR" "ecl_char_code" "CHAR_CODE")
    #+sse2
    :float-sse-pack
    #+sse2
    (ext:float-sse-pack "__m128" "ecl_make_float_sse_pack"
     "ecl_unbox_float_sse_pack" "ecl_unbox_float_sse_pack_unsafe")
    #+sse2
    :double-sse-pack
    #+sse2
    (ext:double-sse-pack "__m128d" "ecl_make_double_sse_pack"
     "ecl_unbox_double_sse_pack" "ecl_unbox_double_sse_pack_unsafe")
    #+sse2
    :int-sse-pack
    #+sse2
    (ext:sse-pack #|<-intentional|# "__m128i" "ecl_make_int_sse_pack"
     "ecl_unbox_int_sse_pack" "ecl_unbox_int_sse_pack_unsafe")
    :object
    (t "cl_object")
    :bool
    (t "bool" "ecl_make_bool" "ecl_to_bool" "ecl_to_bool")
    ;; These types are never selected to unbox data.
    ;; They are here, because we need to know how to print them.
    :void
    (nil "void")
    :pointer-void
    (si::foreign-data "void*" "ecl_make_pointer" "ecl_to_pointer" "ecl_to_pointer")
    :cstring
    (string "char*" "ecl_cstring_to_base_string_or_nil")
    :char*
    (string "char*")
    :int8-t
    #1#
    :uint8-t
    #2#
    #+:uint16-t
    :int16-t
    #+:uint16-t
    ((unsigned-byte 16) "ecl_int16_t" "ecl_make_int16_t" "ecl_to_int16_t"
     #.(if (subtypep '(unsigned-byte 16) 'fixnum) "ecl_fixnum" "ecl_to_int32_t"))
    #+:uint16-t
    :uint16-t
    #+:uint16-t
    ((signed-byte 16) "ecl_uint16_t" "ecl_make_uint16_t" "ecl_to_uint16_t" "ecl_fixnum"
     #.(if (subtypep '(signed-byte 16) 'fixnum) "ecl_fixnum" "ecl_to_unt16_t"))
    #+:uint32-t
    :int32-t
    #+:uint32-t
    ((unsigned-byte 32) "ecl_int32_t" "ecl_make_int32_t" "ecl_to_int32_t"
     #.(if (subtypep '(unsigned-byte 32) 'fixnum) "ecl_fixnum" "ecl_to_int32_t"))
    #+:uint32-t
    :uint32-t
    #+:uint32-t
    ((signed-byte 32) "ecl_uint32_t" "ecl_make_uint32_t" "ecl_to_uint32_t"
     #.(if (subtypep '(signed-byte 32) 'fixnum) "ecl_fixnum" "ecl_to_uint32_t"))
    #+:uint64-t
    :int64-t
    #+:uint64-t
    ((signed-byte 64) "ecl_int64_t" "ecl_make_int64_t" "ecl_to_int64_t" "ecl_to_int64_t")
    #+:uint64-t
    :uint64-t
    #+:uint64-t
    ((signed-byte 64) "ecl_uint64_t" "ecl_make_uint64_t" "ecl_to_uint64_t" "ecl_to_uint64_t")
    :short
    ((integer #.ffi:c-short-min #.ffi:c-short-max) "short"
     "ecl_make_short" "ecl_to_short" "ecl_fixnum")
    :unsigned-short
    ((integer 0 #.ffi:c-ushort-max) "unsigned short"
     "ecl_make_ushort" "ecl_to_ushort" "ecl_fixnum")
    ))

(defparameter +representation-type-hash+
  (loop with table = (make-hash-table :size 128 :test 'eq)
     for record on +representation-types+ by #'cddr
     for rep-type = (first record)
     for information = (second record)
     do (setf (gethash rep-type table) information)
     finally (progn
               #+sse2 ; hack: sse-pack -> int, but int -> int-sse-pack
               (setf (gethash :int-sse-pack table)
                     (list* 'ext:int-sse-pack (cdr (gethash :int-sse-pack table))))
               (return table))))

(defun c-number-rep-type-p (rep-type)
  (member rep-type +all-number-rep-types+))

(defun c-integer-rep-type-p (rep-type)
  (member rep-type +all-integer-rep-types+))

(defun c-integer-rep-type-bits (rep-type)
  (abs (cdr (assoc rep-type +all-integer-rep-type-pairs+))))

(defun c-number-type-p (type)
  (c-number-rep-type-p (lisp-type->rep-type type)))

(defun c-integer-type-p (type)
  (c-integer-rep-type-p (lisp-type->rep-type type)))

(defun c-integer-type-bits (type)
  (c-number-rep-type-bits (lisp-type->rep-type type)))

(defun rep-type-record (rep-type)
  (gethash rep-type +representation-type-hash+))

(defun rep-type->lisp-type (rep-type)
  (let ((output (rep-type-record rep-type)))
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
    ((rep-type-record type)
     type)
    (t
     (do ((l +representation-types+ (cddr l)))
	 ((endp l) :object)
       (when (subtypep type (first (second l)))
	 (return-from lisp-type->rep-type (first l)))))))

(defun rep-type-name (type)
  (or (second (rep-type-record type))
      (cmperr "Not a valid C type name ~S" type)))

(defun lisp-type-p (type)
  (subtypep type 'T))

(defun wt-to-object-conversion (loc-rep-type loc)
  (when (and (consp loc) (member (first loc) '(single-float-value
					       double-float-value
					       long-float-value)))
    (wt (third loc)) ;; VV index
    (return-from wt-to-object-conversion))
  (let ((x (third (rep-type-record loc-rep-type))))
    (unless x
      (cmperr "Cannot coerce C variable of type ~A to lisp object" loc-rep-type))
    (wt x "(" loc ")")))

(defun wt-from-object-conversion (dest-type loc-type rep-type loc)
  (let ((x (cdddr (rep-type-record rep-type))))
    (unless x
      (cmperr "Cannot coerce lisp object to C type ~A" rep-type))
    (wt (if (or (policy-assume-no-errors)
                (subtypep loc-type dest-type))
	    (second x)
            (first x))
	"(" loc ")")))

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

(defun loc-type (loc)
  (cond ((eq loc NIL) 'NULL)
	((var-p loc) (var-type loc))
        ((vv-p loc) (vv-type loc))
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
                             ((lisp-type-p type) type)
                             (t (rep-type->lisp-type type)))))
	   (BIND (var-type (second loc)))
	   (LCL (or (third loc) T))
	   (THE (second loc))
	   (CALL-NORMAL (fourth loc))
	   (otherwise T)))))

(defun loc-representation-type (loc)
  (cond ((member loc '(NIL T)) :object)
	((var-p loc) (var-rep-type loc))
        ((vv-p loc) :object)
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
           ((JUMP-TRUE JUMP-FALSE) :bool)
	   (THE (loc-representation-type (third loc)))
	   (otherwise :object)))))

(defun wt-coerce-loc (dest-rep-type loc)
  (setq dest-rep-type (lisp-type->rep-type dest-rep-type))
  ;(print dest-rep-type)
  ;(print loc)
  (let* ((dest-type (rep-type->lisp-type dest-rep-type))
	 (loc-type (loc-type loc))
	 (loc-rep-type (loc-representation-type loc)))
    (labels ((coercion-error ()
	       (cmpwarn "Unable to coerce lisp object from type (~S,~S)~%~
			to C/C++ type (~S,~S)"
                        loc-type loc-rep-type dest-type dest-rep-type))
	     (ensure-valid-object-type (a-lisp-type)
	       (when (subtypep `(AND ,loc-type ,a-lisp-type) NIL)
		 (coercion-error))))
      (when (eq dest-rep-type loc-rep-type)
	(wt loc)
	(return-from wt-coerce-loc))
      (case dest-rep-type
	(#.+all-integer-rep-types+
	 (case loc-rep-type
	   (#.+all-number-rep-types+
	    (wt "(" (rep-type-name dest-rep-type) ")(" loc ")"))
	   ((:object)
	    (ensure-valid-object-type dest-type)
	    (wt-from-object-conversion dest-type loc-type dest-rep-type loc))
	   (otherwise
	    (coercion-error))))
	((:char :unsigned-char :wchar)
	 (case loc-rep-type
	   ((:char :unsigned-char :wchar)
	    (wt "(" (rep-type-name dest-rep-type) ")(" loc ")"))
	   ((:object)
	    (ensure-valid-object-type dest-type)
	    (wt-from-object-conversion dest-type loc-type dest-rep-type loc))
	   (otherwise
	    (coercion-error))))
	((:float :double :long-double)
	 (case loc-rep-type
	   (#.+all-number-rep-types+
	    (wt "(" (rep-type-name dest-rep-type) ")(" loc ")"))
	   ((:object)
	    ;; We relax the check a bit, because it is valid in C to coerce
	    ;; between floats of different types.
	    (ensure-valid-object-type 'FLOAT)
	    (wt-from-object-conversion dest-type loc-type dest-rep-type loc))
	   (otherwise
	    (coercion-error))))
	((:bool)
	 (case loc-rep-type
	   (#.+all-number-rep-types+ ; number type
	    (wt "1"))
	   ((:object)
	    (wt "(" loc ")!=ECL_NIL"))
	   (otherwise
	    (coercion-error))))
	((:object)
         #+sse2
	 (case loc-rep-type
	   ((:int-sse-pack :float-sse-pack :double-sse-pack)
            (when (>= (cmp-env-optimization 'speed) 1)
              (cmpwarn-style "Boxing a value of type ~S - performance degraded."
                             loc-rep-type))))
	 (wt-to-object-conversion loc-rep-type loc))
	((:pointer-void)
	 (case loc-rep-type
	   ((:object)
	    (wt-from-object-conversion dest-type loc-type dest-rep-type loc))
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
	#+sse2
	((:int-sse-pack :float-sse-pack :double-sse-pack)
	 (case loc-rep-type
	   ((:object)
	    (wt-from-object-conversion 'ext:sse-pack loc-type dest-rep-type loc))
           ;; Implicitly cast between SSE subtypes
           ((:int-sse-pack :float-sse-pack :double-sse-pack)
            (wt (ecase dest-rep-type
                  (:int-sse-pack (ecase loc-rep-type
                                   (:float-sse-pack "_mm_castps_si128")
                                   (:double-sse-pack "_mm_castpd_si128")))
                  (:float-sse-pack (ecase loc-rep-type
                                     (:int-sse-pack "_mm_castsi128_ps")
                                     (:double-sse-pack "_mm_castpd_ps")))
                  (:double-sse-pack (ecase loc-rep-type
                                      (:int-sse-pack "_mm_castsi128_pd")
                                      (:float-sse-pack "_mm_castps_pd"))))
                "(" loc ")"))
	   (otherwise
	    (coercion-error))))
	(t
	 (coercion-error))))))

;;; ----------------------------------------------------------------------
;;; C/C++ DECLARATIONS AND HEADERS
;;;
;;; All lines from CLINES statements are grouped at the beginning of the header
;;; Notice that it does not make sense to guarantee that c-lines statements
;;; are produced in-between the function definitions, because two functions
;;; might be collapsed into one, or we might not produce that function at all
;;; and rather inline it.
;;;
(defun c1clines (args)
  (unless (every #'stringp args)
    (cmperr "The argument to CLINES, ~s, is not a list of strings." args))
  (setf *clines-string-list* (nconc *clines-string-list* (copy-list args)))
  '(progn))

(defun output-clines (output-stream)
  (flet ((parse-one-string (s output-stream)
           (with-input-from-string (stream s)
             (loop for c = (read-char stream nil nil)
                while c
                do (if (eq c #\@)
                       (let ((object (handler-case (read stream)
                                       (serious-condition (c)
                                         (cmperr "Unable to parse FFI:CLINES string~& ~S"
                                                 s)))))
                         (let ((*compiler-output1* output-stream))
                           (wt (add-object object :permanent t))))
                       (write-char c output-stream))))))
    (loop for s in *clines-string-list*
       do (terpri output-stream)
       do (if (find #\@ s)
              (parse-one-string s output-stream)
              (write-string s output-stream)))
    (terpri output-stream)
    (setf *clines-string-list* nil)))

;; ----------------------------------------------------------------------
;; C/C++ INLINE CODE
;;

(defun c1c-inline (args)
  ;; We are on the safe side by assuming that the form has side effects
  (destructuring-bind (arguments arg-types output-type c-expression
				 &rest rest
				 &key (side-effects t) one-liner
				 &aux output-rep-type)
      args
    (unless (= (length arguments) (length arg-types))
      (cmperr "In a C-INLINE form the number of declare arguments and the number of supplied ones do not match:~%~S"
	      `(C-INLINE ,@args)))
    ;; We cannot handle :cstrings as input arguments. :cstrings are
    ;; null-terminated strings, but not all of our lisp strings will
    ;; be null terminated. In particular, those with a fill pointer
    ;; will not.
    (let ((ndx (position :cstring arg-types)))
      (when ndx
	(let* ((var (gensym))
               (arguments (copy-list arguments))
	       (value (elt arguments ndx)))
	  (setf (elt arguments ndx) var
		(elt arg-types ndx) :char*)
	  (return-from c1c-inline
	    (c1expr
	     `(ffi::with-cstring (,var ,value)
	       (c-inline ,arguments ,arg-types ,output-type ,c-expression
		,@rest)))))))
    ;; Find out the output types of the inline form. The syntax is rather relaxed
    ;; 	output-type = lisp-type | c-type | (values {lisp-type | c-type}*)
    (flet ((produce-type-pair (type)
	     (if (lisp-type-p type)
		 (cons type (lisp-type->rep-type type))
		 (cons (rep-type->lisp-type type) type))))
      (cond ((eq output-type ':void)
	     (setf output-rep-type '()
		   output-type 'NIL))
	    ((equal output-type '(VALUES &REST t))
	     (setf output-rep-type '((VALUES &REST t))))
	    ((and (consp output-type) (eql (first output-type) 'VALUES))
	     (let ((x (mapcar #'produce-type-pair (rest output-type))))
	       (setf output-rep-type (mapcar #'cdr x)
		     output-type `(VALUES ,@(mapcar #'car x)))))
	    (t
	     (let ((x (produce-type-pair output-type)))
	       (setf output-type (car x)
		     output-rep-type (list (cdr x)))))))
    (let* ((processed-arguments '()))
      (unless (and (listp arguments)
		   (listp arg-types)
		   (stringp c-expression))
	(cmperr "C-INLINE: syntax error in ~S"
		(list* 'c-inline args)))
      (do ((processed-arguments '())
	   (processed-arg-types '()))
	  ((and (endp arguments) (endp arg-types))
	   (make-c1form* 'C-INLINE :type output-type
                         :side-effects side-effects
                         :args
			 (nreverse processed-arguments)
			 (nreverse processed-arg-types)
			 output-rep-type
			 c-expression
			 side-effects
			 one-liner))
	(push (or (pop arg-types) 'T) processed-arg-types)
	(push (c1expr (pop arguments)) processed-arguments)))))

(defun produce-inline-loc (inlined-arguments arg-types output-rep-type
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

    (setf coerced-arguments (coerce-locs inlined-arguments arg-types args-to-be-saved))
    ;;(setf output-rep-type (lisp-type->rep-type output-rep-type))

    ;; If the form does not output any data, and there are no side
    ;; effects, try to omit it.
    (when (null output-rep-type)
      (if side-effects
	  (progn
	    (wt-nl)
	    (wt-c-inline-loc output-rep-type c-expression coerced-arguments t nil)
	    (when one-liner (wt ";")))
	  (cmpnote "Ignoring form ~S" c-expression))
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
    (flet ((make-output-var (type)
	     (let ((var (make-lcl-var :rep-type type)))
	       (wt-nl (rep-type-name type) " " var ";")
	       var)))
      (open-inline-block)
      (let ((output-vars (mapcar #'make-output-var output-rep-type)))
	(wt-c-inline-loc output-rep-type c-expression coerced-arguments side-effects output-vars)
	(cond ((= (length output-vars) 1)
	       (first output-vars))
	      (t
	       (loop for v in output-vars
		     for i from 0
		     do (let ((*destination* `(VALUE ,i))) (set-loc v)))
	       (wt "cl_env_copy->nvalues=" (length output-vars) ";")
	       'VALUES))))))

(defun c2c-inline (c1form arguments &rest rest)
  (declare (ignore c1form))
  (let ((*inline-blocks* 0)
        (*temp* *temp*))
    (unwind-exit (apply #'produce-inline-loc (inline-args arguments) rest))
    (close-inline-blocks)))

(defun coerce-locs (inlined-args &optional types args-to-be-saved)
  (do* ((l inlined-args (cdr l))
	(item (first l) (first l))
	(i 0 (1+ i))
	(block-opened nil))
       ((endp l)
	inlined-args)
    (let* ((type (if types (pop types) :object))
	   (rep-type (lisp-type->rep-type type))
	   (lisp-type (first item))
	   (loc (second item)))
      (cond ((and (not (loc-movable-p loc)) (member i args-to-be-saved))
	     (let ((lcl (make-lcl-var :rep-type rep-type)))
	       (wt-nl)
	       (unless block-opened
		 (open-inline-block))
	       (wt (rep-type-name rep-type) " " lcl "= ")
	       (wt-coerce-loc rep-type loc)
	       (wt ";")
	       (setq loc lcl)))
	    ((and (not (equal rep-type (loc-representation-type loc))))
	     (setq loc `(COERCE-LOC ,rep-type ,loc))))
      (setf (first l) loc))))

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
	   (cond ((eq k #\#)
                  (wt #\#))
                 ((or (null index) (and next-char (alphanumericp next-char)))
		  (wt #\# k))
		 ((< index (length coerced-arguments))
		  (wt (nth index coerced-arguments)))
		 (t
		  (cmperr "C-INLINE: Variable code exceeds number of arguments")))))
	(otherwise
	 (write-char c *compiler-output1*))))))

(defun c-inline-safe-string (constant-string)
  ;; Produce a text representation of a string that can be used
  ;; in a C-INLINE form, without triggering the @ or # escape
  ;; characters
  (c-filtered-string
   (concatenate 'string
		(loop for c across constant-string
		   when (member c '(#\# #\@))
		   collect c
		   collect c))))
