;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;; CMPC-MACHINE -- Abstract target machine details
;;;;

(in-package #-new-cmp "COMPILER" #+new-cmp "C-BACKEND")

(defconstant +representation-types+
  '(;; These types can be used by ECL to unbox data
    ;; They are sorted from the most specific, to the least specific one.
    (:byte .
    #1=((signed-byte 8) "int8_t" "ecl_make_int8_t" "ecl_to_int8_t" "ecl_fixnum"))
    (:unsigned-byte .
    #2=((unsigned-byte 8) "uint8_t" "ecl_make_uint8_t" "ecl_to_uint8_t" "ecl_fixnum"))
    (:fixnum integer "cl_fixnum" "ecl_make_fixnum" "ecl_to_fixnum" "ecl_fixnum")
    (:int integer "int" "ecl_make_int" "ecl_to_int" "ecl_to_int")
    (:unsigned-int integer "unsigned int" "ecl_make_uint" "ecl_to_uint" "ecl_to_uint")
    (:long integer "long" "ecl_make_long" "ecl_to_long" "ecl_to_long")
    (:unsigned-long integer "unsigned long" "ecl_make_ulong" "ecl_to_ulong" "ecl_to_ulong")
    (:cl-index integer "cl_index" "ecl_make_unsigned_integer" "ecl_to_cl_index" "ecl_fixnum")
    (:long-long integer "ecl_long_long_t" "ecl_make_long_long" "ecl_to_long_long" "ecl_to_long_long")
    (:unsigned-long-long integer "ecl_ulong_long_t" "ecl_make_ulong_long" "ecl_to_ulong_long" "ecl_to_ulong_long")
    (:float single-float "float" "ecl_make_single_float" "ecl_to_float" "ecl_single_float")
    (:double double-float "double" "ecl_make_double_float" "ecl_to_double" "ecl_double_float")
    (:long-double long-float "long double" "ecl_make_long_float" "ecl_to_long_double" "ecl_long_float")
    (:unsigned-char base-char "unsigned char" "CODE_CHAR" "ecl_base_char_code" "CHAR_CODE")
    (:char base-char "char" "CODE_CHAR" "ecl_base_char_code" "CHAR_CODE")
    (:wchar character "ecl_character" "CODE_CHAR" "ecl_char_code" "CHAR_CODE")
    (:float-sse-pack ext::float-sse-pack "__m128" "ecl_make_float_sse_pack"
     "ecl_unbox_float_sse_pack" "ecl_unbox_float_sse_pack_unsafe")
    (:double-sse-pack ext::double-sse-pack "__m128d" "ecl_make_double_sse_pack"
     "ecl_unbox_double_sse_pack" "ecl_unbox_double_sse_pack_unsafe")
    (:int-sse-pack ext::sse-pack #|<-intentional|# "__m128i" "ecl_make_int_sse_pack"
     "ecl_unbox_int_sse_pack" "ecl_unbox_int_sse_pack_unsafe")
    (:object t "cl_object")
    (:bool t "bool" "ecl_make_bool" "ecl_to_bool" "ecl_to_bool")
    ;; These types are never selected to unbox data.
    ;; They are here, because we need to know how to print them.
    (:void nil "void")
    (:pointer-void si::foreign-data "void*" "ecl_make_pointer" "ecl_to_pointer" "ecl_to_pointer")
    (:cstring string "char*" "ecl_cstring_to_base_string_or_nil")
    (:char* string "char*")
    (:int8-t . #1#)
    (:uint8-t . #2#)
    (:int16-t integer "ecl_int16_t" "ecl_make_int16_t" "ecl_to_int16_t" "ecl_to_int16_t")
    (:uint16-t integer "ecl_uint16_t" "ecl_make_uint16_t" "ecl_to_uint16_t" "ecl_to_unt16_t")
    (:int32-t integer "ecl_int32_t" "ecl_make_int32_t" "ecl_to_int32_t" "ecl_to_int32_t")
    (:uint32-t integer "ecl_uint32_t" "ecl_make_uint32_t" "ecl_to_uint32_t" "ecl_to_uint32_t")
    (:int64-t integer "ecl_int64_t" "ecl_make_int64_t" "ecl_to_int64_t" "ecl_to_int64_t")
    (:uint64-t integer "ecl_uint64_t" "ecl_make_uint64_t" "ecl_to_uint64_t" "ecl_to_uint64_t")
    (:short integer "short" "ecl_make_short" "ecl_to_short" "ecl_fixnum")
    (:unsigned-short integer "unsigned short" "ecl_make_ushort" "ecl_to_ushort" "ecl_fixnum")
    ))

(defconstant +this-machine-c-types+
  '((:byte . -8)
    (:unsigned-byte . 8)
    (:unsigned-short . #.(- (logcount ffi:c-ushort-max)))
    (:short . #.(- (logcount ffi:c-ushort-max)))
    (:unsigned-int . #.(logcount ffi:c-uint-max))
    (:int . #.(- (logcount ffi:c-uint-max)))
    (:unsigned-long . #.(logcount ffi:c-ulong-max))
    (:long . #.(- (logcount ffi:c-ulong-max)))
    #+long-long
    (:unsigned-long-long . #.(logcount ffi:c-ulong-long-max))
    #+long-long
    (:long-long . #.(- (logcount ffi:c-ulong-long-max)))
    (:cl-index . #.(logcount most-positive-fixnum))
    (:fixnum . #.(- -1 (logcount most-positive-fixnum)))
    (:uint8-t . 8)
    (:int8-t . -8)
    #+:uint16-t
    (:uint16-t . 16)
    #+:uint16-t
    (:int16-t . -16)
    #+:uint32-t
    (:uint32-t . 32)
    #+:uint32-t
    (:int32-t . -32)
    #+:uint64-t
    (:uint64-t . 64)
    #+:uint64-t
    (:int64-t . -64)
    #+:sse2 (:float-sse-pack . nil)
    #+:sse2 (:double-sse-pack . nil)
    #+:sse2 (:int-sse-pack . nil)
    #+:long-float (:long-double . nil)
    ))

(defconstant +all-machines-c-types+
  '((:object)
    (:float)
    (:double)
    (:char)
    (:unsigned-char)
    (:wchar)
    (:char*)
    (:cstring)
    (:bool)
    (:void)
    (:pointer-void)))

(defun make-rep-type (all-c-types name lisp-type c-name &optional to-lisp from-lisp from-lisp-unsafe)
  (let* ((record (assoc name all-c-types))
	 (bits (cdr record)))
    (when record
      ;; For integer bits we get extra information from ALL-C-TYPES
      (when bits
	(if (plusp bits)
	    (setf lisp-type `(unsigned-byte ,bits))
	    (setf bits (- bits)
		  lisp-type `(signed-byte ,bits))))
      (%make-rep-type
       :name name
       :lisp-type lisp-type
       :bits bits
       :numberp (subtypep lisp-type 'number)
       :integerp (subtypep lisp-type 'integer)
       :c-name c-name
       :to-lisp to-lisp
       :from-lisp from-lisp
       :from-lisp-unsafe from-lisp-unsafe))))

(defun make-rep-type-hash (all-c-types)
  (let ((table (make-hash-table :size 128 :test 'eq)))
    table))

(defun default-machine ()
  (let* ((all-c-types (append +this-machine-c-types+ +all-machines-c-types+))
	 (table (make-hash-table :size 128 :test 'eq))
	 (sorted-rep-types
	  ;; Create the rep-type objects
	  (loop for i from 0
	     for record in +representation-types+
	     for rep-type = (apply #'make-rep-type all-c-types record)
	     when rep-type
	     do (setf (rep-type-index rep-type) i)
	     and collect (setf (gethash (rep-type-name rep-type) table) rep-type))))
    ;; hack: sse-pack -> int, but int -> int-sse-pack
    (let ((r (gethash :int-sse-pack table)))
      (when r
	(setf (rep-type-to-lisp r) 'ext:int-sse-pack)))
    ;; On a second pass, we replace types with more general ones
    (loop with fixnum-rep-type = (gethash ':fixnum table)
       with fixnum-lisp-type = (rep-type-lisp-type fixnum-rep-type)
       for (name . rest) in +representation-types+
       for r = (gethash name table)
       when (and r (subtypep (rep-type-lisp-type r) fixnum-lisp-type))
       do (setf (rep-type-from-lisp-unsafe r) "ecl_fixnum"))
    ;; Create machine object
    (make-machine :c-types all-c-types
		  :rep-type-hash table
		  :sorted-types sorted-rep-types)))

(defun machine-c-type-p (name)
  (gethash name (machine-rep-type-hash *machine*)))

(defun machine-fixnump (number)
  (typep number (rep-type-lisp-type (gethash :fixnum number))))

(defconstant +default-machine+ (setf *machine* (default-machine)))
