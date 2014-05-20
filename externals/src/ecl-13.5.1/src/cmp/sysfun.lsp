;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;; CMPSYSFUN   Database for system functions.
;;;
;;; Copyright (c) 2003, Juan Jose Garcia Ripoll
;;; Copyright (c) 1991, Giuseppe Attardi. All rights reserved.
;;;    Copying of this file is authorized to users who have executed the true
;;;    and proper "License Agreement for ECoLisp".
;;;
;;; DATABASE OF INLINE EXPANSIONS
;;;
;;;	(DEF-INLINE function-name kind ([arg-type]*) return-rep-type
;;;		expansion-string)
;;;
;;; Here, ARG-TYPE is the list of argument types belonging to the lisp family,
;;; while RETURN-REP-TYPE is a representation type, i.e. the C type of the
;;; output expression. EXPANSION-STRING is a C/C++ expression template, like the
;;; ones used by C-INLINE. Finally, KIND can be :ALWAYS, :SAFE or :UNSAFE,
;;; depending on whether the inline expression should be applied always, in safe
;;; or in unsafe compilation mode, respectively.
;;;

(in-package "COMPILER")

(defun def-inline (name safety arg-types return-rep-type expansion
                   &key (one-liner t) (exact-return-type nil) (inline-or-warn nil)
		   (multiple-values t)
                   &aux arg-rep-types)
  (setf safety
	(case safety
	  (:unsafe :inline-unsafe)
	  (:safe :inline-safe)
	  (:always :inline-always)
	  (t (error "In DEF-INLINE, wrong value of SAFETY"))))
  (setf arg-rep-types
	(mapcar #'(lambda (x) (if (eq x '*) x (lisp-type->rep-type x)))
		arg-types))
  (when (eq return-rep-type t)
    (setf return-rep-type :object))
  (when inline-or-warn
    (put-sysprop name 'should-be-inlined t))
  (let* ((return-type (if (and (consp return-rep-type)
                               (eq (first return-rep-type) 'values))
                          t
                          (rep-type->lisp-type return-rep-type)))
         (inline-info
          (make-inline-info :name name
                            :arg-rep-types arg-rep-types
                            :return-rep-type return-rep-type
                            :return-type return-type
                            :arg-types arg-types
                            :exact-return-type exact-return-type
			    :multiple-values multiple-values
                            ;; :side-effects (not (get-sysprop name 'no-side-effects))
                            :one-liner one-liner
                            :expansion expansion))
         (previous (get-sysprop name safety)))
    #+(or)
    (loop for i in previous
       when (and (equalp (inline-info-arg-types i) arg-types)
                 (not (equalp return-type (inline-info-return-type i))))
       do (format t "~&;;; Redundand inline definition for ~A~&;;; ~<~A~>~&;;; ~<~A~>"
                  name i inline-info))
    (put-sysprop name safety (cons inline-info previous))))

(eval-when (:compile-toplevel :execute)
(defparameter +inline-forms+ '(
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ALL FUNCTION DECLARATIONS AND INLINE FORMS
;;;

(def-inline aref :unsafe (t t t) t
 "@0;ecl_aref_unsafe(#0,ecl_fixnum(#1)*(#0)->array.dims[1]+ecl_fixnum(#2))")
(def-inline aref :unsafe ((array t) t t) t
 "@0;(#0)->array.self.t[ecl_fixnum(#1)*(#0)->array.dims[1]+ecl_fixnum(#2)]")
(def-inline aref :unsafe ((array bit) t t) :fixnum
 "@0;ecl_aref_bv(#0,ecl_fixnum(#1)*(#0)->array.dims[1]+ecl_fixnum(#2))")
(def-inline aref :unsafe ((array t) fixnum fixnum) t
 "@0;(#0)->array.self.t[#1*(#0)->array.dims[1]+#2]")
(def-inline aref :unsafe ((array bit) fixnum fixnum) :fixnum
 "@0;ecl_aref_bv(#0,(#1)*(#0)->array.dims[1]+#2)")
(def-inline aref :unsafe ((array base-char) fixnum fixnum) :unsigned-char
 "@0;(#0)->base_string.self[#1*(#0)->array.dims[1]+#2]")
(def-inline aref :unsafe ((array double-float) fixnum fixnum) :double
 "@0;(#0)->array.self.df[#1*(#0)->array.dims[1]+#2]")
(def-inline aref :unsafe ((array single-float) fixnum fixnum) :float
 "@0;(#0)->array.self.sf[#1*(#0)->array.dims[1]+#2]")
(def-inline aref :unsafe ((array fixnum) fixnum fixnum) :fixnum
 "@0;(#0)->array.self.fix[#1*(#0)->array.dims[1]+#2]")

(def-inline aref :always (t t) t "ecl_aref1(#0,ecl_to_size(#1))")
(def-inline aref :always (t fixnum) t "ecl_aref1(#0,#1)")
(def-inline aref :unsafe (t t) t "ecl_aref1(#0,ecl_fixnum(#1))")
(def-inline aref :unsafe ((array bit) t) :fixnum "ecl_aref_bv(#0,ecl_fixnum(#1))")
(def-inline aref :unsafe ((array bit) fixnum) :fixnum "ecl_aref_bv(#0,#1)")
#+unicode
(def-inline aref :unsafe ((array character) fixnum) :wchar
 "(#0)->string.self[#1]")
(def-inline aref :unsafe ((array base-char) fixnum) :unsigned-char
 "(#0)->base_string.self[#1]")
(def-inline aref :unsafe ((array double-float) fixnum) :double
 "(#0)->array.self.df[#1]")
(def-inline aref :unsafe ((array single-float) fixnum) :float
 "(#0)->array.self.sf[#1]")
(def-inline aref :unsafe ((array fixnum) fixnum) :fixnum
 "(#0)->array.self.fix[#1]")

(def-inline row-major-aref :always (t t) t "ecl_aref(#0,ecl_to_size(#1))")
(def-inline row-major-aref :always (t fixnum) t "ecl_aref(#0,#1)")
(def-inline row-major-aref :unsafe (t t) t "ecl_aref_unsafe(#0,ecl_fixnum(#1))")
(def-inline row-major-aref :unsafe (t fixnum) t "ecl_aref_unsafe(#0,#1)")
(def-inline row-major-aref :unsafe ((array bit) t) :fixnum "ecl_aref_bv(#0,ecl_fixnum(#1))")
(def-inline row-major-aref :unsafe ((array bit) fixnum) :fixnum "ecl_aref_bv(#0,#1)")
#+unicode
(def-inline row-major-aref :unsafe ((array character) fixnum) :wchar
 "(#0)->string.self[#1]")
(def-inline row-major-aref :unsafe ((array base-char) fixnum) :unsigned-char
 "(#0)->base_string.self[#1]")
(def-inline row-major-aref :unsafe ((array ext:byte8) fixnum) :uint8-t
 "(#0)->vector.self.b8[#1]")
(def-inline row-major-aref :unsafe ((array ext:integer8) fixnum) :int8-t
 "(#0)->vector.self.i8[#1]")
#+:uint16-t
(def-inline row-major-aref :unsafe ((array ext:byte16) fixnum) :uint16-t
 "(#0)->vector.self.b16[#1]")
#+:uint16-t
(def-inline row-major-aref :unsafe ((array ext:integer16) fixnum) :int16-t
 "(#0)->vector.self.i16[#1]")
#+:uint32-t
(def-inline row-major-aref :unsafe ((array ext:byte32) fixnum) :uint32-t
 "(#0)->vector.self.b32[#1]")
#+:uint32-t
(def-inline row-major-aref :unsafe ((array ext:integer32) fixnum) :int32-t
 "(#0)->vector.self.i32[#1]")
#+:uint64-t
(def-inline row-major-aref :unsafe ((array ext:byte64) fixnum) :uint64-t
 "(#0)->vector.self.b64[#1]")
#+:uint64-t
(def-inline row-major-aref :unsafe ((array ext:integer64) fixnum) :int64-t
 "(#0)->vector.self.i64[#1]")
(def-inline row-major-aref :unsafe ((array double-float) fixnum) :double
 "(#0)->array.self.df[#1]")
(def-inline row-major-aref :unsafe ((array single-float) fixnum) :float
 "(#0)->array.self.sf[#1]")
(def-inline row-major-aref :unsafe ((array fixnum) fixnum) :fixnum
 "(#0)->array.self.fix[#1]")

(def-inline si:row-major-aset :always (t t t) t "ecl_aset(#0,ecl_to_size(#1),#2)")
(def-inline si:row-major-aset :always (t fixnum t) t "ecl_aset(#0,#1,#2)")
(def-inline si:row-major-aset :unsafe (t t t) t "ecl_aset_unsafe(#0,ecl_fixnum(#1),#2)")
(def-inline si:row-major-aset :unsafe (t fixnum t) t "ecl_aset_unsafe(#0,#1,#2)")
(def-inline si:row-major-aset :unsafe ((array t) fixnum t) t
 "(#0)->vector.self.t[#1]= #2")
(def-inline si:row-major-aset :unsafe ((array bit) fixnum t) :fixnum
 "ecl_aset_bv(#0,#1,ecl_fixnum(#2))")
(def-inline si:row-major-aset :unsafe ((array bit) fixnum fixnum) :fixnum
 "ecl_aset_bv(#0,#1,#2)")
(def-inline si:row-major-aset :unsafe ((array base-char) fixnum base-char) :unsigned-char
 "(#0)->base_string.self[#1]= #2")
#+unicode
(def-inline si:row-major-aset :unsafe ((array character) fixnum character) :wchar
 "(#0)->string.self[#1]= #2")
(def-inline si:row-major-aset :unsafe ((array ext:byte8) fixnum ext:byte8) :uint8-t
 "(#0)->vector.self.b8[#1]= #2")
(def-inline si:row-major-aset :unsafe ((array ext:integer8) fixnum ext:integer8) :int8-t
 "(#0)->vector.self.i8[#1]= #2")
#+:uint16-t
(def-inline si:row-major-aset :unsafe ((array ext:byte16) fixnum ext:byte16) :uint16-t
 "(#0)->vector.self.b16[#1]= #2")
#+:uint16-t
(def-inline si:row-major-aset :unsafe ((array ext:integer16) fixnum ext:integer16) :int16-t
 "(#0)->vector.self.i16[#1]= #2")
#+:uint32-t
(def-inline si:row-major-aset :unsafe ((array ext:byte32) fixnum ext:byte32) :uint32-t
 "(#0)->vector.self.b32[#1]= #2")
#+:uint32-t
(def-inline si:row-major-aset :unsafe ((array ext:integer32) fixnum ext:integer32) :int32-t
 "(#0)->vector.self.i32[#1]= #2")
#+:uint64-t
(def-inline si:row-major-aset :unsafe ((array ext:byte64) fixnum ext:byte64) :uint64-t
 "(#0)->vector.self.b64[#1]= #2")
#+:uint64-t
(def-inline si:row-major-aset :unsafe ((array ext:integer64) fixnum ext:integer64) :int64-t
 "(#0)->vector.self.i64[#1]= #2")
(def-inline si:row-major-aset :unsafe ((array double-float) fixnum double-float) :double
 "(#0)->array.self.df[#1]= #2")
(def-inline si:row-major-aset :unsafe ((array single-float) fixnum single-float) :float
 "(#0)->array.self.sf[#1]= #2")
(def-inline si:row-major-aset :unsafe ((array fixnum) fixnum fixnum) :fixnum
 "(#0)->array.self.fix[#1]= #2")

(def-inline si:copy-subarray :always (array ext:array-index array ext:array-index
				      ext:array-index) array
 "@0;(ecl_copy_subarray(#0,#1,#2,#3,#4),#0)")

(def-inline array-rank :unsafe (array) :fixnum
 "@0;(((#0)->d.t == t_array)?(#0)->array.rank:1)")
(def-inline array-rank :always (array) :fixnum
 "ecl_array_rank(#0)")

(def-inline array-dimension :always (t t) fixnum
 "ecl_array_dimension(#0,ecl_to_size(#1))")
(def-inline array-dimension :always (t fixnum) fixnum
 "ecl_array_dimension(#0,#1)")

(def-inline array-total-size :unsafe (t) :fixnum "((#0)->array.dim)")

(def-inline adjustable-array-p :always (t) :bool "@0;(ECL_ARRAYP(#0)? (void)0: FEtype_error_array(#0),ECL_ADJUSTABLE_ARRAY_P(#0))")
(def-inline adjustable-array-p :unsafe (array) :bool "ECL_ADJUSTABLE_ARRAY_P(#0)")

(def-inline svref :always (t t) t "ecl_aref1(#0,ecl_to_size(#1))")
(def-inline svref :always (t fixnum) t "ecl_aref1(#0,#1)")
(def-inline svref :unsafe (t t) t "(#0)->vector.self.t[ecl_fixnum(#1)]")
(def-inline svref :unsafe (t fixnum) t "(#0)->vector.self.t[#1]")

(def-inline si:svset :always (t t t) t "ecl_aset1(#0,ecl_to_size(#1),#2)")
(def-inline si:svset :always (t fixnum t) t "ecl_aset1(#0,#1,#2)")
(def-inline si:svset :unsafe (t t t) t "((#0)->vector.self.t[ecl_fixnum(#1)]=(#2))")
(def-inline si:svset :unsafe (t fixnum t) t "(#0)->vector.self.t[#1]= #2")

(def-inline array-has-fill-pointer-p :always (t) :bool "@0;(ECL_ARRAYP(#0)?(void)0:FEtype_error_array(#0),ECL_ARRAY_HAS_FILL_POINTER_P(#0))")
(def-inline array-has-fill-pointer-p :unsafe (array) :bool "ECL_ARRAY_HAS_FILL_POINTER_P(#0)")

(def-inline fill-pointer :unsafe (t) :fixnum "((#0)->vector.fillp)")


(def-inline si:fill-pointer-set :unsafe (t fixnum) :fixnum
 "((#0)->vector.fillp)=(#1)")

;; file character.d

(def-inline standard-char-p :always (character) :bool "ecl_standard_char_p(#0)")

(def-inline graphic-char-p :always (character) :bool "ecl_graphic_char_p(#0)")

(def-inline alpha-char-p :always (character) :bool "ecl_alpha_char_p(#0)")

(def-inline upper-case-p :always (character) :bool "ecl_upper_case_p(#0)")

(def-inline lower-case-p :always (character) :bool "ecl_lower_case_p(#0)")

(def-inline both-case-p :always (character) :bool "ecl_both_case_p(#0)")

(def-inline alphanumericp :always (character) :bool "ecl_alphanumericp(#0)")

(def-inline char= :always (t t) :bool "ecl_char_code(#0)==ecl_char_code(#1)")
(def-inline char= :always (character character) :bool "(#0)==(#1)")

(def-inline char/= :always (t t) :bool "ecl_char_code(#0)!=ecl_char_code(#1)")
(def-inline char/= :always (character character) :bool "(#0)!=(#1)")

(def-inline char< :always (character character) :bool "(#0)<(#1)")

(def-inline char> :always (character character) :bool "(#0)>(#1)")

(def-inline char<= :always (character character) :bool "(#0)<=(#1)")

(def-inline char>= :always (character character) :bool "(#0)>=(#1)")

(def-inline char-code :always (character) :fixnum "#0")

(def-inline code-char :always (fixnum) :wchar "#0")

(def-inline char-upcase :always (base-char) :unsigned-char "ecl_char_upcase(#0)")
(def-inline char-upcase :always (character) :wchar "ecl_char_upcase(#0)")

(def-inline char-downcase :always (base-char) :unsigned-char "ecl_char_downcase(#0)")
(def-inline char-downcase :always (character) :wchar "ecl_char_downcase(#0)")

(def-inline char-int :always (character) :fixnum "#0")

;; file ffi.d

(def-inline si:foreign-data-p :always (t) :bool "@0;ECL_FOREIGN_DATA_P(#0)")

;; file file.d

(def-inline input-stream-p :always (stream) :bool "ecl_input_stream_p(#0)")

(def-inline output-stream-p :always (stream) :bool "ecl_output_stream_p(#0)")

;; file hash.d

(def-inline gethash :always (t t t) t "ecl_gethash_safe(#0,#1,#2)" :multiple-values nil)
(def-inline gethash :always (t t) t "ecl_gethash_safe(#0,#1,ECL_NIL)" :multiple-values nil)
(def-inline hash-table-count :unsafe (hash-table) ext:array-index "ecl_hash_table_count(#0)")

;; file list.d

(def-inline car :unsafe (cons) t "ECL_CONS_CAR(#0)")
(def-inline car :unsafe (t) t "_ecl_car(#0)")

(def-inline si::cons-car :always (t) t "_ecl_car(#0)")
(def-inline si::cons-car :unsafe (t) t "ECL_CONS_CAR(#0)")

(def-inline cdr :unsafe (cons) t "ECL_CONS_CDR(#0)")
(def-inline cdr :unsafe (t) t "_ecl_cdr(#0)")

(def-inline si::cons-cdr :always (t) t "_ecl_cdr(#0)")
(def-inline si::cons-cdr :unsafe (t) t "ECL_CONS_CDR(#0)")

;; BEGIN-GENERATED (gen-cons-sysfun)

(def-inline car :always (t) t "ecl_car(#0)")
(def-inline car :unsafe (t) t "_ecl_car(#0)")
(def-inline cdr :always (t) t "ecl_cdr(#0)")
(def-inline cdr :unsafe (t) t "_ecl_cdr(#0)")
(def-inline caar :always (t) t "ecl_caar(#0)")
(def-inline caar :unsafe (t) t "_ecl_caar(#0)")
(def-inline cdar :always (t) t "ecl_cdar(#0)")
(def-inline cdar :unsafe (t) t "_ecl_cdar(#0)")
(def-inline cadr :always (t) t "ecl_cadr(#0)")
(def-inline cadr :unsafe (t) t "_ecl_cadr(#0)")
(def-inline cddr :always (t) t "ecl_cddr(#0)")
(def-inline cddr :unsafe (t) t "_ecl_cddr(#0)")
(def-inline caaar :always (t) t "ecl_caaar(#0)")
(def-inline caaar :unsafe (t) t "_ecl_caaar(#0)")
(def-inline cdaar :always (t) t "ecl_cdaar(#0)")
(def-inline cdaar :unsafe (t) t "_ecl_cdaar(#0)")
(def-inline cadar :always (t) t "ecl_cadar(#0)")
(def-inline cadar :unsafe (t) t "_ecl_cadar(#0)")
(def-inline cddar :always (t) t "ecl_cddar(#0)")
(def-inline cddar :unsafe (t) t "_ecl_cddar(#0)")
(def-inline caadr :always (t) t "ecl_caadr(#0)")
(def-inline caadr :unsafe (t) t "_ecl_caadr(#0)")
(def-inline cdadr :always (t) t "ecl_cdadr(#0)")
(def-inline cdadr :unsafe (t) t "_ecl_cdadr(#0)")
(def-inline caddr :always (t) t "ecl_caddr(#0)")
(def-inline caddr :unsafe (t) t "_ecl_caddr(#0)")
(def-inline cdddr :always (t) t "ecl_cdddr(#0)")
(def-inline cdddr :unsafe (t) t "_ecl_cdddr(#0)")
(def-inline caaaar :always (t) t "ecl_caaaar(#0)")
(def-inline caaaar :unsafe (t) t "_ecl_caaaar(#0)")
(def-inline cdaaar :always (t) t "ecl_cdaaar(#0)")
(def-inline cdaaar :unsafe (t) t "_ecl_cdaaar(#0)")
(def-inline cadaar :always (t) t "ecl_cadaar(#0)")
(def-inline cadaar :unsafe (t) t "_ecl_cadaar(#0)")
(def-inline cddaar :always (t) t "ecl_cddaar(#0)")
(def-inline cddaar :unsafe (t) t "_ecl_cddaar(#0)")
(def-inline caadar :always (t) t "ecl_caadar(#0)")
(def-inline caadar :unsafe (t) t "_ecl_caadar(#0)")
(def-inline cdadar :always (t) t "ecl_cdadar(#0)")
(def-inline cdadar :unsafe (t) t "_ecl_cdadar(#0)")
(def-inline caddar :always (t) t "ecl_caddar(#0)")
(def-inline caddar :unsafe (t) t "_ecl_caddar(#0)")
(def-inline cdddar :always (t) t "ecl_cdddar(#0)")
(def-inline cdddar :unsafe (t) t "_ecl_cdddar(#0)")
(def-inline caaadr :always (t) t "ecl_caaadr(#0)")
(def-inline caaadr :unsafe (t) t "_ecl_caaadr(#0)")
(def-inline cdaadr :always (t) t "ecl_cdaadr(#0)")
(def-inline cdaadr :unsafe (t) t "_ecl_cdaadr(#0)")
(def-inline cadadr :always (t) t "ecl_cadadr(#0)")
(def-inline cadadr :unsafe (t) t "_ecl_cadadr(#0)")
(def-inline cddadr :always (t) t "ecl_cddadr(#0)")
(def-inline cddadr :unsafe (t) t "_ecl_cddadr(#0)")
(def-inline caaddr :always (t) t "ecl_caaddr(#0)")
(def-inline caaddr :unsafe (t) t "_ecl_caaddr(#0)")
(def-inline cdaddr :always (t) t "ecl_cdaddr(#0)")
(def-inline cdaddr :unsafe (t) t "_ecl_cdaddr(#0)")
(def-inline cadddr :always (t) t "ecl_cadddr(#0)")
(def-inline cadddr :unsafe (t) t "_ecl_cadddr(#0)")
(def-inline cddddr :always (t) t "ecl_cddddr(#0)")
(def-inline cddddr :unsafe (t) t "_ecl_cddddr(#0)")
;; END-GENERATED

(def-inline cons :always (t t) t "CONS(#0,#1)")

(def-inline endp :safe (t) :bool "ecl_endp(#0)")
(def-inline endp :unsafe (t) :bool "#0==ECL_NIL")

(def-inline nth :always (t t) t "ecl_nth(ecl_to_size(#0),#1)")
(def-inline nth :always (fixnum t) t "ecl_nth(#0,#1)")
(def-inline nth :unsafe (t t) t "ecl_nth(ecl_fixnum(#0),#1)")
(def-inline nth :unsafe (fixnum t) t "ecl_nth(#0,#1)")

(def-inline nthcdr :always (t t) t "ecl_nthcdr(ecl_to_size(#0),#1)")
(def-inline nthcdr :always (fixnum t) t "ecl_nthcdr(#0,#1)")
(def-inline nthcdr :unsafe (t t) t "ecl_nthcdr(ecl_fixnum(#0),#1)")
(def-inline nthcdr :unsafe (fixnum t) t "ecl_nthcdr(#0,#1)")

(def-inline last :always (t) t "ecl_last(#0,1)")

(def-inline list :always nil t "ECL_NIL")
(def-inline list :always (t) t "ecl_list1(#0)")

(def-inline list* :always (t) t "#0")
(def-inline list* :always (t t) t "CONS(#0,#1)")

(def-inline append :always (t t) t "ecl_append(#0,#1)")

(def-inline nconc :always (t t) t "ecl_nconc(#0,#1)")

(def-inline butlast :always (t) t "ecl_butlast(#0,1)")

(def-inline nbutlast :always (t) t "ecl_nbutlast(#0,1)")

;; file num_arith.d

(def-inline 1+ :always (t) t "ecl_one_plus(#0)")
(def-inline 1+ :always (fixnum) t "ecl_make_integer((#0)+1)")
(def-inline 1+ :always (double-loat) :double "(double)(#0)+1")
(def-inline 1+ :always (single-float) :float "(float)(#0)+1")
(def-inline 1+ :always (fixnum) :fixnum "(#0)+1" :exact-return-type t)

(def-inline 1- :always (t) t "ecl_one_minus(#0)")
(def-inline 1- :always (fixnum) t "ecl_make_integer((#0)-1)")
(def-inline 1- :always (double-float) :double "(double)(#0)-1")
(def-inline 1- :always (single-float) :float "(float)(#0)-1")
(def-inline 1- :always (fixnum) :fixnum "(#0)-1" :exact-return-type t)

;; file num_co.d

(def-inline float :always (t single-float) :float "ecl_to_float(#0)")
(def-inline float :always (t double-float) :double "ecl_to_double(#0)")
#+long-float
(def-inline float :always (t long-float) :long-double "ecl_to_long_double(#0)")
(def-inline float :always (fixnum-float) :double "((double)(#0))" :exact-return-type t)
(def-inline float :always (fixnum-float) :float "((float)(#0))" :exact-return-type t)

(def-inline numerator :unsafe (integer) integer "(#0)")
(def-inline numerator :unsafe (ratio) integer "(#0)->ratio.num")

(def-inline denominator :unsafe (integer) integer "ecl_make_fixnum(1)")
(def-inline denominator :unsafe (ratio) integer "(#0)->ratio.den")

(def-inline floor :always (t) (values &rest t) "ecl_floor1(#0)")
(def-inline floor :always (t t) (values &rest t) "ecl_floor2(#0,#1)")
#+(or) ; does not work well, no multiple values
(def-inline floor :always (fixnum fixnum) :fixnum
 "@01;(#0>=0&&#1>0?(#0)/(#1):ecl_ifloor(#0,#1))")

(def-inline ceiling :always (t) (values &rest t) "ecl_ceiling1(#0)")
(def-inline ceiling :always (t t) (values &rest t) "ecl_ceiling2(#0,#1)")

(def-inline truncate :always (t) (values &rest t) "ecl_truncate1(#0)")
(def-inline truncate :always (t t) (values &rest t) "ecl_truncate2(#0,#1)")
#+(or) ; does not work well, no multiple values
(def-inline truncate :always (fixnum-float) :fixnum "(cl_fixnum)(#0)")

(def-inline round :always (t) (values &rest t) "ecl_round1(#0)")
(def-inline round :always (t t) (values &rest t) "ecl_round2(#0,#1)")

(def-inline mod :always (t t) t "(ecl_floor2(#0,#1),cl_env_copy->values[1])")
(def-inline mod :always (fixnum fixnum) :fixnum
 "@01;(#0>=0&&#1>0?(#0)%(#1):ecl_imod(#0,#1))")

(def-inline rem :always (t t) t "(ecl_truncate2(#0,#1),cl_env_copy->values[1])")
(def-inline rem :always (fixnum fixnum) :fixnum "(#0)%(#1)")

(def-inline = :always (t t) :bool "ecl_number_equalp(#0,#1)")
(def-inline = :always (fixnum-float fixnum-float) :bool "(#0)==(#1)")

(def-inline /= :always (t t) :bool "!ecl_number_equalp(#0,#1)")
(def-inline /= :always (fixnum-float fixnum-float) :bool "(#0)!=(#1)")

(def-inline < :always (t t) :bool "ecl_number_compare(#0,#1)<0")
(def-inline < :always (fixnum-float fixnum-float) :bool "(#0)<(#1)")
(def-inline < :always (fixnum-float fixnum-float fixnum-float) :bool
            "@012;((#0)<(#1) && (#1)<(#2))")

(def-inline > :always (t t) :bool "ecl_number_compare(#0,#1)>0")
(def-inline > :always (fixnum-float fixnum-float) :bool "(#0)>(#1)")
(def-inline > :always (fixnum-float fixnum-float fixnum-float) :bool
            "@012;((#0)>(#1) && (#1)>(#2))")

(def-inline <= :always (t t) :bool "ecl_number_compare(#0,#1)<=0")
(def-inline <= :always (fixnum-float fixnum-float) :bool "(#0)<=(#1)")
(def-inline <= :always (fixnum-float fixnum-float fixnum-float) :bool
            "@012;((#0)<=(#1) && (#1)<=(#2))")

(def-inline >= :always (t t) :bool "ecl_number_compare(#0,#1)>=0")
(def-inline >= :always (fixnum-float fixnum-float) :bool "(#0)>=(#1)")
(def-inline >= :always (fixnum-float fixnum-float fixnum-float) :bool
            "@012;((#0)>=(#1) && (#1)>=(#2))")

(def-inline max :always (t t) t "@01;(ecl_number_compare(#0,#1)>=0?#0:#1)")
(def-inline max :always (fixnum fixnum) :fixnum "@01;(#0)>=(#1)?#0:#1")

(def-inline min :always (t t) t "@01;(ecl_number_compare(#0,#1)<=0?#0:#1)")
(def-inline min :always (fixnum fixnum) :fixnum "@01;(#0)<=(#1)?#0:#1")

;; file num_log.d

(def-inline logand :always nil t "ecl_make_fixnum(-1)")
(def-inline logand :always nil :fixnum "-1")
(def-inline logand :always (t t) t "ecl_boole(ECL_BOOLAND,(#0),(#1))")
(def-inline logand :always (fixnum fixnum) :fixnum "((#0) & (#1))")

(def-inline logandc1 :always (t t) t "ecl_boole(ECL_BOOLANDC1,(#0),(#1))")
(def-inline logandc1 :always (fixnum fixnum) :fixnum "(~(#0) & (#1))")

(def-inline logandc2 :always (t t) t "ecl_boole(ECL_BOOLANDC2,(#0),(#1))")
(def-inline logandc2 :always (fixnum fixnum) :fixnum "((#0) & ~(#1))")

(def-inline logeqv :always nil t "ecl_make_fixnum(-1)")
(def-inline logeqv :always nil :fixnum "-1")
(def-inline logeqv :always (t t) t "ecl_boole(ECL_BOOLEQV,(#0),(#1))")
(def-inline logeqv :always (fixnum fixnum) :fixnum "(~( (#0) ^ (#1) ))")

(def-inline logior :always nil t "ecl_make_fixnum(0)")
(def-inline logior :always nil :fixnum "0")
(def-inline logior :always (t t) t "ecl_boole(ECL_BOOLIOR,(#0),(#1))")
(def-inline logior :always (fixnum fixnum) :fixnum "((#0) | (#1))")

(def-inline lognand :always (t t) t "ecl_boole(ECL_BOOLNAND,(#0),(#1))")
(def-inline lognand :always (fixnum fixnum) :fixnum "(~( (#0) & (#1) ))")

(def-inline lognor :always (t t) t "ecl_boole(ECL_BOOLNOR,(#0),(#1))")
(def-inline lognor :always (fixnum fixnum) :fixnum "(~( (#0) | (#1) ))")

(def-inline lognot :always (t) t "ecl_boole(ECL_BOOLXOR,(#0),ecl_make_fixnum(-1))")
(def-inline lognot :always (fixnum) :fixnum "(~(#0))")

(def-inline logorc1 :always (t t) t "ecl_boole(ECL_BOOLORC1,(#0),(#1))")
(def-inline logorc1 :always (fixnum fixnum) :fixnum "(~(#0) | (#1))")

(def-inline logorc2 :always (t t) t "ecl_boole(ECL_BOOLORC2,(#0),(#1))")
(def-inline logorc2 :always (fixnum fixnum) :fixnum "((#0) | ~(#1))")

(def-inline logxor :always nil t "ecl_make_fixnum(0)")
(def-inline logxor :always nil :fixnum "0")
(def-inline logxor :always (t t) t "ecl_boole(ECL_BOOLXOR,(#0),(#1))")
(def-inline logxor :always (fixnum fixnum) :fixnum "((#0) ^ (#1))")

(def-inline boole :always (fixnum t t) t "ecl_boole((#0),(#1),(#2))")

(def-inline logbitp :always ((integer -29 29) fixnum) :bool "(#1 >> #0) & 1")

(def-inline integer-length :always (t) :cl-index "ecl_integer_length(#0)")

(def-inline zerop :always (t) :bool "ecl_zerop(#0)")
(def-inline zerop :always (fixnum-float) :bool "(#0)==0")

(def-inline plusp :always (t) :bool "ecl_plusp(#0)")
(def-inline plusp :always (fixnum-float) :bool "(#0)>0")

(def-inline minusp :always (t) :bool "ecl_minusp(#0)")
(def-inline minusp :always (fixnum-float) :bool "(#0)<0")

(def-inline oddp :always (t) :bool "ecl_oddp(#0)")
(def-inline oddp :always (fixnum fixnum) :bool "(#0) & 1")

(def-inline evenp :always (t) :bool "ecl_evenp(#0)")
(def-inline evenp :always (fixnum fixnum) :bool "~(#0) & 1")

(def-inline abs :always (t t) t "ecl_abs(#0,#1)")

(def-inline exp :always (t) t "ecl_exp(#0)")

(def-inline expt :always (t t) t "ecl_expt(#0,#1)")
(def-inline expt :always ((integer 2 2) (integer 0 29)) :fixnum "(1<<(#1))")
(def-inline expt :always ((integer 0 0) t) :fixnum "0")
(def-inline expt :always ((integer 1 1) t) :fixnum "1")

(def-inline log :always (fixnum-float) :double "log((double)(#0))" :exact-return-type t)
(def-inline log :always (fixnum-float) :float "(float)log((double)(#0))" :exact-return-type t)

(def-inline sqrt :always (number) number "ecl_sqrt(#0)")
(def-inline sqrt :always ((long-float 0.0 *)) :double "sqrt((double)(#0))")
(def-inline sqrt :always ((double-float 0.0 *)) :double "sqrt((double)(#0))")
(def-inline sqrt :always ((single-float 0.0 *)) :float "(float)sqrt((double)(#0))")
(def-inline sqrt :always ((short-float 0.0 *)) :float "(float)sqrt((double)(#0))")

(def-inline sin :always (number) number "ecl_sin(#0)")
(def-inline sin :always (fixnum-float) :double "sin((double)(#0))" :exact-return-type t)
(def-inline sin :always (fixnum-float) :float "(float)sin((double)(#0))" :exact-return-type t)

(def-inline cos :always (t) number "ecl_cos(#0)")
(def-inline cos :always (fixnum-float) :double "cos((double)(#0))" :exact-return-type t)
(def-inline cos :always (fixnum-float) :float "(float)cos((double)(#0))" :exact-return-type t)

(def-inline tan :always (t) number "ecl_tan(#0)")
(def-inline tan :always (fixnum-float) :double "tan((double)(#0))" :exact-return-type t)
(def-inline tan :always (fixnum-float) :float "(float)tan((double)(#0))" :exact-return-type t)

(def-inline sinh :always (t) number "ecl_sinh(#0)")
(def-inline sinh :always (fixnum-float) :double "sinh((double)(#0))" :exact-return-type t)
(def-inline sinh :always (fixnum-float) :float "(float)sinh((double)(#0))" :exact-return-type t)

(def-inline cosh :always (t) number "ecl_cosh(#0)")
(def-inline cosh :always (fixnum-float) :double "cosh((double)(#0))" :exact-return-type t)
(def-inline cosh :always (fixnum-float) :float "(float)cosh((double)(#0))" :exact-return-type t)

(def-inline tanh :always (t) number "ecl_tanh(#0)")
(def-inline tanh :always (fixnum-float) :double "tanh((double)(#0))" :exact-return-type t)
(def-inline tanh :always (fixnum-float) :float "(float)tanh((double)(#0))" :exact-return-type t)

;; file package.d

;; file pathname.d

(def-inline null :always (t) :bool "#0==ECL_NIL")

(def-inline symbolp :always (t) :bool "@0;ECL_SYMBOLP(#0)")

(def-inline atom :always (t) :bool "@0;ECL_ATOM(#0)")

(def-inline consp :always (t) :bool "@0;ECL_CONSP(#0)")

(def-inline listp :always (t) :bool "@0;ECL_LISTP(#0)")

(def-inline numberp :always (t) :bool "ecl_numberp(#0)")

(def-inline integerp :always (t) :bool "@0;ECL_FIXNUMP(#0)||ECL_BIGNUMP(#0)")

(def-inline floatp :always (t) :bool "floatp(#0)")

(def-inline characterp :always (t) :bool "ECL_CHARACTERP(#0)")

(def-inline base-char-p :always (character) :bool "ECL_BASE_CHAR_P(#0)")

(def-inline stringp :always (t) :bool "@0;ECL_STRINGP(#0)")

(def-inline base-string-p :always (t) :bool "@0;ECL_BASE_STRINGP(#0)")

(def-inline bit-vector-p :always (t) :bool "@0;ECL_BIT_VECTOR_P(#0)")

(def-inline vectorp :always (t) :bool "@0;ECL_VECTORP(#0)")

(def-inline arrayp :always (t) :bool "@0;ECL_ARRAYP(#0)")

(def-inline eq :always (t t) :bool "(#0)==(#1)")
(def-inline eq :always (fixnum fixnum) :bool "(#0)==(#1)")

(def-inline eql :always (t t) :bool "ecl_eql(#0,#1)")
(def-inline eql :always (character t) :bool "(ECL_CODE_CHAR(#0)==(#1))")
(def-inline eql :always (t character) :bool "((#0)==ECL_CODE_CHAR(#1))")
(def-inline eql :always (character character) :bool "(#0)==(#1)")
(def-inline eql :always ((not (or complex bignum ratio float)) t) :bool
 "(#0)==(#1)")
(def-inline eql :always (t (not (or complex bignum ratio float))) :bool
 "(#0)==(#1)")
(def-inline eql :always (fixnum fixnum) :bool "(#0)==(#1)")

(def-inline equal :always (t t) :bool "ecl_equal(#0,#1)")
(def-inline equal :always (fixnum fixnum) :bool "(#0)==(#1)")

(def-inline equalp :always (t t) :bool "ecl_equalp(#0,#1)")
(def-inline equalp :always (fixnum fixnum) :bool "(#0)==(#1)")

(def-inline not :always (t) :bool "(#0)==ECL_NIL")

;; file print.d, read.d

(def-inline clear-output :always (stream) NULL "(ecl_clear_output(#0),ECL_NIL)")

(def-inline finish-output :always (stream) NULL "(ecl_finish_output(#0),ECL_NIL)")

(def-inline finish-output :always (stream) NULL "(ecl_force_output(#0),ECL_NIL)")

(def-inline write-char :always (t) t "@0;(ecl_princ_char(ecl_char_code(#0),ECL_NIL),(#0))")

(def-inline clear-input :always (stream) NULL "(ecl_clear_input(#0),ECL_NIL)")

(def-inline copy-readtable :always (null null) t "standard_readtable")

(def-inline boundp :always (t) :bool "ecl_boundp(cl_env_copy,#0)")
(def-inline boundp :unsafe ((and symbol (not null))) :bool "ECL_SYM_VAL(cl_env_copy,#0)!=OBJNULL")

;; file unixsys.d

;; file sequence.d

(def-inline elt :always (t t) t "ecl_elt(#0,ecl_fixnum(#1))")
(def-inline elt :always (t fixnum) t "ecl_elt(#0,#1)")
(def-inline elt :always (vector t) t "ecl_aref1(#0,ecl_fixnum(#1))")
(def-inline elt :always (vector fixnum) t "ecl_aref1(#0,#1)")

(def-inline elt :unsafe (t t) t "ecl_elt(#0,ecl_fixnum(#1))")
(def-inline elt :unsafe (t fixnum) t "ecl_elt(#0,#1)")
(def-inline elt :unsafe (vector t) t "ecl_aref_unsafe(#0,ecl_fixnum(#1))")
(def-inline elt :unsafe (vector fixnum) t "ecl_aref_unsafe(#0,#1)")
(def-inline aref :unsafe ((array bit) t) :fixnum "ecl_aref_bv(#0,ecl_fixnum(#1))")
(def-inline aref :unsafe ((array bit) fixnum) :fixnum "ecl_aref_bv(#0,#1)")
#+unicode
(def-inline aref :unsafe ((array character) fixnum) :wchar
 "(#0)->string.self[#1]")
(def-inline aref :unsafe ((array base-char) fixnum) :unsigned-char
 "(#0)->base_string.self[#1]")
(def-inline aref :unsafe ((array double-float) fixnum) :double
 "(#0)->array.self.df[#1]")
(def-inline aref :unsafe ((array single-float) fixnum) :float
 "(#0)->array.self.sf[#1]")
(def-inline aref :unsafe ((array fixnum) fixnum) :fixnum
 "(#0)->array.self.fix[#1]")

(def-inline si:elt-set :always (t t t) t "ecl_elt_set(#0,ecl_to_size(#1),#2)")
(def-inline si:elt-set :always (t fixnum t) t "ecl_elt_set(#0,#1,#2)")
(def-inline si:elt-set :always (vector t t) t "ecl_aset1(#0,ecl_to_size(#1),#2)")
(def-inline si:elt-set :always (vector fixnum t) t "ecl_aset1(#0,#1,#2)")

(def-inline si:elt-set :unsafe (t t t) t "ecl_elt_set(#0,ecl_fixnum(#1),#2)")
(def-inline si:elt-set :unsafe (vector t t) t "ecl_aset_unsafe(#0,ecl_to_size(#1),#2)")
(def-inline si:elt-set :unsafe (vector fixnum t) t "ecl_aset_unsafe(#0,#1,#2)")

(def-inline length :always (t) :fixnum "ecl_length(#0)")
(def-inline length :unsafe (vector) :fixnum "(#0)->vector.fillp")

(def-inline copy-seq :always (t) t "ecl_copy_seq(#0)")

;; file character.d

(def-inline char :always (t fixnum) t "ecl_aref1(#0,#1)")
(def-inline char :always (t fixnum) :wchar "ecl_char(#0,#1)")
#-unicode
(def-inline char :unsafe (t t) t "ECL_CODE_CHAR((#0)->base_string.self[ecl_fixnum(#1)])")
#-unicode
(def-inline char :unsafe (t fixnum) :unsigned-char "(#0)->base_string.self[#1]")
(def-inline char :unsafe (base-string fixnum) :unsigned-char "(#0)->base_string.self[#1]")
#+unicode
(def-inline char :unsafe (ext:extended-string fixnum) :wchar "(#0)->string.self[#1]")

(def-inline si:char-set :always (t t t) t "si_char_set(#0,#1,#2)")
(def-inline si:char-set :always (t fixnum t) t "ecl_aset1(#0,#1,#2)")
(def-inline si:char-set :always (t fixnum character) :wchar "ecl_char_set(#0,#1,#2)")
#-unicode
(def-inline si:char-set :unsafe (t t t) t
 "@2;((#0)->base_string.self[ecl_fixnum(#1)]=ecl_char_code(#2),(#2))")
#-unicode
(def-inline si:char-set :unsafe (t fixnum character) :unsigned-char
 "(#0)->base_string.self[#1]= #2")
(def-inline si:char-set :unsafe (base-string t t) t
 "@2;((#0)->base_string.self[ecl_fixnum(#1)]=ecl_char_code(#2),(#2))")
(def-inline si:char-set :unsafe (base-string fixnum base-char) :unsigned-char
 "(#0)->base_string.self[#1]= #2")
(def-inline si:char-set :unsafe (ext:extended-string t t) t
 "@2;((#0)->string.self[ecl_fixnum(#1)]=ecl_char_code(#2),(#2))")
(def-inline si:char-set :unsafe (ext:extended-string fixnum character) :unsigned-char
 "(#0)->string.self[#1]= #2")

(def-inline schar :always (t t) t "ecl_elt(#0,ecl_to_size(#1))")
(def-inline schar :always (t fixnum) t "ecl_elt(#0,#1)")
(def-inline schar :always (t fixnum) :wchar "ecl_char(#0,#1)")
(def-inline schar :unsafe (base-string t) t "ECL_CODE_CHAR((#0)->base_string.self[ecl_fixnum(#1)])")
#-unicode
(def-inline schar :unsafe (t fixnum) :unsigned-char "(#0)->base_string.self[#1]")
(def-inline schar :unsafe (base-string fixnum) :unsigned-char "(#0)->base_string.self[#1]")
#+unicode
(def-inline schar :unsafe (ext:extended-string fixnum) :wchar "(#0)->string.self[#1]")

(def-inline si:schar-set :always (t t t) t "ecl_elt_set(#0,ecl_to_size(#1),#2)")
(def-inline si:schar-set :always (t fixnum t) t "ecl_elt_set(#0,#1,#2)")
(def-inline si:schar-set :always (t fixnum character) :wchar "ecl_char_set(#0,#1,#2)")
#-unicode
(def-inline si:schar-set :unsafe (t t t) t
 "@2;((#0)->base_string.self[ecl_fixnum(#1)]=ecl_char_code(#2),(#2))")
#-unicode
(def-inline si:schar-set :unsafe (t fixnum base-char) :unsigned-char
 "(#0)->base_string.self[#1]= #2")
(def-inline si:schar-set :unsafe (base-string t t) t
 "@2;((#0)->base_string.self[ecl_fixnum(#1)]=ecl_char_code(#2),(#2))")
(def-inline si:schar-set :unsafe (base-string fixnum base-char) :unsigned-char
 "(#0)->base_string.self[#1]= #2")
#+unicode
(def-inline si:schar-set :unsafe (ext:extended-string fixnum t) :wchar
 "@2;((#0)->string.self[#1]= ecl_char_code(#2),(#2))")
#+unicode
(def-inline si:schar-set :unsafe (ext:extended-string fixnum character) :wchar
 "(#0)->string.self[#1]= #2")

(def-inline string= :always (string string) :bool "ecl_string_eq(#0,#1)")

;; file structure.d

(def-inline si:structure-name :always (structure) symbol "ECL_STRUCT_NAME(#0)")

(def-inline si:structure-ref :always (t t fixnum) t "ecl_structure_ref(#0,#1,#2)")

(def-inline si:structure-set :always (t t fixnum t) t
 "ecl_structure_set(#0,#1,#2,#3)")

;; file symbol.d

(def-inline get :always (t t t) t "ecl_get(#0,#1,#2)")
(def-inline get :always (t t) t "ecl_get(#0,#1,ECL_NIL)")

(def-inline symbol-name :always (t) string "ecl_symbol_name(#0)")

;; Additions used by the compiler.
;; The following functions do not exist. They are always expanded into the
;; given C code. References to these functions are generated in the C1 phase.

(def-inline shift>> :always (fixnum fixnum) :fixnum "((#0) >> (- (#1)))")

(def-inline shift<< :always (fixnum fixnum) :fixnum "((#0) << (#1))")

(def-inline si:short-float-p :always (t) :bool "@0;ECL_SINGLE_FLOAT_P(#0)")

(def-inline si:single-float-p :always (t) :bool "@0;ECL_SINGLE_FLOAT_P(#0)")

(def-inline si:double-float-p :always (t) :bool "@0;ECL_DOUBLE_FLOAT_P(#0)")

#-long-float
(def-inline si:long-float-p :always (t) :bool "@0;ECL_DOUBLE_FLOAT_P(#0)")
#+long-float
(def-inline si:long-float-p :always (t) :bool "@0;ECL_LONG_FLOAT_P(#0)")

(def-inline ext:fixnump :always (t) :bool "ECL_FIXNUMP(#0)")
(def-inline ext:fixnump :always (fixnum) :bool "1")

;; Functions only available with threads
#+threads
(def-inline mp:lock-count :unsafe (mp:lock) fixnum "((#0)->lock.count)")

;; Functions only available with CLOS

#+clos
(def-inline si:instance-ref :always (t fixnum) t "ecl_instance_ref((#0),(#1))")
#+clos
(def-inline si:instance-ref :unsafe (standard-object fixnum) t
 "(#0)->instance.slots[#1]")

#+clos
(def-inline si::instance-sig :unsafe (standard-object) list
 "(#0)->instance.sig")

#+clos
(def-inline si:instance-set :unsafe (t fixnum t) t
 "ecl_instance_set((#0),(#1),(#2))")
#+clos
(def-inline si:instance-set :unsafe (standard-object fixnum t) t
 "(#0)->instance.slots[#1]=(#2)")

#+clos
(def-inline si:instance-class :always (standard-object) t "ECL_CLASS_OF(#0)")
#+clos
(def-inline class-of :unsafe (standard-object) t "ECL_CLASS_OF(#0)")

#+clos
(def-inline si::instancep :always (t) :bool "@0;ECL_INSTANCEP(#0)")
#+clos
(def-inline si:unbound :always nil t "ECL_UNBOUND")

#+clos
(def-inline si:sl-boundp :always (t) :bool "(#0)!=ECL_UNBOUND")

#+clos
(def-inline clos:standard-instance-access :always (t fixnum) t "ecl_instance_ref((#0),(#1))")
#+clos
(def-inline clos:standard-instance-access :unsafe (standard-object fixnum) t
 "(#0)->instance.slots[#1]")

#+clos
(def-inline clos:funcallable-standard-instance-access :always (t fixnum) t "ecl_instance_ref((#0),(#1))")
#+clos
(def-inline clos:funcallable-standard-instance-access :unsafe (clos:funcallable-standard-object fixnum) t
 "(#0)->instance.slots[#1]")

))) ; eval-when

(loop for i in '#.(mapcar #'rest +inline-forms+)
   do (apply #'def-inline i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FUNCTIONS WHICH CAN BE CALLED FROM C
;;;
;;; The following two lists contain all functions in the core library which do
;;; not belong to the C part of the library, but which should have an exported C
;;; name that users (and compiled code) can refer to. This means, for instance, that
;;; MAKE-ARRAY will be compiled to a function called cl_make_array, etc.
;;;

(in-package "SI")

(defvar c::*in-all-symbols-functions*
  `(;; arraylib.lsp
    make-array vector array-dimensions array-in-bounds-p array-row-major-index
    bit sbit bit-and bit-ior bit-xor bit-eqv bit-nand bit-nor bit-andc1
    bit-andc2 bit-orc1 bit-orc2 bit-not
    vector-pop adjust-array
    ;; assert.lsp
    si::do-check-type si::ecase-error si::etypecase-error
    si::wrong-type-argument si::ccase-error si::ctypecase-error
    ;; config.lsp
    short-site-name long-site-name machine-type machine-instance machine-version
    software-type software-version lisp-implementation-type lisp-implementation-version
    si::lisp-implementation-vcs-id
    ;; assignment.lsp
    si::setf-definition
    ;; conditions.lsp
    si::safe-eval abort continue muffle-warning store-value use-value
    si::bind-simple-restarts si::bind-simple-handlers
    si::assert-failure compute-restarts find-restart invoke-restart
    invoke-restart-interactively make-condition
    ;; describe.lsp
    describe inspect
    ;; iolib.lsp
    read-from-string write-to-string prin1-to-string princ-to-string
    y-or-n-p yes-or-no-p string-to-object dribble ext:make-encoding
    ext:load-encoding
    ;; listlib.lsp
    union nunion intersection nintersection set-difference nset-difference
    set-exclusive-or nset-exclusive-or subsetp rassoc-if rassoc-if-not
    assoc-if assoc-if-not member-if member-if-not subst-if subst-if-not
    nsubst-if nsubst-if-not
    ;; mislib.lsp
    logical-pathname-translations load-logical-pathname-translations decode-universal-time
    encode-universal-time get-decoded-time
    ensure-directories-exist si::simple-program-error si::signal-simple-error
    ;; module.lsp
    provide require
    ;; numlib.lsp
    isqrt phase signum cis
    asin acos asinh acosh atanh ffloor fceiling ftruncate fround
    logtest byte byte-size byte-position ldb ldb-test mask-field dpb
    deposit-field
    ;; packlib.lsp
    find-all-symbols apropos apropos-list
    find-relative-package package-parent package-children
    ;; predlib.lsp
    upgraded-array-element-type upgraded-complex-part-type typep subtypep coerce
    do-deftype si::ratiop si::single-float-p si::short-float-p si::double-float-p
    si::long-float-p
    ;; seq.lsp
    make-sequence concatenate map some every notany notevery map-into
    complement
    ;; seqlib.lsp
    reduce fill replace
    remove remove-if remove-if-not delete delete-if delete-if-not
    count count-if count-if-not substitute substitute-if substitute-if-not
    nsubstitute nsubstitute-if nsubstitute-if-not find find-if find-if-not
    position position-if position-if-not remove-duplicates
    delete-duplicates mismatch search sort stable-sort merge constantly
    ;; setf.lsp
    si::do-defsetf si::do-define-setf-method
    ;; process.lsp
    ext:system
    ;; pprint.lsp
    pprint-fill copy-pprint-dispatch pprint-dispatch
    pprint-linear pprint-newline pprint-tab pprint-tabular
    set-pprint-dispatch pprint-indent

    #+clos
    ,@'(;; combin.lsp
     method-combination-error
     invalid-method-error
     clos:std-compute-applicable-methods
     clos:std-compute-effective-method
     clos:compute-effective-method-function
     ;; std-slot-value.lsp
     clos::standard-instance-access ;; alias clos:funcallable-standard-instance-access
     clos::standard-instance-set
     subclassp of-class-p
     ;; boot.lsp
     slot-boundp
     slot-makunbound
     slot-value
     clos::slot-value-set
     slot-exists-p
     clos::need-to-make-load-form-p
     ;; defclass
     clos:load-defclass
     ;; method
     clos:extract-lambda-list
     clos:extract-specializer-names
     )

    ;; cdr-5
    ext:array-index-p
    ext:negative-fixnum-p ext:non-negative-fixnum-p
    ext:non-positive-fixnum-p ext:positive-fixnum-p
    ext:negative-integer-p ext:non-negative-integer-p
    ext:non-positive-integer-p ext:positive-integer-p 
    ext:negative-rational-p ext:non-negative-rational-p
    ext:non-positive-rational-p ext:positive-rational-p 
    ext:negative-ratio-p ext:non-negative-ratio-p
    ext:non-positive-ratio-p ext:positive-ratio-p 
    ext:negative-real-p ext:non-negative-real-p
    ext:non-positive-real-p ext:positive-real-p 
    ext:negative-float-p ext:non-negative-float-p
    ext:non-positive-float-p ext:positive-float-p 
    ext:negative-short-float-p ext:non-negative-short-float-p
    ext:non-positive-short-float-p ext:positive-short-float-p 
    ext:negative-single-float-p ext:non-negative-single-float-p
    ext:non-positive-single-float-p ext:positive-single-float-p 
    ext:negative-double-float-p ext:non-negative-double-float-p
    ext:non-positive-double-float-p ext:positive-double-float-p 
    ext:negative-long-float-p ext:non-negative-long-float-p
    ext:non-positive-long-float-p ext:positive-long-float-p 
))

(proclaim
  `(si::c-export-fname #+ecl-min ,@c::*in-all-symbols-functions*
    typecase-error-string find-documentation find-declarations
    si::search-keyword si::check-keyword
    si::dm-too-many-arguments si::dm-too-few-arguments
    remove-documentation si::get-documentation
    si::set-documentation si::expand-set-documentation
    si::packages-iterator
    si::pprint-logical-block-helper si::pprint-pop-helper
    si::make-seq-iterator si::seq-iterator-ref
    si::seq-iterator-set si::seq-iterator-next
    si::sequence-count
    si::structure-type-error si::define-structure
    si::coerce-to-list si::coerce-to-vector
    si::traced-old-definition

    #+formatter
    ,@'(
    format-princ format-prin1 format-print-named-character
    format-print-integer
    format-print-cardinal format-print-ordinal format-print-old-roman
    format-print-roman format-fixed format-exponential
    format-general format-dollars
    format-relative-tab format-absolute-tab
    format-justification
	)
    #+clos
    ,@'(;; defclass.lsp
     clos::ensure-class
     clos::find-slot-definition
     ;; combin.lsp
     clos::simple-code-walker
     ;; kernel.lsp
     clos::install-method
     clos::default-initargs-of
     ;; clos::generic-function-lambda-list
     ;; clos::generic-function-argument-precedence-order
     ;; clos::generic-function-method-combination
     ;; clos::generic-function-method-class
     ;; clos::generic-function-methods
     ;; clos::method-generic-function
     ;; clos::method-lambda-list
     ;; clos::method-specializers
     ;; clos::method-qualifiers
     ;; clos::method-function
     ;; clos::method-plist
     clos::associate-methods-to-gfun
     ;; method.lsp
     clos::pop-next-method
     )))

