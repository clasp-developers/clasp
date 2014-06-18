;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;
;;; Copyright (c) 2010, Alexander Gavrilov (angavrilov@gmail.com)
;;;
;;; This file defines macros for wrapping C-level SSE intrinsics.
;;;

(in-package #:SSE)

;;; The compound SSE pack type

(deftype sse-pack (&optional item)
  (ecase item
    (* 'ext:sse-pack)
    ((single-float float) 'float-sse-pack)
    (double-float 'double-sse-pack)
    (integer 'int-sse-pack)))

;;; Helper macros and functions

(defmacro typename-case (value &body clauses)
  "Syntax: (case value &body clauses)"
  `(cond ,@(mapcar (lambda (clause)
                     `((subtypep ,value ',(first clause))
                       ,@(rest clause)))
                   clauses)
         (t (error "Unsupported type name: ~S" ,value))))

(defun foreign-type-of (lt)
  (typename-case lt
    (nil :object)
    (int-sse-pack :int-sse-pack)
    (float-sse-pack :float-sse-pack)
    (double-sse-pack :double-sse-pack)
    (boolean :bool)
    (single-float :float)
    (double-float :double)
    (fixnum :fixnum)
    #+uint32-t
    (ext:integer32 :int32-t)
    #+uint32-t
    (ext:byte32 :uint32-t)
    #+uint64-t
    (ext:integer64 :int64-t)
    #+uint64-t
    (ext:byte64 :uint64-t)
    (integer :fixnum)))

(defun pointer-c-type-of (lt)
  (typename-case lt
    (nil "void")
    (int-sse-pack "__m128i")
    (float-sse-pack "float")
    (double-sse-pack "double")))

;; Accept any real values for floating-point arguments:
(defun declaim-arg-type-of (lt)
  (typename-case lt
    ((or single-float double-float) 'real)
    (ext:sse-pack 'ext:sse-pack)
    (fixnum 'fixnum)
    (t lt)))

(defun inline-arg-type-of (lt)
  (typename-case lt
    ((or single-float double-float) 'c::fixnum-float)
    (fixnum 'fixnum)
    (t lt)))

;; Constant expansion
(defun expand-constant (form env &optional chgp)
  (let* ((mform (macroexpand form env))
         (cform (cond ((and (symbolp mform) (constantp mform))
                       (symbol-value mform))
                      (t mform))))
    (values cform (or chgp (not (eql cform form))))))

;; Macro helpers
(defun make-arg-name (index)
  (intern (format nil "ARG~A" index)))

(defun make-arg-nums (lst)
  (loop for i from 0 below (length lst) collect i))

(defun wrap-ret-arg (core ret-type &optional ret-arg)
  (cond ((eq ret-type nil)
         (format nil "(~A,Cnil)" core))
        (ret-arg
         (format nil "@~36R;(~A,#~36R)" ret-arg core ret-arg))
        (t core)))

;; Constant generation
(defun make-pack-of-bin (bin-value &key (as 'int-sse-pack))
  (let* ((all (loop for i from 0 to 15
                 for v = bin-value then (ash v -8)
                 collect (logand v 255)))
         (pack (ext:vector-to-sse-pack
                (make-array 16 :element-type '(unsigned-byte 8) :initial-contents all))))
    (if (eq as 'int-sse-pack)
        pack
        `(the ,as ,(ext:sse-pack-as-elt-type
                    pack (ecase as
                           (EXT:FLOAT-SSE-PACK 'single-float)
                           (EXT:DOUBLE-SSE-PACK 'double-float)))))))

(defmacro def-inline (name mode arg-types ret-type call-str &rest flags)
  `(eval-when (:compile-toplevel :load-toplevel)
     (c::def-inline ',name ',mode ',arg-types ',ret-type ,call-str ,@flags)))

(defmacro def-intrinsic (name arg-types ret-type c-name
                         &key (export t) ret-arg reorder-args immediate-args defun-body)
  "Defines and exports an SSE intrinsic function with matching open-coding rules."
  (let* ((anums (make-arg-nums arg-types))
         (asyms (mapcar #'make-arg-name anums))
         (aftypes (mapcar #'foreign-type-of arg-types))
         (rftype (foreign-type-of ret-type))
         (call-anums (if reorder-args (reverse anums) anums))
         (call-str (wrap-ret-arg (format nil "~A(~{#~36R~^,~})" c-name call-anums) ret-type ret-arg)))
    `(progn
       ,(if export `(export ',name))
       ,@(if immediate-args   ; Generate a constantness verifier macro
             `((define-compiler-macro ,name (&whole whole &environment env ,@asyms &aux chgp)
                 ,@(loop for (arg type) in immediate-args
                      collect `(let ((oldv ,arg))
                                 (multiple-value-setq (,arg chgp) (expand-constant oldv env chgp))
                                 (unless (typep ,arg ',type)
                                   (c::cmperr "In call to ~A: Argument ~S~@[ = ~S~] is not a constant of type ~A"
                                              ',name oldv (unless (eql oldv ,arg) ,arg) ',type))))
                 (if chgp (list ',name ,@asyms) whole))))
       (proclaim '(ftype (function ,(mapcar #'declaim-arg-type-of arg-types) ,(or ret-type 'null)) ,name))
       ,@(if (null immediate-args)
             `((defun ,name ,asyms
                 (declare (optimize (speed 0) (debug 0) (safety 1)))
                 (ffi:c-inline ,asyms ,aftypes ,rftype ,(or defun-body call-str) :one-liner t))))
       (def-inline ,name :always ,(mapcar #'inline-arg-type-of arg-types) ,rftype
                   ,call-str :inline-or-warn t))))

(defmacro def-unary-intrinsic (name ret-type insn cost c-name
                               &key (arg-type ret-type) partial result-size immediate-arg)
  (declare (ignore insn cost partial result-size))
  `(def-intrinsic ,name (,arg-type ,@(if immediate-arg (list immediate-arg)))
     ,ret-type ,c-name :immediate-args ,(if immediate-arg `((arg1 ,immediate-arg)))))

(defmacro def-cvt-to-int32-intrinsic (name ret-type insn cost c-name
                                      &key (arg-type ret-type) partial immediate-arg)
  (declare (ignore insn cost partial))
  (assert (subtypep ret-type '(signed-byte 32)))
  `(def-intrinsic ,name (,arg-type ,@(if immediate-arg (list immediate-arg)))
     ,ret-type ,c-name :immediate-args ,(if immediate-arg `((arg1 ,immediate-arg)))))

(defmacro def-binary-intrinsic (name ret-type insn cost c-name
                                &key (x-type ret-type) (y-type ret-type)
                                commutative tags immediate-arg)
  (declare (ignore insn cost commutative tags))
  `(def-intrinsic ,name (,x-type ,y-type ,@(if immediate-arg (list immediate-arg)))
     ,ret-type ,c-name :immediate-args ,(if immediate-arg `((arg2 ,immediate-arg)))))

(defmacro def-sse-int-intrinsic (name int-type ret-type insn cost c-name
                                 &key (arg-type ret-type) immediate-arg make-temporary defun-body)
  (declare (ignore insn cost make-temporary))
  `(def-intrinsic ,name (,arg-type ,int-type ,@(if immediate-arg (list immediate-arg)))
     ,ret-type ,c-name :immediate-args ,(if immediate-arg `((arg2 ,immediate-arg)))
     :defun-body ,defun-body))

(defmacro def-comparison-intrinsic (name arg-type insn cost c-name &key commutative tags)
  (declare (ignore insn cost commutative tags))
  `(def-intrinsic ,name (,arg-type ,arg-type) boolean ,c-name))

(defmacro %def-aref-intrinsic (tag val-type c-type reader writer &key (aux-args "") (bsize 16))
  "Defines and exports macros and functios that implement vectorized array access."
  (let* ((rftype (foreign-type-of val-type))
         (aref-name (intern (format nil "AREF-~A" tag) *package*))
         (rm-aref-name (intern (format nil "ROW-MAJOR-AREF-~A" tag) *package*))
         (rm-aset-name (intern (format nil "ROW-MAJOR-ASET-~A" tag) *package*))
         (known-elt-types '((single-float "sf")
                            (double-float "df")
                            (ext:byte8 "b8")
                            (ext:integer8 "i8")
                            #+uint16-t (ext:byte16 "b16")
                            #+uint16-t (ext:integer16 "i16")
                            #+uint32-t (ext:byte32 "b32")
                            #+uint32-t (ext:integer32 "i32")
                            #+uint64-t (ext:byte64 "b64")
                            #+uint64-t (ext:integer64 "i64"))))
    (flet ((fmtr (ptr-fmt &rest ptr-args)
             (wrap-ret-arg (format nil "~A((~A*)~?~A)"
                                   reader c-type ptr-fmt ptr-args aux-args)
                           val-type))
           (fmtw (ptr-fmt &rest ptr-args)
             (wrap-ret-arg (format nil "~A((~A*)~?,#2)"
                                   writer c-type ptr-fmt ptr-args)
                           val-type 2)))
      `(progn
         (export ',aref-name)
         (export ',rm-aref-name)
         (defmacro ,aref-name (array &rest indexes)
           (let ((varr (gensym "ARR")))
             `(let ((,varr ,array))
                (declare (:read-only ,varr))
                (,',rm-aref-name ,varr (array-row-major-index ,varr ,@indexes)))))
         (proclaim '(ftype (function (array fixnum) ,(or val-type 'null)) ,rm-aref-name))
         (defun ,rm-aref-name (array index)
           (declare (optimize (speed 0) (debug 0) (safety 2)))
           (ffi:c-inline (array index) (:object :int) ,rftype
             ,(fmtr "ecl_row_major_ptr(#0,#1,~A)" bsize)
             :one-liner t))
         ;; AREF
         (def-inline ,rm-aref-name :always (t t) ,rftype
                     ,(fmtr "ecl_row_major_ptr(#0,fixint(#1),~A)" bsize)
                     :inline-or-warn t)
         (def-inline ,rm-aref-name :always (t fixnum) ,rftype
                     ,(fmtr "ecl_row_major_ptr(#0,#1,~A)" bsize))
         ;; AREF unsafe
         ,@(mapcar (lambda (spec)
                     `(def-inline ,rm-aref-name :unsafe ((array ,(first spec)) fixnum) ,rftype
                                  ,(fmtr "(&(#0)->array.self.~A[#1])" (second spec))))
                   known-elt-types)
         ,@(if writer
               `((define-setf-expander ,aref-name (array &rest indexes)
                   (let ((varr (gensym)) (vidx (gensym)) (vval (gensym)))
                     (values (list varr vidx)
                             (list array `(array-row-major-index ,varr ,@indexes))
                             (list vval)
                             `(,',rm-aset-name ,varr ,vidx ,vval) `(,',rm-aref-name ,varr ,vidx))))
                 (proclaim '(ftype (function (array fixnum ,(declaim-arg-type-of val-type)) ,val-type) ,rm-aset-name))
                 (defun ,rm-aset-name (array index value)
                   (declare (optimize (speed 0) (debug 0) (safety 2)))
                   (prog1 value
                     (ffi:c-inline (array index value) (:object :int ,rftype) :void
                       ,(fmtw "ecl_row_major_ptr(#0,#1,~A)" bsize)
                       :one-liner t)))
                 (defsetf ,rm-aref-name ,rm-aset-name)
                 ;; ASET
                 (def-inline ,rm-aset-name :always (t t ,val-type) ,rftype
                             ,(fmtw "ecl_row_major_ptr(#0,fixint(#1),~A)" bsize)
                             :inline-or-warn t)
                 (def-inline ,rm-aset-name :always (t fixnum ,val-type) ,rftype
                             ,(fmtw "ecl_row_major_ptr(#0,#1,~A)" bsize))
                 ;; ASET unsafe
                 ,@(mapcar (lambda (spec)
                             `(def-inline ,rm-aset-name :unsafe ((array ,(first spec)) fixnum ,val-type) ,rftype
                                          ,(fmtw "(&(#0)->array.self.~A[#1])" (second spec))))
                           known-elt-types)))))))

(defmacro def-aref-intrinsic (tag val-type reader-fun writer-fun &key (ref-size 16))
  `(%def-aref-intrinsic ,tag ,val-type ,(pointer-c-type-of val-type)
                        ,(get reader-fun 'c-function-name) ,(get writer-fun 'c-function-name)
                        :bsize ,ref-size
                        :aux-args ,(get reader-fun 'c-call-aux-args)))

(defmacro def-mem-intrinsic (name c-type ret-type c-name &key (public t)
                             prefix-args (prefix-fmt "~@{#~36R,~}")
                             postfix-args (postfix-fmt "~@{,#~36R~}" pf-p) ret-arg)
  "Defines and exports an SSE memory access intrinsic function with matching open-coding rules."
  (let* ((anums (make-arg-nums (append prefix-args postfix-args)))
         (asyms (mapcar #'make-arg-name anums))
         (prefix-nums (subseq anums 0 (length prefix-args)))
         (postfix-nums (mapcar #'1+ (subseq anums (length prefix-args))))
         (prefix-syms (subseq asyms 0 (length prefix-args)))
         (postfix-syms (subseq asyms (length prefix-args)))
         (prefix-itypes (mapcar #'inline-arg-type-of prefix-args))
         (postfix-itypes (mapcar #'inline-arg-type-of postfix-args))
         (rftype (foreign-type-of ret-type))
         (ptr-idx (length prefix-args))
         (offset-idx (+ ptr-idx 1 (length postfix-args))))
    (flet ((fmt (ptr-text)
             (wrap-ret-arg (format nil "~A(~?(~A*)~?~?)"
                                   c-name prefix-fmt prefix-nums
                                   c-type ptr-text (list ptr-idx offset-idx)
                                   postfix-fmt postfix-nums)
                           ret-type ret-arg)))
      `(progn
         ,(when public `(export ',name))
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (get ',name 'c-function-name) ,c-name)
           ,(if (and pf-p (null postfix-args))
                `(setf (get ',name 'c-call-aux-args) ,postfix-fmt)))
         (proclaim '(ftype (function (,@(mapcar #'declaim-arg-type-of prefix-args) si:foreign-data
                                      ,@(mapcar #'declaim-arg-type-of postfix-args) &optional fixnum) ,ret-type) ,name))
         (defun ,name (,@prefix-syms ptr ,@postfix-syms &optional (offset 0))
           (declare (optimize (speed 0) (debug 0) (safety 1)))
           (ffi:c-inline (,@prefix-syms ptr ,@postfix-syms offset)
                         (,@(mapcar #'foreign-type-of prefix-args) :pointer-void
                          ,@(mapcar #'foreign-type-of postfix-args) :int) ,rftype
             ,(fmt "(((char*)#~A) + #~A)")
             :one-liner t))
         (def-inline ,name :always (,@prefix-itypes t ,@postfix-itypes) ,rftype
                     ,(fmt "ecl_to_pointer(#~A)")
                     :inline-or-warn t)
         (def-inline ,name :always (,@prefix-itypes t ,@postfix-itypes t) ,rftype
                     ,(fmt "(((char*)ecl_to_pointer(#~A)) + fixint(#~A))"))
         (def-inline ,name :always (,@prefix-itypes t ,@postfix-itypes fixnum) ,rftype
                     ,(fmt "(((char*)ecl_to_pointer(#~A)) + #~A)"))
         (def-inline ,name :unsafe (,@prefix-itypes si:foreign-data ,@postfix-itypes) ,rftype
                     ,(fmt "(#~A)->foreign.data"))
         (def-inline ,name :unsafe (,@prefix-itypes si:foreign-data ,@postfix-itypes t) ,rftype
                     ,(fmt "(((char*)(#~A)->foreign.data) + fix(#~A))"))
         (def-inline ,name :unsafe (,@prefix-itypes si:foreign-data ,@postfix-itypes fixnum) ,rftype
                     ,(fmt "(((char*)(#~A)->foreign.data) + #~A)"))))))

(defmacro def-load-intrinsic (name ret-type insn c-name &key register-arg tags size postfix-fmt)
  (declare (ignore insn tags size))
  `(def-mem-intrinsic ,name ,(pointer-c-type-of ret-type) ,ret-type ,c-name
                      :prefix-args ,(if register-arg (list ret-type))
                      :postfix-fmt ,(or postfix-fmt "")))

(defmacro def-store-intrinsic (name ret-type insn c-name &key setf-name)
  (declare (ignore insn))
  `(progn
     (def-mem-intrinsic ,name ,(pointer-c-type-of ret-type) ,ret-type ,c-name
                        :public ,(not setf-name) :postfix-args (,ret-type) :ret-arg 1)
     ,(if setf-name
          `(defsetf ,setf-name (pointer &optional (offset 0)) (value)
             `(,',name ,pointer ,value ,offset)))))

