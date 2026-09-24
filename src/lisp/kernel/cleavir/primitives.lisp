(in-package #:cmp)

;;; see declare-function-in-module (cmpir.lisp) for interpretation of function
;;; attributes and such.

(defstruct primitive
  return-type-name
  argument-type-names
  return-attributes
  argument-attributes
  properties)

(defun primitive-varargs (prim)
  (let ((props (primitive-properties prim)))
    (getf props :varargs)))

(defun define-primitive-info (name return-ty-attributes passed-args-ty &rest function-attrs &key &allow-other-keys)
  (declare (ignore name))
  (let (reversed-argument-types
        return-attributes
        argument-attributes
        (index 0))
    (when (consp return-ty-attributes)
      (setf return-attributes (cdr return-ty-attributes)))
    (dolist (arg passed-args-ty)
      (if (consp arg)
          (let ((arg-ty (car arg))
                (attribute-list (cdr arg)))
            (push arg-ty reversed-argument-types)
            (push (cons index attribute-list) argument-attributes))
          (progn
            (push arg reversed-argument-types)))
      (incf index))
    (let* ((return-ty1 (if (consp return-ty-attributes) (car return-ty-attributes) return-ty-attributes))
           (argument-types (nreverse reversed-argument-types)))
      (make-primitive
       :return-type-name return-ty1
       :argument-type-names argument-types
       :return-attributes return-attributes
       :argument-attributes argument-attributes
       :properties function-attrs))))

(defvar *primitives* (make-hash-table :test 'equal :thread-safe t))

(defun define-primitive (name return-ty-attr args-ty-attr &rest function-attrs &key &allow-other-keys)
  (let ((info (apply #'define-primitive-info name return-ty-attr args-ty-attr function-attrs)))
    (setf (gethash name *primitives*) info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  WARNING WARNING WARNING WARNING
;;;
;;; Don't thoughtlessly change a primitive-unwinds intrinsic to a primitive intrinsic.
;;; + primitive means that the intrinsic doesn't ever throw an exception
;;;     and nothing that it calls throws an exception
;;; + primitive-unwinds means that the intrinsic can throw an exception and should be called with INVOKE
;;;
(defun primitive-unwinds (name return-ty args-ty &rest function-attrs &key &allow-other-keys)
  "Define primitives that can unwind the stack, either directly or through transitive calls"
  (apply #'define-primitive name return-ty args-ty :does-not-throw nil function-attrs))

(defun primitive         (name return-ty args-ty &rest function-attrs &key &allow-other-keys)
  "Define primitives that do NOT unwind the stack directly or through transitive calls"
  (apply #'define-primitive name return-ty args-ty :does-not-throw t function-attrs))

(defun general-entry-point-redirect-name (arity)
  "Return the name of the wrong-number-of-arguments function for the arity"
  (core:fmt nil "general_entry_point_redirect_{}" arity))

(defmacro primitives-macro ()
  `(progn
     ,@'((primitive         "cc_protect_alloca" :void (list :i8*))

         (primitive-unwinds "cc_error_type_error" :void (list :t* :t*) :does-not-return t)
         (primitive-unwinds "cc_error_array_out_of_bounds" :void (list :t* :t* :t*) :does-not-return t)
         (primitive-unwinds "cc_etypecase_error" :void (list :t* :t*) :does-not-return t)
         
         (primitive-unwinds "gdb" :void nil)
         (primitive         "debugInspectTPtr" :void (list :t*))
         (primitive         "debugInspectT_mv" :void (list :tmv))
         (primitive         "debugInspect_return_type" :void (list :return-type))

         (primitive         "debugPointer" :void (list :i8*))
         (primitive         "debug_memory" :void (list :size_t :i8*))
         (primitive         "debug_vaslistPtr" :void (list :vaslist*))
         (primitive         "debugMessage" :void (list :i8*))
         (primitive         "debugBreak" :void ())
         (primitive         "debugPrintI32" :void (list :i32))
         (primitive         "debugPrint_blockFrame" :void (list :t*))
         (primitive         "debugPrint_blockHandleReturnFrom" :void (list :i8* :i8*))
         (primitive         "debugPrint_size_t" :void (list :size_t))
         (primitive         "debug_match_two_uintptr_t" :uintptr_t (list :uintptr_t :uintptr_t))
         (primitive         "lowLevelTrace" :void (list :i32))
         (primitive         "unreachableError" :void nil)
         (primitive-unwinds "cc_breakstep" :void (list :t* :i8*))
         (primitive         "cc_breakstep_after" :void (list :t*))
         (primitive-unwinds "cc_wrong_number_of_arguments" :void (list :t* :size_t :size_t :size_t)
          :does-not-return t)
         (primitive         "cc_list" (list :t* 'llvm-sys:attribute-no-alias)
          (list :size_t) :varargs t)
         (primitive         "cc_gatherRestArguments"
          (list :t* 'llvm-sys:attribute-no-alias)
          (list (list :vaslist* 'llvm-sys:attribute-read-only) :size_t))
         (primitive         "cc_gatherDynamicExtentRestArguments" :t*
          (list (list :vaslist* 'llvm-sys:attribute-read-only) :size_t
           (list :t** 'llvm-sys:attribute-write-only)))
         (primitive         "cc_gatherVaRestArguments" :t*
          (list (list :vaslist* 'llvm-sys:attribute-read-only) :size_t
           (list :vaslist* 'llvm-sys:attribute-write-only)))
         (primitive-unwinds "cc_ifBadKeywordArgumentException" :void (list :t* :t* :t*))
         (primitive-unwinds "cc_error_bugged_come_from" :void (list :size_t) :does-not-return t)
    
         (primitive         "__gxx_personality_v0" :i32 nil :varargs t)
         (primitive         "__cxa_begin_catch" :i8* (list :i8*) )
         (primitive-unwinds "__cxa_end_catch" :void nil)
         (primitive-unwinds "__cxa_rethrow" :void nil :does-not-return t)
         (primitive         +intrinsic/llvm.eh.typeid.for.p0+ :i32 (list :i8*))
         (primitive         "llvm.sadd.with.overflow.i32" :{i32.i1} (list :i32 :i32))
         (primitive         "llvm.sadd.with.overflow.i64" :{i64.i1} (list :i64 :i64))
         (primitive         "llvm.ssub.with.overflow.i32" :{i32.i1} (list :i32 :i32))
         (primitive         "llvm.ssub.with.overflow.i64" :{i64.i1} (list :i64 :i64))
         (primitive         "llvm.ctpop.i64" :i64 (list :i64))
         (primitive         "llvm.fshl.i32" :i32 (list :i32 :i32 :i32))
         (primitive         "llvm.fshr.i32" :i32 (list :i32 :i32 :i32))
         ;; NOTE: FP primitives may signal a floating point exception but this
         ;; is not the same as raising an exception. I think. FIXME: Check.
         (primitive         "llvm.cos.f32" :single-float (list :single-float))
         (primitive         "llvm.sin.f32" :single-float (list :single-float))
         (primitive         "llvm.fabs.f32" :single-float (list :single-float))
         (primitive         "llvm.sqrt.f32" :single-float (list :single-float))
         (primitive         "llvm.exp.f32" :single-float (list :single-float))
         (primitive         "llvm.log.f32" :single-float (list :single-float))
         (primitive         "llvm.floor.f32" :single-float (list :single-float))
         (primitive         "llvm.ceil.f32" :single-float (list :single-float))
         (primitive         "llvm.trunc.f32" :single-float (list :single-float))
         (primitive         "llvm.roundeven.f32" :single-float (list :single-float))
         (primitive         "llvm.pow.f32" :single-float (list :single-float :single-float))
         (primitive         "tanf" :single-float (list :single-float))
         (primitive         "asinf" :single-float (list :single-float))
         (primitive         "acosf" :single-float (list :single-float))
         ;; atan's two argument form might be tricky FIXME
         (primitive         "sinhf" :single-float (list :single-float))
         (primitive         "coshf" :single-float (list :single-float))
         (primitive         "tanhf" :single-float (list :single-float))
         (primitive         "asinhf" :single-float (list :single-float))
         (primitive         "acoshf" :single-float (list :single-float))
         (primitive         "atanhf" :single-float (list :single-float))

         (primitive         "llvm.cos.f64" :double-float (list :double-float))
         (primitive         "llvm.sin.f64" :double-float (list :double-float))
         (primitive         "llvm.fabs.f64" :double-float (list :double-float))
         (primitive         "llvm.sqrt.f64" :double-float (list :double-float))
         (primitive         "llvm.exp.f64" :double-float (list :double-float))
         (primitive         "llvm.log.f64" :double-float (list :double-float))
         (primitive         "llvm.floor.f64" :double-float (list :double-float))
         (primitive         "llvm.trunc.f64" :double-float (list :double-float))
         (primitive         "llvm.ceil.f64" :double-float (list :double-float))
         (primitive         "llvm.roundeven.f64" :double-float (list :double-float))
         (primitive         "llvm.pow.f64" :double-float (list :double-float :double-float))
         (primitive         "tan" :double-float (list :double-float))
         (primitive         "asin" :double-float (list :double-float))
         (primitive         "acos" :double-float (list :double-float))
         ;; atan here but tricky FIXME
         (primitive         "sinh" :double-float (list :double-float))
         (primitive         "cosh" :double-float (list :double-float))
         (primitive         "tanh" :double-float (list :double-float))
         (primitive         "asinh" :double-float (list :double-float))
         (primitive         "acosh" :double-float (list :double-float))
         (primitive         "atanh" :double-float (list :double-float))

         (primitive         "llvm.umin.i64" :i64 (list :i64 :i64))
         (primitive         "llvm.umax.i64" :i64 (list :i64 :i64))

         (primitive         "drag_native_calls" :void nil)

         (primitive         "llvm.experimental.stackmap" :void (list :i64 :i32) :varargs t)
         (primitive         "llvm.va_copy" :void (list :i8* :i8*))
         (primitive         "llvm.va_start" :void (list :i8*))
         (primitive         "llvm.va_end" :void (list :i8*))

         (primitive         "llvm.dbg.addr" :void (list :metadata :metadata :metadata))
         (primitive         "llvm.dbg.declare" :void (list :metadata :metadata :metadata))
         (primitive         "llvm.dbg.value" :void (list :metadata :metadata :metadata))

         (primitive         "llvm.threadlocal.address.p0" :i8* (list :i8*))

         (primitive         "llvm.lifetime.start" :void (list :i64 :i8*))
         (primitive         "llvm.lifetime.end" :void (list :i64 :i8*))

         (primitive         +intrinsic/llvm.stacksave.p0+ :i8* nil)
         (primitive         +intrinsic/llvm.stackrestore.p0+ :void (list :i8*))

         (primitive         "llvm.ptrmask.p0.i64" :t* (list :t* :i64))

         (primitive         "llvm.memcpy.p0.p0.i64" :void (list :i8* :i8* :i64 :i1))
         (primitive         "llvm.memmove.p0.p0.i64" :void (list :i8* :i8* :i64 :i1))
    
         ;; Primitives for Cleavir code

         (primitive         "cm_vref" :return-type (list (list :t* 'llvm-sys:attribute-read-only) :t*))
         (primitive         "cm_vset" :return-type (list :t* :t* :t*))
         (primitive         "cc_ensure_valid_object" :t* (list :t*))
         (primitive         "cc_getPointer" :i8* (list (list :t* 'llvm-sys:attribute-read-only)))
         (primitive-unwinds "cc_checkBound" :size_t (list (list :t* 'llvm-sys:attribute-read-only) :size_t :t*))

         (primitive "cc_verify_tag" :void (list :size_t :t* :size_t))

         (primitive-unwinds "cc_variableCellValue" :t* (list (list :t* 'llvm-sys:attribute-read-only)))
         (primitive         "cc_set_variableCellValue" :void (list :t* :t*))
         (primitive         "cc_getCellTLIndex" :i32 (list :t*))
         (primitive         "cc_specialBind" :t* (list :i32 (list :t* 'llvm-sys:attribute-read-only)))
         (primitive         "cc_specialUnbind" :void (list :i32 (list :t* 'llvm-sys:attribute-read-only)))

         (primitive-unwinds "cc_oddKeywordException" :void (list :t*))
         ;; Marking setjmp as returns_twice is EXTREMELY IMPORTANT.
         ;; Without this attribute, LLVM will apply invalid optimizations.
         ;; For example, it will reuse stack space allocated before a setjmp
         ;; call after the setjmp call, meaning that when a longjmp occurs,
         ;; variables it expects to be there are replaced with whatever other
         ;; value. This can cause very difficult to debug problems!
         ;; - Bike, who's spent a solid two days staring at IR incomprehendingly
         (primitive         "_setjmp" :i32 (list :jmp-buf-tag*) :returns-twice t)
         (primitive-unwinds "_longjmp" :void (list :jmp-buf-tag* :i32) :does-not-return t)
         (primitive-unwinds "cc_throw" :void (list :t*) :does-not-return t)
         (primitive         "cc_catch_tag" :t* (list :t*))
         (primitive         "cc_initialize_block_dynenv"
          :void (list :i8* (list :i8* 'llvm-sys:attribute-read-none)
                 (list :jmp-buf-tag* 'llvm-sys:attribute-read-none))
          :will-return t :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_tagbody_dynenv"
          :void (list :i8* (list :i8* 'llvm-sys:attribute-read-none)
                 (list :jmp-buf-tag* 'llvm-sys:attribute-read-none))
          :will-return t :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_cleanup_dynenv"
          :void (list (list :i8* '(:captures ()))
                 (list :jmp-buf-tag* 'llvm-sys:attribute-read-none))
          :will-return t :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_binding_dynenv"
          :void (list (list :i8* '(:captures ()) 'llvm-sys:attribute-write-only)
                 (list :t* 'llvm-sys:attribute-read-only)
                 (list :t* 'llvm-sys:attribute-read-none))
          :will-return t :memory '(:none (:arg :readwrite)))
         (primitive         "cc_initialize_progv_dynenv"
          :void (list (list :i8* '(:captures ()) 'llvm-sys:attribute-write-only)
                 (list :t* 'llvm-sys:attribute-read-only)
                 (list :t* 'llvm-sys:attribute-read-only))
          :will-return t :memory '(:none (:arg :readwrite)))
         (primitive         "cc_initialize_catch_dynenv"
          :void (list (list :i8* '(:captures ()))
                 (list :jmp-buf-tag* 'llvm-sys:attribute-read-none)
                 (list :t* 'llvm-sys:attribute-read-none))
          :will-return t :memory '(:none (:arg :write)))
         (primitive-unwinds "cc_sjlj_unwind" :void (list :t* :size_t) :does-not-return t)
         ;; This function is called in compiled code VERY frequently
         ;; so we want LLVM to optimize as much as possible.
         ;; In particular, we want it to keep the result in a register
         ;; if there are enough, while being willing to "spill" by
         ;; calling the function again.
         ;; memory(none) is sort of untrue - we access thread-local memory.
         ;; But nothing in it should change after initialization so there should
         ;; be no problem reordering calls around memory accesses.
         ;; It might be a problem if it was used in a coroutine that was resumed
         ;; from another thread, but we don't use coroutines.
         (primitive         "cc_my_thread"
          (list :thread-local-state* '(:align 8)
           ;; KLUDGE: ideally this would be the actual size of a ThreadLocalState
           ;; but we don't actually have that atm (and getting type sizes out of LLVM
           ;;  is pretty confusing because it can differ by target)
           ;; This is everything in thread-local-state, with 8192 multiple values.
           '(:dereferenceable 65586))
          (list) :speculatable t :will-return t :memory :none)
         ;; While this obviously unwinds, it does so by SJLJ and will
         ;; never throw an exception.
         (primitive         "cc_sjlj_continue_unwinding" :void nil :does-not-return t)
         (primitive-unwinds "cc_progvResolveSymbols" :t* (list :t* :t*))
         (primitive-unwinds "cc_progvSetValues" :t* (list :t* :t*))
         (primitive         "cc_progvUnbind" :void (list :t* :t*))
         (primitive-unwinds "cc_safepoint" :void (list))
         (primitive         "llvm.frameaddress.p0" :i8* (list :i32))
         (primitive-unwinds "cc_landingpadUnwindMatchFrameElseRethrow" :size_t (list :i8* :i8*))

         (primitive         "cc_alloc_normal"
          (list :i8* 'llvm-sys:attribute-no-alias '(:align 8))
          (list :size_t)
          :will-return t
          :allockind 'llvm-sys:alloc-kind-alloc :allocsize 0
          :memory '(:none (:inaccessible :readwrite)))
         (primitive         "cc_post_alloc_normal"
          :void (list :i8* :size_t)
          :will-return t :memory '(:none (:arg :write)))
         (primitive         "cc_alloc_immobile"
          (list :i8* 'llvm-sys:attribute-no-alias '(:align 8))
          (list :size_t)
          :will-return t
          :allockind 'llvm-sys:alloc-kind-alloc :allocsize 0
          :memory '(:none (:inaccessible :readwrite)))
         (primitive         "cc_post_alloc_immobile"
          :void (list :i8* :size_t)
          :will-return t :memory '(:none (:arg :write)))

         (primitive         "cc_initialize_cons"
          :void (list (list :i8* '(:captures ())))
          :will-return t
          :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_double"
          :void (list (list :i8* '(:captures ())) :double-float)
          :will-return t
          :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_bignum"
          :void (list (list :i8* '(:captures ())) :i64)
          :will-return t
          :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_closure"
          :void (list (list :i8* '(:captures ())) :t* :size_t)
          :will-return t
          :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_simple_vector_t"
          :void (list (list :i8* '(:captures ())) :size_t)
          :will-return t
          :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_simple_vector_single_float"
          :void (list (list :i8* '(:captures ())) :size_t)
          :will-return t
          :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_simple_vector_double_float"
          :void (list (list :i8* '(:captures ())) :size_t)
          :will-return t
          :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_simple_vector_base_char"
          :void (list (list :i8* '(:captures ())) :size_t)
          :will-return t
          :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_simple_vector_character"
          :void (list (list :i8* '(:captures ())) :size_t)
          :will-return t
          :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_simple_vector_bit"
          :void (list (list :i8* '(:captures ())) :size_t)
          :will-return t
          :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_simple_vector_byte2"
          :void (list (list :i8* '(:captures ())) :size_t)
          :will-return t
          :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_simple_vector_int2"
          :void (list (list :i8* '(:captures ())) :size_t)
          :will-return t
          :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_simple_vector_byte4"
          :void (list (list :i8* '(:captures ())) :size_t)
          :will-return t
          :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_simple_vector_int4"
          :void (list (list :i8* '(:captures ())) :size_t)
          :will-return t
          :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_simple_vector_byte8"
          :void (list (list :i8* '(:captures ())) :size_t)
          :will-return t
          :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_simple_vector_int8"
          :void (list (list :i8* '(:captures ())) :size_t)
          :will-return t
          :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_simple_vector_byte16"
          :void (list (list :i8* '(:captures ())) :size_t)
          :will-return t
          :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_simple_vector_integer16"
          :void (list (list :i8* '(:captures ())) :size_t)
          :will-return t
          :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_simple_vector_byte32"
          :void (list (list :i8* '(:captures ())) :size_t)
          :will-return t
          :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_simple_vector_integer32"
          :void (list (list :i8* '(:captures ())) :size_t)
          :will-return t
          :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_simple_vector_byte64"
          :void (list (list :i8* '(:captures ())) :size_t)
          :will-return t
          :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_simple_vector_integer64"
          :void (list (list :i8* '(:captures ())) :size_t)
          :will-return t
          :memory '(:none (:arg :write)))
         (primitive         "cc_initialize_simple_vector_fixnum"
          :void (list (list :i8* '(:captures ())) :size_t)
          :will-return t
          :memory '(:none (:arg :write)))

         ;; === CLASP-FFI TRANSLATORS ===

         ;; !!! NOTE !!! => PORTING ISSUE/TODO !
         ;; This implementation assumes the following associations:
         ;;
         ;; C++          -> LLVM         (!)
         ;; --------------------------------
         ;; char         -> i8
         ;; short        -> i16
         ;; int          -> i32
         ;; long         -> i64
         ;; long long    -> i64          (!)
         ;; float        -> float
         ;; doubls       -> double
         ;; long double  -> long float   (!)
         ;; size_t       -> size_t
         ;; ssize_t      -> size_t       (!)
         ;; void *       -> i64*         (!)

         ;; FIXNUM
         (primitive-unwinds "from_object_fixnum" (list :i64 'llvm-sys:attribute-sext) (list :t*))
         (primitive-unwinds "to_object_fixnum" :t* (list (list :i64 'llvm-sys:attribute-sext)))

         ;; SHORT & UNSIGNED SHORT
         (primitive-unwinds "from_object_short" (list :i16 'llvm-sys:attribute-sext) (list :t*))
         (primitive-unwinds "to_object_short" :t* (list (list :i16 'llvm-sys:attribute-sext) ))
         (primitive-unwinds "from_object_unsigned_short" :i16 (list :t*))
         (primitive-unwinds "to_object_unsigned_short" :t* (list :i16))

         ;; INT & UNSIGNED INT
         (primitive-unwinds "from_object_int" (list :i32 'llvm-sys:attribute-sext) (list :t*))
         (primitive-unwinds "to_object_int" :t* (list (list :i32 'llvm-sys:attribute-sext) ))
         (primitive-unwinds "from_object_unsigned_int" :i32 (list :t*))
         (primitive-unwinds "to_object_unsigned_int" :t* (list :i32))

         ;; LONG & UNSIGNED LONG
         (primitive-unwinds "from_object_long" :i64 (list :t*))
         (primitive-unwinds "to_object_long" :t* (list (list :i64 'llvm-sys:attribute-sext) ))
         (primitive-unwinds "from_object_unsigned_long" :i64 (list :t*))
         (primitive-unwinds "to_object_unsigned_long" :t* (list :i64))

         ;; LONG LONG & UNSIGNED LONG LONG
         (primitive-unwinds "from_object_long_long" (list :i64 'llvm-sys:attribute-sext) (list :t*))
         (primitive-unwinds "to_object_long_long" :t* (list (list :i64 'llvm-sys:attribute-sext) ))
         (primitive-unwinds "from_object_unsigned_long_long" :i64 (list :t*))
         (primitive-unwinds "to_object_unsigned_long_long" :t* (list :i64))

         ;; INT8 & UINT8
         (primitive-unwinds "from_object_int8" (list :i8 'llvm-sys:attribute-sext) (list :t*))
         (primitive-unwinds "to_object_int8" :t* (list (list :i8 'llvm-sys:attribute-sext)))
         (primitive-unwinds "from_object_uint8" :i8 (list :t*))
         (primitive-unwinds "to_object_uint8" :t* (list :i8))

         ;; INT16 & UINT16
         (primitive-unwinds "from_object_int16" (list :i16 'llvm-sys:attribute-sext) (list :t*))
         (primitive-unwinds "to_object_int16" :t* (list (list :i16 'llvm-sys:attribute-sext) ))
         (primitive-unwinds "from_object_uint16" :i16 (list :t*))
         (primitive-unwinds "to_object_uint16" :t* (list :i16))

         ;; INT32 & UINT32
         (primitive-unwinds "from_object_int32" (list :i32 'llvm-sys:attribute-sext) (list :t*))
         (primitive-unwinds "to_object_int32" :t* (list (list :i32 'llvm-sys:attribute-sext) ))
         (primitive-unwinds "from_object_uint32" :i32 (list :t*))
         (primitive-unwinds "to_object_uint32" :t* (list :i32))

         ;; INT64 & UINT64
         (primitive-unwinds "from_object_int64" (list :i64 'llvm-sys:attribute-sext) (list :t*))
         (primitive-unwinds "to_object_int64" :t* (list (list :i64 'llvm-sys:attribute-sext) ))
         (primitive-unwinds "from_object_uint64" :i64 (list :t*))
         (primitive-unwinds "to_object_uint64" :t* (list :i64))

         ;; i128 HANDLING NOT IMPLEMENTED AS IT IS NOT USED

         ;; SIZE_T
         (primitive-unwinds "from_object_size" :size_t (list :t*))
         (primitive-unwinds "to_object_size" :t* (list :size_t))

         ;; SSIZE_T
         (primitive-unwinds "from_object_ssize" :size_t (list :t*))
         (primitive-unwinds "to_object_ssize" :t* (list :size_t))

         ;; PTRDIFF_T, TIME_T
         ;; (primitive-unwinds "from_object_ptrdiff" :t* (list :t*)) - FIXME !
         ;; (primitive-unwinds "to_object_ptrdiff" :t* (list :uintptr_t)) - FIXME !

         ;; (primitive-unwinds "from_object_time" :t* (list :t*)) - FIXME !
         ;; (primitive-unwinds "to_object_time" :t* (list :t*)) - FIOXME !

         ;; CHAR & UNSIGNED CHAR
         (primitive-unwinds "from_object_char" (list :i8 'llvm-sys:attribute-sext) (list :t*))
         (primitive-unwinds "to_object_char" :t* (list (list :i8 'llvm-sys:attribute-sext) ))
         (primitive-unwinds "from_object_unsigned_char" :i8 (list :t*))
         (primitive-unwinds "to_object_unsigned_char" :t* (list :i8))

         (primitive-unwinds "from_object_claspCharacter" :i32 (list :t*))
         (primitive-unwinds "to_object_claspCharacter" :t* (list :i32))
         (primitive-unwinds "from_object_claspChar" :i8 (list :t*))
         (primitive-unwinds "to_object_claspChar" :t* (list :i8))
    
         ;; FLOAT, DOUBLE & LONG FLOAT
         (primitive-unwinds "from_object_float" :single-float (list :t*))
         (primitive-unwinds "to_object_float" :t* (list :single-float))
         (primitive-unwinds "from_object_double" :double-float (list :t*))
         (primitive-unwinds "to_object_double" (list :t* 'llvm-sys:attribute-no-alias) (list :double-float))
         #+long-float (primitive "from_object_long_double" :long-float (list :t*))
         #+long-float (primitive "to_object_long_double" :t* (list :long-float))

         ;; POINTER / VOID *

         ;; Note: using :void* causes an error - so we use :i64* instead here!
         (primitive-unwinds "from_object_pointer" :i64* (list :t*))
         (primitive-unwinds "to_object_pointer" :t* (list :i64*))
         (primitive-unwinds "to_object_void" :t* (list))
         ;; === END OF TRANSLATORS ===
         (primitive         "cc_read_derivable_cxx_stamp_untagged_object" :i64 (list :i8*))
         #+(or)(primitive         "cc_read_slot" :t* (list :t* :size_t))
         #+(or)(primitive         "cc_write_slot" :t* (list :t* :size_t :t*))

         (primitive-unwinds (general-entry-point-redirect-name 0) :void (list :t*))
         (primitive-unwinds (general-entry-point-redirect-name 1) :void (list :t* :t*))
         (primitive-unwinds (general-entry-point-redirect-name 2) :void (list :t* :t* :t*))
         (primitive-unwinds (general-entry-point-redirect-name 3) :void (list :t* :t* :t* :t*))
         (primitive-unwinds (general-entry-point-redirect-name 4) :void (list :t* :t* :t* :t* :t*))
         (primitive-unwinds (general-entry-point-redirect-name 5) :void (list :t* :t* :t* :t* :t* :t*))
         (primitive-unwinds (general-entry-point-redirect-name 6) :void (list :t* :t* :t* :t* :t* :t* :t*))
         (primitive-unwinds (general-entry-point-redirect-name 7) :void (list :t* :t* :t* :t* :t* :t* :t* :t*))
         
         )
     ))



(eval-when (:load-toplevel :execute)
  (primitives-macro))



(defun lookup-type (type-name)
  (case type-name
    #+short-float (:short-float %short-float%)
    #+short-float (:binary16 %short-float%)
    (:single-float %float%)
    (:double-float %double%)
    #+long-float (:long-float %long-float%)
    #+long-float (:binary80 %long-float%)
    #+long-float (:binary128 %long-float%)
    (:fn-start-up* %fn-start-up*%)
    (:i1 %i1%)
    (:i16 %i16%)
    (:i32 %i32%)
    (:i32* %i32*%)
    (:i64 %i64%)
    (:i64* %i64*%)
    (:i8 %i8%)
    (:i8* %i8*%)
    (:i8** %i8**%)
    (:jmp-buf-tag* %jmp-buf-tag*%)
    (:ltv** %ltv**%)
    (:thread-local-state* %thread-local-state*%)
    (:metadata %metadata%)
    (:return-type %return-type%)
    (:size_t %size_t%)
    (:object %t*%)
    (:t* %t*%)
    (:t** %t**%)
    (:t*[0]* %t*[0]*%)
    (:tmv %tmv%)
    (:tmv* %tmv*%)
    (:uintptr_t %uintptr_t%)
    (:vaslist* %vaslist*%)
    (:void %void%)
    (:void* %void*%)
    (:{i32.i1} %{i32.i1}%)
    (:{i64.i1} %{i64.i1}%)
    (otherwise (error "Illegal type ~a~%" type-name))
    ))

(defun lookup-type-list (type-names)
  (mapcar #'lookup-type type-names))


(defun primitive-return-type (info)
  (lookup-type (primitive-return-type-name info)))

(defun primitive-argument-types (info)
  (lookup-type-list (primitive-argument-type-names info)))
                    
