(in-package #:cmp)


(defstruct (primitive (:type vector) :named)
  return-type-name
  argument-type-names
  return-attributes
  argument-attributes
  properties)


(defun primitive-varargs (prim)
  (let ((props (primitive-properties prim)))
    (getf props :varargs)))

(defun define-primitive-info (name return-ty-attributes passed-args-ty varargs does-not-throw does-not-return returns-twice ltvc)
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
       :properties (list :varargs varargs
                         :does-not-throw does-not-throw
                         :does-not-return does-not-return
                         :returns-twice returns-twice
                         :ltvc ltvc)))))

(defun define-primitive (name return-ty-attr args-ty-attr &key varargs does-not-throw does-not-return returns-twice ltvc)
  (let ((info (define-primitive-info name return-ty-attr args-ty-attr varargs does-not-throw does-not-return returns-twice ltvc)))
    (core::hash-table-setf-gethash *primitives* name info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  WARNING WARNING WARNING WARNING
;;;
;;; Don't thoughtlessly change a primitive-unwinds intrinsic to a primitive intrinsic.
;;; + primitive means that the intrinsic doesn't ever throw an exception
;;;     and nothing that it calls throws an exception
;;; + primitive-unwinds means that the intrinsic can throw an exception and should be called with INVOKE
;;;
(defun primitive-unwinds (name return-ty args-ty &key varargs does-not-return returns-twice ltvc )
  "Define primitives that can unwind the stack, either directly or through transitive calls"
  (define-primitive name return-ty args-ty :varargs varargs :does-not-throw nil :does-not-return does-not-return :returns-twice returns-twice :ltvc ltvc))

(defun primitive         (name return-ty args-ty &key varargs does-not-return returns-twice ltvc)
  "Define primitives that do NOT unwind the stack directly or through transitive calls"
  (define-primitive name return-ty args-ty :varargs varargs :does-not-throw t :does-not-return does-not-return :returns-twice returns-twice :ltvc ltvc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ltvc intrinsics are for initialization of memory at load time
;;; ltvc stands for Load Time Virtual machine Call
;;; They are used to construct the a byte-code machine
;;; Each function takes arguments ( GCRootsInModule* gcroots, char tag, size_t targetIndex, ...args...)
;;;   The gcroots, tag, targetIndex are used to get a reference to a cell in the gcroots
;;;   and the ...args... are used to construct the object.
;;; If you add a new ltvc_xxxx function then do the following to rebuild the virtual machine
;;; To build the machine in the src/core/byte-code-interpreter.cc file use:
;;; (0) Erase the code in byte-code-interpreter.cc
;;; (1) ./waf build_rboehm
;;; (2) (literal::build-c++-machine)
;;; (3) copy the result into byte-code-interpreter.cc
;;;
(defvar *startup-primitives-as-list*
  '((primitive         "ltvc_make_nil" :ltvc-return (list :gcroots-in-module* :i8 :size_t) :ltvc t)
    (primitive         "ltvc_make_t" :ltvc-return (list :gcroots-in-module* :i8 :size_t) :ltvc t)
    (primitive         "ltvc_make_ratio" :ltvc-return (list :gcroots-in-module* :i8 :size_t :t* :t*) :ltvc t)
    (primitive         "ltvc_make_complex" :ltvc-return (list :gcroots-in-module* :i8 :size_t :t* :t*) :ltvc t)
    (primitive         "ltvc_make_cons" :ltvc-return (list :gcroots-in-module* :i8 :size_t) :ltvc t)
    (primitive         "ltvc_rplaca" :ltvc-return (list :gcroots-in-module* :t* :t*) :ltvc t)
    (primitive         "ltvc_rplacd" :ltvc-return (list :gcroots-in-module* :t* :t*) :ltvc t)
    (primitive         "ltvc_make_list" :ltvc-return (list :gcroots-in-module* :i8 :size_t :size_t) :ltvc t)
    (primitive         "ltvc_fill_list" :ltvc-return (list :gcroots-in-module* :t* :size_t) :varargs t :ltvc t)
    (primitive         "ltvc_make_array" :ltvc-return (list :gcroots-in-module* :i8 :size_t :t* :t*) :ltvc t)
    (primitive         "ltvc_setf_row_major_aref" :ltvc-return (list :gcroots-in-module* :t* :size_t :t*) :ltvc t)
    (primitive         "ltvc_make_hash_table" :ltvc-return (list :gcroots-in-module* :i8 :size_t :t*) :ltvc t)
    (primitive         "ltvc_setf_gethash" :ltvc-return (list :gcroots-in-module* :t* :t* :t*) :ltvc t)
    (primitive         "ltvc_make_fixnum" :ltvc-return (list :gcroots-in-module* :i8 :size_t :uintptr_t) :ltvc t)
    (primitive         "ltvc_make_package" :ltvc-return (list :gcroots-in-module* :i8 :size_t :t*) :ltvc t)
    (primitive         "ltvc_make_next_bignum" :ltvc-return (list :gcroots-in-module* :i8 :size_t :bignum) :ltvc t)
    (primitive         "ltvc_make_bitvector" :ltvc-return (list :gcroots-in-module* :i8 :size_t :t*) :ltvc t)
    (primitive         "ltvc_make_symbol" :ltvc-return (list :gcroots-in-module* :i8 :size_t :t* :t*) :ltvc t)
    (primitive         "ltvc_make_character" :ltvc-return (list :gcroots-in-module* :i8 :size_t :uintptr_t) :ltvc t)
    (primitive         "ltvc_make_base_string" :ltvc-return (list :gcroots-in-module* :i8 :size_t :i8*) :ltvc t)
    (primitive         "ltvc_make_pathname" :ltvc-return (list :gcroots-in-module* :i8 :size_t :t* :t* :t* :t* :t* :t*) :ltvc t)
    (primitive         "ltvc_make_function_description" :ltvc-return (list :gcroots-in-module* :i8 :size_t :t* :t* :t* :t* :t* :size_t :size_t :size_t) :ltvc t)
    (primitive         "ltvc_make_global_entry_point" :ltvc-return (list :gcroots-in-module* :i8 :size_t :size_t :t* :size_t) :ltvc t)
    (primitive         "ltvc_make_local_entry_point" :ltvc-return (list :gcroots-in-module* :i8 :size_t :size_t :t*) :ltvc t)
    (primitive         "ltvc_make_random_state" :ltvc-return (list :gcroots-in-module* :i8 :size_t :t*) :ltvc t)
    (primitive         "ltvc_make_float" :ltvc-return (list :gcroots-in-module* :i8 :size_t :single-float) :ltvc t)
    (primitive         "ltvc_make_double" :ltvc-return (list :gcroots-in-module* :i8 :size_t :double-float) :ltvc t)
    (primitive         "ltvc_make_closurette" :ltvc-return (list :gcroots-in-module* :i8 :size_t #|:size_t|# :size_t) :ltvc t)
    (primitive-unwinds "ltvc_set_mlf_creator_funcall" :ltvc-return (list :gcroots-in-module* :i8 :size_t :size_t :i8*) :ltvc t)
    (primitive-unwinds "ltvc_mlf_init_funcall" :ltvc-return (list :gcroots-in-module* :size_t :i8*) :ltvc t)
    (primitive-unwinds "ltvc_mlf_init_basic_call" :ltvc-return (list :gcroots-in-module* :t* :size_t) :varargs t :ltvc t)
    (primitive-unwinds "ltvc_mlf_create_basic_call" :ltvc-return (list :gcroots-in-module* :i8 :size_t :t* :size_t) :varargs t :ltvc t)
    (primitive-unwinds "ltvc_set_ltv_funcall" :ltvc-return (list :gcroots-in-module* :i8 :size_t :size_t :i8*) :ltvc t)
    (primitive-unwinds "ltvc_toplevel_funcall" :ltvc-return (list :gcroots-in-module* :size_t :i8*) :ltvc t)))

(defvar *primitives* (make-hash-table :test 'equal :thread-safe t))

(defun general-entry-point-redirect-name (arity)
  "Return the name of the wrong-number-of-arguments function for the arity"
  (core:fmt nil "general_entry_point_redirect_{}" arity))

(defmacro primitives-macro ()
  "ltvc functions are used to construct the byte-code interpreter"
  `(progn
     ,@*startup-primitives-as-list*
     ,@'((primitive         "ltvc_lookup_literal" :t* (list :gcroots-in-module* :size_t))
         (primitive         "ltvc_lookup_transient" :t* (list :gcroots-in-module* :i8 :size_t))
         (primitive         "makeCompiledFunction" :t* (list
                                                        :i8* ; global-entry-point
                                                        :t* ; environment
                                                        ))
         (primitive         "lexicalValueReference" :t** (list :size_t :size_t :t*))
         (primitive         "cc_match" :t* (list :t* :t*))
;;         (primitive         "registerReference" :t** (list :t**))
;;;    (primitive         "symbolFunctionRead" :t* (list :t*))
;;;         (primitive         "setfSymbolFunctionRead" :t* (list :t*))
    
         (primitive         "activationFrameReferenceFromClosure" :t** (list :t*))
         (primitive         "setParentOfActivationFrame" :void (list :t* :t*))
         (primitive         "makeValueFrameSetParent" :t* (list :i64 :t*))
         (primitive         "invisible_makeValueFrameSetParent" :t* (list :t*))
         (primitive         "makeBlockFrameSetParent" :t* (list :t*))
         (primitive         "makeTagbodyFrameSetParent" :t* (list :t*))

;;;         (primitive         "cc_validate_tagged_pointer" :void (list :t*))
         (primitive-unwinds "makeFunctionFrame" :t* (list :i32 :t*))
         (primitive-unwinds "functionFrameReference" :t** (list :t* :i32))
    
         (primitive-unwinds "cc_register_startup_function" :void (list :size_t :fn-start-up*))
         (primitive         "cc_protect_alloca" :void (list :i8*))

         (primitive-unwinds "cc_error_type_error" :void (list :t* :t*) :does-not-return t)
         (primitive-unwinds "cc_error_array_out_of_bounds" :void (list :t* :t* :t*) :does-not-return t)
         (primitive-unwinds "cc_error_case_failure" :void (list :t* :t* :t* :t*) :does-not-return t)
         
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
         (primitive         "cc_set_breakstep" :void nil)
         (primitive         "cc_unset_breakstep" :void nil)
         (primitive-unwinds "cc_breakstep" :void (list :t* :i8*))
         (primitive         "cc_breakstep_after" :void (list :t*))
         (primitive-unwinds "cc_wrong_number_of_arguments" :void (list :t* :size_t :size_t :size_t)
          :does-not-return t)

         (primitive         "va_lexicalFunction" :t* (list :size_t :size_t :t*))

         (primitive         "cc_list" :t* (list :size_t) :varargs t)
         (primitive         "cc_mvcGatherRest" :t* (list :size_t :t* :size_t))
         (primitive         "cc_mvcGatherRest2" :t* (list :t** :size_t))
         (primitive         "cc_gatherRestArguments" :t* (list :vaslist* :size_t))
         (primitive         "cc_gatherDynamicExtentRestArguments" :t* (list :vaslist* :size_t :t**))
         (primitive         "cc_gatherVaRestArguments" :t* (list :vaslist* :size_t :vaslist*))
         (primitive-unwinds "cc_ifBadKeywordArgumentException" :void (list :t* :t* :t*))

         (primitive         "initializeBlockClosure" :t* (list :t** :t*))
         (primitive         "initializeTagbodyClosure" :t* (list :t** :t*))
    
         (primitive-unwinds "throwReturnFrom" :void (list :size_t :t*) :does-not-return t)
         (primitive-unwinds "throwDynamicGo" :void (list :size_t :size_t :t*) :does-not-return t)

         (primitive-unwinds "blockHandleReturnFrom_or_rethrow" :return-type (list :i8* :i8*))
         (primitive-unwinds "tagbodyHandleDynamicGoIndex_or_rethrow" :i32 (list :i8* :i8*))
         (primitive-unwinds "throwIllegalSwitchValue" :void (list :i32 :size_t) :does-not-return t)
         (primitive-unwinds "cc_error_bugged_come_from" :void (list :size_t) :does-not-return t)
    
         (primitive         "__gxx_personality_v0" :i32 nil :varargs t)
         (primitive         "__cxa_begin_catch" :i8* (list :i8*) )
         (primitive-unwinds "__cxa_end_catch" :void nil)
         (primitive         "llvm.eh.typeid.for" :i32 (list :i8*))
    
         (primitive         "llvm.sadd.with.overflow.i32" :{i32.i1} (list :i32 :i32))
         (primitive         "llvm.sadd.with.overflow.i64" :{i64.i1} (list :i64 :i64))
         (primitive         "llvm.ssub.with.overflow.i32" :{i32.i1} (list :i32 :i32))
         (primitive         "llvm.ssub.with.overflow.i64" :{i64.i1} (list :i64 :i64))
         (primitive         "llvm.ctpop.i64" :i64 (list :i64))
         ;; NOTE: FP primitives may signal a floating point exception but this
         ;; is not the same as raising an exception. I think. FIXME: Check.
         (primitive         "llvm.cos.f32" :single-float (list :single-float))
         (primitive         "llvm.sin.f32" :single-float (list :single-float))
         (primitive         "llvm.fabs.f32" :single-float (list :single-float))
         (primitive         "llvm.sqrt.f32" :single-float (list :single-float))
         (primitive         "llvm.exp.f32" :single-float (list :single-float))
         (primitive         "llvm.log.f32" :single-float (list :single-float))
         (primitive         "llvm.trunc.f32" :single-float (list :single-float))
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
         (primitive         "llvm.trunc.f64" :double-float (list :double-float))
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

         (primitive         "llvm.vector.reduce.fadd.v4f32" :single-float (list :single-float :v4-single-float))
         (primitive         "llvm.vector.reduce.fmul.v4f32" :single-float (list :single-float :v4-single-float))
         (primitive         "llvm.vector.reduce.fadd.v2f64" :double-float (list :double-float :v2-double-float))
         (primitive         "llvm.vector.reduce.fmul.v2f64" :double-float (list :double-float :v2-double-float))
         
         (primitive         "llvm.experimental.stackmap" :void (list :i64 :i32) :varargs t)
         (primitive         "llvm.va_copy" :void (list :i8* :i8*))
         (primitive         "llvm.va_start" :void (list :i8*))
         (primitive         "llvm.va_end" :void (list :i8*))

         (primitive         "llvm.dbg.addr" :void (list :metadata :metadata :metadata))
         (primitive         "llvm.dbg.declare" :void (list :metadata :metadata :metadata))
         (primitive         "llvm.dbg.value" :void (list :metadata :metadata :metadata))

         (primitive         "llvm.lifetime.start" :void (list :i64 :i8*))
         (primitive         "llvm.lifetime.end" :void (list :i64 :i8*))

         (primitive         "llvm.stacksave" :i8* nil)
         (primitive         "llvm.stackrestore" :void (list :i8*))

         (primitive         "llvm.memcpy.p0i8.p0i8.i64" :void (list :i8* :i8* :i64 :i1))
         (primitive         "llvm.memmove.p0i8.p0i8.i64" :void (list :i8* :i8* :i64 :i1))

         (primitive         "saveToMultipleValue0" :void (list :tmv*))
         (primitive         "restoreFromMultipleValue0" :return-type nil)
         (primitive         "cc_save_values" :void (list :size_t :t* :t**))
         (primitive         "cc_load_values" :return-type (list :size_t :t**))
         (primitive         "cc_nvalues" :size_t nil)
         (primitive         "cc_save_all_values" :void (list :size_t :t**))
         (primitive         "cc_load_all_values" :void (list :size_t :t**))
    
         ;; Primitives for Cleavir code

         (primitive         "cm_check_index" :return-type (list :t* :t* :t*))
         (primitive         "cm_vref" :return-type (list :t* :t*))
         (primitive         "cm_vset" :return-type (list :t* :t* :t*))
         (primitive         "cc_ensure_valid_object" :t* (list :t*))
         (primitive         "cc_getPointer" :i8* (list :t*))
         (primitive-unwinds "cc_makeCell" :t* nil)
         (primitive         "cc_simpleBitVectorAref" :i8 (list :t* :size_t))
         (primitive         "cc_simpleBitVectorAset" :void (list :t* :size_t :i8))
         (primitive-unwinds "cc_simpleFloatVectorAref" :single-float (list :t* :t*))
         (primitive-unwinds "cc_simpleDoubleVectorAref" :double-float (list :t* :t*))
         (primitive-unwinds "cc_simpleFloatVectorAset" :void (list :single-float :t* :t*))
         (primitive-unwinds "cc_simpleDoubleVectorAset" :void (list :double-float :t* :t*))
         (primitive         "cc_initialize_gcroots_in_module" :void (list :gcroots-in-module* ; holder
                                                                      :t** ; root_address
                                                                      :size_t ; num_roots
                                                                      :t* ; initial_data
                                                                      :i8** ; transient_alloca
                                                                      :size_t ; transient_entries
                                                                      :size_t ; function_pointer_count
                                                                      :i8** ; fptrs
                                                                      ))
         (primitive         "cc_finish_gcroots_in_module" :void (list :gcroots-in-module*))
         (primitive         "cc_remove_gcroots_in_module" :void (list :gcroots-in-module* ))
         (primitive-unwinds "cc_invoke_sub_run_all_function" :void (list :fn-start-up*))
         (primitive-unwinds "cc_invoke_byte_code_interpreter" :void (list :gcroots-in-module* :i8* :size_t))

         (primitive-unwinds "cc_enclose" :t* (list
                                              :t*
                                              :size_t))
         (primitive         "cc_stack_enclose" :t* (list
                                                    :i8*
                                                    :t*
                                                    :size_t ))
         (primitive-unwinds "cc_initialize_closure" :void (list :t*
                                                            :size_t ) :varargs t)
         (primitive-unwinds "cc_safe_symbol_value" :t* (list :t*))
         (primitive         "cc_setSymbolValue" :void (list :t* :t*))
         (primitive         "cc_TLSymbolValue" :t* (list :t*))
         (primitive         "cc_setTLSymbolValue" :void (list :t* :t*))
         (primitive         "cc_resetTLSymbolValue" :void (list :t* :t*))

         (primitive-unwinds "cc_call_multipleValueOneFormCallWithRet0" :return-type (list :t* :return-type))
         (primitive-unwinds "cc_oddKeywordException" :void (list :t*))
         (primitive         "cc_multipleValuesArrayAddress" :t*[0]* nil)
         ;; Marking setjmp as returns_twice is EXTREMELY IMPORTANT.
         ;; Without this attribute, LLVM will apply invalid optimizations.
         ;; For example, it will reuse stack space allocated before a setjmp
         ;; call after the setjmp call, meaning that when a longjmp occurs,
         ;; variables it expects to be there are replaced with whatever other
         ;; value. This can cause very difficult to debug problems!
         ;; - Bike, who's spent a solid two days staring at IR incomprehendingly
         (primitive         "_setjmp" :i32 (list :jmp-buf-tag*) :returns-twice t)
         (primitive-unwinds "_longjmp" :void (list :jmp-buf-tag* :i32) :does-not-return t)
         ;;         (primitive-unwinds "cc_throw" :void (list :t*) :does-not-return t)
         (primitive-unwinds "cc_createAndPushBlockDynenv" :t* (list :i8* :i8* :jmp-buf-tag*))
         (primitive-unwinds "cc_createAndPushTagbodyDynenv" :t* (list :i8* :i8* :jmp-buf-tag*))
         (primitive         "cc_initializeAndPushCleanupDynenv" :t* (list :i8* :i8* :jmp-buf-tag*))
         (primitive         "cc_initializeAndPushBindingDynenv" :t* (list :i8* :i8* :t* :t*))
         (primitive         "cc_get_dynenv_stack" :t* (list))
         (primitive         "cc_set_dynenv_stack" :void (list :t*))
         (primitive         "cc_dynenv_frame" :i8* (list :t*))
         (primitive-unwinds "cc_sjlj_unwind" :void (list :t* :size_t) :does-not-return t)
         (primitive-unwinds "cc_sjlj_continue_unwinding" :void nil :does-not-return t)
         (primitive         "cc_saveMultipleValue0" :void (list :tmv))
         (primitive         "cc_restoreMultipleValue0" :return-type nil)
         (primitive         "llvm.frameaddress.p0i8" :i8* (list :i32))
         (primitive-unwinds "cc_landingpadUnwindMatchFrameElseRethrow" :size_t (list :i8* :i8*))

         ;; Compiler translators (calls generated by Cleavir)

         (primitive-unwinds "cc_unbox_single_float" :single-float (list :t*))
         (primitive-unwinds "cc_unbox_double_float" :double-float (list :t*))

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
         (primitive-unwinds "to_object_double" :t* (list :double-float))
         #+long-float (primitive "from_object_long_double" :long-float (list :t*))
         #+long-float (primitive "to_object_long_double" :t* (list :long-float))

         ;; POINTER / VOID *

         ;; Note: using :void* causes an error - so we use :i64* instead here!
         (primitive-unwinds "from_object_pointer" :i64* (list :t*))
         (primitive-unwinds "to_object_pointer" :t* (list :i64*))
         (primitive-unwinds "to_object_void" :t* (list))
         ;; === END OF TRANSLATORS ===
         (primitive         "cx_read_stamp" :t* (list :t* :i64))
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
    (:bignum %bignum%)
    (:double-float %double%)
    (:single-float %float%)
    (:v4-single-float (llvm-sys:vector-type-get %float% 4 nil))
    (:v2-double-float (llvm-sys:vector-type-get %double% 2 nil))
    (:fn-start-up* %fn-start-up*%)
    (:gcroots-in-module* %gcroots-in-module*%)
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
    #+long-float (:long-float %long-float%)
    (:ltv** %ltv**%)
    (:ltvc-return %ltvc-return%)
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
                    
