(in-package #:cmp)


(defstruct (primitive (:type vector) :named)
  return-type
  argument-types
  return-attributes
  argument-attributes
  properties)

(defun primitive-varargs (prim)
  (let ((props (primitive-properties prim)))
    (getf props :varargs)))

(defun define-primitive-info (name return-ty-attributes passed-args-ty varargs does-not-throw does-not-return)
  (let (reversed-argument-types
        return-ty
        return-attributes
        argument-attributes
        (index 0))
    (if (consp return-ty-attributes)
        (setf return-ty (car return-ty-attributes)
              return-attributes (cdr return-ty-attributes))
        (setf return-ty return-ty-attributes))
    (dolist (arg passed-args-ty)
      (if (consp arg)
          (let ((arg-ty (car arg))
                (attribute-list (cdr arg)))
            (push arg-ty reversed-argument-types)
            (push (cons index attribute-list) argument-attributes))
          (progn
            (push arg reversed-argument-types)))
      (incf index))
    (let* ((return-ty (if (consp return-ty-attributes) (car return-ty-attributes) return-ty-attributes))
           (argument-types (nreverse reversed-argument-types)))
      (make-primitive
       :return-type return-ty
       :argument-types argument-types
       :return-attributes return-attributes
       :argument-attributes argument-attributes
       :properties (list :varargs varargs
                         :does-not-throw does-not-throw
                         :does-not-return does-not-return)))))

(defun define-primitive (name return-ty-attr args-ty-attr &key varargs does-not-throw does-not-return )
      (let ((info (define-primitive-info name return-ty-attr args-ty-attr varargs does-not-throw does-not-return)))
    (core::hash-table-setf-gethash *primitives* name info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  WARNING WARNING WARNING WARNING
;;;
;;; Don't thoughtlessly change a primitive-unwinds intrinsic to a primitive intrinsic.
;;; + primitive means that the intrinsic doesn't ever throw an exception
;;; + primitive-unwinds means that the intrinsic can throw an exception and should be called with INVOKE
;;; + unless it is cc_throw or cc_unwind - then it should be called with CALL.
;;;
(defun primitive-unwinds (name return-ty args-ty &key varargs does-not-return )
  "Define primitives that can unwind the stack, either directly or through transitive calls"
  (define-primitive name return-ty args-ty :varargs varargs :does-not-throw nil :does-not-return does-not-return))

(defun primitive         (name return-ty args-ty &key varargs does-not-return)
  "Define primitives that do NOT unwind the stack directly or through transitive calls"
  (define-primitive name return-ty args-ty :varargs varargs :does-not-throw t :does-not-return does-not-return))

(defun primitives-in-thread ()
  (let ((*primitives* (make-hash-table :test 'equal)))
    (primitive         "ltvc_assign_source_file_info_handle" %void% (list %i8*% %i8*% %size_t% %i32% %i32*%))
    (primitive         "ltvc_make_nil" %ltvc-return% (list %gcroots-in-module*% %size_t%))
    (primitive         "ltvc_make_t" %ltvc-return% (list %gcroots-in-module*% %size_t%))
    (primitive         "ltvc_make_ratio" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*% %t*%))
    (primitive         "ltvc_make_complex" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*% %t*%))
    (primitive         "ltvc_make_cons" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*% %t*%))
    (primitive         "ltvc_nconc" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*% %t*%))
    (primitive         "ltvc_make_list" %ltvc-return% (list %gcroots-in-module*% %size_t% %size_t%) :varargs t)
    (primitive         "ltvc_make_array" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*% %t*%))
    (primitive         "ltvc_setf_row_major_aref" %ltvc-return% (list %t*% %size_t% %t*%))
    (primitive         "ltvc_make_hash_table" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*%))
    (primitive         "ltvc_setf_gethash" %ltvc-return% (list %t*% %t*% %t*%))
    (primitive         "ltvc_make_fixnum" %ltvc-return% (list %gcroots-in-module*% %size_t% %uintptr_t%))
    (primitive         "ltvc_make_package" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*%))
    (primitive         "ltvc_make_bignum" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*%))
    (primitive         "ltvc_make_bitvector" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*%))
    (primitive         "ltvc_make_symbol" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*% %t*%))
    (primitive         "ltvc_make_character" %ltvc-return% (list %gcroots-in-module*% %size_t% %uintptr_t%))
    (primitive         "ltvc_make_base_string" %ltvc-return% (list %gcroots-in-module*% %size_t% %i8*%))
    (primitive         "ltvc_make_pathname" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*% %t*% %t*% %t*% %t*% %t*%))
    (primitive         "ltvc_make_package" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*%))
    (primitive         "ltvc_make_random_state" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*%))
    (primitive         "ltvc_make_built_in_class" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*%))
    (primitive         "ltvc_make_float" %ltvc-return% (list %gcroots-in-module*% %size_t% %float%))
    (primitive         "ltvc_make_double" %ltvc-return% (list %gcroots-in-module*% %size_t% %double%))
    (primitive         "ltvc_lookup_value" %t*% (list %gcroots-in-module*% %size_t%))
    (primitive         "ltvc_enclose" %ltvc-return% (list %gcroots-in-module*%
                                                          %size_t%
                                                          %t*%
                                                          %i8*%
                                                          %fn-prototype*%
                                                          %i32*%
                                                          %size_t%
                                                          %size_t%
                                                          %size_t%))
    (primitive-unwinds "ltvc_set_mlf_creator_funcall" %ltvc-return% (list %gcroots-in-module*% %size_t% %fn-prototype*% %i8*%))
    (primitive-unwinds "ltvc_mlf_init_funcall" %ltvc-return% (list %fn-prototype*% %i8*%))
    (primitive-unwinds "ltvc_set_ltv_funcall" %ltvc-return% (list %gcroots-in-module*% %size_t% %fn-prototype*% %i8*%))
    (primitive-unwinds "ltvc_set_ltv_funcall_cleavir" %ltvc-return% (list %gcroots-in-module*% %size_t% %fn-prototype*% %i8*%))
    (primitive-unwinds "ltvc_toplevel_funcall" %ltvc-return% (list %fn-prototype*% %i8*%))
  
    (primitive         "newTmv" %void% (list %tmv*%))
    (primitive         "isTrue" %i32% (list %t*%))
    (primitive         "isBound" %i32% (list %t*%))
    (primitive         "valueOrNilIfZero" %t*% (list %return_type%))
    
    (primitive         "makeCompiledFunction" %t*% (list %fn-prototype*% ; funcPtr
                                                         %i8*%
                                                         #| %i32*% ; sourceFileInfoHandleP
                                                         %size_t% ; filePos
                                                         %size_t% ; lineno
                                                         %size_t% ; column 
                                                         |#
                                                         %t*% ; functionNameP
                                                         %t*% ; renv
                                                         %t*%)) ; lambdaListP
    
    (primitive-unwinds "symbolValueRead" %t*% (list %t*%))
    (primitive         "symbolValueReference" %t**% (list %t*%))
    (primitive         "lexicalValueReference" %t**% (list %size_t% %size_t% %t*%))
    (primitive         "registerReference" %t**% (list %t**%))
    (primitive         "symbolFunctionRead" %t*% (list %t*%))
    (primitive         "setfSymbolFunctionRead" %t*% (list %t*%))
    
    (primitive         "activationFrameReferenceFromClosure" %t**% (list %t*%))
    (primitive         "setParentOfActivationFrame" %void% (list %t*% %t*%))
    (primitive         "makeValueFrameSetParent" %t*% (list %i64% %t*%))
    (primitive         "invisible_makeValueFrameSetParent" %t*% (list %t*%))
    (primitive         "makeBlockFrameSetParent" %t*% (list %t*%))
    (primitive         "invisible_makeBlockFrameSetParent" %t*% (list %t*%))
    (primitive         "makeTagbodyFrameSetParent" %t*% (list %t*%))
    (primitive         "invisible_makeTagbodyFrameSetParent" %t*% (list %t*%))

    (primitive         "setFrameUniqueId" %void% (list %size_t% %t*%))
    (primitive         "ignore_setFrameUniqueId" %void% nil)
    (primitive         "ensureFrameUniqueId" %void% (list %size_t% %size_t% %t*%))
    (primitive         "ignore_ensureFrameUniqueId" %void% nil)

    (primitive-unwinds "makeFunctionFrame" %t*% (list %i32% %t*%))
    (primitive-unwinds "functionFrameReference" %t**% (list %t*% %i32%))
    
;;;    (primitive-unwinds "invokeTopLevelFunction" %void% (list %tmv*% %fn-prototype*% %i8*% %i32*% %size_t% %size_t% %size_t% %ltv**%))
    (primitive-unwinds "cc_register_startup_function" %void% (list %fn-start-up*%))
    (primitive-unwinds "cc_invoke_sub_run_all_function" %void% (list %fn-start-up*%))
    
    (primitive         "cc_trackFirstUnexpectedKeyword" %size_t% (list %size_t% %size_t%))
    (primitive-unwinds "gdb" %void% nil)
    (primitive         "debugInspectTPtr" %void% (list %t*%))
    (primitive         "debugInspectT_mv" %void% (list %tmv*%))
    (primitive         "debugInspect_return_type" %void% (list %return_type%))

    (primitive         "debugPointer" %void% (list %i8*%))
    (primitive         "debug_vaslistPtr" %void% (list %vaslist*%))
    (primitive         "debug_va_list" %void% (list %va_list*%))
    (primitive         "debugMessage" %void% (list %i8*%))
    (primitive         "debugPrintI32" %void% (list %i32%))
    (primitive         "debugPrint_blockFrame" %void% (list %t*%))
    (primitive         "debugPrint_blockHandleReturnFrom" %void% (list %i8*% %t*%))
    (primitive         "debugPrint_size_t" %void% (list %size_t%))
    (primitive         "debug_match_two_uintptr_t" %uintptr_t% (list %uintptr_t% %uintptr_t%))
    (primitive         "lowLevelTrace" %void% (list %i32%))
    (primitive         "unreachableError" %void% nil)
    
    (primitive-unwinds "va_tooManyArgumentsException" %void% (list %i8*% %size_t% %size_t%))
    (primitive-unwinds "va_notEnoughArgumentsException" %void% (list %i8*% %size_t% %size_t%))
    (primitive-unwinds "va_ifExcessKeywordArgumentsException" %void% (list %i8*% %size_t% %va_list*% %size_t%))
    (primitive-unwinds "va_symbolFunction" %t*% (list %t*%))
    (primitive         "va_lexicalFunction" %t*% (list %size_t% %size_t% %t*%))
    
    (primitive         "cc_gatherRestArguments" %t*% (list %va_list*% %size_t*%))
    (primitive         "cc_gatherVaRestArguments" %t*% (list %va_list*% %size_t*% %vaslist*%))
    (primitive-unwinds "cc_ifBadKeywordArgumentException" %void% (list %size_t% %size_t% %t*% %function-description*%))
    
    (primitive         "initializeBlockClosure" %t*% (list %t**%))
    (primitive         "ignore_initializeBlockClosure" %t*% (list %t**%))
    (primitive         "initializeTagbodyClosure" %t*% (list %t**%))
    (primitive         "ignore_initializeTagbodyClosure" %t*% (list %t**%))
    (primitive         "pushTagbodyFrame" %size_t% (list %t*%))
    
    (primitive-unwinds "throwReturnFrom" %void% (list %size_t% %t*%) :does-not-return t)
    (primitive-unwinds "throwDynamicGo" %void% (list %size_t% %size_t% %t*%) :does-not-return t)
    
    (primitive         "exceptionStackUnwind" %void% (list %size_t%))
    (primitive         "ignore_exceptionStackUnwind" %void% nil)
    (primitive-unwinds "blockHandleReturnFrom_or_rethrow" %return_type% (list %i8*% %t*%))
    (primitive-unwinds "ignore_blockHandleReturnFrom_or_rethrow" %return_type% (list %i8*% %t*%))
    (primitive-unwinds "tagbodyHandleDynamicGoIndex_or_rethrow" %size_t% (list %i8*% %t*%))
    (primitive-unwinds "throwIllegalSwitchValue" %void% (list %size_t% %size_t%) :does-not-return t)
    
    (primitive         "clasp_terminate" %void% nil)
    (primitive         "__gxx_personality_v0" %i32% nil :varargs t) ;; varargs
    (primitive         "__cxa_begin_catch" %i8*% (list %i8*%) )
    (primitive-unwinds "__cxa_end_catch" %void% nil) ;; This DOES UNWIND!!!!!   use primitive-unwind once you figure out how to make it work
    (primitive-unwinds "__cxa_rethrow" %void% nil)
    (primitive         "llvm.eh.typeid.for" %i32% (list %i8*%))
    
    (primitive         "llvm.sadd.with.overflow.i32" %{i32.i1}% (list %i32% %i32%))
    (primitive         "llvm.sadd.with.overflow.i64" %{i64.i1}% (list %i64% %i64%))
    (primitive         "llvm.ssub.with.overflow.i32" %{i32.i1}% (list %i32% %i32%))
    (primitive         "llvm.ssub.with.overflow.i64" %{i64.i1}% (list %i64% %i64%))
    
    (primitive         "llvm.va_copy" %void% (list %i8*% %i8*%))
    (primitive         "llvm.va_start" %void% (list %i8*%))
    (primitive         "llvm.va_end" %void% (list %i8*%))
    
    (primitive         "debugSourceFileInfoHandle" %void% (list %i32*%))
    
    (primitive         "saveToMultipleValue0" %void% (list %tmv*%))
    (primitive         "restoreFromMultipleValue0" %return_type% nil)
    (primitive         "saveValues" %t*% (list %tmv*%))
    (primitive         "loadValues" %void% (list %tmv*% %t*%))
    
    (primitive         "pushDynamicBinding" %void% (list %t*%))
    (primitive         "popDynamicBinding" %void% (list %t*%))
    
    (primitive         "matchKeywordOnce" %size_t% (list %t*% %t*% %i8*%))
    
    ;; Primitives for Cleavir code

    (primitive         "cc_eql" %i32% (list %t*% %t*%)) ;; eql test
    (primitive-unwinds "cc_bad_tag" %void% (list %t*%)) ;; gf
    (primitive         "cc_fastgf_nil" %t*% nil)
    (primitive-unwinds "cc_dispatch_invalid" %return_type% (list %t*% %t*%)) ;; gf gf-args
    (primitive-unwinds "cc_dispatch_miss" %return_type% (list %t*% %t*%)) ;; gf gf-args
;;;    (primitive-unwinds "cc_dispatch_slot_reader_index_debug"   %return_type% (list %t*% %size_t% %t*% %t*%)) ; effective-method gf gf-args
;;;    (primitive-unwinds "cc_dispatch_slot_writer_index_debug"   %return_type% (list %t*% %size_t% %t*% %t*%)) ; effective-method gf gf-args
    (primitive         "cc_dispatch_slot_reader_index"  %t*% (list %size_t% %t*%)) ; index instance
    (primitive         "cc_dispatch_slot_reader_cons"   %t*% (list %t*%)) ; cons
    (primitive         "cc_dispatch_slot_writer_index"  %t*% (list %t*% %size_t% %t*%)) ; value index instance
    (primitive         "cc_dispatch_slot_writer_cons"   %t*% (list %t*% %t*%)) ; value cons
    (primitive-unwinds "cc_dispatch_effective_method"   %return_type% (list %t*% %t*% %t*%)) ; effective-method gf gf-args
    (primitive-unwinds "cc_dispatch_debug" %void% (list %i32% %uintptr_t%))
    (primitive-unwinds "cc_bound_or_error" %t*% (list %t*% %t*% %t*%)) ; optimized-data instance value
    (primitive         "cc_vaslist_end" %void% (list %t*%))

    (primitive-unwinds "cc_check_if_wrong_number_of_arguments" %void% (list %size_t% %size_t% %size_t% %function-description*%))
    (primitive         "cc_ensure_valid_object" %t*% (list %t*%))
    (primitive         "cc_getPointer" %i8*% (list %t*%))
    (primitive         "cc_setTmvToNil" %void% (list %tmv*%))
    (primitive         "cc_precalcSymbol" %t*% (list %ltv**% %size_t%))
    (primitive         "cc_precalcValue" %t*% (list %ltv**% %size_t%))
    (primitive-unwinds "cc_makeCell" %t*% nil)
    (primitive         "cc_writeCell" %void% (list %t*% %t*%))
    (primitive         "cc_readCell" %t*% (list %t*%))
    (primitive         "cc_t_reference" %t**% nil)
    (primitive         "cc_nil_reference" %t**% nil)
    (primitive         "cc_builtin_nil" %t*% nil)
    (primitive         "cc_fetch" %t*% (list %t*% %size_t%))
;;    (primitive         "cc_va_arg" %t*% (list %vaslist*%))
;;    (primitive         "cc_va_list_length" %size_t% (list %vaslist*%))
    (primitive         "cc_copy_va_list" %void% (list %size_t% %t*[0]*% %vaslist*%))
    (primitive         "cc_realArrayDisplacement" %t*% (list %t*%))
    (primitive         "cc_realArrayDisplacedIndexOffset" %size_t% (list %t*%))
    (primitive         "cc_arrayTotalSize" %size_t% (list %t*%))
    (primitive         "cc_arrayRank" %size_t% (list %t*%))
    (primitive         "cc_arrayDimension" %size_t% (list %t*% %size_t%))
    (primitive         "cc_simpleBitVectorAref" %uint% (list %t*% %size_t%))
    (primitive         "cc_simpleBitVectorAset" %void% (list %t*% %size_t% %uint%))
    (primitive         "cc_initialize_gcroots_in_module" %void% (list %gcroots-in-module*% %t**% %size_t% %t*%))
    (primitive         "cc_shutdown_gcroots_in_module" %void% (list %gcroots-in-module*% ))

    (primitive-unwinds  "cc_enclose" %t*% (list %t*%
                                                %fn-prototype*%
                                                %i8*%
                                                #| %i32*% %size_t% %size_t% %size_t% |#
                                                %size_t% ) :varargs t)
    (primitive         "cc_stack_enclose" %t*% (list %i8*% %t*%
                                                     %fn-prototype*%
                                                     %i8*%
                                                     #| %i32*% %size_t% %size_t% %size_t% |#
                                                     %size_t% ) :varargs t)
    (primitive         "cc_saveThreadLocalMultipleValues" %void% (list %tmv*% %mv-struct*%))
    (primitive         "cc_loadThreadLocalMultipleValues" %void% (list %tmv*% %mv-struct*%))
    (primitive-unwinds "cc_safe_fdefinition" %t*% (list %t*%))
    (primitive         "cc_unsafe_fdefinition" %t*% (list %t*%))
    (primitive-unwinds "cc_safe_setfdefinition" %t*% (list %t*%))
    (primitive         "cc_unsafe_setfdefinition" %t*% (list %t*%))
    (primitive-unwinds "cc_safe_symbol_value" %t*% (list %t*%))
    (primitive         "cc_unsafe_symbol_value" %t*% (list %t*%))
    (primitive         "cc_setSymbolValue" %void% (list %t*% %t*%))

    (primitive         "cc_setup_vaslist" %t*% (list %vaslist*% %va_list*% %size_t%))
    (primitive         "cc_setup_vaslist_internal" %t*% (list %vaslist*% %size_t%))
    (primitive         "cc_rewind_va_list" %void% (list %va_list*% %size_t*% %register-save-area*%))
    (primitive         "cc_rewind_vaslist" %t*% (list %vaslist*% %va_list*% %register-save-area*%))
    (primitive         "cc_push_InvocationHistoryFrame" %void% (list %t*% %InvocationHistoryFrame*% %va_list*% %size_t%))
    (primitive         "cc_pop_InvocationHistoryFrame" %void% (list %t*% %InvocationHistoryFrame*%))
    
    (primitive-unwinds "cc_call_multipleValueOneFormCall" %return_type% (list %t*%))
    (primitive-unwinds "cc_call_multipleValueOneFormCallWithRet0" %return_type% (list %t*% %return_type%))
    (primitive-unwinds "cc_call"   %return_type% (list* %t*% %size_t%
                                                                (make-list core:+number-of-fixed-arguments+
                                                                           :initial-element %t*%))
                        :varargs t)
    (primitive-unwinds "cc_call_callback"   %return_type% (list* %t*% %size_t%
                                                                         (make-list core:+number-of-fixed-arguments+
                                                                                    :initial-element %t*%))
                        :varargs t)
    (primitive         "cc_allowOtherKeywords" %i64% (list %i64% %t*%))
    (primitive         "cc_matchKeywordOnce" %size_t% (list %t*% %t*% %t*%))
    (primitive-unwinds "cc_ifNotKeywordException" %void% (list %t*% %size_t% %va_list*% %function-description*%))
    (primitive         "cc_multipleValuesArrayAddress" %t*[0]*% nil)
    (primitive-unwinds "cc_unwind" %void% (list %t*% %size_t%))
    (primitive-unwinds "cc_throw" %void% (list %t*%) :does-not-return t)
    (primitive         "cc_saveMultipleValue0" %void% (list %tmv*%))
    (primitive         "cc_restoreMultipleValue0" %void% (list %tmv*%))
    (primitive         "cc_pushLandingPadFrame" %t*% nil)
    (primitive         "cc_popLandingPadFrame" %void% (list %t*%))
    (primitive-unwinds "cc_landingpadUnwindMatchFrameElseRethrow" %size_t% (list %i8*% %t*%))

    (primitive         "cc_vaslist_va_list_address" %va_list*% (list %t*%))
    (primitive         "cc_vaslist_remaining_nargs_address" %size_t*% (list %t*%))
    
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
    (primitive-unwinds "from_object_fixnum" (list %i64% 'llvm-sys:attribute-sext) (list %t*%))
    (primitive-unwinds "to_object_fixnum" %t*% (list (list %i64% 'llvm-sys:attribute-sext)))

    ;; SHORT & UNSIGNED SHORT
    (primitive-unwinds "from_object_short" (list %i16% 'llvm-sys:attribute-sext) (list %t*%))
    (primitive-unwinds "to_object_short" %t*% (list (list %i16% 'llvm-sys:attribute-sext) ))
    (primitive-unwinds "from_object_unsigned_short" %i16% (list %t*%))
    (primitive-unwinds "to_object_unsigned_short" %t*% (list %i16%))

    ;; INT & UNSIGNED INT
    (primitive-unwinds "from_object_int" (list %i32% 'llvm-sys:attribute-sext) (list %t*%))
    (primitive-unwinds "to_object_int" %t*% (list (list %i32% 'llvm-sys:attribute-sext) ))
    (primitive-unwinds "from_object_unsigned_int" %i32% (list %t*%))
    (primitive-unwinds "to_object_unsigned_int" %t*% (list %i32%))

    ;; LONG & UNSIGNED LONG
    (primitive-unwinds "from_object_long" %i64% (list %t*%))
    (primitive-unwinds "to_object_long" %t*% (list (list %i64% 'llvm-sys:attribute-sext) ))
    (primitive-unwinds "from_object_unsigned_long" %i64% (list %t*%))
    (primitive-unwinds "to_object_unsigned_long" %t*% (list %i64%))

    ;; LONG LONG & UNSIGNED LONG LONG
    (primitive-unwinds "from_object_long_long" (list %i64% 'llvm-sys:attribute-sext) (list %t*%))
    (primitive-unwinds "to_object_long_long" %t*% (list (list %i64% 'llvm-sys:attribute-sext) ))
    (primitive-unwinds "from_object_unsigned_long_long" %i64% (list %t*%))
    (primitive-unwinds "to_object_unsigned_long_long" %t*% (list %i64%))

    ;; INT8 & UINT8
    (primitive-unwinds "from_object_int8" (list %i8% 'llvm-sys:attribute-sext) (list %t*%))
    (primitive-unwinds "to_object_int8" %t*% (list (list %i8% 'llvm-sys:attribute-sext)))
    (primitive-unwinds "from_object_uint8" %i8% (list %t*%))
    (primitive-unwinds "to_object_uint8" %t*% (list %i8%))

    ;; INT16 & UINT16
    (primitive-unwinds "from_object_int16" (list %i16% 'llvm-sys:attribute-sext) (list %t*%))
    (primitive-unwinds "to_object_int16" %t*% (list (list %i16% 'llvm-sys:attribute-sext) ))
    (primitive-unwinds "from_object_uint16" %i16% (list %t*%))
    (primitive-unwinds "to_object_uint16" %t*% (list %i16%))

    ;; INT32 & UINT32
    (primitive-unwinds "from_object_int32" (list %i32% 'llvm-sys:attribute-sext) (list %t*%))
    (primitive-unwinds "to_object_int32" %t*% (list (list %i32% 'llvm-sys:attribute-sext) ))
    (primitive-unwinds "from_object_uint32" %i32% (list %t*%))
    (primitive-unwinds "to_object_uint32" %t*% (list %i32%))

    ;; INT64 & UINT64
    (primitive-unwinds "from_object_int64" (list %i64% 'llvm-sys:attribute-sext) (list %t*%))
    (primitive-unwinds "to_object_int64" %t*% (list (list %i64% 'llvm-sys:attribute-sext) ))
    (primitive-unwinds "from_object_uint64" %i64% (list %t*%))
    (primitive-unwinds "to_object_uint64" %t*% (list %i64%))

    ;; i128 HANDLING NOT IMPLEMENTED AS IT IS NOT USED

    ;; SIZE_T
    (primitive-unwinds "from_object_size" %size_t% (list %t*%))
    (primitive-unwinds "to_object_size" %t*% (list %size_t%))

    ;; SSIZE_T
    (primitive-unwinds "from_object_ssize" %size_t% (list %t*%))
    (primitive-unwinds "to_object_ssize" %t*% (list %size_t%))

    ;; PTRDIFF_T, TIME_T
    ;; (primitive-unwinds "from_object_ptrdiff" %t*% (list %t*%)) - FIXME !
    ;; (primitive-unwinds "to_object_ptrdiff" %t*% (list %uintptr_t%)) - FIXME !

    ;; (primitive-unwinds "from_object_time" %t*% (list %t*%)) - FIXME !
    ;; (primitive-unwinds "to_object_time" %t*% (list %t*%)) - FIOXME !

    ;; CHAR & UNSIGNED CHAR
    (primitive-unwinds "from_object_char" (list %i8% 'llvm-sys:attribute-sext) (list %t*%))
    (primitive-unwinds "to_object_char" %t*% (list (list %i8% 'llvm-sys:attribute-sext) ))
    (primitive-unwinds "from_object_unsigned_char" %i8% (list %t*%))
    (primitive-unwinds "to_object_unsigned_char" %t*% (list %i8%))

    (primitive-unwinds "from_object_claspCharacter" %i32% (list %t*%))
    (primitive-unwinds "to_object_claspCharacter" %t*% (list %i32%))
    (primitive-unwinds "from_object_claspChar" %i8% (list %t*%))
    (primitive-unwinds "to_object_claspChar" %t*% (list %i8%))
    
    ;; FLOAT, DOUBLE & LONG FLOAT
    (primitive-unwinds "from_object_float" %float% (list %t*%))
    (primitive-unwinds "to_object_float" %t*% (list %float%))
    (primitive-unwinds "from_object_double" %double% (list %t*%))
    (primitive-unwinds "to_object_double" %t*% (list %double%))
    #+long-float (primitive "from_object_long_double" %long-float% (list %t*%))
    #+long-float (primitive "to_object_long_double" %t*% (list %long-float%))

    ;; POINTER / VOID *

    ;;(format *debug-io* "~%*** +VOID+ = ~S, +VOID*+ = ~S~%" %void% %void*%)
    ;; Note: using %void*% causes an error - so we use %i64*% instead here!
    (primitive-unwinds "from_object_pointer" %i64*% (list %t*%))
    (primitive-unwinds "to_object_pointer" %t*% (list %i64*%))
    (primitive-unwinds "to_object_void" %t*% (list))
    ;; === END OF TRANSLATORS ===
    (primitive         "cc_read_stamp" %i64% (list %i8*%))
    *primitives*
  ))

