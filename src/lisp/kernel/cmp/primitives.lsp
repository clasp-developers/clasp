(in-package #:cmp)

;;
;; Define functions within the module
;;

;; bound when a new module is created
(defvar *primitives*)

;;;
;;; An attempt to inline specific functions from intrinsics.cc
;;;


;;#+(or)
(defun create-primitive-function (module name return-ty args-ty varargs does-not-throw does-not-return)
  (let ((fnattrs nil))
    (when does-not-throw (push 'llvm-sys:attribute-no-unwind fnattrs))
    (when does-not-return (push 'llvm-sys:attribute-no-return fnattrs))
    (push '("no-frame-pointer-elim" "false") fnattrs)
    (push "no-frame-pointer-elim-non-leaf" fnattrs)
    (cmp:irc-function-create (llvm-sys:function-type-get return-ty args-ty varargs)
                             'llvm-sys::External-linkage
                             name module
                             :function-attributes fnattrs)))

(defun primitive (module name return-ty args-ty &key varargs does-not-throw does-not-return )
  (mapc #'(lambda (x)
	    (when (equal +tsp*-or-tmv*+ x)
	      (error "When defining primitive ~a --> :tsp*-or-tmv* is only allowed in the first argument position" name ))) (cdr args-ty))
  (if (equal (car args-ty) +tsp*-or-tmv*+)
      (progn ;; create two versions of the function - one prefixed with sp_ and the other with mv_
	(create-primitive-function module
				   (dispatch-function-name name %tsp*%)
				   return-ty
				   (cons %tsp*% (cdr args-ty))
				   varargs does-not-throw does-not-return)
	(create-primitive-function module
				   (dispatch-function-name name %tmv*%)
				   return-ty
				   (cons %tmv*% (cdr args-ty))
				   varargs does-not-throw does-not-return))
      (create-primitive-function module
				 name return-ty args-ty varargs does-not-throw does-not-return))
  (core::hash-table-setf-gethash *primitives* name
				 (list return-ty args-ty '( (:varargs . varargs)
							   (:does-not-throw . does-not-throw)
							   ( :does-not-return . does-not-return) ))))


(defun primitive-nounwind* (module name return-ty args-ty &key varargs does-not-return)
  (primitive module name return-ty args-ty :varargs varargs :does-not-throw t :does-not-return does-not-return))

(defun primitives-in-module (module)
  (let ((*primitives* (make-hash-table :test 'equal)))
    (primitive-nounwind* module "ltvc_assign_source_file_info_handle" %void% (list %i8*% %i8*% %size_t% %i32% %i32*%))
    (primitive-nounwind* module "ltvc_make_nil" %ltvc-return% (list %gcroots-in-module*% %size_t%))
    (primitive-nounwind* module "ltvc_make_t" %ltvc-return% (list %gcroots-in-module*% %size_t%))
    (primitive-nounwind* module "ltvc_make_ratio" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*% %t*%))
    (primitive-nounwind* module "ltvc_make_complex" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*% %t*%))
    (primitive-nounwind* module "ltvc_make_cons" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*% %t*%))
    (primitive-nounwind* module "ltvc_nconc" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*% %t*%))
    (primitive-nounwind* module "ltvc_make_list" %ltvc-return% (list %gcroots-in-module*% %size_t% %size_t%) :varargs t)
    (primitive-nounwind* module "ltvc_make_array" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*% %t*%))
    (primitive-nounwind* module "ltvc_setf_row_major_aref" %ltvc-return% (list %t*% %size_t% %t*%))
    (primitive-nounwind* module "ltvc_make_hash_table" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*%))
    (primitive-nounwind* module "ltvc_setf_gethash" %ltvc-return% (list %t*% %t*% %t*%))
    (primitive-nounwind* module "ltvc_make_fixnum" %ltvc-return% (list %gcroots-in-module*% %size_t% %uintptr_t%))
    (primitive-nounwind* module "ltvc_make_package" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*%))
    (primitive-nounwind* module "ltvc_make_bignum" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*%))
    (primitive-nounwind* module "ltvc_make_bitvector" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*%))
    (primitive-nounwind* module "ltvc_make_symbol" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*% %t*%))
    (primitive-nounwind* module "ltvc_make_character" %ltvc-return% (list %gcroots-in-module*% %size_t% %uintptr_t%))
    (primitive-nounwind* module "ltvc_make_base_string" %ltvc-return% (list %gcroots-in-module*% %size_t% %i8*%))
    (primitive-nounwind* module "ltvc_make_pathname" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*% %t*% %t*% %t*% %t*% %t*%))
    (primitive-nounwind* module "ltvc_make_package" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*%))
    (primitive-nounwind* module "ltvc_make_random_state" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*%))
    (primitive-nounwind* module "ltvc_make_built_in_class" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*%))
    (primitive-nounwind* module "ltvc_make_float" %ltvc-return% (list %gcroots-in-module*% %size_t% %float%))
    (primitive-nounwind* module "ltvc_make_double" %ltvc-return% (list %gcroots-in-module*% %size_t% %double%))
    (primitive          module "ltvc_set_mlf_creator_funcall" %ltvc-return% (list %gcroots-in-module*% %size_t% %fn-prototype*% %i8*%))
    (primitive          module "ltvc_mlf_init_funcall" %ltvc-return% (list %fn-prototype*% %i8*%))
    (primitive          module "ltvc_set_ltv_funcall" %ltvc-return% (list %gcroots-in-module*% %size_t% %fn-prototype*% %i8*%))
    (primitive          module "ltvc_set_ltv_funcall_cleavir" %ltvc-return% (list %gcroots-in-module*% %size_t% %fn-prototype*% %i8*%))
    (primitive          module "ltvc_toplevel_funcall" %ltvc-return% (list %fn-prototype*% %i8*%))
  
;;    (primitive-nounwind* module "newFunction_sp" %void% (list %Function_sp*%))
;;    (primitive-nounwind* module "newTsp" %void% (list %tsp*%))
    (primitive-nounwind* module "copyTsp" %void% (list +tsp*-or-tmv*+ %tsp*%))
    (primitive-nounwind* module "copyTspTptr" %void% (list +tsp*-or-tmv*+ %t*%))
    (primitive-nounwind* module "compareTspTptr" %i32% (list %tsp*% %t*%))
    
    (primitive-nounwind* module "newTmv" %void% (list %tmv*%))
;;    (primitive-nounwind* module "resetTmv" %void% (list %tmv*%))
    (primitive-nounwind* module "copyTmv" %void% (list %tmv*% %tmv*%))
    (primitive-nounwind* module "copyTmvOrSlice" %void% (list +tsp*-or-tmv*+ %tmv*%))
    
    (primitive-nounwind* module "isTrue" %i32% (list %tsp*%))
    (primitive-nounwind* module "isBound" %i32% (list %tsp*%))
    
    (primitive-nounwind* module "internSymbol_tsp" %void% (list %tsp*% %i8*% %i8*%))
    (primitive-nounwind* module "makeSymbol_tsp" %void% (list %tsp*% %i8*%))
    
    (primitive-nounwind* module "internSymbol_symsp" %void% (list %symsp*% %i8*% %i8*%))
    (primitive-nounwind* module "makeSymbol_symsp" %void% (list %symsp*% %i8*%))
    
    (primitive-nounwind* module "makeNil" %void% (list +tsp*-or-tmv*+))
    (primitive-nounwind* module "makeT" %void% (list %tsp*%))
    (primitive-nounwind* module "makeCons" %void% (list %tsp*% %tsp*% %tsp*%))
    (primitive-nounwind* module "makeFixnum" %void% (list %tsp*% %fixnum%))
    (primitive-nounwind* module "makeCharacter" %void% (list %tsp*% %i32%))
    (primitive-nounwind* module "makeBignum" %void% (list %tsp*% %i8*%))
    #+short-float (primitive-nounwind* module "makeShortFloat" %void% (list %tsp*% %double%))
    (primitive-nounwind* module "makeSingleFloat" %void% (list %tsp*% %float%))
    (primitive-nounwind* module "makeDoubleFloat" %void% (list %tsp*% %double%))
    
    #+long-float (primitive-nounwind* module "makeLongFloat" %void% (list %tsp*% %long-float%))
    (primitive-nounwind* module "makeString" %void% (list %tsp*% %i8*%))
    (primitive-nounwind* module "makePathname" %void% (list %tsp*% %i8*%))
    (primitive-nounwind* module "makeCompiledFunction" %void% (list +tsp*-or-tmv*+ %fn-prototype*% %i32*% %size_t% %size_t% %size_t% %tsp*% %tsp*% %afsp*% %tsp*%))
    
    (primitive          module "symbolValueRead" %void% (list +tsp*-or-tmv*+ %tsp*%))
    (primitive-nounwind* module "symbolValueReference" %tsp*% (list %tsp*%))
    (primitive-nounwind* module "lexicalValueReference" %tsp*% (list %i32% %i32% %afsp*%))
    (primitive-nounwind* module "lexicalValueRead" %void% (list +tsp*-or-tmv*+ %i32% %i32% %afsp*%))
    (primitive-nounwind* module "symbolFunctionRead" %void% (list +tsp*-or-tmv*+ %tsp*%))
    (primitive-nounwind* module "setfSymbolFunctionRead" %void% (list %tsp*% %tsp*%))
    (primitive-nounwind* module "lexicalFunctionRead" %void% (list +tsp*-or-tmv*+ %i32% %i32% %afsp*%))
    
    
    (primitive-nounwind* module "makeTagbodyFrame" %void% (list %afsp*%))
    (primitive-nounwind* module "makeValueFrame" %void% (list %tsp*% %i64%))
    (primitive-nounwind* module "setParentOfActivationFrameFromClosure" %void% (list %tsp*% %t*%))
    (primitive-nounwind* module "setParentOfActivationFrame" %void% (list %tsp*% %tsp*%))
    
    ;;  (primitive-nounwind* module "attachDebuggingInfoToValueFrame" %void% (list %afsp*% %tsp*%))
    
    (primitive-nounwind* module "valueFrameReference" %tsp*% (list %afsp*% %i32%))
    
    (primitive          module "makeFunctionFrame" %void% (list %afsp*% %i32% %afsp*%))
    (primitive          module "functionFrameReference" %tsp*% (list %afsp*% %i32%))
    
    (primitive          module "prependMultipleValues" %void% (list +tsp*-or-tmv*+ %tmv*%))
    
    (primitive          module "invokeTopLevelFunction" %void% (list %tmv*% %fn-prototype*% %i8*% %i32*% %size_t% %size_t% %size_t% %ltv**%))
    (primitive          module "cc_register_startup_function" %void% (list %fn-start-up*%))
    (primitive          module "cc_invoke_sub_run_all_function" %void% (list %fn-start-up*%))
    
    (primitive-nounwind* module "activationFrameSize" %i32% (list %afsp*%))
    
    (primitive          module "throwTooManyArgumentsException" %void% (list %i8*% %afsp*% %i32% %i32%))
    (primitive          module "throwNotEnoughArgumentsException" %void% (list %i8*% %afsp*% %i32% %i32%))
    (primitive          module "throwIfExcessKeywordArguments" %void% (list %i8*% %afsp*% %i32%))
    (primitive-nounwind* module "cc_trackFirstUnexpectedKeyword" %size_t% (list %size_t% %size_t%))
    (primitive          module "gdb" %void% nil)
    (primitive-nounwind* module "debugInspectActivationFrame" %void% (list %afsp*%))
    (primitive-nounwind* module "debugInspectT_sp" %void% (list %tsp*%))
    (primitive-nounwind* module "debugInspectTPtr" %void% (list %t*%))
    (primitive-nounwind* module "debugInspectT_mv" %void% (list %tmv*%))
    (primitive-nounwind* module "debugInspect_return_type" %void% (list %return_type%))
;;    (primitive-nounwind* module "debugInspect_mvarray" %void% nil)
    (primitive-nounwind* module "debugPointer" %void% (list %i8*%))
    (primitive-nounwind* module "debug_VaList_SPtr" %void% (list %VaList_S*%))
    (primitive-nounwind* module "debug_va_list" %void% (list %va_list*%))
    (primitive-nounwind* module "debugPrintObject" %void% (list %i8*% %tsp*%))
    (primitive-nounwind* module "debugMessage" %void% (list %i8*%))
    (primitive-nounwind* module "debugPrintI32" %void% (list %i32%))
    (primitive-nounwind* module "debugPrint_size_t" %void% (list %size_t%))
    (primitive-nounwind* module "debug_match_two_uintptr_t" %uintptr_t% (list %uintptr_t% %uintptr_t%))
    (primitive-nounwind* module "lowLevelTrace" %void% (list %i32%))
    (primitive-nounwind* module "unreachableError" %void% nil)
    
    (primitive          module "va_tooManyArgumentsException" %void% (list %i8*% %size_t% %size_t%))
    (primitive          module "va_notEnoughArgumentsException" %void% (list %i8*% %size_t% %size_t%))
    (primitive          module "va_ifExcessKeywordArgumentsException" %void% (list %i8*% %size_t% %va_list*% %size_t%))
    (primitive          module "va_symbolFunction" %t*% (list %tsp*%)) ;; void va_symbolFunction(core::Function_sp fn, core::Symbol_sp sym)
    (primitive-nounwind* module "va_lexicalFunction" %t*% (list %i32% %i32% %afsp*%))
    
    (primitive-nounwind* module "cc_gatherRestArguments" %t*% (list %va_list*% %size_t*%))
    (primitive-nounwind* module "cc_gatherVaRestArguments" %t*% (list %va_list*% %size_t*% %VaList_S*%))
    (primitive          module "cc_ifBadKeywordArgumentException" %void% (list %size_t% %size_t% %t*%))
    
    (primitive-nounwind* module "pushCatchFrame" %size_t% (list %tsp*%))
    (primitive-nounwind* module "pushBlockFrame" %size_t% (list %tsp*%))
    (primitive-nounwind* module "pushTagbodyFrame" %size_t% (list %tsp*%))
    
    (primitive          module "throwCatchThrow" %void% (list %tsp*% #| %tmv*% |#) :does-not-return t)
    (primitive          module "throwReturnFrom" %void% (list %tsp*%) :does-not-return t)
    (primitive          module "throwDynamicGo" %void% (list %size_t% %size_t% %afsp*%) :does-not-return t)
    
    (primitive          module "ifCatchFrameMatchesStoreResultElseRethrow" %void% (list +tsp*-or-tmv*+ %size_t% %i8*%))
    (primitive-nounwind* module "exceptionStackUnwind" %void% (list %size_t%))
    (primitive          module "blockHandleReturnFrom" %void% (list +tsp*-or-tmv*+ %i8*% %size_t%))
    (primitive          module "tagbodyDynamicGoIndexElseRethrow" %size_t% (list %i8*% %size_t%))
    (primitive          module "throwIllegalSwitchValue" %void% (list %size_t% %size_t%) :does-not-return t)
    
    (primitive-nounwind* module "clasp_terminate" %void% (list %i8*% %size_t% %size_t% %i8*%) )
    (primitive-nounwind* module "__gxx_personality_v0" %i32% nil :varargs t) ;; varargs
    (primitive-nounwind* module "__cxa_begin_catch" %i8*% (list %i8*%) )
    (primitive-nounwind* module "__cxa_end_catch" %void% nil) ;; This was a PRIMITIVE
    (primitive          module "__cxa_rethrow" %void% nil)
    (primitive-nounwind* module "llvm.eh.typeid.for" %i32% (list %i8*%))
    
    (primitive-nounwind* module "llvm.sadd.with.overflow.i32" %{i32.i1}% (list %i32% %i32%))
    (primitive-nounwind* module "llvm.sadd.with.overflow.i64" %{i64.i1}% (list %i64% %i64%))
    (primitive-nounwind* module "llvm.ssub.with.overflow.i32" %{i32.i1}% (list %i32% %i32%))
    (primitive-nounwind* module "llvm.ssub.with.overflow.i64" %{i64.i1}% (list %i64% %i64%))
    
    (primitive-nounwind* module "llvm.va_copy" %void% (list %i8*% %i8*%))
    (primitive-nounwind* module "llvm.va_start" %void% (list %i8*%))
    (primitive-nounwind* module "llvm.va_end" %void% (list %i8*%))
    
    (primitive-nounwind* module "copyLoadTimeValue" %void% (list +tsp*-or-tmv*+ %ltv**% %size_t%))
    (primitive-nounwind* module "getLoadTimeValue" %void% (list +tsp*-or-tmv*+ %ltv**% %i32%))
    (primitive-nounwind* module "dumpLoadTimeValues" %void% (list %ltv**%))
    
    (primitive-nounwind* module "debugSourceFileInfoHandle" %void% (list %i32*%))
    
    (primitive-nounwind* module "saveToMultipleValue0" %void% (list %tmv*%))
    (primitive-nounwind* module "restoreFromMultipleValue0" %void% (list +tsp*-or-tmv*+ ))
    (primitive-nounwind* module "saveValues" %void% (list %tsp*% %tmv*%))
    (primitive-nounwind* module "loadValues" %void% (list %tmv*% %tsp*%))
    
   
    (primitive-nounwind* module "progvSaveSpecials" %void% (list %i8**% %tsp*% %tsp*%))
    (primitive-nounwind* module "progvRestoreSpecials" %void% (list %i8**%))
    
    (primitive-nounwind* module "pushDynamicBinding" %void% (list %tsp*%))
    (primitive-nounwind* module "popDynamicBinding" %void% (list %tsp*%))
    
    (primitive-nounwind* module "matchKeywordOnce" %size_t% (list %tsp*% %t*% %i8*%))
    
    ;; Primitives for Cleavir code

    (primitive-nounwind* module "cc_eql" %i32% (list %t*% %t*%)) ;; eql test
    (primitive          module "cc_bad_tag" %void% (list %t*% %t*%)) ;; gf gf-args
    (primitive          module "cc_dispatch_invalid" %return_type% (list %t*% %t*%)) ;; gf gf-args
    (primitive          module "cc_dispatch_miss" %return_type% (list %t*% %t*%)) ;; gf gf-args
    (primitive          module "cc_dispatch_slot_reader"   %return_type% (list %t*% %t*% %t*%)) ; effective-method gf gf-args
    (primitive          module "cc_dispatch_slot_writer"   %return_type% (list %t*% %t*% %t*%)) ; effective-method gf gf-args
    (primitive          module "cc_dispatch_effective_method"   %return_type% (list %t*% %t*% %t*%)) ; effective-method gf gf-args
    (primitive          module "cc_dispatch_debug" %void% (list %i32% %uintptr_t%))

    (primitive-nounwind* module "cc_ensure_valid_object" %t*% (list %t*%))
    (primitive-nounwind* module "cc_getPointer" %i8*% (list %t*%))
    (primitive-nounwind* module "cc_setTmvToNil" %void% (list %tmv*%))
    (primitive-nounwind* module "cc_precalcSymbol" %t*% (list %ltv**% %size_t%))
    (primitive-nounwind* module "cc_precalcValue" %t*% (list %ltv**% %size_t%))
    (primitive-nounwind* module "cc_makeCell" %t*% nil)
    (primitive-nounwind* module "cc_writeCell" %void% (list %t*% %t*%))
    (primitive-nounwind* module "cc_readCell" %t*% (list %t*%))
    (primitive-nounwind* module "cc_t_reference" %t**% nil)
    (primitive-nounwind* module "cc_nil_reference" %t**% nil)
    (primitive-nounwind* module "cc_fetch" %t*% (list %t*% %size_t%))
;;    (primitive-nounwind* module "cc_va_arg" %t*% (list %VaList_S*%))
;;    (primitive-nounwind* module "cc_va_list_length" %size_t% (list %VaList_S*%))
    (primitive-nounwind* module "cc_copy_va_list" %void% (list %size_t% %t*[0]*% %VaList_S*%))
    
    (primitive-nounwind* module "cc_initialize_gcroots_in_module" %void% (list %gcroots-in-module*% %tsp*% %size_t% %t*%))
    (primitive-nounwind* module "cc_shutdown_gcroots_in_module" %void% (list %gcroots-in-module*% ))

    (primitive           module "cc_enclose" %t*% (list %t*% %fn-prototype*% %i32*% %size_t% %size_t% %size_t% %size_t% ) :varargs t)
    (primitive-nounwind* module "cc_stack_enclose" %t*% (list %i8*% %t*% %fn-prototype*% %i32*% %size_t% %size_t% %size_t% %size_t% ) :varargs t)
    (primitive-nounwind* module "cc_saveThreadLocalMultipleValues" %void% (list %tmv*% %mv-struct*%))
    (primitive-nounwind* module "cc_loadThreadLocalMultipleValues" %void% (list %tmv*% %mv-struct*%))
    (primitive          module "cc_safe_fdefinition" %t*% (list %t*%))
    (primitive-nounwind* module "cc_unsafe_fdefinition" %t*% (list %t*%))
    (primitive          module "cc_safe_setfdefinition" %t*% (list %t*%))
    (primitive-nounwind* module "cc_unsafe_setfdefinition" %t*% (list %t*%))
    (primitive          module "cc_safe_symbol_value" %t*% (list %t*%))
    (primitive-nounwind* module "cc_unsafe_symbol_value" %t*% (list %t*%))
    (primitive-nounwind* module "cc_setSymbolValue" %void% (list %t*% %t*%))
    
    (primitive-nounwind* module "cc_rewind_va_list" %void% (list %t*% %va_list*% %size_t*% %register-save-area*%))
    (primitive-nounwind* module "cc_push_InvocationHistoryFrame" %void% (list %t*% %InvocationHistoryFrame*% %va_list*% %size_t*%))
    (primitive-nounwind* module "cc_pop_InvocationHistoryFrame" %void% (list %t*% %InvocationHistoryFrame*%))
    
    (primitive          module "cc_call_multipleValueOneFormCall" %return_type% (list %t*%))
    (primitive          module "cc_call_multipleValueOneFormCallWithRet0" %return_type% (list %t*% %return_type%))
    (primitive          module "cc_call"   %return_type% (list* %t*% %size_t%
                                                                (make-list core:+number-of-fixed-arguments+
                                                                           :initial-element %t*%))
                        :varargs t)
    (primitive          module "cc_call_callback"   %return_type% (list* %t*% %size_t%
                                                                         (make-list core:+number-of-fixed-arguments+
                                                                                    :initial-element %t*%))
                        :varargs t)
    (primitive-nounwind* module "cc_allowOtherKeywords" %i64% (list %i64% %t*%))
    (primitive-nounwind* module "cc_matchKeywordOnce" %size_t% (list %t*% %t*% %t*%))
    (primitive          module "cc_ifNotKeywordException" %void% (list %t*% %size_t% %va_list*%))
    (primitive-nounwind* module "cc_multipleValuesArrayAddress" %t*[0]*% nil)
    (primitive          module "cc_unwind" %void% (list %t*% %size_t%))
    (primitive          module "cc_throw" %void% (list %t*%) :does-not-return t)
    (primitive-nounwind* module "cc_saveMultipleValue0" %void% (list %tmv*%))
    (primitive-nounwind* module "cc_restoreMultipleValue0" %void% (list %tmv*%))
    (primitive-nounwind* module "cc_pushLandingPadFrame" %t*% nil)
    (primitive-nounwind* module "cc_popLandingPadFrame" %void% (list %t*%))
    (primitive          module "cc_landingpadUnwindMatchFrameElseRethrow" %size_t% (list %i8*% %t*%))

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

    ;; SHORT & UNSIGNED SHORT
    (primitive          module "from_object_short" %i16% (list %t*%))
    (primitive          module "to_object_short" %t*% (list %i16%))
    (primitive          module "from_object_unsigned_short" %i16% (list %t*%))
    (primitive          module "to_object_unsigned_short" %t*% (list %i16%))

    ;; INT & UNSIGNED INT
    (primitive          module "from_object_int" %i32% (list %t*%))
    (primitive          module "to_object_int" %t*% (list %i32%))
    (primitive          module "from_object_unsigned_int" %i32% (list %t*%))
    (primitive          module "to_object_unsigned_int" %t*% (list %i32%))

    ;; LONG & UNSIGNED LONG
    (primitive          module "from_object_long" %i64% (list %t*%))
    (primitive          module "to_object_long" %t*% (list %i64%))
    (primitive          module "from_object_unsigned_long" %i64% (list %t*%))
    (primitive          module "to_object_unsigned_long" %t*% (list %i64%))

    ;; LONG LONG & UNSIGNED LONG LONG
    (primitive          module "from_object_long_long" %i64% (list %t*%))
    (primitive          module "to_object_long_long" %t*% (list %i64%))
    (primitive          module "from_object_unsigned_long_long" %i64% (list %t*%))
    (primitive          module "to_object_unsigned_long_long" %t*% (list %i64%))

    ;; INT8 & UINT8
    (primitive          module "from_object_int8" %i8% (list %t*%))
    (primitive          module "to_object_int8" %t*% (list %i8%))
    (primitive          module "from_object_uint8" %i8% (list %t*%))
    (primitive          module "to_object_uint8" %t*% (list %i8%))

    ;; INT16 & UINT16
    (primitive          module "from_object_int16" %i16% (list %t*%))
    (primitive          module "to_object_int16" %t*% (list %i16%))
    (primitive          module "from_object_uint16" %i16% (list %t*%))
    (primitive          module "to_object_uint16" %t*% (list %i16%))

    ;; INT32 & UINT32
    (primitive          module "from_object_int32" %i32% (list %t*%))
    (primitive          module "to_object_int32" %t*% (list %i32%))
    (primitive          module "from_object_uint32" %i32% (list %t*%))
    (primitive          module "to_object_uint32" %t*% (list %i32%))

    ;; INT64 & UINT64
    (primitive          module "from_object_int64" %i64% (list %t*%))
    (primitive          module "to_object_int64" %t*% (list %i64%))
    (primitive          module "from_object_uint64" %i64% (list %t*%))
    (primitive          module "to_object_uint64" %t*% (list %i64%))

    ;; i128 HANDLING NOT IMPLEMENTED AS IT IS NOT USED

    ;; SIZE_T
    (primitive          module "from_object_size" %size_t% (list %t*%))
    (primitive          module "to_object_size" %t*% (list %size_t%))

    ;; SSIZE_T
    (primitive          module "from_object_ssize" %size_t% (list %t*%))
    (primitive          module "to_object_ssize" %t*% (list %size_t%))

    ;; PTRDIFF_T, TIME_T
    ;; (primitive          module "from_object_ptrdiff" %t*% (list %t*%)) - FIXME !
    ;; (primitive          module "to_object_ptrdiff" %t*% (list %uintptr_t%)) - FIXME !

    ;; (primitive          module "from_object_time" %t*% (list %t*%)) - FIXME !
    ;; (primitive          module "to_object_time" %t*% (list %t*%)) - FIOXME !

    ;; CHAR & UNSIGNED CHAR
    (primitive          module "from_object_char" %i8% (list %t*%))
    (primitive          module "to_object_char" %t*% (list %i8%))
    (primitive          module "from_object_unsigned_char" %i8% (list %t*%))
    (primitive          module "to_object_unsigned_char" %t*% (list %i8%))

    ;; FLOAT, DOUBLE & LONG FLOAT
    (primitive          module "from_object_float" %float% (list %t*%))
    (primitive          module "to_object_float" %t*% (list %float%))
    (primitive          module "from_object_double" %double% (list %t*%))
    (primitive          module "to_object_double" %t*% (list %double%))
    #+long-float (primitive module "from_object_long_double" %long-float% (list %t*%))
    #+long-float (primitive module "to_object_long_double" %t*% (list %long-float%))

    ;; POINTER / VOID *

    ;;(format *debug-io* "~%*** +VOID+ = ~S, +VOID*+ = ~S~%" %void% %void*%)
    ;; Note: using %void*% causes an error - so we use %i64*% instead here!
    (primitive          module "from_object_pointer" %i64*% (list %t*%))
    (primitive          module "to_object_pointer" %t*% (list %i64*%))
    (primitive          module "to_object_void" %t*% (list))
    ;; === END OF TRANSLATORS ===
    *primitives*
  ))
