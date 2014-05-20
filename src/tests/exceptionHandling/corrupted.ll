; ModuleID = 'module4'
target triple = "x86_64-apple-macosx"

@globalRunTimeValuesVector = external global { i32*, {}* }*
@":::global-str-/Users/meister/Development/cando/brcl/src/tests/exceptionHandling/testcase.lsp" = internal unnamed_addr constant [79 x i8] c"/Users/meister/Development/cando/brcl/src/tests/exceptionHandling/testcase.lsp\00"
@":::global-str-lambda" = internal unnamed_addr constant [7 x i8] c"lambda\00"
@_ZTIN4core9DynamicGoE = external constant i8
@_ZTIN4core10ReturnFromE = external constant i8

; Function Attrs: nounwind
declare void @newTsp({ i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @resetTsp({ i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @makeUnboundTsp({ i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @sp_copyTsp({ i32*, {}* }*, { i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @mv_copyTsp({ i32*, {}*, i32 }*, { i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @destructTsp({ i32*, {}* }*) #0

; Function Attrs: nounwind
declare i32 @compareTsp({ i32*, {}* }*, { i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @newTmv({ i32*, {}*, i32 }*) #0

; Function Attrs: nounwind
declare void @resetTmv({ i32*, {}*, i32 }*) #0

; Function Attrs: nounwind
declare void @copyTmv({ i32*, {}*, i32 }*, { i32*, {}*, i32 }*) #0

; Function Attrs: nounwind
declare void @sp_copyTmvOrSlice({ i32*, {}* }*, { i32*, {}*, i32 }*) #0

; Function Attrs: nounwind
declare void @mv_copyTmvOrSlice({ i32*, {}*, i32 }*, { i32*, {}*, i32 }*) #0

; Function Attrs: nounwind
declare void @destructTmv({ i32*, {}*, i32 }*) #0

; Function Attrs: nounwind
declare void @newAFsp({ i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @newAFsp_ValueFrameOfSize({ i32*, {}* }*, i32) #0

; Function Attrs: nounwind
declare void @resetAFsp({ i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @copyAFsp({ i32*, {}* }*, { i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @destructAFsp({ i32*, {}* }*) #0

; Function Attrs: nounwind
declare i32 @isNilTsp({ i32*, {}* }*) #0

; Function Attrs: nounwind
declare i32 @isTrueTsp({ i32*, {}* }*) #0

; Function Attrs: nounwind
declare i32 @isBoundTsp({ i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @internSymbol_tsp({ i32*, {}* }*, i8*, i8*) #0

; Function Attrs: nounwind
declare void @makeSymbol_tsp({ i32*, {}* }*, i8*) #0

; Function Attrs: nounwind
declare void @internSymbol_symsp({ i32*, i32* }*, i8*, i8*) #0

; Function Attrs: nounwind
declare void @makeSymbol_symsp({ i32*, i32* }*, i8*) #0

; Function Attrs: nounwind
declare void @sp_makeNil({ i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @mv_makeNil({ i32*, {}*, i32 }*) #0

; Function Attrs: nounwind
declare void @makeT({ i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @makeCons({ i32*, {}* }*, { i32*, {}* }*, { i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @makeFixnum({ i32*, {}* }*, i32) #0

; Function Attrs: nounwind
declare void @makeCharacter({ i32*, {}* }*, i32) #0

; Function Attrs: nounwind
declare void @makeBignum({ i32*, {}* }*, i8*) #0

; Function Attrs: nounwind
declare void @makeShortFloat({ i32*, {}* }*, double) #0

; Function Attrs: nounwind
declare void @makeSingleFloat({ i32*, {}* }*, double) #0

; Function Attrs: nounwind
declare void @makeDoubleFloat({ i32*, {}* }*, double) #0

; Function Attrs: nounwind
declare void @makeLongFloat({ i32*, {}* }*, double) #0

; Function Attrs: nounwind
declare void @makeString({ i32*, {}* }*, i8*) #0

declare void @sp_makeCompiledFunction({ i32*, {}* }*, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)*, i8*, { i32*, {}* }*, { i32*, {}* }*, { i32*, {}* }*)

declare void @mv_makeCompiledFunction({ i32*, {}*, i32 }*, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)*, i8*, { i32*, {}* }*, { i32*, {}* }*, { i32*, {}* }*)

declare void @fillRestTarget({ i32*, {}* }*, { i32*, {}* }*, i32, i8*)

; Function Attrs: nounwind
declare void @sp_symbolValueRead({ i32*, {}* }*, { i32*, i32* }*) #0

; Function Attrs: nounwind
declare void @mv_symbolValueRead({ i32*, {}*, i32 }*, { i32*, i32* }*) #0

; Function Attrs: nounwind
declare void @sp_symbolValueReadOrUnbound({ i32*, {}* }*, { i32*, i32* }*) #0

; Function Attrs: nounwind
declare void @mv_symbolValueReadOrUnbound({ i32*, {}*, i32 }*, { i32*, i32* }*) #0

; Function Attrs: nounwind
declare { i32*, {}* }* @symbolValueReference({ i32*, i32* }*) #0

; Function Attrs: nounwind
declare { i32*, {}* }* @lexicalValueReference(i32, i32, { i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @sp_lexicalValueRead({ i32*, {}* }*, i32, i32, { i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @mv_lexicalValueRead({ i32*, {}*, i32 }*, i32, i32, { i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @sp_symbolFunctionRead({ i32*, {}* }*, { i32*, i32* }*) #0

; Function Attrs: nounwind
declare void @mv_symbolFunctionRead({ i32*, {}*, i32 }*, { i32*, i32* }*) #0

; Function Attrs: nounwind
declare void @setfSymbolFunctionRead({ i32*, {}* }*, { i32*, i32* }*) #0

; Function Attrs: nounwind
declare void @sp_lexicalFunctionRead({ i32*, {}* }*, i32, i32, { i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @mv_lexicalFunctionRead({ i32*, {}*, i32 }*, i32, i32, { i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @makeTagbodyFrame({ i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @makeValueFrame({ i32*, {}* }*, i32, i32) #0

; Function Attrs: nounwind
declare void @makeValueFrameFromReversedCons({ i32*, {}* }*, { i32*, {}* }*, i32) #0

; Function Attrs: nounwind
declare void @setParentOfActivationFrame({ i32*, {}* }*, { i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @attachDebuggingInfoToValueFrame({ i32*, {}* }*, { i32*, {}* }*) #0

; Function Attrs: nounwind
declare { i32*, {}* }* @valueFrameReference({ i32*, {}* }*, i32) #0

declare void @makeFunctionFrame({ i32*, {}* }*, i32, { i32*, {}* }*)

declare { i32*, {}* }* @functionFrameReference({ i32*, {}* }*, i32)

declare void @sp_prependMultipleValues({ i32*, {}* }*, { i32*, {}*, i32 }*)

declare void @mv_prependMultipleValues({ i32*, {}*, i32 }*, { i32*, {}*, i32 }*)

declare void @invokePossibleMultipleValueFunction({ i32*, {}*, i32 }*, { i32*, {}* }*, { i32*, {}* }*)

declare void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }*, { i32*, i32* }*, { i32*, {}* }*)

declare void @mv_invokePossibleMultipleValueSymbolFunction({ i32*, {}*, i32 }*, { i32*, i32* }*, { i32*, {}* }*)

declare void @sp_invokePossibleMultipleValueLexicalFunction({ i32*, {}* }*, i32, i32, { i32*, {}* }*)

declare void @mv_invokePossibleMultipleValueLexicalFunction({ i32*, {}*, i32 }*, i32, i32, { i32*, {}* }*)

declare void @invokeLlvmFunction({ i32*, {}*, i32 }*, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)*, { i32*, {}* }*)

declare void @invokeLlvmFunctionVoid(void ()*)

declare void @invokeFASLLlvmFunctionVoid(void ()*, i8*)

; Function Attrs: nounwind
declare { i32*, {}* }* @activationFrameNil() #0

; Function Attrs: nounwind
declare i32 @activationFrameSize({ i32*, {}* }*) #0

; Function Attrs: nounwind
declare { i32*, {}* }* @activationFrameParentRef({ i32*, {}* }*) #0

declare void @throwTooManyArgumentsException(i8*, { i32*, {}* }*, i32, i32)

declare void @throwNotEnoughArgumentsException(i8*, { i32*, {}* }*, i32, i32)

declare void @throwIfExcessKeywordArguments(i8*, { i32*, {}* }*, i32)

; Function Attrs: nounwind
declare i32 @kw_allowOtherKeywords(i32, { i32*, {}* }*, i32) #0

; Function Attrs: nounwind
declare i32 @kw_trackFirstUnexpectedKeyword(i32, i32) #0

declare void @kw_throwIfBadKeywordArgument(i32, i32, { i32*, {}* }*)

declare void @kw_throwIfNotKeyword({ i32*, {}* }*)

; Function Attrs: nounwind
declare void @gdb() #0

; Function Attrs: nounwind
declare void @debugInvoke() #0

; Function Attrs: nounwind
declare void @debugInspectActivationFrame({ i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @debugInspectObject_sp({ i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @debugInspectObject_mv({ i32*, {}*, i32 }*) #0

; Function Attrs: nounwind
declare void @debugPointer(i8*) #0

; Function Attrs: nounwind
declare void @debugPrintObject(i8*, { i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @debugPrintI32(i32) #0

; Function Attrs: nounwind
declare void @lowLevelTrace(i32) #0

; Function Attrs: nounwind
declare void @unreachableError() #0

; Function Attrs: nounwind
declare void @singleStepCallback() #0

; Function Attrs: nounwind
declare void @trace_setActivationFrameForIHSTop({ i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @trace_setLineNumberColumnForIHSTop(i32, i32) #0

; Function Attrs: nounwind
declare void @trace_exitFunctionScope(i32) #0

; Function Attrs: nounwind
declare void @trace_exitBlockScope(i32) #0

; Function Attrs: nounwind
declare void @trace_exitLetScope(i32) #0

; Function Attrs: nounwind
declare void @trace_exitLetSTARScope(i32) #0

; Function Attrs: nounwind
declare void @trace_exitFletScope(i32) #0

; Function Attrs: nounwind
declare void @trace_exitLabelsScope(i32) #0

; Function Attrs: nounwind
declare void @trace_exitCallScope(i32) #0

; Function Attrs: nounwind
declare void @trace_exitCatchScope(i32) #0

; Function Attrs: nounwind
declare void @trace_exitUnwindProtectScope(i32) #0

; Function Attrs: noreturn
declare void @throwCatchThrow({ i32*, {}* }*, { i32*, {}*, i32 }*) #1

; Function Attrs: noreturn
declare void @throwReturnFrom(i32, { i32*, {}*, i32 }*) #1

; Function Attrs: nounwind
declare void @catchStoreTag({ i32*, {}* }*, { i32*, {}* }*) #0

declare void @sp_catchIfTagMatchesStoreResultElseRethrow({ i32*, {}* }*, { i32*, {}* }*, i8*)

declare void @mv_catchIfTagMatchesStoreResultElseRethrow({ i32*, {}*, i32 }*, { i32*, {}* }*, i8*)

; Function Attrs: nounwind
declare void @catchUnwind({ i32*, {}* }*) #0

declare void @sp_blockHandleReturnFrom({ i32*, {}* }*, i8*)

declare void @mv_blockHandleReturnFrom({ i32*, {}*, i32 }*, i8*)

; Function Attrs: noreturn
declare void @throw_DynamicGo(i32, i32, { i32*, {}* }*) #1

declare i32 @tagbodyDynamicGoIndexElseRethrow({ i32*, {}* }*, i8*)

; Function Attrs: noreturn
declare void @throwIllegalSwitchValue(i32, i32) #1

; Function Attrs: nounwind
declare void @brcl_terminate(i8*, i32, i32, i8*) #0

; Function Attrs: nounwind
declare i32 @__gxx_personality_v0(...) #0

; Function Attrs: nounwind
declare i8* @__cxa_begin_catch(i8*) #0

declare void @__cxa_end_catch()

declare void @__cxa_rethrow()

; Function Attrs: nounwind readnone
declare i32 @llvm.eh.typeid.for(i8*) #2

; Function Attrs: nounwind
declare void @getOrCreateLoadTimeValueArray({ i32*, {}* }**, i8*, i32, i32) #0

; Function Attrs: nounwind
declare void @sp_copyLoadTimeValue({ i32*, {}* }*, { i32*, {}* }**, i32) #0

; Function Attrs: nounwind
declare void @mv_copyLoadTimeValue({ i32*, {}*, i32 }*, { i32*, {}* }**, i32) #0

; Function Attrs: nounwind
declare { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }**, i32) #0

; Function Attrs: nounwind
declare { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }**, i32) #0

; Function Attrs: nounwind
declare void @sp_getLoadTimeValue({ i32*, {}* }*, { i32*, {}* }**, i32) #0

; Function Attrs: nounwind
declare void @mv_getLoadTimeValue({ i32*, {}*, i32 }*, { i32*, {}* }**, i32) #0

; Function Attrs: nounwind
declare void @dumpLoadTimeValues({ i32*, {}* }**) #0

; Function Attrs: nounwind
declare void @ltv_makeCons({ i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @ltv_makeSourceCodeCons({ i32*, {}* }*, i8*, i32, i32) #0

; Function Attrs: nounwind
declare void @ltv_makeArrayObjects({ i32*, {}* }*, { i32*, {}* }*, i32, i32*) #0

; Function Attrs: nounwind
declare void @ltv_makeHashTable({ i32*, {}* }*, { i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @rplaca({ i32*, {}* }*, { i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @rplacd({ i32*, {}* }*, { i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @ltv_initializeArrayObjectsRowMajorArefOrder({ i32*, {}* }*, { i32*, {}* }**, i32*) #0

; Function Attrs: nounwind
declare void @ltv_initializeHashTable({ i32*, {}* }*, i32, { i32*, {}* }**, i32*) #0

; Function Attrs: nounwind
declare void @saveValues({ i32*, {}* }*, { i32*, {}*, i32 }*) #0

; Function Attrs: nounwind
declare void @loadValues({ i32*, {}*, i32 }*, { i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @setjmp_set_jump_address({ i8*, i8*, i8*, i8*, i8* }*, i8*) #0

; Function Attrs: nounwind
declare void @setjmp_user0_set_i32({ i8*, i8*, i8*, i8*, i8* }*, i32) #0

; Function Attrs: nounwind
declare i32 @setjmp_user0_get_i32({ i8*, i8*, i8*, i8*, i8* }*) #0

; Function Attrs: nounwind
declare void @setjmp_user0_allocate_set_tmv({ i8*, i8*, i8*, i8*, i8* }*, { i32*, {}*, i32 }*) #0

; Function Attrs: nounwind
declare void @setjmp_user0_get_tmv({ i32*, {}*, i32 }*, { i8*, i8*, i8*, i8*, i8* }*) #0

; Function Attrs: nounwind
declare void @setjmp_user0_delete_tmv({ i8*, i8*, i8*, i8*, i8* }*) #0

; Function Attrs: nounwind
declare i32 @llvm.eh.sjlj.setjmp(i8*) #0

; Function Attrs: noreturn nounwind
declare void @llvm.eh.sjlj.longjmp(i8*) #3

; Function Attrs: nounwind
declare void @progvSaveSpecials(i8**, { i32*, {}* }*, { i32*, {}* }*) #0

; Function Attrs: nounwind
declare void @progvRestoreSpecials(i8**) #0

; Function Attrs: nounwind
declare void @pushDynamicBinding({ i32*, i32* }*) #0

; Function Attrs: nounwind
declare void @popDynamicBinding({ i32*, i32* }*) #0

; Function Attrs: nounwind
declare i32 @matchKeywordOnce({ i32*, {}* }*, { i32*, {}* }*, i8*) #0

define internal void @lambda({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*
  %ehselector.slot = alloca i32
  store i32 0, i32* %ehselector.slot
  %lambda-args-3- = alloca { i32*, {}* }
  call void @newAFsp({ i32*, {}* }* %lambda-args-3-)
  %temp = alloca { i32*, {}* }
  call void @newTsp({ i32*, {}* }* %temp)
  %temp5 = alloca { i32*, {}* }
  call void @newTsp({ i32*, {}* }* %temp5)
  %"LET*" = alloca { i32*, {}* }
  call void @newAFsp({ i32*, {}* }* %"LET*")
  %temp8 = alloca { i32*, {}* }
  call void @newTsp({ i32*, {}* }* %temp8)
  %tagbody-frame = alloca { i32*, {}* }
  call void @newAFsp({ i32*, {}* }* %tagbody-frame)
  %temp10 = alloca { i32*, {}* }
  call void @newTsp({ i32*, {}* }* %temp10)
  %temp11 = alloca { i32*, {}* }
  call void @newTsp({ i32*, {}* }* %temp11)
  %tsetq = alloca { i32*, {}* }
  call void @newTsp({ i32*, {}* }* %tsetq)
  %call-args = alloca { i32*, {}* }
  call void @newAFsp({ i32*, {}* }* %call-args)
  %call-args13 = alloca { i32*, {}* }
  call void @newAFsp({ i32*, {}* }* %call-args13)
  %call-args14 = alloca { i32*, {}* }
  call void @newAFsp({ i32*, {}* }* %call-args14)
  %call-args16 = alloca { i32*, {}* }
  call void @newAFsp({ i32*, {}* }* %call-args16)
  %tsetq19 = alloca { i32*, {}* }
  call void @newTsp({ i32*, {}* }* %tsetq19)
  %call-args20 = alloca { i32*, {}* }
  call void @newAFsp({ i32*, {}* }* %call-args20)
  %temp23 = alloca { i32*, {}* }
  call void @newTsp({ i32*, {}* }* %temp23)
  %if-cond-tsp = alloca { i32*, {}* }
  call void @newTsp({ i32*, {}* }* %if-cond-tsp)
  %temp24 = alloca { i32*, {}* }
  call void @newTsp({ i32*, {}* }* %temp24)
  br label %body

body:                                             ; preds = %entry
  %given-num-args = call i32 @activationFrameSize({ i32*, {}* }* %activation-frame-ptr)
  %correct-num-args = icmp ne i32 %given-num-args, 0
  br i1 %correct-num-args, label %error, label %continue3

error:                                            ; preds = %body
  %enough-args = icmp slt i32 %given-num-args, 0
  br i1 %enough-args, label %error1, label %continue

error1:                                           ; preds = %error
  invoke void @throwNotEnoughArgumentsException(i8* getelementptr inbounds ([7 x i8]* @":::global-str-lambda", i32 0, i32 0), { i32*, {}* }* %activation-frame-ptr, i32 %given-num-args, i32 0)
          to label %normal-dest unwind label %func-cleanup-landing-pad

normal-dest:                                      ; preds = %error1
  call void @unreachableError()
  unreachable

continue:                                         ; preds = %error
  invoke void @throwTooManyArgumentsException(i8* getelementptr inbounds ([7 x i8]* @":::global-str-lambda", i32 0, i32 0), { i32*, {}* }* %activation-frame-ptr, i32 %given-num-args, i32 0)
          to label %normal-dest2 unwind label %func-cleanup-landing-pad

normal-dest2:                                     ; preds = %continue
  call void @unreachableError()
  unreachable

continue3:                                        ; preds = %body
  call void @copyAFsp({ i32*, {}* }* %lambda-args-3-, { i32*, {}* }* %activation-frame-ptr)
  %value = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @globalRunTimeValuesVector, i32 7)
  call void @attachDebuggingInfoToValueFrame({ i32*, {}* }* %lambda-args-3-, { i32*, {}* }* %value)
  br label %"(TRY-0).top"

"(TRY-0).top":                                    ; preds = %continue3
  br label %"(TRY-0).block-NIL-start"

"(TRY-0).block-NIL-start":                        ; preds = %"(TRY-0).top"
  br label %"(TRY-0).top4"

"(TRY-0).top4":                                   ; preds = %"(TRY-0).block-NIL-start"
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %lambda-args-3-)
  br label %"(TRY-0).top6"

"(TRY-0).top6":                                   ; preds = %"(TRY-0).top4"
  br label %"(TRY-0).LET*-start"

"(TRY-0).LET*-start":                             ; preds = %"(TRY-0).top6"
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %"LET*")
  call void @makeValueFrame({ i32*, {}* }* %"LET*", i32 2, i32 2000002)
  call void @setParentOfActivationFrame({ i32*, {}* }* %"LET*", { i32*, {}* }* %lambda-args-3-)
  %value7 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @globalRunTimeValuesVector, i32 8)
  call void @attachDebuggingInfoToValueFrame({ i32*, {}* }* %"LET*", { i32*, {}* }* %value7)
  %0 = call { i32*, {}* }* @lexicalValueReference(i32 0, i32 0, { i32*, {}* }* %"LET*")
  %"SYMBOL->+KNOWN-TYPEP-PREDICATES+" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @globalRunTimeValuesVector, i32 2)
  call void @sp_symbolValueRead({ i32*, {}* }* %0, { i32*, i32* }* %"SYMBOL->+KNOWN-TYPEP-PREDICATES+")
  %1 = call { i32*, {}* }* @lexicalValueReference(i32 0, i32 1, { i32*, {}* }* %"LET*")
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %1, { i32*, {}* }** @globalRunTimeValuesVector, i32 0)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %"LET*")
  call void @singleStepCallback()
  call void @makeTagbodyFrame({ i32*, {}* }* %tagbody-frame)
  call void @setParentOfActivationFrame({ i32*, {}* }* %tagbody-frame, { i32*, {}* }* %"LET*")
  br label %"(TRY-0).top9"

"(TRY-0).top9":                                   ; preds = %"(TRY-0).LET*-start"
  br label %"(TRY-0).tagbody-#:G6269-0"

"(TRY-0).tagbody-#:G6269-0":                      ; preds = %"(TRY-0).normal-dest27", %"(TRY-0).top9"
  invoke void @throw_DynamicGo(i32 0, i32 2, { i32*, {}* }* %tagbody-frame)
          to label %"(TRY-0).normal-dest" unwind label %"(TRY-0).landing-pad"

"(TRY-0).normal-dest":                            ; preds = %"(TRY-0).tagbody-#:G6269-0"
  call void @unreachableError()
  unreachable

"(TRY-0).from-invoke-that-never-returns":         ; No predecessors!
  br label %"(TRY-0).tagbody-#:G6262-1"

"(TRY-0).tagbody-#:G6262-1":                      ; preds = %"(TRY-0).normal-dest27", %"(TRY-0).from-invoke-that-never-returns"
  %2 = call { i32*, {}* }* @lexicalValueReference(i32 1, i32 1, { i32*, {}* }* %tagbody-frame)
  call void @makeValueFrame({ i32*, {}* }* %call-args, i32 1, i32 2000003)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args, { i32*, {}* }* %tagbody-frame)
  %call-args-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 0)
  call void @sp_lexicalValueRead({ i32*, {}* }* %call-args-ref-0, i32 1, i32 0, { i32*, {}* }* %tagbody-frame)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:FIRST" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @globalRunTimeValuesVector, i32 3)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %tsetq, { i32*, i32* }* %"SYMBOL->COMMON-LISP:FIRST", { i32*, {}* }* %call-args)
          to label %"(TRY-0).normal-dest12" unwind label %"(TRY-0).landing-pad"

"(TRY-0).normal-dest12":                          ; preds = %"(TRY-0).tagbody-#:G6262-1"
  call void @sp_copyTsp({ i32*, {}* }* %2, { i32*, {}* }* %tsetq)
  call void @sp_copyTsp({ i32*, {}* }* %temp11, { i32*, {}* }* %tsetq)
  call void @trace_setLineNumberColumnForIHSTop(i32 8, i32 3)
  call void @makeValueFrame({ i32*, {}* }* %call-args13, i32 3, i32 2000004)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args13, { i32*, {}* }* %tagbody-frame)
  %call-args13-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args13, i32 0)
  call void @trace_setLineNumberColumnForIHSTop(i32 8, i32 16)
  call void @makeValueFrame({ i32*, {}* }* %call-args14, i32 1, i32 2000005)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args14, { i32*, {}* }* %tagbody-frame)
  %call-args14-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args14, i32 0)
  call void @sp_lexicalValueRead({ i32*, {}* }* %call-args14-ref-0, i32 1, i32 1, { i32*, {}* }* %tagbody-frame)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:CAR" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @globalRunTimeValuesVector, i32 4)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %call-args13-ref-0, { i32*, i32* }* %"SYMBOL->COMMON-LISP:CAR", { i32*, {}* }* %call-args14)
          to label %"(TRY-0).normal-dest15" unwind label %"(TRY-0).landing-pad"

"(TRY-0).normal-dest15":                          ; preds = %"(TRY-0).normal-dest12"
  %call-args13-ref-1 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args13, i32 1)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args13-ref-1, { i32*, {}* }** @globalRunTimeValuesVector, i32 9)
  %call-args13-ref-2 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args13, i32 2)
  call void @trace_setLineNumberColumnForIHSTop(i32 8, i32 40)
  call void @makeValueFrame({ i32*, {}* }* %call-args16, i32 1, i32 2000006)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args16, { i32*, {}* }* %tagbody-frame)
  %call-args16-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args16, i32 0)
  call void @sp_lexicalValueRead({ i32*, {}* }* %call-args16-ref-0, i32 1, i32 1, { i32*, {}* }* %tagbody-frame)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:CDR" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @globalRunTimeValuesVector, i32 5)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %call-args13-ref-2, { i32*, i32* }* %"SYMBOL->COMMON-LISP:CDR", { i32*, {}* }* %call-args16)
          to label %"(TRY-0).normal-dest17" unwind label %"(TRY-0).landing-pad"

"(TRY-0).normal-dest17":                          ; preds = %"(TRY-0).normal-dest15"
  call void @singleStepCallback()
  %"SYMBOL->PUT-SYSPROP" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @globalRunTimeValuesVector, i32 6)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %temp11, { i32*, i32* }* %"SYMBOL->PUT-SYSPROP", { i32*, {}* }* %call-args13)
          to label %"(TRY-0).normal-dest18" unwind label %"(TRY-0).landing-pad"

"(TRY-0).normal-dest18":                          ; preds = %"(TRY-0).normal-dest17"
  %3 = call { i32*, {}* }* @lexicalValueReference(i32 1, i32 0, { i32*, {}* }* %tagbody-frame)
  call void @makeValueFrame({ i32*, {}* }* %call-args20, i32 1, i32 2000007)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args20, { i32*, {}* }* %tagbody-frame)
  %call-args20-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args20, i32 0)
  call void @sp_lexicalValueRead({ i32*, {}* }* %call-args20-ref-0, i32 1, i32 0, { i32*, {}* }* %tagbody-frame)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:CDR21" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @globalRunTimeValuesVector, i32 5)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %tsetq19, { i32*, i32* }* %"SYMBOL->COMMON-LISP:CDR21", { i32*, {}* }* %call-args20)
          to label %"(TRY-0).normal-dest22" unwind label %"(TRY-0).landing-pad"

"(TRY-0).normal-dest22":                          ; preds = %"(TRY-0).normal-dest18"
  call void @sp_copyTsp({ i32*, {}* }* %3, { i32*, {}* }* %tsetq19)
  call void @sp_copyTsp({ i32*, {}* }* %temp8, { i32*, {}* }* %tsetq19)
  br label %"(TRY-0).tagbody-#:G6263-2"

"(TRY-0).tagbody-#:G6263-2":                      ; preds = %"(TRY-0).normal-dest27", %"(TRY-0).normal-dest22"
  call void @sp_lexicalValueRead({ i32*, {}* }* %if-cond-tsp, i32 1, i32 0, { i32*, {}* }* %tagbody-frame)
  %4 = call i32 @isTrueTsp({ i32*, {}* }* %if-cond-tsp)
  %ifcond = icmp eq i32 %4, 1
  br i1 %ifcond, label %"(TRY-0).then", label %"(TRY-0).else"

"(TRY-0).then":                                   ; preds = %"(TRY-0).tagbody-#:G6263-2"
  invoke void @throw_DynamicGo(i32 0, i32 1, { i32*, {}* }* %tagbody-frame)
          to label %"(TRY-0).normal-dest25" unwind label %"(TRY-0).landing-pad"

"(TRY-0).normal-dest25":                          ; preds = %"(TRY-0).then"
  call void @unreachableError()
  unreachable

"(TRY-0).from-invoke-that-never-returns26":       ; No predecessors!
  br label %"(TRY-0).ifcont"

"(TRY-0).else":                                   ; preds = %"(TRY-0).tagbody-#:G6263-2"
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %temp8, { i32*, {}* }** @globalRunTimeValuesVector, i32 0)
  br label %"(TRY-0).ifcont"

"(TRY-0).ifcont":                                 ; preds = %"(TRY-0).else", %"(TRY-0).from-invoke-that-never-returns26"
  br label %"(TRY-0).normal-cleanup"

"(TRY-0).normal-cleanup":                         ; preds = %"(TRY-0).ifcont"
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %temp8, { i32*, {}* }** @globalRunTimeValuesVector, i32 0)
  br label %"(TRY-0).try-cont"

"(TRY-0).landing-pad":                            ; preds = %"(TRY-0).then", %"(TRY-0).normal-dest18", %"(TRY-0).normal-dest17", %"(TRY-0).normal-dest15", %"(TRY-0).normal-dest12", %"(TRY-0).tagbody-#:G6262-1", %"(TRY-0).tagbody-#:G6269-0"
  %5 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
          catch i8* @_ZTIN4core9DynamicGoE
          catch i8* @_ZTIN4core10ReturnFromE
  %6 = extractvalue { i8*, i32 } %5, 0
  store i8* %6, i8** %exn.slot
  %7 = extractvalue { i8*, i32 } %5, 1
  store i32 %7, i32* %ehselector.slot
  br label %"(TRY-0).dispatch-header"

"(TRY-0).dispatch-header":                        ; preds = %"(TRY-0).landing-pad"
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %temp8, { i32*, {}* }** @globalRunTimeValuesVector, i32 0)
  br label %"(TRY-0).dispatch-TYPEID-CORE-DYNAMIC-GO-6302"

"(TRY-0).dispatch-TYPEID-CORE-DYNAMIC-GO-6302":   ; preds = %"(TRY-0).dispatch-header"
  %ehselector-slot = load i32* %ehselector.slot
  %8 = call i32 @llvm.eh.typeid.for(i8* @_ZTIN4core9DynamicGoE)
  %9 = icmp eq i32 %ehselector-slot, %8
  br i1 %9, label %"(TRY-0).handler-block6306", label %"(TRY-0).dispatch-header33"

"(TRY-0).handler-block6306":                      ; preds = %"(TRY-0).dispatch-TYPEID-CORE-DYNAMIC-GO-6302"
  %exn = load i8** %exn.slot
  %10 = call i8* @__cxa_begin_catch(i8* %exn)
  %11 = invoke i32 @tagbodyDynamicGoIndexElseRethrow({ i32*, {}* }* %tagbody-frame, i8* %10)
          to label %"(TRY-0).normal-dest27" unwind label %"(TRY-0).landing-pad32"

"(TRY-0).normal-dest27":                          ; preds = %"(TRY-0).handler-block6306"
  switch i32 %11, label %"(TRY-0).switch-default" [
    i32 0, label %"(TRY-0).tagbody-#:G6269-0"
    i32 1, label %"(TRY-0).tagbody-#:G6262-1"
    i32 2, label %"(TRY-0).tagbody-#:G6263-2"
  ]

"(TRY-0).switch-default":                         ; preds = %"(TRY-0).normal-dest27"
  invoke void @throwIllegalSwitchValue(i32 %11, i32 3)
          to label %"(TRY-0).normal-dest28" unwind label %"(TRY-0).landing-pad32"

"(TRY-0).normal-dest28":                          ; preds = %"(TRY-0).switch-default"
  call void @unreachableError()
  unreachable

"(TRY-0).from-invoke-that-never-returns29":       ; No predecessors!
  invoke void @__cxa_end_catch()
          to label %"(TRY-0).normal-dest30" unwind label %"(TRY-0).landing-pad32"

"(TRY-0).normal-dest30":                          ; preds = %"(TRY-0).from-invoke-that-never-returns29"
  br label %"(TRY-0).try-cont"

"(TRY-0).try-cont":                               ; preds = %"(TRY-0).normal-dest30", %"(TRY-0).normal-cleanup"
  call void @mv_copyLoadTimeValue({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }** @globalRunTimeValuesVector, i32 0)
  br label %"(TRY-0).normal-cleanup31"

"(TRY-0).normal-cleanup31":                       ; preds = %"(TRY-0).try-cont"
  br label %"(TRY-0).try-cont34"

"(TRY-0).landing-pad32":                          ; preds = %"(TRY-0).from-invoke-that-never-returns29", %"(TRY-0).switch-default", %"(TRY-0).handler-block6306"
  %12 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
          catch i8* @_ZTIN4core10ReturnFromE
  %13 = extractvalue { i8*, i32 } %12, 0
  store i8* %13, i8** %exn.slot
  %14 = extractvalue { i8*, i32 } %12, 1
  store i32 %14, i32* %ehselector.slot
  br label %"(TRY-0).dispatch-header33"

"(TRY-0).dispatch-header33":                      ; preds = %"(TRY-0).landing-pad32", %"(TRY-0).dispatch-TYPEID-CORE-DYNAMIC-GO-6302"
  br label %"(TRY-0).dispatch-header37"

"(TRY-0).try-cont34":                             ; preds = %"(TRY-0).normal-cleanup31"
  br label %"(TRY-0).normal-cleanup35"

"(TRY-0).normal-cleanup35":                       ; preds = %"(TRY-0).try-cont34"
  br label %"(TRY-0).try-cont42"

"(TRY-0).landing-pad36":                          ; No predecessors!
  %15 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
          catch i8* @_ZTIN4core10ReturnFromE
  %16 = extractvalue { i8*, i32 } %15, 0
  store i8* %16, i8** %exn.slot
  %17 = extractvalue { i8*, i32 } %15, 1
  store i32 %17, i32* %ehselector.slot
  br label %"(TRY-0).dispatch-header37"

"(TRY-0).dispatch-header37":                      ; preds = %"(TRY-0).landing-pad36", %"(TRY-0).dispatch-header33"
  br label %"(TRY-0).dispatch-TYPEID-CORE-RETURN-FROM-6155"

"(TRY-0).dispatch-TYPEID-CORE-RETURN-FROM-6155":  ; preds = %"(TRY-0).dispatch-header37"
  %ehselector-slot38 = load i32* %ehselector.slot
  %18 = call i32 @llvm.eh.typeid.for(i8* @_ZTIN4core10ReturnFromE)
  %19 = icmp eq i32 %ehselector-slot38, %18
  br i1 %19, label %"(TRY-0).handler-block6159", label %"(TRY-0).dispatch-header45"

"(TRY-0).handler-block6159":                      ; preds = %"(TRY-0).dispatch-TYPEID-CORE-RETURN-FROM-6155"
  %exn39 = load i8** %exn.slot
  %20 = call i8* @__cxa_begin_catch(i8* %exn39)
  invoke void @mv_blockHandleReturnFrom({ i32*, {}*, i32 }* %result-ptr, i8* %20)
          to label %"(TRY-0).normal-dest40" unwind label %"(TRY-0).landing-pad44"

"(TRY-0).normal-dest40":                          ; preds = %"(TRY-0).handler-block6159"
  invoke void @__cxa_end_catch()
          to label %"(TRY-0).normal-dest41" unwind label %"(TRY-0).landing-pad44"

"(TRY-0).normal-dest41":                          ; preds = %"(TRY-0).normal-dest40"
  br label %"(TRY-0).try-cont42"

"(TRY-0).try-cont42":                             ; preds = %"(TRY-0).normal-dest41", %"(TRY-0).normal-cleanup35"
  br label %"(TRY-0).normal-cleanup43"

"(TRY-0).normal-cleanup43":                       ; preds = %"(TRY-0).try-cont42"
  br label %"(TRY-0).try-cont46"

"(TRY-0).landing-pad44":                          ; preds = %"(TRY-0).normal-dest40", %"(TRY-0).handler-block6159"
  %21 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %22 = extractvalue { i8*, i32 } %21, 0
  store i8* %22, i8** %exn.slot
  %23 = extractvalue { i8*, i32 } %21, 1
  store i32 %23, i32* %ehselector.slot
  br label %"(TRY-0).dispatch-header45"

"(TRY-0).dispatch-header45":                      ; preds = %"(TRY-0).landing-pad44", %"(TRY-0).dispatch-TYPEID-CORE-RETURN-FROM-6155"
  br label %func-ehcleanup

"(TRY-0).try-cont46":                             ; preds = %"(TRY-0).normal-cleanup43"
  br label %return-block

func-cleanup-landing-pad:                         ; preds = %continue, %error1
  %24 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %25 = extractvalue { i8*, i32 } %24, 0
  store i8* %25, i8** %exn.slot
  %26 = extractvalue { i8*, i32 } %24, 1
  store i32 %26, i32* %ehselector.slot
  br label %func-ehcleanup

func-ehcleanup:                                   ; preds = %func-cleanup-landing-pad, %"(TRY-0).dispatch-header45"
  call void @destructTsp({ i32*, {}* }* %temp24)
  call void @destructTsp({ i32*, {}* }* %if-cond-tsp)
  call void @destructTsp({ i32*, {}* }* %temp23)
  call void @destructAFsp({ i32*, {}* }* %call-args20)
  call void @destructTsp({ i32*, {}* }* %tsetq19)
  call void @destructAFsp({ i32*, {}* }* %call-args16)
  call void @destructAFsp({ i32*, {}* }* %call-args14)
  call void @destructAFsp({ i32*, {}* }* %call-args13)
  call void @destructAFsp({ i32*, {}* }* %call-args)
  call void @destructTsp({ i32*, {}* }* %tsetq)
  call void @destructTsp({ i32*, {}* }* %temp11)
  call void @destructTsp({ i32*, {}* }* %temp10)
  call void @destructAFsp({ i32*, {}* }* %tagbody-frame)
  call void @destructTsp({ i32*, {}* }* %temp8)
  call void @destructAFsp({ i32*, {}* }* %"LET*")
  call void @destructTsp({ i32*, {}* }* %temp5)
  call void @destructTsp({ i32*, {}* }* %temp)
  call void @destructAFsp({ i32*, {}* }* %lambda-args-3-)
  br label %func-ehresume

func-ehresume:                                    ; preds = %func-ehcleanup
  %exn7 = load i8** %exn.slot
  %sel = load i32* %ehselector.slot
  %lpad.val = insertvalue { i8*, i32 } undef, i8* %exn7, 0
  call void @debugPrintI32(i32 90)
  %lpad.val8 = insertvalue { i8*, i32 } %lpad.val, i32 %sel, 1
  call void @debugPrintI32(i32 91)
  resume { i8*, i32 } %lpad.val8

func-terminate-lpad:                              ; No predecessors!
  %27 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          catch i8* null
  call void @brcl_terminate(i8* getelementptr inbounds ([79 x i8]* @":::global-str-/Users/meister/Development/cando/brcl/src/tests/exceptionHandling/testcase.lsp", i32 0, i32 0), i32 1, i32 0, i8* getelementptr inbounds ([7 x i8]* @":::global-str-lambda", i32 0, i32 0))
  call void @unreachableError()
  unreachable

return-block:                                     ; preds = %"(TRY-0).try-cont46"
  call void @destructTsp({ i32*, {}* }* %temp24)
  call void @destructTsp({ i32*, {}* }* %if-cond-tsp)
  call void @destructTsp({ i32*, {}* }* %temp23)
  call void @destructAFsp({ i32*, {}* }* %call-args20)
  call void @destructTsp({ i32*, {}* }* %tsetq19)
  call void @destructAFsp({ i32*, {}* }* %call-args16)
  call void @destructAFsp({ i32*, {}* }* %call-args14)
  call void @destructAFsp({ i32*, {}* }* %call-args13)
  call void @destructAFsp({ i32*, {}* }* %call-args)
  call void @destructTsp({ i32*, {}* }* %tsetq)
  call void @destructTsp({ i32*, {}* }* %temp11)
  call void @destructTsp({ i32*, {}* }* %temp10)
  call void @destructAFsp({ i32*, {}* }* %tagbody-frame)
  call void @destructTsp({ i32*, {}* }* %temp8)
  call void @destructAFsp({ i32*, {}* }* %"LET*")
  call void @destructTsp({ i32*, {}* }* %temp5)
  call void @destructTsp({ i32*, {}* }* %temp)
  call void @destructAFsp({ i32*, {}* }* %lambda-args-3-)
  ret void
}

attributes #0 = { nounwind }
attributes #1 = { noreturn }
attributes #2 = { nounwind readnone }
attributes #3 = { noreturn nounwind }
