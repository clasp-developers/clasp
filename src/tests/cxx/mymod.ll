; ModuleID = 'module2'
target triple = "x86_64-apple-macosx"

@globalRunTimeValuesVector = external global { i32*, {}* }*
@":::global-str-compile-in-env" = internal unnamed_addr constant [15 x i8] c"compile-in-env\00"
@":::global-str-lambda" = internal unnamed_addr constant [7 x i8] c"lambda\00"
@":::global-str-A" = internal unnamed_addr constant [2 x i8] c"A\00"
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
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %lambda-args-1- = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %lambda-args-1-)
  %temp = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp)
  %temp4 = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp4)
  %call-args = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args)
  %given-num-args = call i32 @activationFrameSize({ i32*, {}* }* %activation-frame-ptr)
  %correct-num-args = icmp eq i32 %given-num-args, 0
  br i1 %correct-num-args, label %continue3, label %error

error:                                            ; preds = %entry
  %enough-args = icmp slt i32 %given-num-args, 0
  br i1 %enough-args, label %error1, label %continue

error1:                                           ; preds = %error
  invoke void @throwNotEnoughArgumentsException(i8* getelementptr inbounds ([7 x i8]* @":::global-str-lambda", i64 0, i64 0), { i32*, {}* }* %activation-frame-ptr, i32 %given-num-args, i32 0)
          to label %normal-dest unwind label %func-cleanup-landing-pad

normal-dest:                                      ; preds = %error1
  unreachable

continue:                                         ; preds = %error
  invoke void @throwTooManyArgumentsException(i8* getelementptr inbounds ([7 x i8]* @":::global-str-lambda", i64 0, i64 0), { i32*, {}* }* %activation-frame-ptr, i32 %given-num-args, i32 0)
          to label %normal-dest2 unwind label %func-cleanup-landing-pad

normal-dest2:                                     ; preds = %continue
  unreachable

continue3:                                        ; preds = %entry
  call void @copyAFsp({ i32*, {}* }* %lambda-args-1-, { i32*, {}* }* %activation-frame-ptr)
  %value = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @globalRunTimeValuesVector, i32 2)
  call void @attachDebuggingInfoToValueFrame({ i32*, {}* }* %lambda-args-1-, { i32*, {}* }* %value)
  call void @trace_setLineNumberColumnForIHSTop(i32 505, i32 3), !dbg !7
  call void @makeValueFrame({ i32*, {}* }* %call-args, i32 3, i32 2000000), !dbg !7
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args, { i32*, {}* }* %lambda-args-1-), !dbg !7
  %call-args-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 0), !dbg !7
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-0, { i32*, {}* }** @globalRunTimeValuesVector, i32 3), !dbg !7
  %call-args-ref-1 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 1), !dbg !7
  %CONS = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @globalRunTimeValuesVector, i32 6), !dbg !7
  %"SYMBOL->A" = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @globalRunTimeValuesVector, i32 3), !dbg !7
  invoke void @sp_makeCompiledFunction({ i32*, {}* }* %call-args-ref-1, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @A, i8* getelementptr inbounds ([15 x i8]* @":::global-str-compile-in-env", i64 0, i64 0), { i32*, {}* }* %"SYMBOL->A", { i32*, {}* }* %CONS, { i32*, {}* }* %lambda-args-1-)
          to label %"(TRY-0).normal-dest" unwind label %"(TRY-0).landing-pad", !dbg !7

"(TRY-0).normal-dest":                            ; preds = %continue3
  %call-args-ref-2 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 2), !dbg !7
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-2, { i32*, {}* }** @globalRunTimeValuesVector, i32 0), !dbg !7
  call void @singleStepCallback(), !dbg !7
  %"SYMBOL->*FSET" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @globalRunTimeValuesVector, i32 1), !dbg !7
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %temp4, { i32*, i32* }* %"SYMBOL->*FSET", { i32*, {}* }* %call-args)
          to label %"(TRY-0).normal-dest5" unwind label %"(TRY-0).landing-pad", !dbg !7

"(TRY-0).normal-dest5":                           ; preds = %"(TRY-0).normal-dest"
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %temp4, { i32*, {}* }** @globalRunTimeValuesVector, i32 0), !dbg !7
  call void @mv_copyLoadTimeValue({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }** @globalRunTimeValuesVector, i32 3), !dbg !7
  call void @destructAFsp({ i32*, {}* }* %call-args), !dbg !7
  call void @destructTsp({ i32*, {}* }* %temp4), !dbg !7
  call void @destructTsp({ i32*, {}* }* %temp), !dbg !7
  call void @destructAFsp({ i32*, {}* }* %lambda-args-1-), !dbg !7
  ret void, !dbg !7

"(TRY-0).landing-pad":                            ; preds = %"(TRY-0).normal-dest", %continue3
  %0 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup, !dbg !7
  %1 = extractvalue { i8*, i32 } %0, 0, !dbg !7
  store i8* %1, i8** %exn.slot, align 8, !dbg !7
  %2 = extractvalue { i8*, i32 } %0, 1, !dbg !7
  store i32 %2, i32* %ehselector.slot, align 4, !dbg !7
  br label %func-ehcleanup, !dbg !7

func-cleanup-landing-pad:                         ; preds = %continue, %error1
  %3 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup, !dbg !7
  %4 = extractvalue { i8*, i32 } %3, 0, !dbg !7
  store i8* %4, i8** %exn.slot, align 8, !dbg !7
  %5 = extractvalue { i8*, i32 } %3, 1, !dbg !7
  store i32 %5, i32* %ehselector.slot, align 4, !dbg !7
  br label %func-ehcleanup, !dbg !7

func-ehcleanup:                                   ; preds = %func-cleanup-landing-pad, %"(TRY-0).landing-pad"
  %sel = phi i32 [ %5, %func-cleanup-landing-pad ], [ %2, %"(TRY-0).landing-pad" ]
  %exn7 = phi i8* [ %4, %func-cleanup-landing-pad ], [ %1, %"(TRY-0).landing-pad" ]
  call void @destructAFsp({ i32*, {}* }* %call-args), !dbg !7
  call void @destructTsp({ i32*, {}* }* %temp4), !dbg !7
  call void @destructTsp({ i32*, {}* }* %temp), !dbg !7
  call void @destructAFsp({ i32*, {}* }* %lambda-args-1-), !dbg !7
  %lpad.val = insertvalue { i8*, i32 } undef, i8* %exn7, 0, !dbg !7
  call void @debugPrintI32(i32 90), !dbg !7
  %lpad.val8 = insertvalue { i8*, i32 } %lpad.val, i32 %sel, 1, !dbg !7
  call void @debugPrintI32(i32 91), !dbg !7
  resume { i8*, i32 } %lpad.val8, !dbg !7
}

define internal void @A({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
"(TRY-0).entry":
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %lambda-args-2- = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %lambda-args-2-)
  %temp = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp)
  %call-args = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args)
  %given-num-args = call i32 @activationFrameSize({ i32*, {}* }* %activation-frame-ptr)
  %correct-num-args = icmp eq i32 %given-num-args, 0
  br i1 %correct-num-args, label %"(TRY-0).continue3", label %"(TRY-0).error"

"(TRY-0).error":                                  ; preds = %"(TRY-0).entry"
  %enough-args = icmp slt i32 %given-num-args, 0
  br i1 %enough-args, label %"(TRY-0).error1", label %"(TRY-0).continue"

"(TRY-0).error1":                                 ; preds = %"(TRY-0).error"
  invoke void @throwNotEnoughArgumentsException(i8* getelementptr inbounds ([2 x i8]* @":::global-str-A", i64 0, i64 0), { i32*, {}* }* %activation-frame-ptr, i32 %given-num-args, i32 0)
          to label %"(TRY-0).normal-dest" unwind label %"(TRY-0).func-cleanup-landing-pad"

"(TRY-0).normal-dest":                            ; preds = %"(TRY-0).error1"
  unreachable

"(TRY-0).continue":                               ; preds = %"(TRY-0).error"
  invoke void @throwTooManyArgumentsException(i8* getelementptr inbounds ([2 x i8]* @":::global-str-A", i64 0, i64 0), { i32*, {}* }* %activation-frame-ptr, i32 %given-num-args, i32 0)
          to label %"(TRY-0).normal-dest2" unwind label %"(TRY-0).func-cleanup-landing-pad"

"(TRY-0).normal-dest2":                           ; preds = %"(TRY-0).continue"
  unreachable

"(TRY-0).continue3":                              ; preds = %"(TRY-0).entry"
  call void @copyAFsp({ i32*, {}* }* %lambda-args-2-, { i32*, {}* }* %activation-frame-ptr)
  %value = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @globalRunTimeValuesVector, i32 4)
  call void @attachDebuggingInfoToValueFrame({ i32*, {}* }* %lambda-args-2-, { i32*, {}* }* %value)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %lambda-args-2-)
  call void @trace_setLineNumberColumnForIHSTop(i32 1, i32 13), !dbg !14
  call void @makeValueFrame({ i32*, {}* }* %call-args, i32 1, i32 2000001), !dbg !14
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args, { i32*, {}* }* %lambda-args-2-), !dbg !14
  %call-args-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 0), !dbg !14
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-0, { i32*, {}* }** @globalRunTimeValuesVector, i32 5), !dbg !14
  call void @singleStepCallback(), !dbg !14
  %"SYMBOL->COMMON-LISP:PRINT" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @globalRunTimeValuesVector, i32 0), !dbg !14
  invoke void @mv_invokePossibleMultipleValueSymbolFunction({ i32*, {}*, i32 }* %result-ptr, { i32*, i32* }* %"SYMBOL->COMMON-LISP:PRINT", { i32*, {}* }* %call-args)
          to label %"(TRY-0).try-cont" unwind label %"(TRY-0).landing-pad", !dbg !14

"(TRY-0).landing-pad":                            ; preds = %"(TRY-0).continue3"
  %0 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
          catch i8* @_ZTIN4core10ReturnFromE, !dbg !14
  %1 = extractvalue { i8*, i32 } %0, 0, !dbg !14
  store i8* %1, i8** %exn.slot, align 8, !dbg !14
  %2 = extractvalue { i8*, i32 } %0, 1, !dbg !14
  store i32 %2, i32* %ehselector.slot, align 4, !dbg !14
  %3 = call i32 @llvm.eh.typeid.for(i8* @_ZTIN4core10ReturnFromE), !dbg !14
  %4 = icmp eq i32 %2, %3, !dbg !14
  br i1 %4, label %"(TRY-0).handler-block6118", label %"(TRY-0).func-ehcleanup", !dbg !14

"(TRY-0).handler-block6118":                      ; preds = %"(TRY-0).landing-pad"
  %5 = call i8* @__cxa_begin_catch(i8* %1), !dbg !14
  invoke void @mv_blockHandleReturnFrom({ i32*, {}*, i32 }* %result-ptr, i8* %5)
          to label %"(TRY-0).normal-dest6" unwind label %"(TRY-0).landing-pad9", !dbg !14

"(TRY-0).normal-dest6":                           ; preds = %"(TRY-0).handler-block6118"
  invoke void @__cxa_end_catch()
          to label %"(TRY-0).try-cont" unwind label %"(TRY-0).landing-pad9", !dbg !14

"(TRY-0).try-cont":                               ; preds = %"(TRY-0).normal-dest6", %"(TRY-0).continue3"
  call void @destructAFsp({ i32*, {}* }* %call-args), !dbg !14
  call void @destructTsp({ i32*, {}* }* %temp), !dbg !14
  call void @destructAFsp({ i32*, {}* }* %lambda-args-2-), !dbg !14
  ret void, !dbg !14

"(TRY-0).landing-pad9":                           ; preds = %"(TRY-0).normal-dest6", %"(TRY-0).handler-block6118"
  %6 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup, !dbg !14
  %7 = extractvalue { i8*, i32 } %6, 0, !dbg !14
  store i8* %7, i8** %exn.slot, align 8, !dbg !14
  %8 = extractvalue { i8*, i32 } %6, 1, !dbg !14
  store i32 %8, i32* %ehselector.slot, align 4, !dbg !14
  br label %"(TRY-0).func-ehcleanup", !dbg !14

"(TRY-0).func-cleanup-landing-pad":               ; preds = %"(TRY-0).continue", %"(TRY-0).error1"
  %9 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup, !dbg !14
  %10 = extractvalue { i8*, i32 } %9, 0, !dbg !14
  store i8* %10, i8** %exn.slot, align 8, !dbg !14
  %11 = extractvalue { i8*, i32 } %9, 1, !dbg !14
  store i32 %11, i32* %ehselector.slot, align 4, !dbg !14
  br label %"(TRY-0).func-ehcleanup", !dbg !14

"(TRY-0).func-ehcleanup":                         ; preds = %"(TRY-0).landing-pad", %"(TRY-0).landing-pad9", %"(TRY-0).func-cleanup-landing-pad"
  %sel = phi i32 [ %11, %"(TRY-0).func-cleanup-landing-pad" ], [ %8, %"(TRY-0).landing-pad9" ], [ %2, %"(TRY-0).landing-pad" ]
  %exn7 = phi i8* [ %10, %"(TRY-0).func-cleanup-landing-pad" ], [ %7, %"(TRY-0).landing-pad9" ], [ %1, %"(TRY-0).landing-pad" ]
  call void @destructAFsp({ i32*, {}* }* %call-args), !dbg !14
  call void @destructTsp({ i32*, {}* }* %temp), !dbg !14
  call void @destructAFsp({ i32*, {}* }* %lambda-args-2-), !dbg !14
  %lpad.val = insertvalue { i8*, i32 } undef, i8* %exn7, 0, !dbg !14
  call void @debugPrintI32(i32 90), !dbg !14
  %lpad.val8 = insertvalue { i8*, i32 } %lpad.val, i32 %sel, 1, !dbg !14
  call void @debugPrintI32(i32 91), !dbg !14
  resume { i8*, i32 } %lpad.val8, !dbg !14
}

attributes #0 = { nounwind }
attributes #1 = { noreturn }
attributes #2 = { nounwind readnone }
attributes #3 = { noreturn nounwind }

!llvm.dbg.cu = !{!0}

!0 = metadata !{i32 786449,
		metadata !1,
		i32 32768,
		metadata !"brcl Common Lisp compiler",
		i1 false,
		metadata !"-v",
		i32 1,
		metadata !2,
		metadata !3,
		metadata !4,
		metadata !5,
		metadata !6,
		metadata !"split-name.log"} ; [ DW_TAG_compile_unit ] [/compile-in-env] [DW_LANG_lo_user]
!1 = metadata !{metadata !"compile-in-env", metadata !""}
!2 = metadata !{i32 0}
!3 = metadata !{i32 0}
    !4 = metadata !{metadata !9,metadata !17}
!5 = metadata !{i32 0}
!6 = metadata !{i32 0}
!7 = metadata !{i32 505, i32 3, metadata !8, null}
!8 = metadata !{i32 786443, metadata !1, metadata !9, i32 0, i32 0, i32 0} ; [ DW_TAG_lexical_block ] [/compile-in-env]
!9 = metadata !{i32 786478,
		metadata !1,
		metadata !19,
		metadata !"lambda",
		metadata !"lambda",
		metadata !"lambda",
		i32 0,
		metadata !10,
		i1 false,
		i1 true,
		i32 0,
		i32 0,
		null,
		i32 0,
		i1 false,
		void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @lambda,
		null,
		null,
		metadata !13,
		i32 0} ; [ DW_TAG_subprogram ] [line 0] [def] [lambda]
!10 = metadata !{i32 786453,
		 i32 0,
		 null,
		 metadata !"",
		 i32 0,
		 i64 0,
		 i64 0,
		 i64 0,
		 i32 0,
		 null,
		 metadata !11,
		 i32 0,
		 null,
		 null,
		 null} ; [ DW_TAG_subroutine_type ] [line 0, size 0, align 0, offset 0] [from ]
!11 = metadata !{metadata !12}
!12 = metadata !{i32 786468, null, null, metadata !"int", i32 0, i64 32, i64 32, i64 0, i32 0, i32 13} ; [ DW_TAG_base_type ] [int] [line 0, size 32, align 32, offset 0, enc DW_ATE_signed_fixed]
!13 = metadata !{i32 786468}
!14 = metadata !{i32 1, i32 13, metadata !15, null}
!15 = metadata !{i32 786443, metadata !1, metadata !16, i32 0, i32 0, i32 2} ; [ DW_TAG_lexical_block ] [/compile-in-env]
!16 = metadata !{i32 786443, metadata !1, metadata !17, i32 0, i32 0, i32 1} ; [ DW_TAG_lexical_block ] [/compile-in-env]
!17 = metadata !{i32 786478,
		 metadata !1,
		 null,
		 metadata !"A",
		 metadata !"A",
		 metadata !"A",
		 i32 0,
		 metadata !10,
		 i1 false,
		 i1 true,
		 i32 0,
		 i32 0,
		 null,
		 i32 0,
		 i1 false,
		 void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @A,
		 null,
		 null,
		 metadata !18,
		 i32 0} ; [ DW_TAG_subprogram ] [line 0] [def] [A]
!18 = metadata !{i32 786468}
!19 = metadata !{i32 786473, metadata !1}
