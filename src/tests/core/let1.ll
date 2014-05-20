; ModuleID = 'let1.bc'
target triple = "x86_64-apple-macosx"

@":::global-str-/Users/meister/Development/cando/brcl/src/tests/core/let1.lsp" = internal unnamed_addr constant [62 x i8] c"/Users/meister/Development/cando/brcl/src/tests/core/let1.lsp\00"
@load-time-value-vector = internal global { i32*, {}* }* null
@":::global-str-repl" = internal unnamed_addr constant [5 x i8] c"repl\00"
@":::symbol-name-T" = internal unnamed_addr constant [2 x i8] c"T\00"
@":::package-name-COMMON-LISP" = internal unnamed_addr constant [12 x i8] c"COMMON-LISP\00"
@constant-array = internal constant [1 x i32] [i32 1]
@":::symbol-name-A" = internal unnamed_addr constant [2 x i8] c"A\00"
@":::package-name-CORE" = internal unnamed_addr constant [5 x i8] c"CORE\00"
@constant-array1 = internal constant [1 x i32] [i32 4]
@":::symbol-name-PRINT" = internal unnamed_addr constant [6 x i8] c"PRINT\00"
@":::str" = internal unnamed_addr constant [5 x i8] c"done\00"
@_ZTIN4core9DynamicGoE = external constant i8
@":::global-str-___user_let1" = internal unnamed_addr constant [13 x i8] c"___user_let1\00"
@":::global-str-___loadTimeDataInitializer" = internal unnamed_addr constant [27 x i8] c"___loadTimeDataInitializer\00"

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

declare void @sp_makeCompiledFunction({ i32*, {}* }*, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)*, i8*, { i32*, {}* }*, { i32*, {}* }*)

declare void @mv_makeCompiledFunction({ i32*, {}*, i32 }*, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)*, i8*, { i32*, {}* }*, { i32*, {}* }*)

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
declare void @makeValueFrame({ i32*, {}* }*, i32) #0

; Function Attrs: nounwind
declare void @makeValueFrameFromReversedCons({ i32*, {}* }*, { i32*, {}* }*) #0

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
declare void @cando_terminate(i8*, i32, i32, i8*) #0

; Function Attrs: nounwind
declare i32 @__gxx_personality_v0(...) #0

; Function Attrs: nounwind
declare i8* @__cxa_begin_catch(i8*) #0

declare void @__cxa_end_catch()

declare void @__cxa_rethrow()

; Function Attrs: nounwind readnone
declare i32 @llvm.eh.typeid.for(i8*) #2

; Function Attrs: nounwind
declare void @getLoadTimeValueArray({ i32*, {}* }**, i8*, i32, i32) #0

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

define internal void @___loadTimeDataInitializer() {
entry:
  %array-element-type = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %array-element-type)
  call void @getLoadTimeValueArray({ i32*, {}* }** @load-time-value-vector, i8* getelementptr inbounds ([62 x i8]* @":::global-str-/Users/meister/Development/cando/brcl/src/tests/core/let1.lsp", i64 0, i64 0), i32 7, i32 1)
  %0 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  call void @sp_makeNil({ i32*, {}* }* %0)
  %1 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 1)
  call void @makeT({ i32*, {}* }* %1)
  %2 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 2)
  call void @internSymbol_tsp({ i32*, {}* }* %2, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-T", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %array-element-type, { i32*, {}* }** @load-time-value-vector, i32 2)
  %3 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 3)
  call void @ltv_makeArrayObjects({ i32*, {}* }* %3, { i32*, {}* }* %array-element-type, i32 1, i32* getelementptr inbounds ([1 x i32]* @constant-array, i64 0, i64 0))
  %4 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  call void @internSymbol_tsp({ i32*, {}* }* %4, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-A", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %5 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 3)
  call void @ltv_initializeArrayObjectsRowMajorArefOrder({ i32*, {}* }* %5, { i32*, {}* }** @load-time-value-vector, i32* getelementptr inbounds ([1 x i32]* @constant-array1, i64 0, i64 0))
  %6 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 5)
  call void @makeFixnum({ i32*, {}* }* %6, i32 1)
  %7 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  call void @internSymbol_symsp({ i32*, i32* }* %7, i8* getelementptr inbounds ([6 x i8]* @":::symbol-name-PRINT", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  %8 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 6)
  call void @makeString({ i32*, {}* }* %8, i8* getelementptr inbounds ([5 x i8]* @":::str", i64 0, i64 0))
  ret void
}

define internal void @repl({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %LET = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %LET)
  %temp = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp)
  %tagbody-frame = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %tagbody-frame)
  %temp1 = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp1)
  %call-args = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args)
  %temp2 = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp2)
  %call-args3 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args3)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %LET)
  call void @makeValueFrame({ i32*, {}* }* %LET, i32 1)
  %0 = call { i32*, {}* }* @activationFrameNil()
  call void @setParentOfActivationFrame({ i32*, {}* }* %LET, { i32*, {}* }* %0)
  %value = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 3)
  call void @attachDebuggingInfoToValueFrame({ i32*, {}* }* %LET, { i32*, {}* }* %value)
  %1 = call { i32*, {}* }* @lexicalValueReference(i32 0, i32 0, { i32*, {}* }* %LET)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %1, { i32*, {}* }** @load-time-value-vector, i32 5)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %LET)
  call void @singleStepCallback()
  call void @makeTagbodyFrame({ i32*, {}* }* %tagbody-frame)
  br label %"(TRY-6559170).tagbody-A-0"

"(TRY-6559170).tagbody-A-0":                      ; preds = %"(TRY-6998754).normal-dest6", %entry
  call void @trace_setLineNumberColumnForIHSTop(i32 4, i32 6)
  call void @makeValueFrame({ i32*, {}* }* %call-args, i32 1)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args, { i32*, {}* }* %tagbody-frame)
  %call-args-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 0)
  call void @sp_lexicalValueRead({ i32*, {}* }* %call-args-ref-0, i32 1, i32 0, { i32*, {}* }* %tagbody-frame)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:PRINT" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  invoke void @mv_invokePossibleMultipleValueSymbolFunction({ i32*, {}*, i32 }* %result-ptr, { i32*, i32* }* %"SYMBOL->COMMON-LISP:PRINT", { i32*, {}* }* %call-args)
          to label %"(TRY-6559170).tagbody-B-1" unwind label %"(TRY-6998754).landing-pad"

"(TRY-6559170).tagbody-B-1":                      ; preds = %"(TRY-6998754).normal-dest6", %"(TRY-6559170).tagbody-A-0"
  call void @trace_setLineNumberColumnForIHSTop(i32 6, i32 6)
  call void @makeValueFrame({ i32*, {}* }* %call-args3, i32 1)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args3, { i32*, {}* }* %tagbody-frame)
  %call-args3-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args3, i32 0)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args3-ref-0, { i32*, {}* }** @load-time-value-vector, i32 6)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args3)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:PRINT4" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  invoke void @mv_invokePossibleMultipleValueSymbolFunction({ i32*, {}*, i32 }* %result-ptr, { i32*, i32* }* %"SYMBOL->COMMON-LISP:PRINT4", { i32*, {}* }* %call-args3)
          to label %"(TRY-6998754).normal-dest5" unwind label %"(TRY-6998754).landing-pad"

"(TRY-6998754).normal-dest5":                     ; preds = %"(TRY-6559170).tagbody-B-1"
  call void @mv_copyLoadTimeValue({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }** @load-time-value-vector, i32 0)
  ret void

"(TRY-6998754).landing-pad":                      ; preds = %"(TRY-6559170).tagbody-B-1", %"(TRY-6559170).tagbody-A-0"
  %2 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
          catch i8* @_ZTIN4core9DynamicGoE
  %3 = extractvalue { i8*, i32 } %2, 0
  store i8* %3, i8** %exn.slot, align 8
  %4 = extractvalue { i8*, i32 } %2, 1
  store i32 %4, i32* %ehselector.slot, align 4
  call void @mv_copyLoadTimeValue({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }** @load-time-value-vector, i32 0)
  %5 = call i32 @llvm.eh.typeid.for(i8* @_ZTIN4core9DynamicGoE)
  %6 = icmp eq i32 %4, %5
  br i1 %6, label %"(TRY-6998754).handler-block6289", label %func-ehcleanup

"(TRY-6998754).handler-block6289":                ; preds = %"(TRY-6998754).landing-pad"
  %7 = call i8* @__cxa_begin_catch(i8* %3)
  %8 = invoke i32 @tagbodyDynamicGoIndexElseRethrow({ i32*, {}* }* %tagbody-frame, i8* %7)
          to label %"(TRY-6998754).normal-dest6" unwind label %"(TRY-6559170).landing-pad"

"(TRY-6998754).normal-dest6":                     ; preds = %"(TRY-6998754).handler-block6289"
  switch i32 %8, label %"(TRY-6998754).switch-default" [
    i32 0, label %"(TRY-6559170).tagbody-A-0"
    i32 1, label %"(TRY-6559170).tagbody-B-1"
  ]

"(TRY-6998754).switch-default":                   ; preds = %"(TRY-6998754).normal-dest6"
  invoke void @throwIllegalSwitchValue(i32 %8, i32 2)
          to label %"(TRY-6998754).normal-dest7" unwind label %"(TRY-6559170).landing-pad"

"(TRY-6998754).normal-dest7":                     ; preds = %"(TRY-6998754).switch-default"
  unreachable

"(TRY-6559170).landing-pad":                      ; preds = %"(TRY-6998754).switch-default", %"(TRY-6998754).handler-block6289"
  %9 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %10 = extractvalue { i8*, i32 } %9, 0
  store i8* %10, i8** %exn.slot, align 8
  %11 = extractvalue { i8*, i32 } %9, 1
  store i32 %11, i32* %ehselector.slot, align 4
  br label %func-ehcleanup

func-ehcleanup:                                   ; preds = %"(TRY-6559170).landing-pad", %"(TRY-6998754).landing-pad"
  %sel12 = phi i32 [ %11, %"(TRY-6559170).landing-pad" ], [ %4, %"(TRY-6998754).landing-pad" ]
  %exn710 = phi i8* [ %10, %"(TRY-6559170).landing-pad" ], [ %3, %"(TRY-6998754).landing-pad" ]
  %lpad.val = insertvalue { i8*, i32 } undef, i8* %exn710, 0
  %lpad.val8 = insertvalue { i8*, i32 } %lpad.val, i32 %sel12, 1
  resume { i8*, i32 } %lpad.val8
}

define void @___user_let1() {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %result = alloca { i32*, {}*, i32 }, align 8
  call void @newTmv({ i32*, {}*, i32 }* %result)
  invoke void @invokeLlvmFunctionVoid(void ()* @___loadTimeDataInitializer)
          to label %normal-dest unwind label %func-cleanup-landing-pad

normal-dest:                                      ; preds = %entry
  %0 = call { i32*, {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ i32*, {}*, i32 }* %result, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl, { i32*, {}* }* %0)
          to label %normal-dest1 unwind label %func-cleanup-landing-pad

normal-dest1:                                     ; preds = %normal-dest
  ret void

func-cleanup-landing-pad:                         ; preds = %normal-dest, %entry
  %1 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %2 = extractvalue { i8*, i32 } %1, 0
  store i8* %2, i8** %exn.slot, align 8
  %3 = extractvalue { i8*, i32 } %1, 1
  store i32 %3, i32* %ehselector.slot, align 4
  resume { i8*, i32 } %1
}

attributes #0 = { nounwind }
attributes #1 = { noreturn }
attributes #2 = { nounwind readnone }
attributes #3 = { noreturn nounwind }

!llvm.dbg.cu = !{!0}

!0 = metadata !{i32 786449, metadata !1, i32 32768, metadata !"brcl Common Lisp compiler", i1 false, metadata !"-v", i32 1, metadata !2, metadata !2, metadata !3, metadata !2, metadata !2, metadata !"split-name.log"} ; [ DW_TAG_compile_unit ] [/Users/meister/Development/cando/brcl/src/tests/core/let1.lsp] [DW_LANG_lo_user]
!1 = metadata !{metadata !"let1.lsp", metadata !"/Users/meister/Development/cando/brcl/src/tests/core"}
!2 = metadata !{i32 0}
!3 = metadata !{metadata !4, metadata !9}
!4 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl", metadata !"repl", metadata !"repl", i32 1, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl, null, null, metadata !2, i32 1} ; [ DW_TAG_subprogram ] [line 1] [def] [repl]
!5 = metadata !{i32 786473, metadata !1}          ; [ DW_TAG_file_type ] [/Users/meister/Development/cando/brcl/src/tests/core/let1.lsp]
!6 = metadata !{i32 786453, i32 0, i32 0, metadata !"", i32 0, i64 0, i64 0, i64 0, i32 0, null, metadata !7, i32 0, i32 0} ; [ DW_TAG_subroutine_type ] [line 0, size 0, align 0, offset 0] [from ]
!7 = metadata !{metadata !8}
!8 = metadata !{i32 786468, null, null, metadata !"int", i32 0, i64 32, i64 32, i64 0, i32 0, i32 13} ; [ DW_TAG_base_type ] [int] [line 0, size 32, align 32, offset 0, enc DW_ATE_signed_fixed]
!9 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"___user_let1", metadata !"___user_let1", metadata !"___user_let1", i32 0, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ()* @___user_let1, null, null, metadata !2, i32 0} ; [ DW_TAG_subprogram ] [line 0] [def] [___user_let1]
