; ModuleID = 'test.bc-min-rc'
target triple = "x86_64-apple-macosx10.7.0"

@":::global-str-/Users/meister/Development/cando/clasp/src/main/test.lsp" = internal unnamed_addr constant [57 x i8] c"/Users/meister/Development/cando/clasp/src/main/test.lsp\00"
@load-time-value-vector = internal global { {}* }* null
@":::global-str-repl" = internal unnamed_addr constant [5 x i8] c"repl\00"
@":::symbol-name-TEST-DEBUG" = internal unnamed_addr constant [11 x i8] c"TEST-DEBUG\00"
@":::package-name-CORE" = internal unnamed_addr constant [5 x i8] c"CORE\00"
@":::global-str-TEST-DEBUG" = internal unnamed_addr constant [11 x i8] c"TEST-DEBUG\00"
@":::symbol-name-T" = internal unnamed_addr constant [2 x i8] c"T\00"
@":::package-name-CL" = internal unnamed_addr constant [3 x i8] c"CL\00"
@constant-array = internal constant [1 x i32] zeroinitializer
@constant-array1 = internal constant [0 x i32] zeroinitializer
@":::str" = internal unnamed_addr constant [20 x i8] c"About to test debug\00"
@":::symbol-name-PRINT" = internal unnamed_addr constant [6 x i8] c"PRINT\00"
@":::str2" = internal unnamed_addr constant [14 x i8] c"testing debug\00"
@":::symbol-name-DEBUG" = internal unnamed_addr constant [6 x i8] c"DEBUG\00"
@":::str3" = internal unnamed_addr constant [5 x i8] c"Done\00"
@_ZTIN4core10ReturnFromE = external constant i8
@":::symbol-name-*FSET" = internal unnamed_addr constant [6 x i8] c"*FSET\00"
@":::global-str-repl4" = internal unnamed_addr constant [6 x i8] c"repl4\00"
@":::global-str-repl5" = internal unnamed_addr constant [6 x i8] c"repl5\00"
@":::global-str-__MAIN_test" = internal unnamed_addr constant [12 x i8] c"__MAIN_test\00"
@":::global-str-___loadTimeDataInitializer" = internal unnamed_addr constant [27 x i8] c"___loadTimeDataInitializer\00"

; Function Attrs: nounwind
declare void @newFunction_sp({ i32* }*) #0

; Function Attrs: nounwind
declare void @destructFunction_sp({ i32* }*) #0

; Function Attrs: nounwind
declare void @newTsp({ {}* }*) #0

; Function Attrs: nounwind
declare void @resetTsp({ {}* }*) #0

; Function Attrs: nounwind
declare void @makeUnboundTsp({ {}* }*) #0

; Function Attrs: nounwind
declare void @sp_copyTsp({ {}* }*, { {}* }*) #0

; Function Attrs: nounwind
declare void @mv_copyTsp({ {}*, i32 }*, { {}* }*) #0

; Function Attrs: nounwind
declare void @destructTsp({ {}* }*) #0

; Function Attrs: nounwind
declare i32 @compareTsp({ {}* }*, { {}* }*) #0

; Function Attrs: nounwind
declare void @newTmv({ {}*, i32 }*) #0

; Function Attrs: nounwind
declare void @resetTmv({ {}*, i32 }*) #0

; Function Attrs: nounwind
declare void @copyTmv({ {}*, i32 }*, { {}*, i32 }*) #0

; Function Attrs: nounwind
declare void @sp_copyTmvOrSlice({ {}* }*, { {}*, i32 }*) #0

; Function Attrs: nounwind
declare void @mv_copyTmvOrSlice({ {}*, i32 }*, { {}*, i32 }*) #0

; Function Attrs: nounwind
declare void @destructTmv({ {}*, i32 }*) #0

; Function Attrs: nounwind
declare void @newAFsp({ {}* }*) #0

; Function Attrs: nounwind
declare void @newAFsp_ValueFrameOfSize({ {}* }*, i32) #0

; Function Attrs: nounwind
declare void @resetAFsp({ {}* }*) #0

; Function Attrs: nounwind
declare void @copyAFsp({ {}* }*, { {}* }*) #0

; Function Attrs: nounwind
declare void @destructAFsp({ {}* }*) #0

; Function Attrs: nounwind
declare i32 @isNilTsp({ {}* }*) #0

; Function Attrs: nounwind
declare i32 @isTrueTsp({ {}* }*) #0

; Function Attrs: nounwind
declare i32 @isBoundTsp({ {}* }*) #0

; Function Attrs: nounwind
declare void @internSymbol_tsp({ {}* }*, i8*, i8*) #0

; Function Attrs: nounwind
declare void @makeSymbol_tsp({ {}* }*, i8*) #0

; Function Attrs: nounwind
declare void @internSymbol_symsp({ i32* }*, i8*, i8*) #0

; Function Attrs: nounwind
declare void @makeSymbol_symsp({ i32* }*, i8*) #0

; Function Attrs: nounwind
declare void @sp_makeNil({ {}* }*) #0

; Function Attrs: nounwind
declare void @mv_makeNil({ {}*, i32 }*) #0

; Function Attrs: nounwind
declare void @makeT({ {}* }*) #0

; Function Attrs: nounwind
declare void @makeCons({ {}* }*, { {}* }*, { {}* }*) #0

; Function Attrs: nounwind
declare void @makeFixnum({ {}* }*, i32) #0

; Function Attrs: nounwind
declare void @makeCharacter({ {}* }*, i32) #0

; Function Attrs: nounwind
declare void @makeBignum({ {}* }*, i8*) #0

; Function Attrs: nounwind
declare void @makeSingleFloat({ {}* }*, float) #0

; Function Attrs: nounwind
declare void @makeDoubleFloat({ {}* }*, double) #0

; Function Attrs: nounwind
declare void @makeString({ {}* }*, i8*) #0

declare void @sp_makeCompiledFunction({ {}* }*, void ({ {}*, i32 }*, { {}* }*, i32, { {}* }*)*, i8*, { {}* }*, { {}* }*, { {}* }*)

declare void @mv_makeCompiledFunction({ {}*, i32 }*, void ({ {}*, i32 }*, { {}* }*, i32, { {}* }*)*, i8*, { {}* }*, { {}* }*, { {}* }*)

declare void @fillRestTarget({ {}* }*, { {}* }*, i32, i8*)

; Function Attrs: nounwind
declare void @sp_symbolValueRead({ {}* }*, { i32* }*) #0

; Function Attrs: nounwind
declare void @mv_symbolValueRead({ {}*, i32 }*, { i32* }*) #0

; Function Attrs: nounwind
declare void @sp_symbolValueReadOrUnbound({ {}* }*, { i32* }*) #0

; Function Attrs: nounwind
declare void @mv_symbolValueReadOrUnbound({ {}*, i32 }*, { i32* }*) #0

; Function Attrs: nounwind
declare { {}* }* @symbolValueReference({ i32* }*) #0

; Function Attrs: nounwind
declare { {}* }* @lexicalValueReference(i32, i32, { {}* }*) #0

; Function Attrs: nounwind
declare void @sp_lexicalValueRead({ {}* }*, i32, i32, { {}* }*) #0

; Function Attrs: nounwind
declare void @mv_lexicalValueRead({ {}*, i32 }*, i32, i32, { {}* }*) #0

; Function Attrs: nounwind
declare void @sp_symbolFunctionRead({ {}* }*, { i32* }*) #0

; Function Attrs: nounwind
declare void @mv_symbolFunctionRead({ {}*, i32 }*, { i32* }*) #0

; Function Attrs: nounwind
declare void @setfSymbolFunctionRead({ {}* }*, { i32* }*) #0

; Function Attrs: nounwind
declare void @sp_lexicalFunctionRead({ {}* }*, i32, i32, { {}* }*) #0

; Function Attrs: nounwind
declare void @mv_lexicalFunctionRead({ {}*, i32 }*, i32, i32, { {}* }*) #0

; Function Attrs: nounwind
declare void @makeTagbodyFrame({ {}* }*) #0

; Function Attrs: nounwind
declare void @makeValueFrame({ {}* }*, i32, i32) #0

; Function Attrs: nounwind
declare void @makeValueFrameFromReversedCons({ {}* }*, { {}* }*, i32) #0

; Function Attrs: nounwind
declare void @setParentOfActivationFrame({ {}* }*, { {}* }*) #0

; Function Attrs: nounwind
declare void @attachDebuggingInfoToValueFrame({ {}* }*, { {}* }*) #0

; Function Attrs: nounwind
declare { {}* }* @valueFrameReference({ {}* }*, i32) #0

declare void @makeFunctionFrame({ {}* }*, i32, { {}* }*)

declare { {}* }* @functionFrameReference({ {}* }*, i32)

declare void @sp_prependMultipleValues({ {}* }*, { {}*, i32 }*)

declare void @mv_prependMultipleValues({ {}*, i32 }*, { {}*, i32 }*)

declare void @symbolFunction({ i32* }*, { {}* }*)

declare void @lexicalFunction({ i32* }*, i32, i32, { {}* }*)

declare void @invokePossibleMultipleValueFunction({ {}*, i32 }*, { {}* }*, { {}* }*)

declare void @sp_invokePossibleMultipleValueSymbolFunction({ {}* }*, { i32* }*, { {}* }*)

declare void @mv_invokePossibleMultipleValueSymbolFunction({ {}*, i32 }*, { i32* }*, { {}* }*)

declare void @sp_invokePossibleMultipleValueLexicalFunction({ {}* }*, i32, i32, { {}* }*, { {}* }*)

declare void @mv_invokePossibleMultipleValueLexicalFunction({ {}*, i32 }*, i32, i32, { {}* }*, { {}* }*)

declare void @invokeLlvmFunction({ {}*, i32 }*, void ({ {}*, i32 }*, { {}* }*, i32, { {}* }*)*, { {}* }*)

declare void @invokeLlvmFunctionVoid(void ()*)

declare void @invokeFASLLlvmFunctionVoid(void ()*, i8*)

; Function Attrs: nounwind
declare { {}* }* @activationFrameNil() #0

; Function Attrs: nounwind
declare i32 @activationFrameSize({ {}* }*) #0

; Function Attrs: nounwind
declare { {}* }* @activationFrameParentRef({ {}* }*) #0

declare void @throwTooManyArgumentsException(i8*, { {}* }*, i32, i32)

declare void @throwNotEnoughArgumentsException(i8*, { {}* }*, i32, i32)

declare void @throwIfExcessKeywordArguments(i8*, { {}* }*, i32)

; Function Attrs: nounwind
declare i32 @kw_allowOtherKeywords(i32, { {}* }*, i32) #0

; Function Attrs: nounwind
declare i32 @kw_trackFirstUnexpectedKeyword(i32, i32) #0

declare void @kw_throwIfBadKeywordArgument(i32, i32, { {}* }*)

declare void @kw_throwIfNotKeyword({ {}* }*)

; Function Attrs: nounwind
declare void @gdb() #0

; Function Attrs: nounwind
declare void @debugInvoke() #0

; Function Attrs: nounwind
declare void @debugInspectActivationFrame({ {}* }*) #0

; Function Attrs: nounwind
declare void @debugInspectObject_sp({ {}* }*) #0

; Function Attrs: nounwind
declare void @debugInspectObject_mv({ {}*, i32 }*) #0

; Function Attrs: nounwind
declare void @debugPointer(i8*) #0

; Function Attrs: nounwind
declare void @debugPrintObject(i8*, { {}* }*) #0

; Function Attrs: nounwind
declare void @debugPrintI32(i32) #0

; Function Attrs: nounwind
declare void @lowLevelTrace(i32) #0

; Function Attrs: nounwind
declare void @unreachableError() #0

; Function Attrs: nounwind
declare void @singleStepCallback() #0

declare void @va_throwTooManyArgumentsException(i8*, i32, i32)

declare void @va_throwNotEnoughArgumentsException(i8*, i32, i32)

declare void @va_throwIfExcessKeywordArguments(i8*, i32, { {}* }*, i32)

declare void @va_fillActivationFrameWithRequiredVarargs({ {}* }*, i32, { {}* }*)

declare void @va_coerceToFunction({ i32* }*, { {}* }*)

declare void @va_symbolFunction({ i32* }*, { i32* }*)

declare void @va_lexicalFunction({ i32* }*, i32, i32, { {}* }*)

declare void @sp_FUNCALL({ {}* }*, { i32* }*, i32, { {}* }*)

declare void @mv_FUNCALL({ {}*, i32 }*, { i32* }*, i32, { {}* }*)

declare void @sp_FUNCALL_activationFrame({ {}* }*, { i32* }*, { {}* }*)

declare void @mv_FUNCALL_activationFrame({ {}*, i32 }*, { i32* }*, { {}* }*)

declare void @va_fillRestTarget({ {}* }*, i32, { {}* }*, i32, i8*)

; Function Attrs: nounwind
declare i32 @va_allowOtherKeywords(i32, i32, { {}* }*, i32) #0

declare void @va_throwIfBadKeywordArgument(i32, i32, i32, { {}* }*)

; Function Attrs: nounwind
declare void @trace_setActivationFrameForIHSTop({ {}* }*) #0

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
declare void @throwCatchThrow({ {}* }*, { {}*, i32 }*) #1

; Function Attrs: noreturn
declare void @throwReturnFrom(i32, { {}*, i32 }*) #1

; Function Attrs: nounwind
declare void @catchStoreTag({ {}* }*, { {}* }*) #0

declare void @sp_catchIfTagMatchesStoreResultElseRethrow({ {}* }*, { {}* }*, i8*)

declare void @mv_catchIfTagMatchesStoreResultElseRethrow({ {}*, i32 }*, { {}* }*, i8*)

; Function Attrs: nounwind
declare void @catchUnwind({ {}* }*) #0

declare void @sp_blockHandleReturnFrom({ {}* }*, i8*)

declare void @mv_blockHandleReturnFrom({ {}*, i32 }*, i8*)

; Function Attrs: noreturn
declare void @throw_DynamicGo(i32, i32, { {}* }*) #1

declare i32 @tagbodyDynamicGoIndexElseRethrow({ {}* }*, i8*)

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
declare void @getOrCreateLoadTimeValueArray({ {}* }**, i8*, i32, i32) #0

; Function Attrs: nounwind
declare void @sp_copyLoadTimeValue({ {}* }*, { {}* }**, i32) #0

; Function Attrs: nounwind
declare void @mv_copyLoadTimeValue({ {}*, i32 }*, { {}* }**, i32) #0

; Function Attrs: nounwind
declare { {}* }* @loadTimeValueReference({ {}* }**, i32) #0

; Function Attrs: nounwind
declare { i32* }* @loadTimeSymbolReference({ {}* }**, i32) #0

; Function Attrs: nounwind
declare void @sp_getLoadTimeValue({ {}* }*, { {}* }**, i32) #0

; Function Attrs: nounwind
declare void @mv_getLoadTimeValue({ {}*, i32 }*, { {}* }**, i32) #0

; Function Attrs: nounwind
declare void @dumpLoadTimeValues({ {}* }**) #0

; Function Attrs: nounwind
declare void @ltv_makeCons({ {}* }*) #0

; Function Attrs: nounwind
declare void @ltv_makeSourceCodeCons({ {}* }*, i8*, i32, i32) #0

; Function Attrs: nounwind
declare void @ltv_makeArrayObjects({ {}* }*, { {}* }*, i32, i32*) #0

; Function Attrs: nounwind
declare void @ltv_makeHashTable({ {}* }*, { {}* }*) #0

; Function Attrs: nounwind
declare void @rplaca({ {}* }*, { {}* }*) #0

; Function Attrs: nounwind
declare void @rplacd({ {}* }*, { {}* }*) #0

; Function Attrs: nounwind
declare void @ltv_initializeArrayObjectsRowMajorArefOrder({ {}* }*, { {}* }**, i32*) #0

; Function Attrs: nounwind
declare void @ltv_initializeHashTable({ {}* }*, i32, { {}* }**, i32*) #0

; Function Attrs: nounwind
declare void @saveValues({ {}* }*, { {}*, i32 }*) #0

; Function Attrs: nounwind
declare void @loadValues({ {}*, i32 }*, { {}* }*) #0

; Function Attrs: nounwind
declare void @setjmp_set_jump_address({ i8*, i8*, i8*, i8*, i8* }*, i8*) #0

; Function Attrs: nounwind
declare void @setjmp_user0_set_i32({ i8*, i8*, i8*, i8*, i8* }*, i32) #0

; Function Attrs: nounwind
declare i32 @setjmp_user0_get_i32({ i8*, i8*, i8*, i8*, i8* }*) #0

; Function Attrs: nounwind
declare void @setjmp_user0_allocate_set_tmv({ i8*, i8*, i8*, i8*, i8* }*, { {}*, i32 }*) #0

; Function Attrs: nounwind
declare void @setjmp_user0_get_tmv({ {}*, i32 }*, { i8*, i8*, i8*, i8*, i8* }*) #0

; Function Attrs: nounwind
declare void @setjmp_user0_delete_tmv({ i8*, i8*, i8*, i8*, i8* }*) #0

; Function Attrs: nounwind
declare i32 @llvm.eh.sjlj.setjmp(i8*) #0

; Function Attrs: noreturn nounwind
declare void @llvm.eh.sjlj.longjmp(i8*) #3

; Function Attrs: nounwind
declare void @progvSaveSpecials(i8**, { {}* }*, { {}* }*) #0

; Function Attrs: nounwind
declare void @progvRestoreSpecials(i8**) #0

; Function Attrs: nounwind
declare void @pushDynamicBinding({ i32* }*) #0

; Function Attrs: nounwind
declare void @popDynamicBinding({ i32* }*) #0

; Function Attrs: nounwind
declare i32 @matchKeywordOnce({ {}* }*, { {}* }*, i8*) #0

define internal void @___loadTimeDataInitializer() {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %0 = alloca { {}*, i32 }, align 8
  call void @newTmv({ {}*, i32 }* %0)
  %array-element-type = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %array-element-type)
  call void @getOrCreateLoadTimeValueArray({ {}* }** @load-time-value-vector, i8* getelementptr inbounds ([57 x i8]* @":::global-str-/Users/meister/Development/cando/clasp/src/main/test.lsp", i64 0, i64 0), i32 8, i32 3)
  %1 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 0)
  call void @sp_makeNil({ {}* }* %1)
  %2 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 1)
  call void @makeT({ {}* }* %2)
  %3 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 2)
  call void @internSymbol_tsp({ {}* }* %3, i8* getelementptr inbounds ([11 x i8]* @":::symbol-name-TEST-DEBUG", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %4 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 3)
  call void @internSymbol_tsp({ {}* }* %4, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-T", i64 0, i64 0), i8* getelementptr inbounds ([3 x i8]* @":::package-name-CL", i64 0, i64 0))
  call void @sp_copyLoadTimeValue({ {}* }* %array-element-type, { {}* }** @load-time-value-vector, i32 3)
  %5 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 4)
  call void @ltv_makeArrayObjects({ {}* }* %5, { {}* }* %array-element-type, i32 1, i32* getelementptr inbounds ([1 x i32]* @constant-array, i64 0, i64 0))
  %6 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 4)
  call void @ltv_initializeArrayObjectsRowMajorArefOrder({ {}* }* %6, { {}* }** @load-time-value-vector, i32* getelementptr inbounds ([0 x i32]* @constant-array1, i64 0, i64 0))
  %7 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 5)
  call void @makeString({ {}* }* %7, i8* getelementptr inbounds ([20 x i8]* @":::str", i64 0, i64 0))
  %8 = call { i32* }* @loadTimeSymbolReference({ {}* }** @load-time-value-vector, i32 0)
  call void @internSymbol_symsp({ i32* }* %8, i8* getelementptr inbounds ([6 x i8]* @":::symbol-name-PRINT", i64 0, i64 0), i8* getelementptr inbounds ([3 x i8]* @":::package-name-CL", i64 0, i64 0))
  %9 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 6)
  call void @makeString({ {}* }* %9, i8* getelementptr inbounds ([14 x i8]* @":::str2", i64 0, i64 0))
  %10 = call { i32* }* @loadTimeSymbolReference({ {}* }** @load-time-value-vector, i32 1)
  call void @internSymbol_symsp({ i32* }* %10, i8* getelementptr inbounds ([6 x i8]* @":::symbol-name-DEBUG", i64 0, i64 0), i8* getelementptr inbounds ([3 x i8]* @":::package-name-CL", i64 0, i64 0))
  %11 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 7)
  call void @makeString({ {}* }* %11, i8* getelementptr inbounds ([5 x i8]* @":::str3", i64 0, i64 0))
  %12 = call { i32* }* @loadTimeSymbolReference({ {}* }** @load-time-value-vector, i32 2)
  call void @internSymbol_symsp({ i32* }* %12, i8* getelementptr inbounds ([6 x i8]* @":::symbol-name-*FSET", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %13 = call { {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ {}*, i32 }* %0, void ({ {}*, i32 }*, { {}* }*, i32, { {}* }*)* @repl, { {}* }* %13)
          to label %normal-dest unwind label %func-cleanup-landing-pad

normal-dest:                                      ; preds = %entry
  %14 = call { {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ {}*, i32 }* %0, void ({ {}*, i32 }*, { {}* }*, i32, { {}* }*)* @repl4, { {}* }* %14)
          to label %normal-dest1 unwind label %func-cleanup-landing-pad

normal-dest1:                                     ; preds = %normal-dest
  %15 = call { {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ {}*, i32 }* %0, void ({ {}*, i32 }*, { {}* }*, i32, { {}* }*)* @repl5, { {}* }* %15)
          to label %normal-dest2 unwind label %func-cleanup-landing-pad

normal-dest2:                                     ; preds = %normal-dest1
  ret void

func-cleanup-landing-pad:                         ; preds = %normal-dest1, %normal-dest, %entry
  %16 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %17 = extractvalue { i8*, i32 } %16, 0
  store i8* %17, i8** %exn.slot, align 8
  %18 = extractvalue { i8*, i32 } %16, 1
  store i32 %18, i32* %ehselector.slot, align 4
  resume { i8*, i32 } %16
}

define internal void @repl({ {}*, i32 }* %result-ptr, { {}* }* %closed-af-ptr, i32 %num-varargs, { {}* }* %va-list) {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %0 = alloca [3 x { {}* }], align 8
  %.sub = getelementptr inbounds [3 x { {}* }]* %0, i64 0, i64 0
  call void @newTsp({ {}* }* %.sub)
  %gep1 = getelementptr inbounds [3 x { {}* }]* %0, i64 0, i64 1
  call void @newTsp({ {}* }* %gep1)
  %gep2 = getelementptr inbounds [3 x { {}* }]* %0, i64 0, i64 2
  call void @newTsp({ {}* }* %gep2)
  %func = alloca { i32* }, align 8
  call void @newFunction_sp({ i32* }* %func)
  call void @trace_setLineNumberColumnForIHSTop(i32 503, i32 3)
  call void @sp_copyLoadTimeValue({ {}* }* %.sub, { {}* }** @load-time-value-vector, i32 2)
  %"SYMBOL->CL:NIL" = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 0)
  %"SYMBOL->TEST-DEBUG" = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 2)
  %1 = call { {}* }* @activationFrameNil()
  invoke void @sp_makeCompiledFunction({ {}* }* %gep1, void ({ {}*, i32 }*, { {}* }*, i32, { {}* }*)* @TEST-DEBUG, i8* getelementptr inbounds ([57 x i8]* @":::global-str-/Users/meister/Development/cando/clasp/src/main/test.lsp", i64 0, i64 0), { {}* }* %"SYMBOL->TEST-DEBUG", { {}* }* %"SYMBOL->CL:NIL", { {}* }* %1)
          to label %normal-dest unwind label %func-cleanup-landing-pad

normal-dest:                                      ; preds = %entry
  call void @sp_copyLoadTimeValue({ {}* }* %gep2, { {}* }** @load-time-value-vector, i32 0)
  %"SYMBOL->*FSET" = call { i32* }* @loadTimeSymbolReference({ {}* }** @load-time-value-vector, i32 2)
  invoke void @va_symbolFunction({ i32* }* %func, { i32* }* %"SYMBOL->*FSET")
          to label %normal-dest3 unwind label %func-cleanup-landing-pad

normal-dest3:                                     ; preds = %normal-dest
  invoke void @mv_FUNCALL({ {}*, i32 }* %result-ptr, { i32* }* %func, i32 3, { {}* }* %.sub)
          to label %normal-dest4 unwind label %func-cleanup-landing-pad

normal-dest4:                                     ; preds = %normal-dest3
  ret void

func-cleanup-landing-pad:                         ; preds = %normal-dest3, %normal-dest, %entry
  %2 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %3 = extractvalue { i8*, i32 } %2, 0
  store i8* %3, i8** %exn.slot, align 8
  %4 = extractvalue { i8*, i32 } %2, 1
  store i32 %4, i32* %ehselector.slot, align 4
  resume { i8*, i32 } %2
}

define internal void @TEST-DEBUG({ {}*, i32 }* %result-ptr, { {}* }* %closed-af-ptr, i32 %num-varargs, { {}* }* %va-list) {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %lambda-args-2- = alloca { {}* }, align 8
  call void @newAFsp({ {}* }* %lambda-args-2-)
  %temp = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp)
  %0 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %0)
  %func = alloca { i32* }, align 8
  call void @newFunction_sp({ i32* }* %func)
  %1 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %1)
  %func9 = alloca { i32* }, align 8
  call void @newFunction_sp({ i32* }* %func9)
  %2 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %2)
  %func14 = alloca { i32* }, align 8
  call void @newFunction_sp({ i32* }* %func14)
  call void @makeValueFrame({ {}* }* %lambda-args-2-, i32 0, i32 2000001)
  call void @setParentOfActivationFrame({ {}* }* %lambda-args-2-, { {}* }* %closed-af-ptr)
  %correct-num-args = icmp eq i32 %num-varargs, 0
  br i1 %correct-num-args, label %continue3, label %error

error:                                            ; preds = %entry
  %enough-args = icmp slt i32 %num-varargs, 0
  br i1 %enough-args, label %error1, label %continue

error1:                                           ; preds = %error
  invoke void @va_throwNotEnoughArgumentsException(i8* getelementptr inbounds ([11 x i8]* @":::global-str-TEST-DEBUG", i64 0, i64 0), i32 %num-varargs, i32 0)
          to label %normal-dest unwind label %func-cleanup-landing-pad

normal-dest:                                      ; preds = %error1
  call void @unreachableError()
  unreachable

continue:                                         ; preds = %error
  invoke void @va_throwTooManyArgumentsException(i8* getelementptr inbounds ([11 x i8]* @":::global-str-TEST-DEBUG", i64 0, i64 0), i32 %num-varargs, i32 0)
          to label %normal-dest2 unwind label %func-cleanup-landing-pad

normal-dest2:                                     ; preds = %continue
  call void @unreachableError()
  unreachable

continue3:                                        ; preds = %entry
  invoke void @va_fillActivationFrameWithRequiredVarargs({ {}* }* %lambda-args-2-, i32 0, { {}* }* %va-list)
          to label %normal-dest4 unwind label %func-cleanup-landing-pad

normal-dest4:                                     ; preds = %continue3
  %value = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 4)
  call void @attachDebuggingInfoToValueFrame({ {}* }* %lambda-args-2-, { {}* }* %value)
  call void @trace_setActivationFrameForIHSTop({ {}* }* %lambda-args-2-)
  call void @trace_setLineNumberColumnForIHSTop(i32 2, i32 3)
  call void @sp_copyLoadTimeValue({ {}* }* %0, { {}* }** @load-time-value-vector, i32 5)
  %"SYMBOL->CL:PRINT" = call { i32* }* @loadTimeSymbolReference({ {}* }** @load-time-value-vector, i32 0)
  invoke void @va_symbolFunction({ i32* }* %func, { i32* }* %"SYMBOL->CL:PRINT")
          to label %"(TRY-0).normal-dest" unwind label %"(TRY-0).landing-pad"

"(TRY-0).normal-dest":                            ; preds = %normal-dest4
  invoke void @sp_FUNCALL({ {}* }* %temp, { i32* }* %func, i32 1, { {}* }* %0)
          to label %"(TRY-0).normal-dest6" unwind label %"(TRY-0).landing-pad"

"(TRY-0).normal-dest6":                           ; preds = %"(TRY-0).normal-dest"
  call void @trace_setLineNumberColumnForIHSTop(i32 3, i32 3)
  call void @sp_copyLoadTimeValue({ {}* }* %1, { {}* }** @load-time-value-vector, i32 6)
  %"SYMBOL->CL:DEBUG" = call { i32* }* @loadTimeSymbolReference({ {}* }** @load-time-value-vector, i32 1)
  invoke void @va_symbolFunction({ i32* }* %func9, { i32* }* %"SYMBOL->CL:DEBUG")
          to label %"(TRY-0).normal-dest10" unwind label %"(TRY-0).landing-pad"

"(TRY-0).normal-dest10":                          ; preds = %"(TRY-0).normal-dest6"
  invoke void @sp_FUNCALL({ {}* }* %temp, { i32* }* %func9, i32 1, { {}* }* %1)
          to label %"(TRY-0).normal-dest11" unwind label %"(TRY-0).landing-pad"

"(TRY-0).normal-dest11":                          ; preds = %"(TRY-0).normal-dest10"
  call void @trace_setLineNumberColumnForIHSTop(i32 4, i32 3)
  call void @sp_copyLoadTimeValue({ {}* }* %2, { {}* }** @load-time-value-vector, i32 7)
  %"SYMBOL->CL:PRINT15" = call { i32* }* @loadTimeSymbolReference({ {}* }** @load-time-value-vector, i32 0)
  invoke void @va_symbolFunction({ i32* }* %func14, { i32* }* %"SYMBOL->CL:PRINT15")
          to label %"(TRY-0).normal-dest16" unwind label %"(TRY-0).landing-pad"

"(TRY-0).normal-dest16":                          ; preds = %"(TRY-0).normal-dest11"
  invoke void @mv_FUNCALL({ {}*, i32 }* %result-ptr, { i32* }* %func14, i32 1, { {}* }* %2)
          to label %"(TRY-0).try-cont" unwind label %"(TRY-0).landing-pad"

"(TRY-0).landing-pad":                            ; preds = %"(TRY-0).normal-dest16", %"(TRY-0).normal-dest11", %"(TRY-0).normal-dest10", %"(TRY-0).normal-dest6", %"(TRY-0).normal-dest", %normal-dest4
  %3 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
          catch i8* @_ZTIN4core10ReturnFromE
  %4 = extractvalue { i8*, i32 } %3, 0
  store i8* %4, i8** %exn.slot, align 8
  %5 = extractvalue { i8*, i32 } %3, 1
  store i32 %5, i32* %ehselector.slot, align 4
  %6 = call i32 @llvm.eh.typeid.for(i8* @_ZTIN4core10ReturnFromE)
  %7 = icmp eq i32 %5, %6
  br i1 %7, label %"(TRY-0).handler-block8781", label %func-ehcleanup

"(TRY-0).handler-block8781":                      ; preds = %"(TRY-0).landing-pad"
  %8 = call i8* @__cxa_begin_catch(i8* %4)
  invoke void @mv_blockHandleReturnFrom({ {}*, i32 }* %result-ptr, i8* %8)
          to label %"(TRY-0).normal-dest18" unwind label %"(TRY-0).landing-pad21"

"(TRY-0).normal-dest18":                          ; preds = %"(TRY-0).handler-block8781"
  invoke void @__cxa_end_catch()
          to label %"(TRY-0).try-cont" unwind label %"(TRY-0).landing-pad21"

"(TRY-0).try-cont":                               ; preds = %"(TRY-0).normal-dest18", %"(TRY-0).normal-dest16"
  ret void

"(TRY-0).landing-pad21":                          ; preds = %"(TRY-0).normal-dest18", %"(TRY-0).handler-block8781"
  %9 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %10 = extractvalue { i8*, i32 } %9, 0
  store i8* %10, i8** %exn.slot, align 8
  %11 = extractvalue { i8*, i32 } %9, 1
  store i32 %11, i32* %ehselector.slot, align 4
  br label %func-ehcleanup

func-cleanup-landing-pad:                         ; preds = %continue3, %continue, %error1
  %12 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %13 = extractvalue { i8*, i32 } %12, 0
  store i8* %13, i8** %exn.slot, align 8
  %14 = extractvalue { i8*, i32 } %12, 1
  store i32 %14, i32* %ehselector.slot, align 4
  br label %func-ehcleanup

func-ehcleanup:                                   ; preds = %func-cleanup-landing-pad, %"(TRY-0).landing-pad21", %"(TRY-0).landing-pad"
  %sel = phi i32 [ %14, %func-cleanup-landing-pad ], [ %11, %"(TRY-0).landing-pad21" ], [ %5, %"(TRY-0).landing-pad" ]
  %exn7 = phi i8* [ %13, %func-cleanup-landing-pad ], [ %10, %"(TRY-0).landing-pad21" ], [ %4, %"(TRY-0).landing-pad" ]
  %lpad.val = insertvalue { i8*, i32 } undef, i8* %exn7, 0
  %lpad.val8 = insertvalue { i8*, i32 } %lpad.val, i32 %sel, 1
  resume { i8*, i32 } %lpad.val8
}

define internal void @repl4({ {}*, i32 }* %result-ptr, { {}* }* %closed-af-ptr, i32 %num-varargs, { {}* }* %va-list) {
entry:
  call void @mv_copyLoadTimeValue({ {}*, i32 }* %result-ptr, { {}* }** @load-time-value-vector, i32 0)
  ret void
}

define internal void @repl5({ {}*, i32 }* %result-ptr, { {}* }* %closed-af-ptr, i32 %num-varargs, { {}* }* %va-list) {
entry:
  call void @mv_copyLoadTimeValue({ {}*, i32 }* %result-ptr, { {}* }** @load-time-value-vector, i32 2)
  ret void
}

define void @__MAIN_test() {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  invoke void @invokeLlvmFunctionVoid(void ()* @___loadTimeDataInitializer)
          to label %normal-dest unwind label %func-cleanup-landing-pad

normal-dest:                                      ; preds = %entry
  ret void

func-cleanup-landing-pad:                         ; preds = %entry
  %0 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %1 = extractvalue { i8*, i32 } %0, 0
  store i8* %1, i8** %exn.slot, align 8
  %2 = extractvalue { i8*, i32 } %0, 1
  store i32 %2, i32* %ehselector.slot, align 4
  resume { i8*, i32 } %0
}

attributes #0 = { nounwind }
attributes #1 = { noreturn }
attributes #2 = { nounwind readnone }
attributes #3 = { noreturn nounwind }

!llvm.module.flags = !{!0}

!0 = metadata !{i32 2, metadata !"Dwarf Version", i32 2}
