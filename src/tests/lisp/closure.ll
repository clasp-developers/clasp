; ModuleID = 'closure.bc'
target triple = "x86_64-apple-macosx10.7.0"

@":::global-str-/Users/meister/Development/cando/brcl/src/tests/lisp/closure.lsp" = internal unnamed_addr constant [65 x i8] c"/Users/meister/Development/cando/brcl/src/tests/lisp/closure.lsp\00"
@load-time-value-vector = internal global { {}* }* null
@":::global-str-repl" = internal unnamed_addr constant [5 x i8] c"repl\00"
@":::symbol-name-T" = internal unnamed_addr constant [2 x i8] c"T\00"
@":::package-name-CL" = internal unnamed_addr constant [3 x i8] c"CL\00"
@constant-array = internal constant [1 x i32] [i32 1]
@":::symbol-name-A" = internal unnamed_addr constant [2 x i8] c"A\00"
@":::package-name-COMMON-LISP-USER" = internal unnamed_addr constant [17 x i8] c"COMMON-LISP-USER\00"
@constant-array1 = internal constant [1 x i32] [i32 4]
@":::global-str-lambda" = internal unnamed_addr constant [7 x i8] c"lambda\00"
@constant-array2 = internal constant [1 x i32] [i32 1]
@":::symbol-name-Y" = internal unnamed_addr constant [2 x i8] c"Y\00"
@constant-array3 = internal constant [1 x i32] [i32 7]
@":::symbol-name-LAMBDA" = internal unnamed_addr constant [7 x i8] c"LAMBDA\00"
@":::symbol-name-FUNCALL" = internal unnamed_addr constant [8 x i8] c"FUNCALL\00"
@":::global-str-__MAIN_closure" = internal unnamed_addr constant [15 x i8] c"__MAIN_closure\00"
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
  %array-element-type1 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %array-element-type1)
  call void @getOrCreateLoadTimeValueArray({ {}* }** @load-time-value-vector, i8* getelementptr inbounds ([65 x i8]* @":::global-str-/Users/meister/Development/cando/brcl/src/tests/lisp/closure.lsp", i64 0, i64 0), i32 9, i32 1)
  %1 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 0)
  call void @sp_makeNil({ {}* }* %1)
  %2 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 1)
  call void @makeT({ {}* }* %2)
  %3 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 2)
  call void @internSymbol_tsp({ {}* }* %3, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-T", i64 0, i64 0), i8* getelementptr inbounds ([3 x i8]* @":::package-name-CL", i64 0, i64 0))
  call void @sp_copyLoadTimeValue({ {}* }* %array-element-type, { {}* }** @load-time-value-vector, i32 2)
  %4 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 3)
  call void @ltv_makeArrayObjects({ {}* }* %4, { {}* }* %array-element-type, i32 1, i32* getelementptr inbounds ([1 x i32]* @constant-array, i64 0, i64 0))
  %5 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 4)
  call void @internSymbol_tsp({ {}* }* %5, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-A", i64 0, i64 0), i8* getelementptr inbounds ([17 x i8]* @":::package-name-COMMON-LISP-USER", i64 0, i64 0))
  %6 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 3)
  call void @ltv_initializeArrayObjectsRowMajorArefOrder({ {}* }* %6, { {}* }** @load-time-value-vector, i32* getelementptr inbounds ([1 x i32]* @constant-array1, i64 0, i64 0))
  %7 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 5)
  call void @makeFixnum({ {}* }* %7, i32 1)
  call void @sp_copyLoadTimeValue({ {}* }* %array-element-type1, { {}* }** @load-time-value-vector, i32 2)
  %8 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 6)
  call void @ltv_makeArrayObjects({ {}* }* %8, { {}* }* %array-element-type1, i32 1, i32* getelementptr inbounds ([1 x i32]* @constant-array2, i64 0, i64 0))
  %9 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 7)
  call void @internSymbol_tsp({ {}* }* %9, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-Y", i64 0, i64 0), i8* getelementptr inbounds ([17 x i8]* @":::package-name-COMMON-LISP-USER", i64 0, i64 0))
  %10 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 6)
  call void @ltv_initializeArrayObjectsRowMajorArefOrder({ {}* }* %10, { {}* }** @load-time-value-vector, i32* getelementptr inbounds ([1 x i32]* @constant-array3, i64 0, i64 0))
  %11 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 8)
  call void @internSymbol_tsp({ {}* }* %11, i8* getelementptr inbounds ([7 x i8]* @":::symbol-name-LAMBDA", i64 0, i64 0), i8* getelementptr inbounds ([3 x i8]* @":::package-name-CL", i64 0, i64 0))
  %12 = call { i32* }* @loadTimeSymbolReference({ {}* }** @load-time-value-vector, i32 0)
  call void @internSymbol_symsp({ i32* }* %12, i8* getelementptr inbounds ([8 x i8]* @":::symbol-name-FUNCALL", i64 0, i64 0), i8* getelementptr inbounds ([3 x i8]* @":::package-name-CL", i64 0, i64 0))
  %13 = call { {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ {}*, i32 }* %0, void ({ {}*, i32 }*, { {}* }*, i32, { {}* }*)* @repl, { {}* }* %13)
          to label %normal-dest unwind label %func-cleanup-landing-pad

normal-dest:                                      ; preds = %entry
  call void @destructTsp({ {}* }* %array-element-type1)
  call void @destructTsp({ {}* }* %array-element-type)
  call void @destructTmv({ {}*, i32 }* %0)
  ret void

func-cleanup-landing-pad:                         ; preds = %entry
  %14 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %15 = extractvalue { i8*, i32 } %14, 0
  store i8* %15, i8** %exn.slot, align 8
  %16 = extractvalue { i8*, i32 } %14, 1
  store i32 %16, i32* %ehselector.slot, align 4
  call void @destructTsp({ {}* }* %array-element-type1)
  call void @destructTsp({ {}* }* %array-element-type)
  call void @destructTmv({ {}*, i32 }* %0)
  resume { i8*, i32 } %14
}

define internal void @repl({ {}*, i32 }* %result-ptr, { {}* }* %closed-af-ptr, i32 %num-varargs, { {}* }* %va-list) {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %LET = alloca { {}* }, align 8
  call void @newAFsp({ {}* }* %LET)
  %temp = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp)
  %0 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %0)
  %func = alloca { i32* }, align 8
  call void @newFunction_sp({ i32* }* %func)
  call void @trace_setActivationFrameForIHSTop({ {}* }* %LET)
  call void @makeValueFrame({ {}* }* %LET, i32 1, i32 2000044)
  %1 = call { {}* }* @activationFrameNil()
  call void @setParentOfActivationFrame({ {}* }* %LET, { {}* }* %1)
  %value = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 3)
  call void @attachDebuggingInfoToValueFrame({ {}* }* %LET, { {}* }* %value)
  %2 = call { {}* }* @lexicalValueReference(i32 0, i32 0, { {}* }* %LET)
  call void @sp_copyLoadTimeValue({ {}* }* %2, { {}* }** @load-time-value-vector, i32 5)
  call void @trace_setActivationFrameForIHSTop({ {}* }* %LET)
  call void @singleStepCallback()
  call void @trace_setLineNumberColumnForIHSTop(i32 3, i32 14)
  %"SYMBOL->CL:NIL" = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 0)
  %"SYMBOL->CL:LAMBDA" = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 8)
  invoke void @sp_makeCompiledFunction({ {}* }* %0, void ({ {}*, i32 }*, { {}* }*, i32, { {}* }*)* @lambda, i8* getelementptr inbounds ([65 x i8]* @":::global-str-/Users/meister/Development/cando/brcl/src/tests/lisp/closure.lsp", i64 0, i64 0), { {}* }* %"SYMBOL->CL:LAMBDA", { {}* }* %"SYMBOL->CL:NIL", { {}* }* %LET)
          to label %"(TRY-0).normal-dest" unwind label %"(TRY-0).landing-pad"

"(TRY-0).normal-dest":                            ; preds = %entry
  %"SYMBOL->CL:FUNCALL" = call { i32* }* @loadTimeSymbolReference({ {}* }** @load-time-value-vector, i32 0)
  invoke void @va_symbolFunction({ i32* }* %func, { i32* }* %"SYMBOL->CL:FUNCALL")
          to label %"(TRY-0).normal-dest1" unwind label %"(TRY-0).landing-pad"

"(TRY-0).normal-dest1":                           ; preds = %"(TRY-0).normal-dest"
  invoke void @mv_FUNCALL({ {}*, i32 }* %result-ptr, { i32* }* %func, i32 1, { {}* }* %0)
          to label %"(TRY-0).normal-dest2" unwind label %"(TRY-0).landing-pad"

"(TRY-0).normal-dest2":                           ; preds = %"(TRY-0).normal-dest1"
  call void @destructFunction_sp({ i32* }* %func)
  call void @destructTsp({ {}* }* %0)
  call void @destructTsp({ {}* }* %temp)
  call void @destructAFsp({ {}* }* %LET)
  ret void

"(TRY-0).landing-pad":                            ; preds = %"(TRY-0).normal-dest1", %"(TRY-0).normal-dest", %entry
  %3 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %4 = extractvalue { i8*, i32 } %3, 0
  store i8* %4, i8** %exn.slot, align 8
  %5 = extractvalue { i8*, i32 } %3, 1
  store i32 %5, i32* %ehselector.slot, align 4
  call void @destructFunction_sp({ i32* }* %func)
  call void @destructTsp({ {}* }* %0)
  call void @destructTsp({ {}* }* %temp)
  call void @destructAFsp({ {}* }* %LET)
  resume { i8*, i32 } %3
}

define internal void @lambda({ {}*, i32 }* %result-ptr, { {}* }* %closed-af-ptr, i32 %num-varargs, { {}* }* %va-list) {
"(TRY-0).entry":
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %lambda-args-30- = alloca { {}* }, align 8
  call void @newAFsp({ {}* }* %lambda-args-30-)
  %temp-result = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp-result)
  %temp-result2 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp-result2)
  %temp = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp)
  call void @debugInspectActivationFrame({ {}* }* %closed-af-ptr)
  call void @makeValueFrame({ {}* }* %lambda-args-30-, i32 1, i32 2000045)
  %enough-args = icmp slt i32 %num-varargs, 0
  br i1 %enough-args, label %"(TRY-0).error", label %"(TRY-0).continue"

"(TRY-0).error":                                  ; preds = %"(TRY-0).entry"
  invoke void @va_throwNotEnoughArgumentsException(i8* getelementptr inbounds ([7 x i8]* @":::global-str-lambda", i64 0, i64 0), i32 %num-varargs, i32 0)
          to label %"(TRY-0).normal-dest" unwind label %"(TRY-0).func-cleanup-landing-pad"

"(TRY-0).normal-dest":                            ; preds = %"(TRY-0).error"
  call void @unreachableError()
  unreachable

"(TRY-0).continue":                               ; preds = %"(TRY-0).entry"
  invoke void @va_throwIfExcessKeywordArguments(i8* getelementptr inbounds ([7 x i8]* @":::global-str-lambda", i64 0, i64 0), i32 %num-varargs, { {}* }* %va-list, i32 0)
          to label %"(TRY-0).normal-dest1" unwind label %"(TRY-0).func-cleanup-landing-pad"

"(TRY-0).normal-dest1":                           ; preds = %"(TRY-0).continue"
  %0 = call { {}* }* @lexicalValueReference(i32 0, i32 0, { {}* }* %lambda-args-30-)
  call void @sp_lexicalValueRead({ {}* }* %0, i32 1, i32 0, { {}* }* %lambda-args-30-)
  call void @setParentOfActivationFrame({ {}* }* %lambda-args-30-, { {}* }* %closed-af-ptr)
  call void @debugInspectActivationFrame({ {}* }* %lambda-args-30-)

  %value = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 6)
  call void @attachDebuggingInfoToValueFrame({ {}* }* %lambda-args-30-, { {}* }* %value)
  call void @mv_lexicalValueRead({ {}*, i32 }* %result-ptr, i32 0, i32 0, { {}* }* %lambda-args-30-)
  call void @destructTsp({ {}* }* %temp)
  call void @destructTsp({ {}* }* %temp-result2)
  call void @destructTsp({ {}* }* %temp-result)
  call void @destructAFsp({ {}* }* %lambda-args-30-)
  ret void

"(TRY-0).func-cleanup-landing-pad":               ; preds = %"(TRY-0).continue", %"(TRY-0).error"
  %1 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %2 = extractvalue { i8*, i32 } %1, 0
  store i8* %2, i8** %exn.slot, align 8
  %3 = extractvalue { i8*, i32 } %1, 1
  store i32 %3, i32* %ehselector.slot, align 4
  call void @destructTsp({ {}* }* %temp)
  call void @destructTsp({ {}* }* %temp-result2)
  call void @destructTsp({ {}* }* %temp-result)
  call void @destructAFsp({ {}* }* %lambda-args-30-)
  resume { i8*, i32 } %1
}

define void @__MAIN_closure() {
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
