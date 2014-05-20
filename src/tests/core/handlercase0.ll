; ModuleID = 'handlercase0.bc'
target triple = "x86_64-apple-macosx"

@":::global-str-/Users/meister/Development/cando/brcl/src/tests/core/handlercase0.lsp" = internal unnamed_addr constant [70 x i8] c"/Users/meister/Development/cando/brcl/src/tests/core/handlercase0.lsp\00"
@load-time-value-vector = internal global { i32*, {}* }* null
@":::global-str-repl" = internal unnamed_addr constant [5 x i8] c"repl\00"
@":::symbol-name-T" = internal unnamed_addr constant [2 x i8] c"T\00"
@":::package-name-COMMON-LISP" = internal unnamed_addr constant [12 x i8] c"COMMON-LISP\00"
@constant-array = internal constant [1 x i32] [i32 1]
@":::symbol-name-XXX" = internal unnamed_addr constant [4 x i8] c"XXX\00"
@":::package-name-CORE" = internal unnamed_addr constant [5 x i8] c"CORE\00"
@constant-array1 = internal constant [1 x i32] [i32 4]
@constant-array2 = internal constant [1 x i32] zeroinitializer
@constant-array3 = internal constant [0 x i32] zeroinitializer
@":::symbol-name-*HANDLER-CLUSTERS*" = internal unnamed_addr constant [19 x i8] c"*HANDLER-CLUSTERS*\00"
@":::symbol-name-ERROR" = internal unnamed_addr constant [6 x i8] c"ERROR\00"
@":::global-str-lambda" = internal unnamed_addr constant [7 x i8] c"lambda\00"
@":::symbol-name-CONS" = internal unnamed_addr constant [5 x i8] c"CONS\00"
@":::symbol-name-LIST" = internal unnamed_addr constant [5 x i8] c"LIST\00"
@":::str" = internal unnamed_addr constant [5 x i8] c"Bomb\00"
@_ZTIN4core2GoE = external constant i8
@_ZTIN4core10ReturnFromE = external constant i8
@constant-array4 = internal constant [1 x i32] [i32 1]
@":::symbol-name-C" = internal unnamed_addr constant [2 x i8] c"C\00"
@constant-array5 = internal constant [1 x i32] [i32 9]
@":::str6" = internal unnamed_addr constant [7 x i8] c"Error \00"
@":::symbol-name-PRINT" = internal unnamed_addr constant [6 x i8] c"PRINT\00"
@":::global-str-___user_handlercase0" = internal unnamed_addr constant [21 x i8] c"___user_handlercase0\00"
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
declare void @throw_Go(i32, i32) #1

declare i32 @tagbodyGoIndexElseRethrow(i8*)

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
  %array-element-type1 = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %array-element-type1)
  %array-element-type2 = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %array-element-type2)
  call void @getLoadTimeValueArray({ i32*, {}* }** @load-time-value-vector, i8* getelementptr inbounds ([70 x i8]* @":::global-str-/Users/meister/Development/cando/brcl/src/tests/core/handlercase0.lsp", i64 0, i64 0), i32 11, i32 5)
  call void @lowLevelTrace(i32 1000000018)
  %0 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  call void @sp_makeNil({ i32*, {}* }* %0)
  call void @lowLevelTrace(i32 1000000019)
  %1 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 1)
  call void @makeT({ i32*, {}* }* %1)
  call void @lowLevelTrace(i32 1000000020)
  %2 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 2)
  call void @internSymbol_tsp({ i32*, {}* }* %2, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-T", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %array-element-type, { i32*, {}* }** @load-time-value-vector, i32 2)
  call void @lowLevelTrace(i32 1000000021)
  %3 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 3)
  call void @ltv_makeArrayObjects({ i32*, {}* }* %3, { i32*, {}* }* %array-element-type, i32 1, i32* getelementptr inbounds ([1 x i32]* @constant-array, i64 0, i64 0))
  call void @lowLevelTrace(i32 1000000022)
  %4 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  call void @internSymbol_tsp({ i32*, {}* }* %4, i8* getelementptr inbounds ([4 x i8]* @":::symbol-name-XXX", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %5 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 3)
  call void @ltv_initializeArrayObjectsRowMajorArefOrder({ i32*, {}* }* %5, { i32*, {}* }** @load-time-value-vector, i32* getelementptr inbounds ([1 x i32]* @constant-array1, i64 0, i64 0))
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %array-element-type1, { i32*, {}* }** @load-time-value-vector, i32 2)
  call void @lowLevelTrace(i32 1000000023)
  %6 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 5)
  call void @ltv_makeArrayObjects({ i32*, {}* }* %6, { i32*, {}* }* %array-element-type1, i32 1, i32* getelementptr inbounds ([1 x i32]* @constant-array2, i64 0, i64 0))
  %7 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 5)
  call void @ltv_initializeArrayObjectsRowMajorArefOrder({ i32*, {}* }* %7, { i32*, {}* }** @load-time-value-vector, i32* getelementptr inbounds ([0 x i32]* @constant-array3, i64 0, i64 0))
  call void @lowLevelTrace(i32 1000000024)
  %8 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  call void @internSymbol_symsp({ i32*, i32* }* %8, i8* getelementptr inbounds ([19 x i8]* @":::symbol-name-*HANDLER-CLUSTERS*", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  call void @lowLevelTrace(i32 1000000028)
  %9 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 6)
  call void @internSymbol_tsp({ i32*, {}* }* %9, i8* getelementptr inbounds ([6 x i8]* @":::symbol-name-ERROR", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  call void @lowLevelTrace(i32 1000000032)
  %10 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 1)
  call void @internSymbol_symsp({ i32*, i32* }* %10, i8* getelementptr inbounds ([5 x i8]* @":::symbol-name-CONS", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  call void @lowLevelTrace(i32 1000000033)
  %11 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 2)
  call void @internSymbol_symsp({ i32*, i32* }* %11, i8* getelementptr inbounds ([5 x i8]* @":::symbol-name-LIST", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  call void @lowLevelTrace(i32 1000000035)
  %12 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 7)
  call void @makeString({ i32*, {}* }* %12, i8* getelementptr inbounds ([5 x i8]* @":::str", i64 0, i64 0))
  call void @lowLevelTrace(i32 1000000036)
  %13 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 3)
  call void @internSymbol_symsp({ i32*, i32* }* %13, i8* getelementptr inbounds ([6 x i8]* @":::symbol-name-ERROR", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %array-element-type2, { i32*, {}* }** @load-time-value-vector, i32 2)
  call void @lowLevelTrace(i32 1000000039)
  %14 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 8)
  call void @ltv_makeArrayObjects({ i32*, {}* }* %14, { i32*, {}* }* %array-element-type2, i32 1, i32* getelementptr inbounds ([1 x i32]* @constant-array4, i64 0, i64 0))
  call void @lowLevelTrace(i32 1000000040)
  %15 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 9)
  call void @internSymbol_tsp({ i32*, {}* }* %15, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-C", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %16 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 8)
  call void @ltv_initializeArrayObjectsRowMajorArefOrder({ i32*, {}* }* %16, { i32*, {}* }** @load-time-value-vector, i32* getelementptr inbounds ([1 x i32]* @constant-array5, i64 0, i64 0))
  call void @lowLevelTrace(i32 1000000043)
  %17 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 10)
  call void @makeString({ i32*, {}* }* %17, i8* getelementptr inbounds ([7 x i8]* @":::str6", i64 0, i64 0))
  call void @lowLevelTrace(i32 1000000044)
  %18 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  call void @internSymbol_symsp({ i32*, i32* }* %18, i8* getelementptr inbounds ([6 x i8]* @":::symbol-name-PRINT", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  ret void
}

define internal void @repl({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %temp = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp)
  %LET = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %LET)
  %temp1 = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp1)
  %temp2 = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp2)
  %LET3 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %LET3)
  %call-args = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args)
  %call-args6 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args6)
  %call-args7 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args7)
  %temp13 = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp13)
  %temp-mv-result = alloca { i32*, {}*, i32 }, align 8
  call void @newTmv({ i32*, {}*, i32 }* %temp-mv-result)
  %call-args14 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args14)
  %temp19 = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp19)
  %temp-mv-result20 = alloca { i32*, {}*, i32 }, align 8
  call void @newTmv({ i32*, {}*, i32 }* %temp-mv-result20)
  %LET21 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %LET21)
  %temp23 = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp23)
  %call-args24 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args24)
  %call-args25 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args25)
  %0 = call { i32*, {}* }* @activationFrameNil()
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %0)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %LET)
  call void @makeValueFrame({ i32*, {}* }* %LET, i32 1)
  %1 = call { i32*, {}* }* @activationFrameNil()
  call void @setParentOfActivationFrame({ i32*, {}* }* %LET, { i32*, {}* }* %1)
  %value = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 3)
  call void @attachDebuggingInfoToValueFrame({ i32*, {}* }* %LET, { i32*, {}* }* %value)
  %2 = call { i32*, {}* }* @lexicalValueReference(i32 0, i32 0, { i32*, {}* }* %LET)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %2, { i32*, {}* }** @load-time-value-vector, i32 0)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %LET)
  call void @singleStepCallback()
  br label %"(TRY-10315042).tagbody-#:G2866-0"

"(TRY-10315042).tagbody-#:G2866-0":               ; preds = %"(TRY-10320770).normal-dest28", %entry
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %LET3)
  call void @makeValueFrame({ i32*, {}* }* %LET3, i32 0)
  call void @setParentOfActivationFrame({ i32*, {}* }* %LET3, { i32*, {}* }* %LET)
  %value4 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 5)
  call void @attachDebuggingInfoToValueFrame({ i32*, {}* }* %LET3, { i32*, {}* }* %value4)
  %"SYMBOL->*HANDLER-CLUSTERS*" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  call void @pushDynamicBinding({ i32*, i32* }* %"SYMBOL->*HANDLER-CLUSTERS*")
  %"SYMBOL->*HANDLER-CLUSTERS*5" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  %3 = call { i32*, {}* }* @symbolValueReference({ i32*, i32* }* %"SYMBOL->*HANDLER-CLUSTERS*5")
  call void @lowLevelTrace(i32 1000000025)
  call void @trace_setLineNumberColumnForIHSTop(i32 8, i32 38)
  call void @makeValueFrame({ i32*, {}* }* %call-args, i32 2)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args, { i32*, {}* }* %LET)
  %call-args-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 0)
  call void @lowLevelTrace(i32 1000000026)
  call void @trace_setLineNumberColumnForIHSTop(i32 8, i32 45)
  call void @makeValueFrame({ i32*, {}* }* %call-args6, i32 1)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args6, { i32*, {}* }* %LET)
  %call-args6-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args6, i32 0)
  call void @lowLevelTrace(i32 1000000027)
  call void @trace_setLineNumberColumnForIHSTop(i32 8, i32 52)
  call void @makeValueFrame({ i32*, {}* }* %call-args7, i32 2)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args7, { i32*, {}* }* %LET)
  %call-args7-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args7, i32 0)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args7-ref-0, { i32*, {}* }** @load-time-value-vector, i32 6)
  %call-args7-ref-1 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args7, i32 1)
  %"SYMBOL->nil" = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  invoke void @sp_makeCompiledFunction({ i32*, {}* }* %call-args7-ref-1, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @lambda, i8* getelementptr inbounds ([70 x i8]* @":::global-str-/Users/meister/Development/cando/brcl/src/tests/core/handlercase0.lsp", i64 0, i64 0), { i32*, {}* }* %"SYMBOL->nil", { i32*, {}* }* %LET)
          to label %"(TRY-10321579).normal-dest" unwind label %"(TRY-10321579).landing-pad"

"(TRY-10321579).normal-dest":                     ; preds = %"(TRY-10315042).tagbody-#:G2866-0"
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args7)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:CONS" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 1)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %call-args6-ref-0, { i32*, i32* }* %"SYMBOL->COMMON-LISP:CONS", { i32*, {}* }* %call-args7)
          to label %"(TRY-10321579).normal-dest8" unwind label %"(TRY-10321579).landing-pad"

"(TRY-10321579).normal-dest8":                    ; preds = %"(TRY-10321579).normal-dest"
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args6)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:LIST" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 2)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %call-args-ref-0, { i32*, i32* }* %"SYMBOL->COMMON-LISP:LIST", { i32*, {}* }* %call-args6)
          to label %"(TRY-10321579).normal-dest9" unwind label %"(TRY-10321579).landing-pad"

"(TRY-10321579).normal-dest9":                    ; preds = %"(TRY-10321579).normal-dest8"
  %call-args-ref-1 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 1)
  %"SYMBOL->*HANDLER-CLUSTERS*10" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  call void @sp_symbolValueRead({ i32*, {}* }* %call-args-ref-1, { i32*, i32* }* %"SYMBOL->*HANDLER-CLUSTERS*10")
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:CONS11" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 1)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %3, { i32*, i32* }* %"SYMBOL->COMMON-LISP:CONS11", { i32*, {}* }* %call-args)
          to label %"(TRY-10321579).normal-dest12" unwind label %"(TRY-10321579).landing-pad"

"(TRY-10321579).normal-dest12":                   ; preds = %"(TRY-10321579).normal-dest9"
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %LET3)
  call void @singleStepCallback()
  call void @lowLevelTrace(i32 1000000034)
  call void @trace_setLineNumberColumnForIHSTop(i32 13, i32 23)
  call void @makeValueFrame({ i32*, {}* }* %call-args14, i32 1)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args14, { i32*, {}* }* %LET3)
  %call-args14-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args14, i32 0)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args14-ref-0, { i32*, {}* }** @load-time-value-vector, i32 7)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args14)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:ERROR" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 3)
  invoke void @mv_invokePossibleMultipleValueSymbolFunction({ i32*, {}*, i32 }* %temp-mv-result, { i32*, i32* }* %"SYMBOL->COMMON-LISP:ERROR", { i32*, {}* }* %call-args14)
          to label %"(TRY-10321579).normal-dest15" unwind label %"(TRY-10321579).landing-pad"

"(TRY-10321579).normal-dest15":                   ; preds = %"(TRY-10321579).normal-dest12"
  call void @lowLevelTrace(i32 1000000037)
  invoke void @throwReturnFrom(i32 0, { i32*, {}*, i32 }* %temp-mv-result)
          to label %"(TRY-10321579).normal-dest16" unwind label %"(TRY-10321579).landing-pad"

"(TRY-10321579).normal-dest16":                   ; preds = %"(TRY-10321579).normal-dest15"
  unreachable

"(TRY-10321579).landing-pad":                     ; preds = %"(TRY-10321579).normal-dest15", %"(TRY-10321579).normal-dest12", %"(TRY-10321579).normal-dest9", %"(TRY-10321579).normal-dest8", %"(TRY-10321579).normal-dest", %"(TRY-10315042).tagbody-#:G2866-0"
  %4 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
          catch i8* @_ZTIN4core2GoE
          catch i8* @_ZTIN4core10ReturnFromE
  call void @lowLevelTrace(i32 1000000038)
  %5 = extractvalue { i8*, i32 } %4, 0
  store i8* %5, i8** %exn.slot, align 8
  %6 = extractvalue { i8*, i32 } %4, 1
  store i32 %6, i32* %ehselector.slot, align 4
  %"SYMBOL->*HANDLER-CLUSTERS*18" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  call void @popDynamicBinding({ i32*, i32* }* %"SYMBOL->*HANDLER-CLUSTERS*18")
  br label %"(TRY-10320770).dispatch-header"

"(TRY-10315042).tagbody-TAG-1":                   ; preds = %"(TRY-10320770).normal-dest28"
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %LET21)
  call void @makeValueFrame({ i32*, {}* }* %LET21, i32 1)
  call void @setParentOfActivationFrame({ i32*, {}* }* %LET21, { i32*, {}* }* %LET)
  %value22 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 8)
  call void @attachDebuggingInfoToValueFrame({ i32*, {}* }* %LET21, { i32*, {}* }* %value22)
  %7 = call { i32*, {}* }* @lexicalValueReference(i32 0, i32 0, { i32*, {}* }* %LET21)
  call void @sp_lexicalValueRead({ i32*, {}* }* %7, i32 0, i32 0, { i32*, {}* }* %LET)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %LET21)
  call void @singleStepCallback()
  call void @lowLevelTrace(i32 1000000041)
  call void @trace_setLineNumberColumnForIHSTop(i32 16, i32 9)
  call void @makeValueFrame({ i32*, {}* }* %call-args24, i32 1)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args24, { i32*, {}* }* %LET21)
  %call-args24-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args24, i32 0)
  call void @lowLevelTrace(i32 1000000042)
  call void @trace_setLineNumberColumnForIHSTop(i32 16, i32 17)
  call void @makeValueFrame({ i32*, {}* }* %call-args25, i32 2)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args25, { i32*, {}* }* %LET21)
  %call-args25-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args25, i32 0)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args25-ref-0, { i32*, {}* }** @load-time-value-vector, i32 10)
  %call-args25-ref-1 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args25, i32 1)
  call void @sp_lexicalValueRead({ i32*, {}* }* %call-args25-ref-1, i32 0, i32 0, { i32*, {}* }* %LET21)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args25)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:LIST26" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 2)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %call-args24-ref-0, { i32*, i32* }* %"SYMBOL->COMMON-LISP:LIST26", { i32*, {}* }* %call-args25)
          to label %"(TRY-10347894).normal-dest" unwind label %"(TRY-10347894).landing-pad"

"(TRY-10347894).normal-dest":                     ; preds = %"(TRY-10315042).tagbody-TAG-1"
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args24)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:PRINT" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  invoke void @mv_invokePossibleMultipleValueSymbolFunction({ i32*, {}*, i32 }* %temp-mv-result20, { i32*, i32* }* %"SYMBOL->COMMON-LISP:PRINT", { i32*, {}* }* %call-args24)
          to label %"(TRY-10347894).normal-dest27" unwind label %"(TRY-10347894).landing-pad"

"(TRY-10347894).normal-dest27":                   ; preds = %"(TRY-10347894).normal-dest"
  call void @lowLevelTrace(i32 1000000046)
  invoke void @throwReturnFrom(i32 0, { i32*, {}*, i32 }* %temp-mv-result20)
          to label %"(TRY-10320770).normal-dest" unwind label %"(TRY-10320770).landing-pad"

"(TRY-10347894).landing-pad":                     ; preds = %"(TRY-10347894).normal-dest", %"(TRY-10315042).tagbody-TAG-1"
  %8 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
          catch i8* @_ZTIN4core2GoE
          catch i8* @_ZTIN4core10ReturnFromE
  call void @lowLevelTrace(i32 1000000045)
  %9 = extractvalue { i8*, i32 } %8, 0
  store i8* %9, i8** %exn.slot, align 8
  %10 = extractvalue { i8*, i32 } %8, 1
  store i32 %10, i32* %ehselector.slot, align 4
  br label %"(TRY-10320770).dispatch-header"

"(TRY-10320770).normal-dest":                     ; preds = %"(TRY-10347894).normal-dest27"
  unreachable

"(TRY-10320770).landing-pad":                     ; preds = %"(TRY-10347894).normal-dest27"
  %11 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
          catch i8* @_ZTIN4core2GoE
          catch i8* @_ZTIN4core10ReturnFromE
  call void @lowLevelTrace(i32 1000000047)
  %12 = extractvalue { i8*, i32 } %11, 0
  store i8* %12, i8** %exn.slot, align 8
  %13 = extractvalue { i8*, i32 } %11, 1
  store i32 %13, i32* %ehselector.slot, align 4
  br label %"(TRY-10320770).dispatch-header"

"(TRY-10320770).dispatch-header":                 ; preds = %"(TRY-10320770).landing-pad", %"(TRY-10347894).landing-pad", %"(TRY-10321579).landing-pad"
  %exn = phi i8* [ %12, %"(TRY-10320770).landing-pad" ], [ %9, %"(TRY-10347894).landing-pad" ], [ %5, %"(TRY-10321579).landing-pad" ]
  %ehselector-slot = phi i32 [ %13, %"(TRY-10320770).landing-pad" ], [ %10, %"(TRY-10347894).landing-pad" ], [ %6, %"(TRY-10321579).landing-pad" ]
  call void @mv_copyLoadTimeValue({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }** @load-time-value-vector, i32 0)
  %14 = call i32 @llvm.eh.typeid.for(i8* @_ZTIN4core2GoE)
  %15 = icmp eq i32 %ehselector-slot, %14
  br i1 %15, label %"(TRY-10320770).handler-block304023", label %"(TRY-10314110).dispatch-header"

"(TRY-10320770).handler-block304023":             ; preds = %"(TRY-10320770).dispatch-header"
  %16 = call i8* @__cxa_begin_catch(i8* %exn)
  %17 = invoke i32 @tagbodyGoIndexElseRethrow(i8* %16)
          to label %"(TRY-10320770).normal-dest28" unwind label %"(TRY-10315042).landing-pad"

"(TRY-10320770).normal-dest28":                   ; preds = %"(TRY-10320770).handler-block304023"
  switch i32 %17, label %"(TRY-10320770).switch-default" [
    i32 0, label %"(TRY-10315042).tagbody-#:G2866-0"
    i32 1, label %"(TRY-10315042).tagbody-TAG-1"
  ]

"(TRY-10320770).switch-default":                  ; preds = %"(TRY-10320770).normal-dest28"
  invoke void @throwIllegalSwitchValue(i32 %17, i32 2)
          to label %"(TRY-10320770).normal-dest29" unwind label %"(TRY-10315042).landing-pad"

"(TRY-10320770).normal-dest29":                   ; preds = %"(TRY-10320770).switch-default"
  unreachable

"(TRY-10315042).landing-pad":                     ; preds = %"(TRY-10320770).switch-default", %"(TRY-10320770).handler-block304023"
  %18 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
          catch i8* @_ZTIN4core10ReturnFromE
  call void @lowLevelTrace(i32 1000000048)
  %19 = extractvalue { i8*, i32 } %18, 0
  store i8* %19, i8** %exn.slot, align 8
  %20 = extractvalue { i8*, i32 } %18, 1
  store i32 %20, i32* %ehselector.slot, align 4
  br label %"(TRY-10314110).dispatch-header"

"(TRY-10314110).dispatch-header":                 ; preds = %"(TRY-10315042).landing-pad", %"(TRY-10320770).dispatch-header"
  %exn3340 = phi i8* [ %19, %"(TRY-10315042).landing-pad" ], [ %exn, %"(TRY-10320770).dispatch-header" ]
  %ehselector-slot3238 = phi i32 [ %20, %"(TRY-10315042).landing-pad" ], [ %ehselector-slot, %"(TRY-10320770).dispatch-header" ]
  %21 = call i32 @llvm.eh.typeid.for(i8* @_ZTIN4core10ReturnFromE)
  %22 = icmp eq i32 %ehselector-slot3238, %21
  br i1 %22, label %"(TRY-10314110).handler-block305332", label %func-ehcleanup

"(TRY-10314110).handler-block305332":             ; preds = %"(TRY-10314110).dispatch-header"
  %23 = call i8* @__cxa_begin_catch(i8* %exn3340)
  invoke void @mv_blockHandleReturnFrom({ i32*, {}*, i32 }* %result-ptr, i8* %23)
          to label %"(TRY-10314110).normal-dest" unwind label %func-cleanup-landing-pad

"(TRY-10314110).normal-dest":                     ; preds = %"(TRY-10314110).handler-block305332"
  invoke void @__cxa_end_catch()
          to label %"(TRY-10314110).try-cont" unwind label %func-cleanup-landing-pad

"(TRY-10314110).try-cont":                        ; preds = %"(TRY-10314110).normal-dest"
  ret void

func-cleanup-landing-pad:                         ; preds = %"(TRY-10314110).normal-dest", %"(TRY-10314110).handler-block305332"
  %24 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  call void @lowLevelTrace(i32 1000000050)
  %25 = extractvalue { i8*, i32 } %24, 0
  store i8* %25, i8** %exn.slot, align 8
  %26 = extractvalue { i8*, i32 } %24, 1
  store i32 %26, i32* %ehselector.slot, align 4
  call void @debugPrintI32(i32 100)
  br label %func-ehcleanup

func-ehcleanup:                                   ; preds = %func-cleanup-landing-pad, %"(TRY-10314110).dispatch-header"
  %sel = phi i32 [ %26, %func-cleanup-landing-pad ], [ %ehselector-slot3238, %"(TRY-10314110).dispatch-header" ]
  %exn7 = phi i8* [ %25, %func-cleanup-landing-pad ], [ %exn3340, %"(TRY-10314110).dispatch-header" ]
  call void @debugPrintI32(i32 101)
  %lpad.val = insertvalue { i8*, i32 } undef, i8* %exn7, 0
  call void @debugPrintI32(i32 90)
  %lpad.val8 = insertvalue { i8*, i32 } %lpad.val, i32 %sel, 1
  call void @debugPrintI32(i32 91)
  resume { i8*, i32 } %lpad.val8
}

define internal void @lambda({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
"(TRY-10321579).entry":
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %lambda-args-24- = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %lambda-args-24-)
  %temp = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp)
  %tsetq = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %tsetq)
  %given-num-args = call i32 @activationFrameSize({ i32*, {}* }* %activation-frame-ptr)
  %correct-num-args = icmp eq i32 %given-num-args, 1
  br i1 %correct-num-args, label %"(TRY-10321579).continue3", label %"(TRY-10321579).error"

"(TRY-10321579).error":                           ; preds = %"(TRY-10321579).entry"
  %enough-args = icmp slt i32 %given-num-args, 1
  br i1 %enough-args, label %"(TRY-10321579).error1", label %"(TRY-10321579).continue"

"(TRY-10321579).error1":                          ; preds = %"(TRY-10321579).error"
  invoke void @throwNotEnoughArgumentsException(i8* getelementptr inbounds ([7 x i8]* @":::global-str-lambda", i64 0, i64 0), { i32*, {}* }* %activation-frame-ptr, i32 %given-num-args, i32 1)
          to label %"(TRY-10321579).normal-dest" unwind label %"(TRY-10321579).func-cleanup-landing-pad"

"(TRY-10321579).normal-dest":                     ; preds = %"(TRY-10321579).error1"
  unreachable

"(TRY-10321579).continue":                        ; preds = %"(TRY-10321579).error"
  invoke void @throwTooManyArgumentsException(i8* getelementptr inbounds ([7 x i8]* @":::global-str-lambda", i64 0, i64 0), { i32*, {}* }* %activation-frame-ptr, i32 %given-num-args, i32 1)
          to label %"(TRY-10321579).normal-dest2" unwind label %"(TRY-10321579).func-cleanup-landing-pad"

"(TRY-10321579).normal-dest2":                    ; preds = %"(TRY-10321579).continue"
  unreachable

"(TRY-10321579).continue3":                       ; preds = %"(TRY-10321579).entry"
  call void @copyAFsp({ i32*, {}* }* %lambda-args-24-, { i32*, {}* }* %activation-frame-ptr)
  %0 = call { i32*, {}* }* @lexicalValueReference(i32 1, i32 0, { i32*, {}* }* %lambda-args-24-)
  call void @sp_lexicalValueRead({ i32*, {}* }* %tsetq, i32 0, i32 0, { i32*, {}* }* %lambda-args-24-)
  call void @sp_copyTsp({ i32*, {}* }* %0, { i32*, {}* }* %tsetq)
  call void @sp_copyTsp({ i32*, {}* }* %temp, { i32*, {}* }* %tsetq)
  call void @lowLevelTrace(i32 1009000029)
  invoke void @throw_Go(i32 0, i32 1)
          to label %"(TRY-10331489).normal-dest" unwind label %"(TRY-10331489).landing-pad"

"(TRY-10331489).normal-dest":                     ; preds = %"(TRY-10321579).continue3"
  unreachable

"(TRY-10331489).landing-pad":                     ; preds = %"(TRY-10321579).continue3"
  %1 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  call void @lowLevelTrace(i32 1000000029)
  %2 = extractvalue { i8*, i32 } %1, 0
  store i8* %2, i8** %exn.slot, align 8
  %3 = extractvalue { i8*, i32 } %1, 1
  store i32 %3, i32* %ehselector.slot, align 4
  call void @lowLevelTrace(i32 1010000029)
  br label %"(TRY-10321579).func-ehcleanup"

"(TRY-10321579).func-cleanup-landing-pad":        ; preds = %"(TRY-10321579).continue", %"(TRY-10321579).error1"
  %4 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  call void @lowLevelTrace(i32 1000000030)
  %5 = extractvalue { i8*, i32 } %4, 0
  store i8* %5, i8** %exn.slot, align 8
  %6 = extractvalue { i8*, i32 } %4, 1
  store i32 %6, i32* %ehselector.slot, align 4
  call void @debugPrintI32(i32 100)
  br label %"(TRY-10321579).func-ehcleanup"

"(TRY-10321579).func-ehcleanup":                  ; preds = %"(TRY-10321579).func-cleanup-landing-pad", %"(TRY-10331489).landing-pad"
  %sel = phi i32 [ %6, %"(TRY-10321579).func-cleanup-landing-pad" ], [ %3, %"(TRY-10331489).landing-pad" ]
  %exn7 = phi i8* [ %5, %"(TRY-10321579).func-cleanup-landing-pad" ], [ %2, %"(TRY-10331489).landing-pad" ]
  call void @debugPrintI32(i32 101)
  call void @lowLevelTrace(i32 1020000029)
  %lpad.val = insertvalue { i8*, i32 } undef, i8* %exn7, 0
  call void @debugPrintI32(i32 90)
  %lpad.val8 = insertvalue { i8*, i32 } %lpad.val, i32 %sel, 1
  call void @debugPrintI32(i32 91)
  call void @lowLevelTrace(i32 1030000029)
  resume { i8*, i32 } %lpad.val8
}

define void @___user_handlercase0() {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %result = alloca { i32*, {}*, i32 }, align 8
  call void @newTmv({ i32*, {}*, i32 }* %result)
  call void @lowLevelTrace(i32 1000000052)
  call void @lowLevelTrace(i32 1000000053)
  invoke void @invokeLlvmFunctionVoid(void ()* @___loadTimeDataInitializer)
          to label %normal-dest unwind label %func-cleanup-landing-pad

normal-dest:                                      ; preds = %entry
  call void @lowLevelTrace(i32 1000000054)
  %0 = call { i32*, {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ i32*, {}*, i32 }* %result, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl, { i32*, {}* }* %0)
          to label %normal-dest1 unwind label %func-cleanup-landing-pad

normal-dest1:                                     ; preds = %normal-dest
  ret void

func-cleanup-landing-pad:                         ; preds = %normal-dest, %entry
  %1 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  call void @lowLevelTrace(i32 1000000055)
  %2 = extractvalue { i8*, i32 } %1, 0
  store i8* %2, i8** %exn.slot, align 8
  %3 = extractvalue { i8*, i32 } %1, 1
  store i32 %3, i32* %ehselector.slot, align 4
  call void @debugPrintI32(i32 100)
  call void @debugPrintI32(i32 101)
  call void @debugPrintI32(i32 90)
  call void @debugPrintI32(i32 91)
  resume { i8*, i32 } %1
}

attributes #0 = { nounwind }
attributes #1 = { noreturn }
attributes #2 = { nounwind readnone }
attributes #3 = { noreturn nounwind }

!llvm.dbg.cu = !{!0}

!0 = metadata !{i32 786449, metadata !1, i32 32768, metadata !"brcl Common Lisp compiler", i1 false, metadata !"-v", i32 1, metadata !2, metadata !2, metadata !3, metadata !2, metadata !2, metadata !"split-name.log"} ; [ DW_TAG_compile_unit ] [/Users/meister/Development/cando/brcl/src/tests/core/handlercase0.lsp] [DW_LANG_lo_user]
!1 = metadata !{metadata !"handlercase0.lsp", metadata !"/Users/meister/Development/cando/brcl/src/tests/core"}
!2 = metadata !{i32 0}
!3 = metadata !{metadata !4, metadata !9, metadata !14}
!4 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl", metadata !"repl", metadata !"repl", i32 4, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl, null, null, metadata !2, i32 4} ; [ DW_TAG_subprogram ] [line 4] [def] [repl]
!5 = metadata !{i32 786473, metadata !1}          ; [ DW_TAG_file_type ] [/Users/meister/Development/cando/brcl/src/tests/core/handlercase0.lsp]
!6 = metadata !{i32 786453, i32 0, i32 0, metadata !"", i32 0, i64 0, i64 0, i64 0, i32 0, null, metadata !7, i32 0, i32 0} ; [ DW_TAG_subroutine_type ] [line 0, size 0, align 0, offset 0] [from ]
!7 = metadata !{metadata !8}
!8 = metadata !{i32 786468, null, null, metadata !"int", i32 0, i64 32, i64 32, i64 0, i32 0, i32 13} ; [ DW_TAG_base_type ] [int] [line 0, size 32, align 32, offset 0, enc DW_ATE_signed_fixed]
!9 = metadata !{i32 786478, metadata !1, metadata !10, metadata !"lambda", metadata !"lambda", metadata !"lambda", i32 10, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @lambda, null, null, metadata !2, i32 10} ; [ DW_TAG_subprogram ] [line 10] [def] [lambda]
!10 = metadata !{i32 786443, metadata !1, metadata !11, i32 8, i32 13, i32 3} ; [ DW_TAG_lexical_block ] [/Users/meister/Development/cando/brcl/src/tests/core/handlercase0.lsp]
!11 = metadata !{i32 786443, metadata !1, metadata !12, i32 5, i32 8, i32 2} ; [ DW_TAG_lexical_block ] [/Users/meister/Development/cando/brcl/src/tests/core/handlercase0.lsp]
!12 = metadata !{i32 786443, metadata !1, metadata !13, i32 5, i32 2, i32 1} ; [ DW_TAG_lexical_block ] [/Users/meister/Development/cando/brcl/src/tests/core/handlercase0.lsp]
!13 = metadata !{i32 786443, metadata !1, metadata !4, i32 4, i32 2, i32 0} ; [ DW_TAG_lexical_block ] [/Users/meister/Development/cando/brcl/src/tests/core/handlercase0.lsp]
!14 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"___user_handlercase0", metadata !"___user_handlercase0", metadata !"___user_handlercase0", i32 0, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ()* @___user_handlercase0, null, null, metadata !2, i32 0} ; [ DW_TAG_subprogram ] [line 0] [def] [___user_handlercase0]
