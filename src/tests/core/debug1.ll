; ModuleID = 'debug1.bc'
target triple = "x86_64-apple-macosx"

@":::global-str-/Users/meister/Development/cando/brcl/src/tests/core/debug1.lsp" = internal unnamed_addr constant [64 x i8] c"/Users/meister/Development/cando/brcl/src/tests/core/debug1.lsp\00"
@load-time-value-vector = internal global { i32*, {}* }* null
@":::global-str-repl" = internal unnamed_addr constant [5 x i8] c"repl\00"
@":::symbol-name-Z" = internal unnamed_addr constant [2 x i8] c"Z\00"
@":::package-name-CORE" = internal unnamed_addr constant [5 x i8] c"CORE\00"
@":::global-str-Z" = internal unnamed_addr constant [2 x i8] c"Z\00"
@":::symbol-name-T" = internal unnamed_addr constant [2 x i8] c"T\00"
@":::package-name-COMMON-LISP" = internal unnamed_addr constant [12 x i8] c"COMMON-LISP\00"
@constant-array = internal constant [1 x i32] [i32 2]
@":::symbol-name-U" = internal unnamed_addr constant [2 x i8] c"U\00"
@":::symbol-name-V" = internal unnamed_addr constant [2 x i8] c"V\00"
@constant-array1 = internal constant [2 x i32] [i32 5, i32 6]
@":::str" = internal unnamed_addr constant [3 x i8] c"z1\00"
@":::symbol-name-PRINT" = internal unnamed_addr constant [6 x i8] c"PRINT\00"
@":::str2" = internal unnamed_addr constant [5 x i8] c"Test\00"
@":::symbol-name-ERROR" = internal unnamed_addr constant [6 x i8] c"ERROR\00"
@":::str3" = internal unnamed_addr constant [3 x i8] c"z2\00"
@_ZTIN4core10ReturnFromE = external constant i8
@":::symbol-name-*FSET" = internal unnamed_addr constant [6 x i8] c"*FSET\00"
@":::global-str-repl4" = internal unnamed_addr constant [6 x i8] c"repl4\00"
@":::global-str-repl5" = internal unnamed_addr constant [6 x i8] c"repl5\00"
@":::global-str-repl6" = internal unnamed_addr constant [6 x i8] c"repl6\00"
@":::symbol-name-Y" = internal unnamed_addr constant [2 x i8] c"Y\00"
@":::global-str-Y" = internal unnamed_addr constant [2 x i8] c"Y\00"
@constant-array7 = internal constant [1 x i32] [i32 1]
@":::symbol-name-M" = internal unnamed_addr constant [2 x i8] c"M\00"
@constant-array8 = internal constant [1 x i32] [i32 12]
@":::str9" = internal unnamed_addr constant [3 x i8] c"y1\00"
@":::str10" = internal unnamed_addr constant [3 x i8] c"y2\00"
@":::global-str-repl11" = internal unnamed_addr constant [7 x i8] c"repl11\00"
@":::global-str-repl12" = internal unnamed_addr constant [7 x i8] c"repl12\00"
@":::global-str-repl13" = internal unnamed_addr constant [7 x i8] c"repl13\00"
@":::symbol-name-X" = internal unnamed_addr constant [2 x i8] c"X\00"
@":::global-str-X" = internal unnamed_addr constant [2 x i8] c"X\00"
@constant-array14 = internal constant [1 x i32] zeroinitializer
@constant-array15 = internal constant [0 x i32] zeroinitializer
@":::str16" = internal unnamed_addr constant [3 x i8] c"x1\00"
@constant-array17 = internal constant [1 x i32] [i32 1]
@":::symbol-name-K" = internal unnamed_addr constant [2 x i8] c"K\00"
@constant-array18 = internal constant [1 x i32] [i32 20]
@":::str19" = internal unnamed_addr constant [3 x i8] c"x2\00"
@":::global-str-repl20" = internal unnamed_addr constant [7 x i8] c"repl20\00"
@":::global-str-repl21" = internal unnamed_addr constant [7 x i8] c"repl21\00"
@":::global-str-___user_debug1" = internal unnamed_addr constant [15 x i8] c"___user_debug1\00"
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
  %array-element-type1 = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %array-element-type1)
  %array-element-type2 = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %array-element-type2)
  %array-element-type3 = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %array-element-type3)
  call void @getLoadTimeValueArray({ i32*, {}* }** @load-time-value-vector, i8* getelementptr inbounds ([64 x i8]* @":::global-str-/Users/meister/Development/cando/brcl/src/tests/core/debug1.lsp", i64 0, i64 0), i32 24, i32 5)
  %0 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  call void @sp_makeNil({ i32*, {}* }* %0)
  %1 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 1)
  call void @makeT({ i32*, {}* }* %1)
  %2 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 2)
  call void @internSymbol_tsp({ i32*, {}* }* %2, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-Z", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %3 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 3)
  call void @internSymbol_tsp({ i32*, {}* }* %3, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-T", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %array-element-type, { i32*, {}* }** @load-time-value-vector, i32 3)
  %4 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  call void @ltv_makeArrayObjects({ i32*, {}* }* %4, { i32*, {}* }* %array-element-type, i32 1, i32* getelementptr inbounds ([1 x i32]* @constant-array, i64 0, i64 0))
  %5 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 5)
  call void @internSymbol_tsp({ i32*, {}* }* %5, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-U", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %6 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 6)
  call void @internSymbol_tsp({ i32*, {}* }* %6, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-V", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %7 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  call void @ltv_initializeArrayObjectsRowMajorArefOrder({ i32*, {}* }* %7, { i32*, {}* }** @load-time-value-vector, i32* getelementptr inbounds ([2 x i32]* @constant-array1, i64 0, i64 0))
  %8 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 7)
  call void @makeString({ i32*, {}* }* %8, i8* getelementptr inbounds ([3 x i8]* @":::str", i64 0, i64 0))
  %9 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  call void @internSymbol_symsp({ i32*, i32* }* %9, i8* getelementptr inbounds ([6 x i8]* @":::symbol-name-PRINT", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  %10 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 8)
  call void @makeString({ i32*, {}* }* %10, i8* getelementptr inbounds ([5 x i8]* @":::str2", i64 0, i64 0))
  %11 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 1)
  call void @internSymbol_symsp({ i32*, i32* }* %11, i8* getelementptr inbounds ([6 x i8]* @":::symbol-name-ERROR", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  %12 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 9)
  call void @makeString({ i32*, {}* }* %12, i8* getelementptr inbounds ([3 x i8]* @":::str3", i64 0, i64 0))
  %13 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 2)
  call void @internSymbol_symsp({ i32*, i32* }* %13, i8* getelementptr inbounds ([6 x i8]* @":::symbol-name-*FSET", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %14 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 10)
  call void @internSymbol_tsp({ i32*, {}* }* %14, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-Y", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %array-element-type1, { i32*, {}* }** @load-time-value-vector, i32 3)
  %15 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 11)
  call void @ltv_makeArrayObjects({ i32*, {}* }* %15, { i32*, {}* }* %array-element-type1, i32 1, i32* getelementptr inbounds ([1 x i32]* @constant-array7, i64 0, i64 0))
  %16 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 12)
  call void @internSymbol_tsp({ i32*, {}* }* %16, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-M", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %17 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 11)
  call void @ltv_initializeArrayObjectsRowMajorArefOrder({ i32*, {}* }* %17, { i32*, {}* }** @load-time-value-vector, i32* getelementptr inbounds ([1 x i32]* @constant-array8, i64 0, i64 0))
  %18 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 13)
  call void @makeString({ i32*, {}* }* %18, i8* getelementptr inbounds ([3 x i8]* @":::str9", i64 0, i64 0))
  %19 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 14)
  call void @makeFixnum({ i32*, {}* }* %19, i32 2)
  %20 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 3)
  call void @internSymbol_symsp({ i32*, i32* }* %20, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-Z", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %21 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 15)
  call void @makeString({ i32*, {}* }* %21, i8* getelementptr inbounds ([3 x i8]* @":::str10", i64 0, i64 0))
  %22 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 16)
  call void @internSymbol_tsp({ i32*, {}* }* %22, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-X", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %array-element-type2, { i32*, {}* }** @load-time-value-vector, i32 3)
  %23 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 17)
  call void @ltv_makeArrayObjects({ i32*, {}* }* %23, { i32*, {}* }* %array-element-type2, i32 1, i32* getelementptr inbounds ([1 x i32]* @constant-array14, i64 0, i64 0))
  %24 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 17)
  call void @ltv_initializeArrayObjectsRowMajorArefOrder({ i32*, {}* }* %24, { i32*, {}* }** @load-time-value-vector, i32* getelementptr inbounds ([0 x i32]* @constant-array15, i64 0, i64 0))
  %25 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 18)
  call void @makeString({ i32*, {}* }* %25, i8* getelementptr inbounds ([3 x i8]* @":::str16", i64 0, i64 0))
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %array-element-type3, { i32*, {}* }** @load-time-value-vector, i32 3)
  %26 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 19)
  call void @ltv_makeArrayObjects({ i32*, {}* }* %26, { i32*, {}* }* %array-element-type3, i32 1, i32* getelementptr inbounds ([1 x i32]* @constant-array17, i64 0, i64 0))
  %27 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 20)
  call void @internSymbol_tsp({ i32*, {}* }* %27, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-K", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %28 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 19)
  call void @ltv_initializeArrayObjectsRowMajorArefOrder({ i32*, {}* }* %28, { i32*, {}* }** @load-time-value-vector, i32* getelementptr inbounds ([1 x i32]* @constant-array18, i64 0, i64 0))
  %29 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 21)
  call void @makeFixnum({ i32*, {}* }* %29, i32 10)
  %30 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 22)
  call void @makeFixnum({ i32*, {}* }* %30, i32 1)
  %31 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  call void @internSymbol_symsp({ i32*, i32* }* %31, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-Y", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %32 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 23)
  call void @makeString({ i32*, {}* }* %32, i8* getelementptr inbounds ([3 x i8]* @":::str19", i64 0, i64 0))
  ret void
}

define internal void @repl({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %call-args = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args)
  call void @trace_setLineNumberColumnForIHSTop(i32 5, i32 3)
  call void @makeValueFrame({ i32*, {}* }* %call-args, i32 3, i32 2000000)
  %0 = call { i32*, {}* }* @activationFrameNil()
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args, { i32*, {}* }* %0)
  %call-args-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 0)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-0, { i32*, {}* }** @load-time-value-vector, i32 2)
  %call-args-ref-1 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 1)
  %"SYMBOL->nil" = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  %"SYMBOL->Z" = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 2)
  %1 = call { i32*, {}* }* @activationFrameNil()
  invoke void @sp_makeCompiledFunction({ i32*, {}* }* %call-args-ref-1, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @Z, i8* getelementptr inbounds ([64 x i8]* @":::global-str-/Users/meister/Development/cando/brcl/src/tests/core/debug1.lsp", i64 0, i64 0), { i32*, {}* }* %"SYMBOL->Z", { i32*, {}* }* %"SYMBOL->nil", { i32*, {}* }* %1)
          to label %normal-dest unwind label %func-cleanup-landing-pad

normal-dest:                                      ; preds = %entry
  %call-args-ref-2 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 2)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-2, { i32*, {}* }** @load-time-value-vector, i32 0)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args)
  call void @singleStepCallback()
  %"SYMBOL->*FSET" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 2)
  invoke void @mv_invokePossibleMultipleValueSymbolFunction({ i32*, {}*, i32 }* %result-ptr, { i32*, i32* }* %"SYMBOL->*FSET", { i32*, {}* }* %call-args)
          to label %normal-dest1 unwind label %func-cleanup-landing-pad

normal-dest1:                                     ; preds = %normal-dest
  ret void

func-cleanup-landing-pad:                         ; preds = %normal-dest, %entry
  %2 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %3 = extractvalue { i8*, i32 } %2, 0
  store i8* %3, i8** %exn.slot, align 8
  %4 = extractvalue { i8*, i32 } %2, 1
  store i32 %4, i32* %ehselector.slot, align 4
  resume { i8*, i32 } %2
}

define internal void @Z({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %lambda-args-1- = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %lambda-args-1-)
  %temp = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp)
  %call-args = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args)
  %call-args5 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args5)
  %call-args7 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args7)
  %given-num-args = call i32 @activationFrameSize({ i32*, {}* }* %activation-frame-ptr)
  %correct-num-args = icmp eq i32 %given-num-args, 2
  br i1 %correct-num-args, label %continue3, label %error

error:                                            ; preds = %entry
  %enough-args = icmp slt i32 %given-num-args, 2
  br i1 %enough-args, label %error1, label %continue

error1:                                           ; preds = %error
  invoke void @throwNotEnoughArgumentsException(i8* getelementptr inbounds ([2 x i8]* @":::global-str-Z", i64 0, i64 0), { i32*, {}* }* %activation-frame-ptr, i32 %given-num-args, i32 2)
          to label %normal-dest unwind label %func-cleanup-landing-pad

normal-dest:                                      ; preds = %error1
  unreachable

continue:                                         ; preds = %error
  invoke void @throwTooManyArgumentsException(i8* getelementptr inbounds ([2 x i8]* @":::global-str-Z", i64 0, i64 0), { i32*, {}* }* %activation-frame-ptr, i32 %given-num-args, i32 2)
          to label %normal-dest2 unwind label %func-cleanup-landing-pad

normal-dest2:                                     ; preds = %continue
  unreachable

continue3:                                        ; preds = %entry
  call void @copyAFsp({ i32*, {}* }* %lambda-args-1-, { i32*, {}* }* %activation-frame-ptr)
  %value = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  call void @attachDebuggingInfoToValueFrame({ i32*, {}* }* %lambda-args-1-, { i32*, {}* }* %value)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %lambda-args-1-)
  call void @trace_setLineNumberColumnForIHSTop(i32 3, i32 3)
  call void @makeValueFrame({ i32*, {}* }* %call-args, i32 1, i32 2000001)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args, { i32*, {}* }* %lambda-args-1-)
  %call-args-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 0)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-0, { i32*, {}* }** @load-time-value-vector, i32 7)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:PRINT" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %temp, { i32*, i32* }* %"SYMBOL->COMMON-LISP:PRINT", { i32*, {}* }* %call-args)
          to label %"(TRY-0).normal-dest" unwind label %"(TRY-0).landing-pad"

"(TRY-0).normal-dest":                            ; preds = %continue3
  call void @trace_setLineNumberColumnForIHSTop(i32 4, i32 3)
  call void @makeValueFrame({ i32*, {}* }* %call-args5, i32 1, i32 2000002)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args5, { i32*, {}* }* %lambda-args-1-)
  %call-args5-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args5, i32 0)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args5-ref-0, { i32*, {}* }** @load-time-value-vector, i32 8)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args5)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:ERROR" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 1)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %temp, { i32*, i32* }* %"SYMBOL->COMMON-LISP:ERROR", { i32*, {}* }* %call-args5)
          to label %"(TRY-0).normal-dest6" unwind label %"(TRY-0).landing-pad"

"(TRY-0).normal-dest6":                           ; preds = %"(TRY-0).normal-dest"
  call void @trace_setLineNumberColumnForIHSTop(i32 5, i32 3)
  call void @makeValueFrame({ i32*, {}* }* %call-args7, i32 1, i32 2000003)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args7, { i32*, {}* }* %lambda-args-1-)
  %call-args7-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args7, i32 0)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args7-ref-0, { i32*, {}* }** @load-time-value-vector, i32 9)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args7)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:PRINT8" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  invoke void @mv_invokePossibleMultipleValueSymbolFunction({ i32*, {}*, i32 }* %result-ptr, { i32*, i32* }* %"SYMBOL->COMMON-LISP:PRINT8", { i32*, {}* }* %call-args7)
          to label %"(TRY-0).try-cont" unwind label %"(TRY-0).landing-pad"

"(TRY-0).landing-pad":                            ; preds = %"(TRY-0).normal-dest6", %"(TRY-0).normal-dest", %continue3
  %0 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
          catch i8* @_ZTIN4core10ReturnFromE
  %1 = extractvalue { i8*, i32 } %0, 0
  store i8* %1, i8** %exn.slot, align 8
  %2 = extractvalue { i8*, i32 } %0, 1
  store i32 %2, i32* %ehselector.slot, align 4
  %3 = call i32 @llvm.eh.typeid.for(i8* @_ZTIN4core10ReturnFromE)
  %4 = icmp eq i32 %2, %3
  br i1 %4, label %"(TRY-0).handler-block2786", label %func-ehcleanup

"(TRY-0).handler-block2786":                      ; preds = %"(TRY-0).landing-pad"
  %5 = call i8* @__cxa_begin_catch(i8* %1)
  invoke void @mv_blockHandleReturnFrom({ i32*, {}*, i32 }* %result-ptr, i8* %5)
          to label %"(TRY-0).normal-dest10" unwind label %"(TRY-0).landing-pad13"

"(TRY-0).normal-dest10":                          ; preds = %"(TRY-0).handler-block2786"
  invoke void @__cxa_end_catch()
          to label %"(TRY-0).try-cont" unwind label %"(TRY-0).landing-pad13"

"(TRY-0).try-cont":                               ; preds = %"(TRY-0).normal-dest10", %"(TRY-0).normal-dest6"
  ret void

"(TRY-0).landing-pad13":                          ; preds = %"(TRY-0).normal-dest10", %"(TRY-0).handler-block2786"
  %6 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %7 = extractvalue { i8*, i32 } %6, 0
  store i8* %7, i8** %exn.slot, align 8
  %8 = extractvalue { i8*, i32 } %6, 1
  store i32 %8, i32* %ehselector.slot, align 4
  br label %func-ehcleanup

func-cleanup-landing-pad:                         ; preds = %continue, %error1
  %9 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %10 = extractvalue { i8*, i32 } %9, 0
  store i8* %10, i8** %exn.slot, align 8
  %11 = extractvalue { i8*, i32 } %9, 1
  store i32 %11, i32* %ehselector.slot, align 4
  br label %func-ehcleanup

func-ehcleanup:                                   ; preds = %func-cleanup-landing-pad, %"(TRY-0).landing-pad13", %"(TRY-0).landing-pad"
  %sel = phi i32 [ %11, %func-cleanup-landing-pad ], [ %8, %"(TRY-0).landing-pad13" ], [ %2, %"(TRY-0).landing-pad" ]
  %exn7 = phi i8* [ %10, %func-cleanup-landing-pad ], [ %7, %"(TRY-0).landing-pad13" ], [ %1, %"(TRY-0).landing-pad" ]
  %lpad.val = insertvalue { i8*, i32 } undef, i8* %exn7, 0
  %lpad.val8 = insertvalue { i8*, i32 } %lpad.val, i32 %sel, 1
  resume { i8*, i32 } %lpad.val8
}

define internal void @repl4({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  call void @mv_copyLoadTimeValue({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }** @load-time-value-vector, i32 0)
  ret void
}

define internal void @repl5({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  call void @mv_copyLoadTimeValue({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }** @load-time-value-vector, i32 2)
  ret void
}

define internal void @repl6({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %call-args = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args)
  call void @trace_setLineNumberColumnForIHSTop(i32 10, i32 3)
  call void @makeValueFrame({ i32*, {}* }* %call-args, i32 3, i32 2000004)
  %0 = call { i32*, {}* }* @activationFrameNil()
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args, { i32*, {}* }* %0)
  %call-args-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 0)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-0, { i32*, {}* }** @load-time-value-vector, i32 10)
  %call-args-ref-1 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 1)
  %"SYMBOL->nil" = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  %"SYMBOL->Y" = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 10)
  %1 = call { i32*, {}* }* @activationFrameNil()
  invoke void @sp_makeCompiledFunction({ i32*, {}* }* %call-args-ref-1, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @Y, i8* getelementptr inbounds ([64 x i8]* @":::global-str-/Users/meister/Development/cando/brcl/src/tests/core/debug1.lsp", i64 0, i64 0), { i32*, {}* }* %"SYMBOL->Y", { i32*, {}* }* %"SYMBOL->nil", { i32*, {}* }* %1)
          to label %normal-dest unwind label %func-cleanup-landing-pad

normal-dest:                                      ; preds = %entry
  %call-args-ref-2 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 2)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-2, { i32*, {}* }** @load-time-value-vector, i32 0)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args)
  call void @singleStepCallback()
  %"SYMBOL->*FSET" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 2)
  invoke void @mv_invokePossibleMultipleValueSymbolFunction({ i32*, {}*, i32 }* %result-ptr, { i32*, i32* }* %"SYMBOL->*FSET", { i32*, {}* }* %call-args)
          to label %normal-dest1 unwind label %func-cleanup-landing-pad

normal-dest1:                                     ; preds = %normal-dest
  ret void

func-cleanup-landing-pad:                         ; preds = %normal-dest, %entry
  %2 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %3 = extractvalue { i8*, i32 } %2, 0
  store i8* %3, i8** %exn.slot, align 8
  %4 = extractvalue { i8*, i32 } %2, 1
  store i32 %4, i32* %ehselector.slot, align 4
  resume { i8*, i32 } %2
}

define internal void @Y({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %lambda-args-2- = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %lambda-args-2-)
  %temp = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp)
  %call-args = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args)
  %call-args5 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args5)
  %call-args7 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args7)
  %given-num-args = call i32 @activationFrameSize({ i32*, {}* }* %activation-frame-ptr)
  %correct-num-args = icmp eq i32 %given-num-args, 1
  br i1 %correct-num-args, label %continue3, label %error

error:                                            ; preds = %entry
  %enough-args = icmp slt i32 %given-num-args, 1
  br i1 %enough-args, label %error1, label %continue

error1:                                           ; preds = %error
  invoke void @throwNotEnoughArgumentsException(i8* getelementptr inbounds ([2 x i8]* @":::global-str-Y", i64 0, i64 0), { i32*, {}* }* %activation-frame-ptr, i32 %given-num-args, i32 1)
          to label %normal-dest unwind label %func-cleanup-landing-pad

normal-dest:                                      ; preds = %error1
  unreachable

continue:                                         ; preds = %error
  invoke void @throwTooManyArgumentsException(i8* getelementptr inbounds ([2 x i8]* @":::global-str-Y", i64 0, i64 0), { i32*, {}* }* %activation-frame-ptr, i32 %given-num-args, i32 1)
          to label %normal-dest2 unwind label %func-cleanup-landing-pad

normal-dest2:                                     ; preds = %continue
  unreachable

continue3:                                        ; preds = %entry
  call void @copyAFsp({ i32*, {}* }* %lambda-args-2-, { i32*, {}* }* %activation-frame-ptr)
  %value = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 11)
  call void @attachDebuggingInfoToValueFrame({ i32*, {}* }* %lambda-args-2-, { i32*, {}* }* %value)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %lambda-args-2-)
  call void @trace_setLineNumberColumnForIHSTop(i32 8, i32 3)
  call void @makeValueFrame({ i32*, {}* }* %call-args, i32 1, i32 2000005)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args, { i32*, {}* }* %lambda-args-2-)
  %call-args-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 0)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-0, { i32*, {}* }** @load-time-value-vector, i32 13)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:PRINT" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %temp, { i32*, i32* }* %"SYMBOL->COMMON-LISP:PRINT", { i32*, {}* }* %call-args)
          to label %"(TRY-0).normal-dest" unwind label %"(TRY-0).landing-pad"

"(TRY-0).normal-dest":                            ; preds = %continue3
  call void @trace_setLineNumberColumnForIHSTop(i32 9, i32 3)
  call void @makeValueFrame({ i32*, {}* }* %call-args5, i32 2, i32 2000006)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args5, { i32*, {}* }* %lambda-args-2-)
  %call-args5-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args5, i32 0)
  call void @sp_lexicalValueRead({ i32*, {}* }* %call-args5-ref-0, i32 0, i32 0, { i32*, {}* }* %lambda-args-2-)
  %call-args5-ref-1 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args5, i32 1)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args5-ref-1, { i32*, {}* }** @load-time-value-vector, i32 14)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args5)
  call void @singleStepCallback()
  %"SYMBOL->Z" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 3)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %temp, { i32*, i32* }* %"SYMBOL->Z", { i32*, {}* }* %call-args5)
          to label %"(TRY-0).normal-dest6" unwind label %"(TRY-0).landing-pad"

"(TRY-0).normal-dest6":                           ; preds = %"(TRY-0).normal-dest"
  call void @trace_setLineNumberColumnForIHSTop(i32 10, i32 3)
  call void @makeValueFrame({ i32*, {}* }* %call-args7, i32 1, i32 2000007)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args7, { i32*, {}* }* %lambda-args-2-)
  %call-args7-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args7, i32 0)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args7-ref-0, { i32*, {}* }** @load-time-value-vector, i32 15)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args7)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:PRINT8" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  invoke void @mv_invokePossibleMultipleValueSymbolFunction({ i32*, {}*, i32 }* %result-ptr, { i32*, i32* }* %"SYMBOL->COMMON-LISP:PRINT8", { i32*, {}* }* %call-args7)
          to label %"(TRY-0).try-cont" unwind label %"(TRY-0).landing-pad"

"(TRY-0).landing-pad":                            ; preds = %"(TRY-0).normal-dest6", %"(TRY-0).normal-dest", %continue3
  %0 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
          catch i8* @_ZTIN4core10ReturnFromE
  %1 = extractvalue { i8*, i32 } %0, 0
  store i8* %1, i8** %exn.slot, align 8
  %2 = extractvalue { i8*, i32 } %0, 1
  store i32 %2, i32* %ehselector.slot, align 4
  %3 = call i32 @llvm.eh.typeid.for(i8* @_ZTIN4core10ReturnFromE)
  %4 = icmp eq i32 %2, %3
  br i1 %4, label %"(TRY-0).handler-block3004", label %func-ehcleanup

"(TRY-0).handler-block3004":                      ; preds = %"(TRY-0).landing-pad"
  %5 = call i8* @__cxa_begin_catch(i8* %1)
  invoke void @mv_blockHandleReturnFrom({ i32*, {}*, i32 }* %result-ptr, i8* %5)
          to label %"(TRY-0).normal-dest10" unwind label %"(TRY-0).landing-pad13"

"(TRY-0).normal-dest10":                          ; preds = %"(TRY-0).handler-block3004"
  invoke void @__cxa_end_catch()
          to label %"(TRY-0).try-cont" unwind label %"(TRY-0).landing-pad13"

"(TRY-0).try-cont":                               ; preds = %"(TRY-0).normal-dest10", %"(TRY-0).normal-dest6"
  ret void

"(TRY-0).landing-pad13":                          ; preds = %"(TRY-0).normal-dest10", %"(TRY-0).handler-block3004"
  %6 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %7 = extractvalue { i8*, i32 } %6, 0
  store i8* %7, i8** %exn.slot, align 8
  %8 = extractvalue { i8*, i32 } %6, 1
  store i32 %8, i32* %ehselector.slot, align 4
  br label %func-ehcleanup

func-cleanup-landing-pad:                         ; preds = %continue, %error1
  %9 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %10 = extractvalue { i8*, i32 } %9, 0
  store i8* %10, i8** %exn.slot, align 8
  %11 = extractvalue { i8*, i32 } %9, 1
  store i32 %11, i32* %ehselector.slot, align 4
  br label %func-ehcleanup

func-ehcleanup:                                   ; preds = %func-cleanup-landing-pad, %"(TRY-0).landing-pad13", %"(TRY-0).landing-pad"
  %sel = phi i32 [ %11, %func-cleanup-landing-pad ], [ %8, %"(TRY-0).landing-pad13" ], [ %2, %"(TRY-0).landing-pad" ]
  %exn7 = phi i8* [ %10, %func-cleanup-landing-pad ], [ %7, %"(TRY-0).landing-pad13" ], [ %1, %"(TRY-0).landing-pad" ]
  %lpad.val = insertvalue { i8*, i32 } undef, i8* %exn7, 0
  %lpad.val8 = insertvalue { i8*, i32 } %lpad.val, i32 %sel, 1
  resume { i8*, i32 } %lpad.val8
}

define internal void @repl11({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  call void @mv_copyLoadTimeValue({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }** @load-time-value-vector, i32 0)
  ret void
}

define internal void @repl12({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  call void @mv_copyLoadTimeValue({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }** @load-time-value-vector, i32 10)
  ret void
}

define internal void @repl13({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %call-args = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args)
  call void @trace_setLineNumberColumnForIHSTop(i32 16, i32 3)
  call void @makeValueFrame({ i32*, {}* }* %call-args, i32 3, i32 2000008)
  %0 = call { i32*, {}* }* @activationFrameNil()
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args, { i32*, {}* }* %0)
  %call-args-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 0)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-0, { i32*, {}* }** @load-time-value-vector, i32 16)
  %call-args-ref-1 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 1)
  %"SYMBOL->nil" = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  %"SYMBOL->X" = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 16)
  %1 = call { i32*, {}* }* @activationFrameNil()
  invoke void @sp_makeCompiledFunction({ i32*, {}* }* %call-args-ref-1, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @X, i8* getelementptr inbounds ([64 x i8]* @":::global-str-/Users/meister/Development/cando/brcl/src/tests/core/debug1.lsp", i64 0, i64 0), { i32*, {}* }* %"SYMBOL->X", { i32*, {}* }* %"SYMBOL->nil", { i32*, {}* }* %1)
          to label %normal-dest unwind label %func-cleanup-landing-pad

normal-dest:                                      ; preds = %entry
  %call-args-ref-2 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 2)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-2, { i32*, {}* }** @load-time-value-vector, i32 0)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args)
  call void @singleStepCallback()
  %"SYMBOL->*FSET" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 2)
  invoke void @mv_invokePossibleMultipleValueSymbolFunction({ i32*, {}*, i32 }* %result-ptr, { i32*, i32* }* %"SYMBOL->*FSET", { i32*, {}* }* %call-args)
          to label %normal-dest1 unwind label %func-cleanup-landing-pad

normal-dest1:                                     ; preds = %normal-dest
  ret void

func-cleanup-landing-pad:                         ; preds = %normal-dest, %entry
  %2 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %3 = extractvalue { i8*, i32 } %2, 0
  store i8* %3, i8** %exn.slot, align 8
  %4 = extractvalue { i8*, i32 } %2, 1
  store i32 %4, i32* %ehselector.slot, align 4
  resume { i8*, i32 } %2
}

define internal void @X({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %lambda-args-3- = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %lambda-args-3-)
  %temp = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp)
  %call-args = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args)
  %LET = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %LET)
  %temp7 = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp7)
  %call-args8 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args8)
  %call-args10 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args10)
  %given-num-args = call i32 @activationFrameSize({ i32*, {}* }* %activation-frame-ptr)
  %correct-num-args = icmp eq i32 %given-num-args, 0
  br i1 %correct-num-args, label %continue3, label %error

error:                                            ; preds = %entry
  %enough-args = icmp slt i32 %given-num-args, 0
  br i1 %enough-args, label %error1, label %continue

error1:                                           ; preds = %error
  invoke void @throwNotEnoughArgumentsException(i8* getelementptr inbounds ([2 x i8]* @":::global-str-X", i64 0, i64 0), { i32*, {}* }* %activation-frame-ptr, i32 %given-num-args, i32 0)
          to label %normal-dest unwind label %func-cleanup-landing-pad

normal-dest:                                      ; preds = %error1
  unreachable

continue:                                         ; preds = %error
  invoke void @throwTooManyArgumentsException(i8* getelementptr inbounds ([2 x i8]* @":::global-str-X", i64 0, i64 0), { i32*, {}* }* %activation-frame-ptr, i32 %given-num-args, i32 0)
          to label %normal-dest2 unwind label %func-cleanup-landing-pad

normal-dest2:                                     ; preds = %continue
  unreachable

continue3:                                        ; preds = %entry
  call void @copyAFsp({ i32*, {}* }* %lambda-args-3-, { i32*, {}* }* %activation-frame-ptr)
  %value = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 17)
  call void @attachDebuggingInfoToValueFrame({ i32*, {}* }* %lambda-args-3-, { i32*, {}* }* %value)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %lambda-args-3-)
  call void @trace_setLineNumberColumnForIHSTop(i32 13, i32 3)
  call void @makeValueFrame({ i32*, {}* }* %call-args, i32 1, i32 2000009)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args, { i32*, {}* }* %lambda-args-3-)
  %call-args-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 0)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-0, { i32*, {}* }** @load-time-value-vector, i32 18)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:PRINT" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %temp, { i32*, i32* }* %"SYMBOL->COMMON-LISP:PRINT", { i32*, {}* }* %call-args)
          to label %"(TRY-0).normal-dest" unwind label %"(TRY-0).landing-pad14"

"(TRY-0).normal-dest":                            ; preds = %continue3
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %LET)
  call void @makeValueFrame({ i32*, {}* }* %LET, i32 1, i32 2000010)
  call void @setParentOfActivationFrame({ i32*, {}* }* %LET, { i32*, {}* }* %lambda-args-3-)
  %value6 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 19)
  call void @attachDebuggingInfoToValueFrame({ i32*, {}* }* %LET, { i32*, {}* }* %value6)
  %0 = call { i32*, {}* }* @lexicalValueReference(i32 0, i32 0, { i32*, {}* }* %LET)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %0, { i32*, {}* }** @load-time-value-vector, i32 21)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %LET)
  call void @singleStepCallback()
  call void @trace_setLineNumberColumnForIHSTop(i32 15, i32 5)
  call void @makeValueFrame({ i32*, {}* }* %call-args8, i32 1, i32 2000011)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args8, { i32*, {}* }* %LET)
  %call-args8-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args8, i32 0)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args8-ref-0, { i32*, {}* }** @load-time-value-vector, i32 22)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args8)
  call void @singleStepCallback()
  %"SYMBOL->Y" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %temp, { i32*, i32* }* %"SYMBOL->Y", { i32*, {}* }* %call-args8)
          to label %"(TRY-0).normal-dest9" unwind label %"(TRY-0).landing-pad"

"(TRY-0).normal-dest9":                           ; preds = %"(TRY-0).normal-dest"
  call void @trace_setLineNumberColumnForIHSTop(i32 16, i32 3)
  call void @makeValueFrame({ i32*, {}* }* %call-args10, i32 1, i32 2000012)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args10, { i32*, {}* }* %lambda-args-3-)
  %call-args10-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args10, i32 0)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args10-ref-0, { i32*, {}* }** @load-time-value-vector, i32 23)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args10)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:PRINT11" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  invoke void @mv_invokePossibleMultipleValueSymbolFunction({ i32*, {}*, i32 }* %result-ptr, { i32*, i32* }* %"SYMBOL->COMMON-LISP:PRINT11", { i32*, {}* }* %call-args10)
          to label %"(TRY-0).try-cont18" unwind label %"(TRY-0).landing-pad14"

"(TRY-0).landing-pad":                            ; preds = %"(TRY-0).normal-dest"
  %1 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
          catch i8* @_ZTIN4core10ReturnFromE
  %2 = extractvalue { i8*, i32 } %1, 0
  store i8* %2, i8** %exn.slot, align 8
  %3 = extractvalue { i8*, i32 } %1, 1
  store i32 %3, i32* %ehselector.slot, align 4
  br label %"(TRY-0).dispatch-header15"

"(TRY-0).landing-pad14":                          ; preds = %"(TRY-0).normal-dest9", %continue3
  %4 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
          catch i8* @_ZTIN4core10ReturnFromE
  %5 = extractvalue { i8*, i32 } %4, 0
  store i8* %5, i8** %exn.slot, align 8
  %6 = extractvalue { i8*, i32 } %4, 1
  store i32 %6, i32* %ehselector.slot, align 4
  br label %"(TRY-0).dispatch-header15"

"(TRY-0).dispatch-header15":                      ; preds = %"(TRY-0).landing-pad14", %"(TRY-0).landing-pad"
  %exn = phi i8* [ %5, %"(TRY-0).landing-pad14" ], [ %2, %"(TRY-0).landing-pad" ]
  %ehselector-slot = phi i32 [ %6, %"(TRY-0).landing-pad14" ], [ %3, %"(TRY-0).landing-pad" ]
  %7 = call i32 @llvm.eh.typeid.for(i8* @_ZTIN4core10ReturnFromE)
  %8 = icmp eq i32 %ehselector-slot, %7
  br i1 %8, label %"(TRY-0).handler-block3222", label %func-ehcleanup

"(TRY-0).handler-block3222":                      ; preds = %"(TRY-0).dispatch-header15"
  %9 = call i8* @__cxa_begin_catch(i8* %exn)
  invoke void @mv_blockHandleReturnFrom({ i32*, {}*, i32 }* %result-ptr, i8* %9)
          to label %"(TRY-0).normal-dest16" unwind label %"(TRY-0).landing-pad20"

"(TRY-0).normal-dest16":                          ; preds = %"(TRY-0).handler-block3222"
  invoke void @__cxa_end_catch()
          to label %"(TRY-0).try-cont18" unwind label %"(TRY-0).landing-pad20"

"(TRY-0).try-cont18":                             ; preds = %"(TRY-0).normal-dest16", %"(TRY-0).normal-dest9"
  ret void

"(TRY-0).landing-pad20":                          ; preds = %"(TRY-0).normal-dest16", %"(TRY-0).handler-block3222"
  %10 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %11 = extractvalue { i8*, i32 } %10, 0
  store i8* %11, i8** %exn.slot, align 8
  %12 = extractvalue { i8*, i32 } %10, 1
  store i32 %12, i32* %ehselector.slot, align 4
  br label %func-ehcleanup

func-cleanup-landing-pad:                         ; preds = %continue, %error1
  %13 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %14 = extractvalue { i8*, i32 } %13, 0
  store i8* %14, i8** %exn.slot, align 8
  %15 = extractvalue { i8*, i32 } %13, 1
  store i32 %15, i32* %ehselector.slot, align 4
  br label %func-ehcleanup

func-ehcleanup:                                   ; preds = %func-cleanup-landing-pad, %"(TRY-0).landing-pad20", %"(TRY-0).dispatch-header15"
  %sel = phi i32 [ %15, %func-cleanup-landing-pad ], [ %12, %"(TRY-0).landing-pad20" ], [ %ehselector-slot, %"(TRY-0).dispatch-header15" ]
  %exn7 = phi i8* [ %14, %func-cleanup-landing-pad ], [ %11, %"(TRY-0).landing-pad20" ], [ %exn, %"(TRY-0).dispatch-header15" ]
  %lpad.val = insertvalue { i8*, i32 } undef, i8* %exn7, 0
  %lpad.val8 = insertvalue { i8*, i32 } %lpad.val, i32 %sel, 1
  resume { i8*, i32 } %lpad.val8
}

define internal void @repl20({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  call void @mv_copyLoadTimeValue({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }** @load-time-value-vector, i32 0)
  ret void
}

define internal void @repl21({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  call void @mv_copyLoadTimeValue({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }** @load-time-value-vector, i32 16)
  ret void
}

define void @___user_debug1() {
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
  %1 = call { i32*, {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ i32*, {}*, i32 }* %result, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl4, { i32*, {}* }* %1)
          to label %normal-dest2 unwind label %func-cleanup-landing-pad

normal-dest2:                                     ; preds = %normal-dest1
  %2 = call { i32*, {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ i32*, {}*, i32 }* %result, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl5, { i32*, {}* }* %2)
          to label %normal-dest3 unwind label %func-cleanup-landing-pad

normal-dest3:                                     ; preds = %normal-dest2
  %3 = call { i32*, {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ i32*, {}*, i32 }* %result, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl6, { i32*, {}* }* %3)
          to label %normal-dest4 unwind label %func-cleanup-landing-pad

normal-dest4:                                     ; preds = %normal-dest3
  %4 = call { i32*, {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ i32*, {}*, i32 }* %result, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl11, { i32*, {}* }* %4)
          to label %normal-dest5 unwind label %func-cleanup-landing-pad

normal-dest5:                                     ; preds = %normal-dest4
  %5 = call { i32*, {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ i32*, {}*, i32 }* %result, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl12, { i32*, {}* }* %5)
          to label %normal-dest6 unwind label %func-cleanup-landing-pad

normal-dest6:                                     ; preds = %normal-dest5
  %6 = call { i32*, {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ i32*, {}*, i32 }* %result, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl13, { i32*, {}* }* %6)
          to label %normal-dest7 unwind label %func-cleanup-landing-pad

normal-dest7:                                     ; preds = %normal-dest6
  %7 = call { i32*, {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ i32*, {}*, i32 }* %result, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl20, { i32*, {}* }* %7)
          to label %normal-dest8 unwind label %func-cleanup-landing-pad

normal-dest8:                                     ; preds = %normal-dest7
  %8 = call { i32*, {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ i32*, {}*, i32 }* %result, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl21, { i32*, {}* }* %8)
          to label %normal-dest9 unwind label %func-cleanup-landing-pad

normal-dest9:                                     ; preds = %normal-dest8
  ret void

func-cleanup-landing-pad:                         ; preds = %normal-dest8, %normal-dest7, %normal-dest6, %normal-dest5, %normal-dest4, %normal-dest3, %normal-dest2, %normal-dest1, %normal-dest, %entry
  %9 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %10 = extractvalue { i8*, i32 } %9, 0
  store i8* %10, i8** %exn.slot, align 8
  %11 = extractvalue { i8*, i32 } %9, 1
  store i32 %11, i32* %ehselector.slot, align 4
  resume { i8*, i32 } %9
}

attributes #0 = { nounwind }
attributes #1 = { noreturn }
attributes #2 = { nounwind readnone }
attributes #3 = { noreturn nounwind }

!llvm.dbg.cu = !{!0}

!0 = metadata !{i32 786449, metadata !1, i32 32768, metadata !"brcl Common Lisp compiler", i1 false, metadata !"-v", i32 1, metadata !2, metadata !2, metadata !3, metadata !2, metadata !2, metadata !"split-name.log"} ; [ DW_TAG_compile_unit ] [/Users/meister/Development/cando/brcl/src/tests/core/debug1.lsp] [DW_LANG_lo_user]
!1 = metadata !{metadata !"debug1.lsp", metadata !"/Users/meister/Development/cando/brcl/src/tests/core"}
!2 = metadata !{i32 0}
!3 = metadata !{metadata !4, metadata !9, metadata !11, metadata !12, metadata !13, metadata !14, metadata !16, metadata !17, metadata !18, metadata !19, metadata !21, metadata !22, metadata !23}
!4 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl", metadata !"repl", metadata !"repl", i32 5, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl, null, null, metadata !2, i32 5} ; [ DW_TAG_subprogram ] [line 5] [def] [repl]
!5 = metadata !{i32 786473, metadata !1}          ; [ DW_TAG_file_type ] [/Users/meister/Development/cando/brcl/src/tests/core/debug1.lsp]
!6 = metadata !{i32 786453, i32 0, i32 0, metadata !"", i32 0, i64 0, i64 0, i64 0, i32 0, null, metadata !7, i32 0, i32 0} ; [ DW_TAG_subroutine_type ] [line 0, size 0, align 0, offset 0] [from ]
!7 = metadata !{metadata !8}
!8 = metadata !{i32 786468, null, null, metadata !"int", i32 0, i64 32, i64 32, i64 0, i32 0, i32 13} ; [ DW_TAG_base_type ] [int] [line 0, size 32, align 32, offset 0, enc DW_ATE_signed_fixed]
!9 = metadata !{i32 786478, metadata !1, metadata !10, metadata !"Z", metadata !"Z", metadata !"Z", i32 5, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @Z, null, null, metadata !2, i32 5} ; [ DW_TAG_subprogram ] [line 5] [def] [Z]
!10 = metadata !{i32 786443, metadata !1, metadata !4, i32 5, i32 3, i32 0} ; [ DW_TAG_lexical_block ] [/Users/meister/Development/cando/brcl/src/tests/core/debug1.lsp]
!11 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl4", metadata !"repl4", metadata !"repl4", i32 0, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl4, null, null, metadata !2, i32 0} ; [ DW_TAG_subprogram ] [line 0] [def] [repl4]
!12 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl5", metadata !"repl5", metadata !"repl5", i32 0, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl5, null, null, metadata !2, i32 0} ; [ DW_TAG_subprogram ] [line 0] [def] [repl5]
!13 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl6", metadata !"repl6", metadata !"repl6", i32 10, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl6, null, null, metadata !2, i32 10} ; [ DW_TAG_subprogram ] [line 10] [def] [repl6]
!14 = metadata !{i32 786478, metadata !1, metadata !15, metadata !"Y", metadata !"Y", metadata !"Y", i32 10, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @Y, null, null, metadata !2, i32 10} ; [ DW_TAG_subprogram ] [line 10] [def] [Y]
!15 = metadata !{i32 786443, metadata !1, metadata !13, i32 10, i32 3, i32 5} ; [ DW_TAG_lexical_block ] [/Users/meister/Development/cando/brcl/src/tests/core/debug1.lsp]
!16 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl11", metadata !"repl11", metadata !"repl11", i32 0, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl11, null, null, metadata !2, i32 0} ; [ DW_TAG_subprogram ] [line 0] [def] [repl11]
!17 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl12", metadata !"repl12", metadata !"repl12", i32 0, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl12, null, null, metadata !2, i32 0} ; [ DW_TAG_subprogram ] [line 0] [def] [repl12]
!18 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl13", metadata !"repl13", metadata !"repl13", i32 16, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl13, null, null, metadata !2, i32 16} ; [ DW_TAG_subprogram ] [line 16] [def] [repl13]
!19 = metadata !{i32 786478, metadata !1, metadata !20, metadata !"X", metadata !"X", metadata !"X", i32 16, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @X, null, null, metadata !2, i32 16} ; [ DW_TAG_subprogram ] [line 16] [def] [X]
!20 = metadata !{i32 786443, metadata !1, metadata !18, i32 16, i32 3, i32 10} ; [ DW_TAG_lexical_block ] [/Users/meister/Development/cando/brcl/src/tests/core/debug1.lsp]
!21 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl20", metadata !"repl20", metadata !"repl20", i32 0, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl20, null, null, metadata !2, i32 0} ; [ DW_TAG_subprogram ] [line 0] [def] [repl20]
!22 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl21", metadata !"repl21", metadata !"repl21", i32 0, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl21, null, null, metadata !2, i32 0} ; [ DW_TAG_subprogram ] [line 0] [def] [repl21]
!23 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"___user_debug1", metadata !"___user_debug1", metadata !"___user_debug1", i32 0, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ()* @___user_debug1, null, null, metadata !2, i32 0} ; [ DW_TAG_subprogram ] [line 0] [def] [___user_debug1]
