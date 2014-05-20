; ModuleID = 'defun0.bc'
target triple = "x86_64-apple-macosx"

@":::global-str-/Users/meister/Development/cando/brcl/src/tests/core/defun0.lsp" = internal unnamed_addr constant [64 x i8] c"/Users/meister/Development/cando/brcl/src/tests/core/defun0.lsp\00"
@load-time-value-vector = internal global { i32*, {}* }* null
@":::global-str-repl" = internal unnamed_addr constant [5 x i8] c"repl\00"
@":::symbol-name-TESTDEFUN" = internal unnamed_addr constant [10 x i8] c"TESTDEFUN\00"
@":::package-name-CORE" = internal unnamed_addr constant [5 x i8] c"CORE\00"
@":::global-str-TESTDEFUN" = internal unnamed_addr constant [10 x i8] c"TESTDEFUN\00"
@":::symbol-name-+" = internal unnamed_addr constant [2 x i8] c"+\00"
@":::package-name-COMMON-LISP" = internal unnamed_addr constant [12 x i8] c"COMMON-LISP\00"
@_ZTIN4core10ReturnFromE = external constant i8
@":::symbol-name-*FSET" = internal unnamed_addr constant [6 x i8] c"*FSET\00"
@":::global-str-repl1" = internal unnamed_addr constant [6 x i8] c"repl1\00"
@":::global-str-repl2" = internal unnamed_addr constant [6 x i8] c"repl2\00"
@":::global-str-repl3" = internal unnamed_addr constant [6 x i8] c"repl3\00"
@":::symbol-name-T" = internal unnamed_addr constant [2 x i8] c"T\00"
@constant-array = internal constant [1 x i32] [i32 1]
@":::symbol-name-G6470" = internal unnamed_addr constant [6 x i8] c"G6470\00"
@constant-array4 = internal constant [1 x i32] [i32 5]
@":::symbol-name-EQL" = internal unnamed_addr constant [4 x i8] c"EQL\00"
@":::str" = internal unnamed_addr constant [11 x i8] c"PASSED %s\0A\00"
@":::symbol-name-BFORMAT" = internal unnamed_addr constant [8 x i8] c"BFORMAT\00"
@":::str5" = internal unnamed_addr constant [8 x i8] c"FAILED \00"
@":::str6" = internal unnamed_addr constant [3 x i8] c"%s\00"
@":::str7" = internal unnamed_addr constant [8 x i8] c" -> %s\0A\00"
@":::symbol-name-/Users/meister/Development/cando/brcl/src/tests/core/defun0.lsp" = internal unnamed_addr constant [64 x i8] c"/Users/meister/Development/cando/brcl/src/tests/core/defun0.lsp\00"
@":::global-str-___user_defun0" = internal unnamed_addr constant [15 x i8] c"___user_defun0\00"
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
  call void @getLoadTimeValueArray({ i32*, {}* }** @load-time-value-vector, i8* getelementptr inbounds ([64 x i8]* @":::global-str-/Users/meister/Development/cando/brcl/src/tests/core/defun0.lsp", i64 0, i64 0), i32 20, i32 6)
  %0 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  call void @sp_makeNil({ i32*, {}* }* %0)
  %1 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 1)
  call void @makeT({ i32*, {}* }* %1)
  %2 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 2)
  call void @internSymbol_tsp({ i32*, {}* }* %2, i8* getelementptr inbounds ([10 x i8]* @":::symbol-name-TESTDEFUN", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %3 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  call void @internSymbol_symsp({ i32*, i32* }* %3, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-+", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  %4 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 1)
  call void @internSymbol_symsp({ i32*, i32* }* %4, i8* getelementptr inbounds ([6 x i8]* @":::symbol-name-*FSET", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %5 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 3)
  call void @internSymbol_tsp({ i32*, {}* }* %5, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-T", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %array-element-type, { i32*, {}* }** @load-time-value-vector, i32 3)
  %6 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  call void @ltv_makeArrayObjects({ i32*, {}* }* %6, { i32*, {}* }* %array-element-type, i32 1, i32* getelementptr inbounds ([1 x i32]* @constant-array, i64 0, i64 0))
  %7 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 5)
  call void @makeSymbol_tsp({ i32*, {}* }* %7, i8* getelementptr inbounds ([6 x i8]* @":::symbol-name-G6470", i64 0, i64 0))
  %8 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  call void @ltv_initializeArrayObjectsRowMajorArefOrder({ i32*, {}* }* %8, { i32*, {}* }** @load-time-value-vector, i32* getelementptr inbounds ([1 x i32]* @constant-array4, i64 0, i64 0))
  %9 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 6)
  call void @makeFixnum({ i32*, {}* }* %9, i32 1)
  %10 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 7)
  call void @makeFixnum({ i32*, {}* }* %10, i32 2)
  %11 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 2)
  call void @internSymbol_symsp({ i32*, i32* }* %11, i8* getelementptr inbounds ([10 x i8]* @":::symbol-name-TESTDEFUN", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %12 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 8)
  call void @makeFixnum({ i32*, {}* }* %12, i32 3)
  %13 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 3)
  call void @internSymbol_symsp({ i32*, i32* }* %13, i8* getelementptr inbounds ([4 x i8]* @":::symbol-name-EQL", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  %14 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  call void @internSymbol_symsp({ i32*, i32* }* %14, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-T", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  %15 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 9)
  call void @makeString({ i32*, {}* }* %15, i8* getelementptr inbounds ([11 x i8]* @":::str", i64 0, i64 0))
  %16 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 5)
  call void @internSymbol_symsp({ i32*, i32* }* %16, i8* getelementptr inbounds ([8 x i8]* @":::symbol-name-BFORMAT", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %17 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 10)
  call void @makeString({ i32*, {}* }* %17, i8* getelementptr inbounds ([8 x i8]* @":::str5", i64 0, i64 0))
  %18 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 11)
  call void @makeString({ i32*, {}* }* %18, i8* getelementptr inbounds ([3 x i8]* @":::str6", i64 0, i64 0))
  %19 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 12)
  call void @makeString({ i32*, {}* }* %19, i8* getelementptr inbounds ([8 x i8]* @":::str7", i64 0, i64 0))
  %20 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 13)
  call void @ltv_makeSourceCodeCons({ i32*, {}* }* %20, i8* getelementptr inbounds ([64 x i8]* @":::symbol-name-/Users/meister/Development/cando/brcl/src/tests/core/defun0.lsp", i64 0, i64 0), i32 3, i32 7)
  %21 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 14)
  call void @internSymbol_tsp({ i32*, {}* }* %21, i8* getelementptr inbounds ([4 x i8]* @":::symbol-name-EQL", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  %22 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 15)
  call void @ltv_makeSourceCodeCons({ i32*, {}* }* %22, i8* getelementptr inbounds ([64 x i8]* @":::symbol-name-/Users/meister/Development/cando/brcl/src/tests/core/defun0.lsp", i64 0, i64 0), i32 3, i32 11)
  %23 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 16)
  call void @ltv_makeSourceCodeCons({ i32*, {}* }* %23, i8* getelementptr inbounds ([64 x i8]* @":::symbol-name-/Users/meister/Development/cando/brcl/src/tests/core/defun0.lsp", i64 0, i64 0), i32 3, i32 12)
  %24 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 17)
  call void @ltv_makeSourceCodeCons({ i32*, {}* }* %24, i8* getelementptr inbounds ([64 x i8]* @":::symbol-name-/Users/meister/Development/cando/brcl/src/tests/core/defun0.lsp", i64 0, i64 0), i32 3, i32 22)
  %25 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 18)
  call void @ltv_makeSourceCodeCons({ i32*, {}* }* %25, i8* getelementptr inbounds ([64 x i8]* @":::symbol-name-/Users/meister/Development/cando/brcl/src/tests/core/defun0.lsp", i64 0, i64 0), i32 3, i32 24)
  %26 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 19)
  call void @ltv_makeSourceCodeCons({ i32*, {}* }* %26, i8* getelementptr inbounds ([64 x i8]* @":::symbol-name-/Users/meister/Development/cando/brcl/src/tests/core/defun0.lsp", i64 0, i64 0), i32 3, i32 28)
  %27 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 15)
  %28 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 16)
  %29 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 19)
  call void @rplaca({ i32*, {}* }* %27, { i32*, {}* }* %28)
  call void @rplacd({ i32*, {}* }* %27, { i32*, {}* }* %29)
  %30 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 18)
  %31 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 7)
  %32 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  call void @rplaca({ i32*, {}* }* %30, { i32*, {}* }* %31)
  call void @rplacd({ i32*, {}* }* %30, { i32*, {}* }* %32)
  %33 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 17)
  %34 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 6)
  %35 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 18)
  call void @rplaca({ i32*, {}* }* %33, { i32*, {}* }* %34)
  call void @rplacd({ i32*, {}* }* %33, { i32*, {}* }* %35)
  %36 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 16)
  %37 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 2)
  %38 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 17)
  call void @rplaca({ i32*, {}* }* %36, { i32*, {}* }* %37)
  call void @rplacd({ i32*, {}* }* %36, { i32*, {}* }* %38)
  %39 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 13)
  %40 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 14)
  %41 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 15)
  call void @rplaca({ i32*, {}* }* %39, { i32*, {}* }* %40)
  call void @rplacd({ i32*, {}* }* %39, { i32*, {}* }* %41)
  %42 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 19)
  %43 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 8)
  %44 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  call void @rplaca({ i32*, {}* }* %42, { i32*, {}* }* %43)
  call void @rplacd({ i32*, {}* }* %42, { i32*, {}* }* %44)
  ret void
}

define internal void @repl({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %call-args = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args)
  call void @trace_setLineNumberColumnForIHSTop(i32 1, i32 25)
  call void @makeValueFrame({ i32*, {}* }* %call-args, i32 3)
  %0 = call { i32*, {}* }* @activationFrameNil()
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args, { i32*, {}* }* %0)
  %call-args-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 0)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-0, { i32*, {}* }** @load-time-value-vector, i32 2)
  %call-args-ref-1 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 1)
  %"SYMBOL->nil" = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  %1 = call { i32*, {}* }* @activationFrameNil()
  invoke void @sp_makeCompiledFunction({ i32*, {}* }* %call-args-ref-1, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @TESTDEFUN, i8* getelementptr inbounds ([64 x i8]* @":::global-str-/Users/meister/Development/cando/brcl/src/tests/core/defun0.lsp", i64 0, i64 0), { i32*, {}* }* %"SYMBOL->nil", { i32*, {}* }* %1)
          to label %normal-dest unwind label %func-cleanup-landing-pad

normal-dest:                                      ; preds = %entry
  %call-args-ref-2 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 2)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-2, { i32*, {}* }** @load-time-value-vector, i32 0)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args)
  call void @singleStepCallback()
  %"SYMBOL->*FSET" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 1)
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
  call void @debugPrintI32(i32 100)
  call void @debugPrintI32(i32 101)
  call void @debugPrintI32(i32 90)
  call void @debugPrintI32(i32 91)
  resume { i8*, i32 } %2
}

define internal void @TESTDEFUN({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
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
  %given-num-args = call i32 @activationFrameSize({ i32*, {}* }* %activation-frame-ptr)
  %correct-num-args = icmp eq i32 %given-num-args, 2
  br i1 %correct-num-args, label %continue3, label %error

error:                                            ; preds = %entry
  %enough-args = icmp slt i32 %given-num-args, 2
  br i1 %enough-args, label %error1, label %continue

error1:                                           ; preds = %error
  invoke void @throwNotEnoughArgumentsException(i8* getelementptr inbounds ([10 x i8]* @":::global-str-TESTDEFUN", i64 0, i64 0), { i32*, {}* }* %activation-frame-ptr, i32 %given-num-args, i32 2)
          to label %normal-dest unwind label %func-cleanup-landing-pad

normal-dest:                                      ; preds = %error1
  unreachable

continue:                                         ; preds = %error
  invoke void @throwTooManyArgumentsException(i8* getelementptr inbounds ([10 x i8]* @":::global-str-TESTDEFUN", i64 0, i64 0), { i32*, {}* }* %activation-frame-ptr, i32 %given-num-args, i32 2)
          to label %normal-dest2 unwind label %func-cleanup-landing-pad

normal-dest2:                                     ; preds = %continue
  unreachable

continue3:                                        ; preds = %entry
  call void @copyAFsp({ i32*, {}* }* %lambda-args-1-, { i32*, {}* }* %activation-frame-ptr)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %lambda-args-1-)
  call void @trace_setLineNumberColumnForIHSTop(i32 1, i32 25)
  call void @makeValueFrame({ i32*, {}* }* %call-args, i32 2)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args, { i32*, {}* }* %lambda-args-1-)
  %call-args-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 0)
  call void @sp_lexicalValueRead({ i32*, {}* }* %call-args-ref-0, i32 0, i32 0, { i32*, {}* }* %lambda-args-1-)
  %call-args-ref-1 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 1)
  call void @sp_lexicalValueRead({ i32*, {}* }* %call-args-ref-1, i32 0, i32 1, { i32*, {}* }* %lambda-args-1-)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:+" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  invoke void @mv_invokePossibleMultipleValueSymbolFunction({ i32*, {}*, i32 }* %result-ptr, { i32*, i32* }* %"SYMBOL->COMMON-LISP:+", { i32*, {}* }* %call-args)
          to label %"(TRY-6813492).try-cont" unwind label %"(TRY-6813492).landing-pad"

"(TRY-6813492).landing-pad":                      ; preds = %continue3
  %0 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
          catch i8* @_ZTIN4core10ReturnFromE
  %1 = extractvalue { i8*, i32 } %0, 0
  store i8* %1, i8** %exn.slot, align 8
  %2 = extractvalue { i8*, i32 } %0, 1
  store i32 %2, i32* %ehselector.slot, align 4
  %3 = call i32 @llvm.eh.typeid.for(i8* @_ZTIN4core10ReturnFromE)
  %4 = icmp eq i32 %2, %3
  br i1 %4, label %"(TRY-6813492).handler-block6208", label %func-ehcleanup

"(TRY-6813492).handler-block6208":                ; preds = %"(TRY-6813492).landing-pad"
  %5 = call i8* @__cxa_begin_catch(i8* %1)
  invoke void @mv_blockHandleReturnFrom({ i32*, {}*, i32 }* %result-ptr, i8* %5)
          to label %"(TRY-6813492).normal-dest4" unwind label %"(TRY-6747199).landing-pad"

"(TRY-6813492).normal-dest4":                     ; preds = %"(TRY-6813492).handler-block6208"
  invoke void @__cxa_end_catch()
          to label %"(TRY-6813492).try-cont" unwind label %"(TRY-6747199).landing-pad"

"(TRY-6813492).try-cont":                         ; preds = %"(TRY-6813492).normal-dest4", %continue3
  ret void

"(TRY-6747199).landing-pad":                      ; preds = %"(TRY-6813492).normal-dest4", %"(TRY-6813492).handler-block6208"
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
  call void @debugPrintI32(i32 100)
  br label %func-ehcleanup

func-ehcleanup:                                   ; preds = %func-cleanup-landing-pad, %"(TRY-6747199).landing-pad", %"(TRY-6813492).landing-pad"
  %sel = phi i32 [ %11, %func-cleanup-landing-pad ], [ %8, %"(TRY-6747199).landing-pad" ], [ %2, %"(TRY-6813492).landing-pad" ]
  %exn7 = phi i8* [ %10, %func-cleanup-landing-pad ], [ %7, %"(TRY-6747199).landing-pad" ], [ %1, %"(TRY-6813492).landing-pad" ]
  call void @debugPrintI32(i32 101)
  %lpad.val = insertvalue { i8*, i32 } undef, i8* %exn7, 0
  call void @debugPrintI32(i32 90)
  %lpad.val8 = insertvalue { i8*, i32 } %lpad.val, i32 %sel, 1
  call void @debugPrintI32(i32 91)
  resume { i8*, i32 } %lpad.val8
}

define internal void @repl1({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  call void @mv_copyLoadTimeValue({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }** @load-time-value-vector, i32 0)
  ret void
}

define internal void @repl2({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  call void @mv_copyLoadTimeValue({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }** @load-time-value-vector, i32 2)
  ret void
}

define internal void @repl3({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %LET = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %LET)
  %call-args = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args)
  %call-args1 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args1)
  %temp = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp)
  %if-cond-tsp = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %if-cond-tsp)
  %if-cond-tsp3 = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %if-cond-tsp3)
  %call-args6 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args6)
  %temp9 = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp9)
  %call-args10 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args10)
  %if-cond-tsp14 = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %if-cond-tsp14)
  %call-args17 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args17)
  %call-args23 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args23)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %LET)
  call void @makeValueFrame({ i32*, {}* }* %LET, i32 1)
  %0 = call { i32*, {}* }* @activationFrameNil()
  call void @setParentOfActivationFrame({ i32*, {}* }* %LET, { i32*, {}* }* %0)
  %value = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  call void @attachDebuggingInfoToValueFrame({ i32*, {}* }* %LET, { i32*, {}* }* %value)
  %1 = call { i32*, {}* }* @lexicalValueReference(i32 0, i32 0, { i32*, {}* }* %LET)
  call void @trace_setLineNumberColumnForIHSTop(i32 3, i32 7)
  call void @makeValueFrame({ i32*, {}* }* %call-args, i32 2)
  %2 = call { i32*, {}* }* @activationFrameNil()
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args, { i32*, {}* }* %2)
  %call-args-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 0)
  call void @trace_setLineNumberColumnForIHSTop(i32 3, i32 12)
  call void @makeValueFrame({ i32*, {}* }* %call-args1, i32 2)
  %3 = call { i32*, {}* }* @activationFrameNil()
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args1, { i32*, {}* }* %3)
  %call-args1-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args1, i32 0)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args1-ref-0, { i32*, {}* }** @load-time-value-vector, i32 6)
  %call-args1-ref-1 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args1, i32 1)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args1-ref-1, { i32*, {}* }** @load-time-value-vector, i32 7)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args1)
  call void @singleStepCallback()
  %"SYMBOL->TESTDEFUN" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 2)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %call-args-ref-0, { i32*, i32* }* %"SYMBOL->TESTDEFUN", { i32*, {}* }* %call-args1)
          to label %"(TRY-7656223).normal-dest" unwind label %"(TRY-7656223).landing-pad"

"(TRY-7656223).normal-dest":                      ; preds = %entry
  %call-args-ref-1 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 1)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-1, { i32*, {}* }** @load-time-value-vector, i32 8)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:EQL" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 3)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %1, { i32*, i32* }* %"SYMBOL->COMMON-LISP:EQL", { i32*, {}* }* %call-args)
          to label %"(TRY-7656223).normal-dest2" unwind label %"(TRY-7656223).landing-pad"

"(TRY-7656223).normal-dest2":                     ; preds = %"(TRY-7656223).normal-dest"
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %LET)
  call void @singleStepCallback()
  call void @sp_lexicalValueRead({ i32*, {}* }* %if-cond-tsp, i32 0, i32 0, { i32*, {}* }* %LET)
  %4 = call i32 @isTrueTsp({ i32*, {}* }* %if-cond-tsp)
  %ifcond = icmp eq i32 %4, 1
  br i1 %ifcond, label %"(TRY-7656223).then", label %"(TRY-7656223).else8"

"(TRY-7656223).then":                             ; preds = %"(TRY-7656223).normal-dest2"
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %if-cond-tsp3, { i32*, {}* }** @load-time-value-vector, i32 0)
  %5 = call i32 @isTrueTsp({ i32*, {}* }* %if-cond-tsp3)
  %ifcond4 = icmp eq i32 %5, 1
  br i1 %ifcond4, label %"(TRY-7656223).then5", label %"(TRY-7656223).else"

"(TRY-7656223).then5":                            ; preds = %"(TRY-7656223).then"
  call void @makeValueFrame({ i32*, {}* }* %call-args6, i32 3)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args6, { i32*, {}* }* %LET)
  %call-args6-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args6, i32 0)
  %"SYMBOL->COMMON-LISP:T" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  call void @sp_symbolValueRead({ i32*, {}* }* %call-args6-ref-0, { i32*, i32* }* %"SYMBOL->COMMON-LISP:T")
  %call-args6-ref-1 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args6, i32 1)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args6-ref-1, { i32*, {}* }** @load-time-value-vector, i32 9)
  %call-args6-ref-2 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args6, i32 2)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args6-ref-2, { i32*, {}* }** @load-time-value-vector, i32 0)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args6)
  call void @singleStepCallback()
  %"SYMBOL->BFORMAT" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 5)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %temp, { i32*, i32* }* %"SYMBOL->BFORMAT", { i32*, {}* }* %call-args6)
          to label %"(TRY-7656223).ifcont27" unwind label %"(TRY-7656223).landing-pad"

"(TRY-7656223).else":                             ; preds = %"(TRY-7656223).then"
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %temp, { i32*, {}* }** @load-time-value-vector, i32 0)
  br label %"(TRY-7656223).ifcont27"

"(TRY-7656223).else8":                            ; preds = %"(TRY-7656223).normal-dest2"
  call void @makeValueFrame({ i32*, {}* }* %call-args10, i32 2)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args10, { i32*, {}* }* %LET)
  %call-args10-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args10, i32 0)
  %"SYMBOL->COMMON-LISP:T11" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  call void @sp_symbolValueRead({ i32*, {}* }* %call-args10-ref-0, { i32*, i32* }* %"SYMBOL->COMMON-LISP:T11")
  %call-args10-ref-1 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args10, i32 1)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args10-ref-1, { i32*, {}* }** @load-time-value-vector, i32 10)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args10)
  call void @singleStepCallback()
  %"SYMBOL->BFORMAT12" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 5)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %temp9, { i32*, i32* }* %"SYMBOL->BFORMAT12", { i32*, {}* }* %call-args10)
          to label %"(TRY-7656223).normal-dest13" unwind label %"(TRY-7656223).landing-pad"

"(TRY-7656223).normal-dest13":                    ; preds = %"(TRY-7656223).else8"
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %if-cond-tsp14, { i32*, {}* }** @load-time-value-vector, i32 0)
  %6 = call i32 @isTrueTsp({ i32*, {}* }* %if-cond-tsp14)
  %ifcond15 = icmp eq i32 %6, 1
  br i1 %ifcond15, label %"(TRY-7656223).then16", label %"(TRY-7656223).else21"

"(TRY-7656223).then16":                           ; preds = %"(TRY-7656223).normal-dest13"
  call void @makeValueFrame({ i32*, {}* }* %call-args17, i32 3)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args17, { i32*, {}* }* %LET)
  %call-args17-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args17, i32 0)
  %"SYMBOL->COMMON-LISP:T18" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  call void @sp_symbolValueRead({ i32*, {}* }* %call-args17-ref-0, { i32*, i32* }* %"SYMBOL->COMMON-LISP:T18")
  %call-args17-ref-1 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args17, i32 1)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args17-ref-1, { i32*, {}* }** @load-time-value-vector, i32 11)
  %call-args17-ref-2 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args17, i32 2)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args17-ref-2, { i32*, {}* }** @load-time-value-vector, i32 0)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args17)
  call void @singleStepCallback()
  %"SYMBOL->BFORMAT19" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 5)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %temp9, { i32*, i32* }* %"SYMBOL->BFORMAT19", { i32*, {}* }* %call-args17)
          to label %"(TRY-7656223).ifcont22" unwind label %"(TRY-7656223).landing-pad"

"(TRY-7656223).else21":                           ; preds = %"(TRY-7656223).normal-dest13"
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %temp9, { i32*, {}* }** @load-time-value-vector, i32 0)
  br label %"(TRY-7656223).ifcont22"

"(TRY-7656223).ifcont22":                         ; preds = %"(TRY-7656223).else21", %"(TRY-7656223).then16"
  call void @trace_setLineNumberColumnForIHSTop(i32 3, i32 7)
  call void @makeValueFrame({ i32*, {}* }* %call-args23, i32 3)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args23, { i32*, {}* }* %LET)
  %call-args23-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args23, i32 0)
  %"SYMBOL->COMMON-LISP:T24" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  call void @sp_symbolValueRead({ i32*, {}* }* %call-args23-ref-0, { i32*, i32* }* %"SYMBOL->COMMON-LISP:T24")
  %call-args23-ref-1 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args23, i32 1)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args23-ref-1, { i32*, {}* }** @load-time-value-vector, i32 12)
  %call-args23-ref-2 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args23, i32 2)
  call void @sp_getLoadTimeValue({ i32*, {}* }* %call-args23-ref-2, { i32*, {}* }** @load-time-value-vector, i32 13)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %call-args23)
  call void @singleStepCallback()
  %"SYMBOL->BFORMAT25" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 5)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %temp, { i32*, i32* }* %"SYMBOL->BFORMAT25", { i32*, {}* }* %call-args23)
          to label %"(TRY-7656223).ifcont27" unwind label %"(TRY-7656223).landing-pad"

"(TRY-7656223).ifcont27":                         ; preds = %"(TRY-7656223).ifcont22", %"(TRY-7656223).else", %"(TRY-7656223).then5"
  call void @mv_lexicalValueRead({ i32*, {}*, i32 }* %result-ptr, i32 0, i32 0, { i32*, {}* }* %LET)
  ret void

"(TRY-7656223).landing-pad":                      ; preds = %"(TRY-7656223).ifcont22", %"(TRY-7656223).then16", %"(TRY-7656223).else8", %"(TRY-7656223).then5", %"(TRY-7656223).normal-dest", %entry
  %7 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %8 = extractvalue { i8*, i32 } %7, 0
  store i8* %8, i8** %exn.slot, align 8
  %9 = extractvalue { i8*, i32 } %7, 1
  store i32 %9, i32* %ehselector.slot, align 4
  call void @debugPrintI32(i32 101)
  call void @debugPrintI32(i32 90)
  call void @debugPrintI32(i32 91)
  resume { i8*, i32 } %7
}

define void @___user_defun0() {
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
  invoke void @invokeLlvmFunction({ i32*, {}*, i32 }* %result, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl1, { i32*, {}* }* %1)
          to label %normal-dest2 unwind label %func-cleanup-landing-pad

normal-dest2:                                     ; preds = %normal-dest1
  %2 = call { i32*, {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ i32*, {}*, i32 }* %result, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl2, { i32*, {}* }* %2)
          to label %normal-dest3 unwind label %func-cleanup-landing-pad

normal-dest3:                                     ; preds = %normal-dest2
  %3 = call { i32*, {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ i32*, {}*, i32 }* %result, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl3, { i32*, {}* }* %3)
          to label %normal-dest4 unwind label %func-cleanup-landing-pad

normal-dest4:                                     ; preds = %normal-dest3
  ret void

func-cleanup-landing-pad:                         ; preds = %normal-dest3, %normal-dest2, %normal-dest1, %normal-dest, %entry
  %4 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %5 = extractvalue { i8*, i32 } %4, 0
  store i8* %5, i8** %exn.slot, align 8
  %6 = extractvalue { i8*, i32 } %4, 1
  store i32 %6, i32* %ehselector.slot, align 4
  call void @debugPrintI32(i32 100)
  call void @debugPrintI32(i32 101)
  call void @debugPrintI32(i32 90)
  call void @debugPrintI32(i32 91)
  resume { i8*, i32 } %4
}

attributes #0 = { nounwind }
attributes #1 = { noreturn }
attributes #2 = { nounwind readnone }
attributes #3 = { noreturn nounwind }

!llvm.dbg.cu = !{!0}

!0 = metadata !{i32 786449, metadata !1, i32 32768, metadata !"brcl Common Lisp compiler", i1 false, metadata !"-v", i32 1, metadata !2, metadata !2, metadata !3, metadata !2, metadata !2, metadata !"split-name.log"} ; [ DW_TAG_compile_unit ] [/Users/meister/Development/cando/brcl/src/tests/core/defun0.lsp] [DW_LANG_lo_user]
!1 = metadata !{metadata !"defun0.lsp", metadata !"/Users/meister/Development/cando/brcl/src/tests/core"}
!2 = metadata !{i32 0}
!3 = metadata !{metadata !4, metadata !9, metadata !11, metadata !12, metadata !13, metadata !14}
!4 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl", metadata !"repl", metadata !"repl", i32 1, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl, null, null, metadata !2, i32 1} ; [ DW_TAG_subprogram ] [line 1] [def] [repl]
!5 = metadata !{i32 786473, metadata !1}          ; [ DW_TAG_file_type ] [/Users/meister/Development/cando/brcl/src/tests/core/defun0.lsp]
!6 = metadata !{i32 786453, i32 0, i32 0, metadata !"", i32 0, i64 0, i64 0, i64 0, i32 0, null, metadata !7, i32 0, i32 0} ; [ DW_TAG_subroutine_type ] [line 0, size 0, align 0, offset 0] [from ]
!7 = metadata !{metadata !8}
!8 = metadata !{i32 786468, null, null, metadata !"int", i32 0, i64 32, i64 32, i64 0, i32 0, i32 13} ; [ DW_TAG_base_type ] [int] [line 0, size 32, align 32, offset 0, enc DW_ATE_signed_fixed]
!9 = metadata !{i32 786478, metadata !1, metadata !10, metadata !"TESTDEFUN", metadata !"TESTDEFUN", metadata !"TESTDEFUN", i32 1, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @TESTDEFUN, null, null, metadata !2, i32 1} ; [ DW_TAG_subprogram ] [line 1] [def] [TESTDEFUN]
!10 = metadata !{i32 786443, metadata !1, metadata !4, i32 1, i32 25, i32 0} ; [ DW_TAG_lexical_block ] [/Users/meister/Development/cando/brcl/src/tests/core/defun0.lsp]
!11 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl1", metadata !"repl1", metadata !"repl1", i32 0, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl1, null, null, metadata !2, i32 0} ; [ DW_TAG_subprogram ] [line 0] [def] [repl1]
!12 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl2", metadata !"repl2", metadata !"repl2", i32 0, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl2, null, null, metadata !2, i32 0} ; [ DW_TAG_subprogram ] [line 0] [def] [repl2]
!13 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl3", metadata !"repl3", metadata !"repl3", i32 3, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl3, null, null, metadata !2, i32 3} ; [ DW_TAG_subprogram ] [line 3] [def] [repl3]
!14 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"___user_defun0", metadata !"___user_defun0", metadata !"___user_defun0", i32 0, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ()* @___user_defun0, null, null, metadata !2, i32 0} ; [ DW_TAG_subprogram ] [line 0] [def] [___user_defun0]
