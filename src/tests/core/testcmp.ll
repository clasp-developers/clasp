; ModuleID = 'testcmp.bc'
target triple = "x86_64-apple-macosx"

@":::global-str-/Users/meister/Development/cando/brcl/src/TESTS/CORE/testcmp.lsp" = internal unnamed_addr constant [65 x i8] c"/Users/meister/Development/cando/brcl/src/TESTS/CORE/testcmp.lsp\00"
@load-time-value-vector = internal global { i32*, {}* }* null
@":::global-str-repl" = internal unnamed_addr constant [5 x i8] c"repl\00"
@":::symbol-name-A" = internal unnamed_addr constant [2 x i8] c"A\00"
@":::package-name-CORE" = internal unnamed_addr constant [5 x i8] c"CORE\00"
@":::global-str-A" = internal unnamed_addr constant [2 x i8] c"A\00"
@":::symbol-name-T" = internal unnamed_addr constant [2 x i8] c"T\00"
@":::package-name-COMMON-LISP" = internal unnamed_addr constant [12 x i8] c"COMMON-LISP\00"
@constant-array = internal constant [1 x i32] [i32 2]
@":::symbol-name-X" = internal unnamed_addr constant [2 x i8] c"X\00"
@":::symbol-name-Y" = internal unnamed_addr constant [2 x i8] c"Y\00"
@constant-array1 = internal constant [2 x i32] [i32 5, i32 6]
@":::symbol-name-+" = internal unnamed_addr constant [2 x i8] c"+\00"
@_ZTIN4core10ReturnFromE = external constant i8
@":::symbol-name-*FSET" = internal unnamed_addr constant [6 x i8] c"*FSET\00"
@":::global-str-repl2" = internal unnamed_addr constant [6 x i8] c"repl2\00"
@":::global-str-repl3" = internal unnamed_addr constant [6 x i8] c"repl3\00"
@":::global-str-repl4" = internal unnamed_addr constant [6 x i8] c"repl4\00"
@":::symbol-name-PRINT" = internal unnamed_addr constant [6 x i8] c"PRINT\00"
@":::global-str-repl5" = internal unnamed_addr constant [6 x i8] c"repl5\00"
@":::symbol-name-YODA" = internal unnamed_addr constant [5 x i8] c"YODA\00"
@":::package-name-KEYWORD" = internal unnamed_addr constant [8 x i8] c"KEYWORD\00"
@":::symbol-name-MAKE-PACKAGE" = internal unnamed_addr constant [13 x i8] c"MAKE-PACKAGE\00"
@":::global-str-repl6" = internal unnamed_addr constant [6 x i8] c"repl6\00"
@":::symbol-name-B" = internal unnamed_addr constant [2 x i8] c"B\00"
@":::package-name-YODA" = internal unnamed_addr constant [5 x i8] c"YODA\00"
@":::global-str-__MAIN_testcmp" = internal unnamed_addr constant [15 x i8] c"__MAIN_testcmp\00"
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

define internal void @___loadTimeDataInitializer() {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %0 = alloca { i32*, {}*, i32 }, align 8
  call void @newTmv({ i32*, {}*, i32 }* %0)
  %array-element-type = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %array-element-type)
  call void @getOrCreateLoadTimeValueArray({ i32*, {}* }** @load-time-value-vector, i8* getelementptr inbounds ([65 x i8]* @":::global-str-/Users/meister/Development/cando/brcl/src/TESTS/CORE/testcmp.lsp", i64 0, i64 0), i32 10, i32 6)
  %1 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  call void @sp_makeNil({ i32*, {}* }* %1)
  %2 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 1)
  call void @makeT({ i32*, {}* }* %2)
  %3 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 2)
  call void @internSymbol_tsp({ i32*, {}* }* %3, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-A", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %4 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 3)
  call void @internSymbol_tsp({ i32*, {}* }* %4, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-T", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %array-element-type, { i32*, {}* }** @load-time-value-vector, i32 3)
  %5 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  call void @ltv_makeArrayObjects({ i32*, {}* }* %5, { i32*, {}* }* %array-element-type, i32 1, i32* getelementptr inbounds ([1 x i32]* @constant-array, i64 0, i64 0))
  %6 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 5)
  call void @internSymbol_tsp({ i32*, {}* }* %6, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-X", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %7 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 6)
  call void @internSymbol_tsp({ i32*, {}* }* %7, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-Y", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %8 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  call void @ltv_initializeArrayObjectsRowMajorArefOrder({ i32*, {}* }* %8, { i32*, {}* }** @load-time-value-vector, i32* getelementptr inbounds ([2 x i32]* @constant-array1, i64 0, i64 0))
  %9 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  call void @internSymbol_symsp({ i32*, i32* }* %9, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-+", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  %10 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 1)
  call void @internSymbol_symsp({ i32*, i32* }* %10, i8* getelementptr inbounds ([6 x i8]* @":::symbol-name-*FSET", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %11 = call { i32*, {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ i32*, {}*, i32 }* %0, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl, { i32*, {}* }* %11)
          to label %normal-dest unwind label %func-cleanup-landing-pad

normal-dest:                                      ; preds = %entry
  %12 = call { i32*, {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ i32*, {}*, i32 }* %0, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl2, { i32*, {}* }* %12)
          to label %normal-dest1 unwind label %func-cleanup-landing-pad

normal-dest1:                                     ; preds = %normal-dest
  %13 = call { i32*, {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ i32*, {}*, i32 }* %0, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl3, { i32*, {}* }* %13)
          to label %normal-dest2 unwind label %func-cleanup-landing-pad

normal-dest2:                                     ; preds = %normal-dest1
  %14 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 7)
  call void @makeFixnum({ i32*, {}* }* %14, i32 1)
  %15 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 8)
  call void @makeFixnum({ i32*, {}* }* %15, i32 2)
  %16 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 2)
  call void @internSymbol_symsp({ i32*, i32* }* %16, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-A", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %17 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 3)
  call void @internSymbol_symsp({ i32*, i32* }* %17, i8* getelementptr inbounds ([6 x i8]* @":::symbol-name-PRINT", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  %18 = call { i32*, {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ i32*, {}*, i32 }* %0, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl4, { i32*, {}* }* %18)
          to label %normal-dest3 unwind label %func-cleanup-landing-pad

normal-dest3:                                     ; preds = %normal-dest2
  %19 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  call void @internSymbol_symsp({ i32*, i32* }* %19, i8* getelementptr inbounds ([5 x i8]* @":::symbol-name-YODA", i64 0, i64 0), i8* getelementptr inbounds ([8 x i8]* @":::package-name-KEYWORD", i64 0, i64 0))
  %20 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 5)
  call void @internSymbol_symsp({ i32*, i32* }* %20, i8* getelementptr inbounds ([13 x i8]* @":::symbol-name-MAKE-PACKAGE", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  %21 = call { i32*, {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ i32*, {}*, i32 }* %0, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl5, { i32*, {}* }* %21)
          to label %normal-dest4 unwind label %func-cleanup-landing-pad

normal-dest4:                                     ; preds = %normal-dest3
  %22 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 9)
  call void @internSymbol_tsp({ i32*, {}* }* %22, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-B", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-YODA", i64 0, i64 0))
  %23 = call { i32*, {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ i32*, {}*, i32 }* %0, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl6, { i32*, {}* }* %23)
          to label %normal-dest5 unwind label %func-cleanup-landing-pad

normal-dest5:                                     ; preds = %normal-dest4
  call void @destructTsp({ i32*, {}* }* %array-element-type)
  call void @destructTmv({ i32*, {}*, i32 }* %0)
  ret void

func-cleanup-landing-pad:                         ; preds = %normal-dest4, %normal-dest3, %normal-dest2, %normal-dest1, %normal-dest, %entry
  %24 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %25 = extractvalue { i8*, i32 } %24, 0
  store i8* %25, i8** %exn.slot, align 8
  %26 = extractvalue { i8*, i32 } %24, 1
  store i32 %26, i32* %ehselector.slot, align 4
  call void @destructTsp({ i32*, {}* }* %array-element-type)
  call void @destructTmv({ i32*, {}*, i32 }* %0)
  resume { i8*, i32 } %24
}

define internal void @repl({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %call-args = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args)
  call void @trace_setLineNumberColumnForIHSTop(i32 2, i32 10), !dbg !17
  call void @makeValueFrame({ i32*, {}* }* %call-args, i32 3, i32 2000000), !dbg !17
  %0 = call { i32*, {}* }* @activationFrameNil(), !dbg !17
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args, { i32*, {}* }* %0), !dbg !17
  %call-args-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 0), !dbg !17
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-0, { i32*, {}* }** @load-time-value-vector, i32 2), !dbg !17
  %call-args-ref-1 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 1), !dbg !17
  %"SYMBOL->COMMON-LISP:NIL" = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0), !dbg !17
  %"SYMBOL->A" = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 2), !dbg !17
  %1 = call { i32*, {}* }* @activationFrameNil(), !dbg !17
  invoke void @sp_makeCompiledFunction({ i32*, {}* }* %call-args-ref-1, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @A, i8* getelementptr inbounds ([65 x i8]* @":::global-str-/Users/meister/Development/cando/brcl/src/TESTS/CORE/testcmp.lsp", i64 0, i64 0), { i32*, {}* }* %"SYMBOL->A", { i32*, {}* }* %"SYMBOL->COMMON-LISP:NIL", { i32*, {}* }* %1)
          to label %normal-dest unwind label %func-cleanup-landing-pad, !dbg !17

normal-dest:                                      ; preds = %entry
  %call-args-ref-2 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 2), !dbg !17
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-2, { i32*, {}* }** @load-time-value-vector, i32 0), !dbg !17
  call void @singleStepCallback(), !dbg !17
  %"SYMBOL->*FSET" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 1), !dbg !17
  invoke void @mv_invokePossibleMultipleValueSymbolFunction({ i32*, {}*, i32 }* %result-ptr, { i32*, i32* }* %"SYMBOL->*FSET", { i32*, {}* }* %call-args)
          to label %normal-dest1 unwind label %func-cleanup-landing-pad, !dbg !17

normal-dest1:                                     ; preds = %normal-dest
  call void @destructAFsp({ i32*, {}* }* %call-args), !dbg !17
  ret void, !dbg !17

func-cleanup-landing-pad:                         ; preds = %normal-dest, %entry
  %2 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup, !dbg !17
  %3 = extractvalue { i8*, i32 } %2, 0, !dbg !17
  store i8* %3, i8** %exn.slot, align 8, !dbg !17
  %4 = extractvalue { i8*, i32 } %2, 1, !dbg !17
  store i32 %4, i32* %ehselector.slot, align 4, !dbg !17
  call void @destructAFsp({ i32*, {}* }* %call-args), !dbg !17
  resume { i8*, i32 } %2, !dbg !17
}

define internal void @A({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
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
  invoke void @throwNotEnoughArgumentsException(i8* getelementptr inbounds ([2 x i8]* @":::global-str-A", i64 0, i64 0), { i32*, {}* }* %activation-frame-ptr, i32 %given-num-args, i32 2)
          to label %normal-dest unwind label %func-cleanup-landing-pad

normal-dest:                                      ; preds = %error1
  unreachable

continue:                                         ; preds = %error
  invoke void @throwTooManyArgumentsException(i8* getelementptr inbounds ([2 x i8]* @":::global-str-A", i64 0, i64 0), { i32*, {}* }* %activation-frame-ptr, i32 %given-num-args, i32 2)
          to label %normal-dest2 unwind label %func-cleanup-landing-pad

normal-dest2:                                     ; preds = %continue
  unreachable

continue3:                                        ; preds = %entry
  call void @copyAFsp({ i32*, {}* }* %lambda-args-1-, { i32*, {}* }* %activation-frame-ptr)
  %value = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  call void @attachDebuggingInfoToValueFrame({ i32*, {}* }* %lambda-args-1-, { i32*, {}* }* %value)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %lambda-args-1-)
  call void @trace_setLineNumberColumnForIHSTop(i32 2, i32 16), !dbg !18
  call void @makeValueFrame({ i32*, {}* }* %call-args, i32 2, i32 2000001), !dbg !18
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args, { i32*, {}* }* %lambda-args-1-), !dbg !18
  %call-args-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 0), !dbg !18
  call void @sp_lexicalValueRead({ i32*, {}* }* %call-args-ref-0, i32 0, i32 0, { i32*, {}* }* %lambda-args-1-), !dbg !18
  %call-args-ref-1 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 1), !dbg !18
  call void @sp_lexicalValueRead({ i32*, {}* }* %call-args-ref-1, i32 0, i32 1, { i32*, {}* }* %lambda-args-1-), !dbg !18
  call void @singleStepCallback(), !dbg !18
  %"SYMBOL->COMMON-LISP:+" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 0), !dbg !18
  invoke void @mv_invokePossibleMultipleValueSymbolFunction({ i32*, {}*, i32 }* %result-ptr, { i32*, i32* }* %"SYMBOL->COMMON-LISP:+", { i32*, {}* }* %call-args)
          to label %"(TRY-0).try-cont" unwind label %"(TRY-0).landing-pad", !dbg !18

"(TRY-0).landing-pad":                            ; preds = %continue3
  %0 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
          catch i8* @_ZTIN4core10ReturnFromE, !dbg !18
  %1 = extractvalue { i8*, i32 } %0, 0, !dbg !18
  store i8* %1, i8** %exn.slot, align 8, !dbg !18
  %2 = extractvalue { i8*, i32 } %0, 1, !dbg !18
  store i32 %2, i32* %ehselector.slot, align 4, !dbg !18
  %3 = call i32 @llvm.eh.typeid.for(i8* @_ZTIN4core10ReturnFromE), !dbg !18
  %4 = icmp eq i32 %2, %3, !dbg !18
  br i1 %4, label %"(TRY-0).handler-block6101", label %func-ehcleanup, !dbg !18

"(TRY-0).handler-block6101":                      ; preds = %"(TRY-0).landing-pad"
  %5 = call i8* @__cxa_begin_catch(i8* %1), !dbg !18
  invoke void @mv_blockHandleReturnFrom({ i32*, {}*, i32 }* %result-ptr, i8* %5)
          to label %"(TRY-0).normal-dest5" unwind label %"(TRY-0).landing-pad8", !dbg !18

"(TRY-0).normal-dest5":                           ; preds = %"(TRY-0).handler-block6101"
  invoke void @__cxa_end_catch()
          to label %"(TRY-0).try-cont" unwind label %"(TRY-0).landing-pad8", !dbg !18

"(TRY-0).try-cont":                               ; preds = %"(TRY-0).normal-dest5", %continue3
  call void @destructAFsp({ i32*, {}* }* %call-args), !dbg !18
  call void @destructTsp({ i32*, {}* }* %temp), !dbg !18
  call void @destructAFsp({ i32*, {}* }* %lambda-args-1-), !dbg !18
  ret void, !dbg !18

"(TRY-0).landing-pad8":                           ; preds = %"(TRY-0).normal-dest5", %"(TRY-0).handler-block6101"
  %6 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup, !dbg !18
  %7 = extractvalue { i8*, i32 } %6, 0, !dbg !18
  store i8* %7, i8** %exn.slot, align 8, !dbg !18
  %8 = extractvalue { i8*, i32 } %6, 1, !dbg !18
  store i32 %8, i32* %ehselector.slot, align 4, !dbg !18
  br label %func-ehcleanup, !dbg !18

func-cleanup-landing-pad:                         ; preds = %continue, %error1
  %9 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup, !dbg !18
  %10 = extractvalue { i8*, i32 } %9, 0, !dbg !18
  store i8* %10, i8** %exn.slot, align 8, !dbg !18
  %11 = extractvalue { i8*, i32 } %9, 1, !dbg !18
  store i32 %11, i32* %ehselector.slot, align 4, !dbg !18
  br label %func-ehcleanup, !dbg !18

func-ehcleanup:                                   ; preds = %func-cleanup-landing-pad, %"(TRY-0).landing-pad8", %"(TRY-0).landing-pad"
  %sel = phi i32 [ %11, %func-cleanup-landing-pad ], [ %8, %"(TRY-0).landing-pad8" ], [ %2, %"(TRY-0).landing-pad" ]
  %exn7 = phi i8* [ %10, %func-cleanup-landing-pad ], [ %7, %"(TRY-0).landing-pad8" ], [ %1, %"(TRY-0).landing-pad" ]
  call void @destructAFsp({ i32*, {}* }* %call-args), !dbg !18
  call void @destructTsp({ i32*, {}* }* %temp), !dbg !18
  call void @destructAFsp({ i32*, {}* }* %lambda-args-1-), !dbg !18
  %lpad.val = insertvalue { i8*, i32 } undef, i8* %exn7, 0, !dbg !18
  %lpad.val8 = insertvalue { i8*, i32 } %lpad.val, i32 %sel, 1, !dbg !18
  resume { i8*, i32 } %lpad.val8, !dbg !18
}

define internal void @repl2({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  call void @mv_copyLoadTimeValue({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }** @load-time-value-vector, i32 0)
  ret void
}

define internal void @repl3({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  call void @mv_copyLoadTimeValue({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }** @load-time-value-vector, i32 2)
  ret void
}

define internal void @repl4({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %call-args = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args)
  %call-args1 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args1)
  call void @trace_setLineNumberColumnForIHSTop(i32 4, i32 1), !dbg !21
  call void @makeValueFrame({ i32*, {}* }* %call-args, i32 1, i32 2000002), !dbg !21
  %0 = call { i32*, {}* }* @activationFrameNil(), !dbg !21
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args, { i32*, {}* }* %0), !dbg !21
  %call-args-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 0), !dbg !21
  call void @trace_setLineNumberColumnForIHSTop(i32 4, i32 8), !dbg !23
  call void @makeValueFrame({ i32*, {}* }* %call-args1, i32 2, i32 2000003), !dbg !23
  %1 = call { i32*, {}* }* @activationFrameNil(), !dbg !23
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args1, { i32*, {}* }* %1), !dbg !23
  %call-args1-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args1, i32 0), !dbg !23
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args1-ref-0, { i32*, {}* }** @load-time-value-vector, i32 7), !dbg !23
  %call-args1-ref-1 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args1, i32 1), !dbg !23
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args1-ref-1, { i32*, {}* }** @load-time-value-vector, i32 8), !dbg !23
  call void @singleStepCallback(), !dbg !23
  %"SYMBOL->A" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 2), !dbg !23
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %call-args-ref-0, { i32*, i32* }* %"SYMBOL->A", { i32*, {}* }* %call-args1)
          to label %normal-dest unwind label %func-cleanup-landing-pad, !dbg !23

normal-dest:                                      ; preds = %entry
  call void @singleStepCallback(), !dbg !23
  %"SYMBOL->COMMON-LISP:PRINT" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 3), !dbg !23
  invoke void @mv_invokePossibleMultipleValueSymbolFunction({ i32*, {}*, i32 }* %result-ptr, { i32*, i32* }* %"SYMBOL->COMMON-LISP:PRINT", { i32*, {}* }* %call-args)
          to label %normal-dest2 unwind label %func-cleanup-landing-pad, !dbg !23

normal-dest2:                                     ; preds = %normal-dest
  call void @destructAFsp({ i32*, {}* }* %call-args1), !dbg !23
  call void @destructAFsp({ i32*, {}* }* %call-args), !dbg !23
  ret void, !dbg !23

func-cleanup-landing-pad:                         ; preds = %normal-dest, %entry
  %2 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup, !dbg !23
  %3 = extractvalue { i8*, i32 } %2, 0, !dbg !23
  store i8* %3, i8** %exn.slot, align 8, !dbg !23
  %4 = extractvalue { i8*, i32 } %2, 1, !dbg !23
  store i32 %4, i32* %ehselector.slot, align 4, !dbg !23
  call void @destructAFsp({ i32*, {}* }* %call-args1), !dbg !23
  call void @destructAFsp({ i32*, {}* }* %call-args), !dbg !23
  resume { i8*, i32 } %2, !dbg !23
}

define internal void @repl5({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %call-args = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args)
  call void @trace_setLineNumberColumnForIHSTop(i32 7, i32 3), !dbg !24
  call void @makeValueFrame({ i32*, {}* }* %call-args, i32 1, i32 2000004), !dbg !24
  %0 = call { i32*, {}* }* @activationFrameNil(), !dbg !24
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args, { i32*, {}* }* %0), !dbg !24
  %call-args-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 0), !dbg !24
  %"SYMBOL->:YODA" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 4), !dbg !24
  call void @sp_symbolValueRead({ i32*, {}* }* %call-args-ref-0, { i32*, i32* }* %"SYMBOL->:YODA"), !dbg !24
  call void @singleStepCallback(), !dbg !24
  %"SYMBOL->COMMON-LISP:MAKE-PACKAGE" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 5), !dbg !24
  invoke void @mv_invokePossibleMultipleValueSymbolFunction({ i32*, {}*, i32 }* %result-ptr, { i32*, i32* }* %"SYMBOL->COMMON-LISP:MAKE-PACKAGE", { i32*, {}* }* %call-args)
          to label %normal-dest unwind label %func-cleanup-landing-pad, !dbg !24

normal-dest:                                      ; preds = %entry
  call void @destructAFsp({ i32*, {}* }* %call-args), !dbg !24
  ret void, !dbg !24

func-cleanup-landing-pad:                         ; preds = %entry
  %1 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup, !dbg !24
  %2 = extractvalue { i8*, i32 } %1, 0, !dbg !24
  store i8* %2, i8** %exn.slot, align 8, !dbg !24
  %3 = extractvalue { i8*, i32 } %1, 1, !dbg !24
  store i32 %3, i32* %ehselector.slot, align 4, !dbg !24
  call void @destructAFsp({ i32*, {}* }* %call-args), !dbg !24
  resume { i8*, i32 } %1, !dbg !24
}

define internal void @repl6({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %call-args = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args)
  call void @trace_setLineNumberColumnForIHSTop(i32 9, i32 1), !dbg !26
  call void @makeValueFrame({ i32*, {}* }* %call-args, i32 1, i32 2000005), !dbg !26
  %0 = call { i32*, {}* }* @activationFrameNil(), !dbg !26
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args, { i32*, {}* }* %0), !dbg !26
  %call-args-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 0), !dbg !26
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-0, { i32*, {}* }** @load-time-value-vector, i32 9), !dbg !26
  call void @singleStepCallback(), !dbg !26
  %"SYMBOL->COMMON-LISP:PRINT" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 3), !dbg !26
  invoke void @mv_invokePossibleMultipleValueSymbolFunction({ i32*, {}*, i32 }* %result-ptr, { i32*, i32* }* %"SYMBOL->COMMON-LISP:PRINT", { i32*, {}* }* %call-args)
          to label %normal-dest unwind label %func-cleanup-landing-pad, !dbg !26

normal-dest:                                      ; preds = %entry
  call void @destructAFsp({ i32*, {}* }* %call-args), !dbg !26
  ret void, !dbg !26

func-cleanup-landing-pad:                         ; preds = %entry
  %1 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup, !dbg !26
  %2 = extractvalue { i8*, i32 } %1, 0, !dbg !26
  store i8* %2, i8** %exn.slot, align 8, !dbg !26
  %3 = extractvalue { i8*, i32 } %1, 1, !dbg !26
  store i32 %3, i32* %ehselector.slot, align 4, !dbg !26
  call void @destructAFsp({ i32*, {}* }* %call-args), !dbg !26
  resume { i8*, i32 } %1, !dbg !26
}

define internal void @__MAIN_testcmp() {
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

!llvm.dbg.cu = !{!0}

!0 = metadata !{i32 786449, metadata !1, i32 32768, metadata !"brcl Common Lisp compiler", i1 false, metadata !"-v", i32 1, metadata !2, metadata !2, metadata !3, metadata !2, metadata !2, metadata !"split-name.log"} ; [ DW_TAG_compile_unit ] [/Users/meister/Development/cando/brcl/src/TESTS/CORE//testcmp] [DW_LANG_lo_user]
!1 = metadata !{metadata !"testcmp", metadata !"/Users/meister/Development/cando/brcl/src/TESTS/CORE/"}
!2 = metadata !{i32 0}
!3 = metadata !{metadata !4, metadata !9, metadata !11, metadata !12, metadata !13, metadata !14, metadata !15, metadata !16}
!4 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl", metadata !"repl", metadata !"repl", i32 2, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl, null, null, metadata !2, i32 2} ; [ DW_TAG_subprogram ] [line 2] [def] [repl]
!5 = metadata !{i32 786473, metadata !1}          ; [ DW_TAG_file_type ] [/Users/meister/Development/cando/brcl/src/TESTS/CORE//testcmp]
!6 = metadata !{i32 786453, i32 0, i32 0, metadata !"", i32 0, i64 0, i64 0, i64 0, i32 0, null, metadata !7, i32 0, i32 0} ; [ DW_TAG_subroutine_type ] [line 0, size 0, align 0, offset 0] [from ]
!7 = metadata !{metadata !8}
!8 = metadata !{i32 786468, null, null, metadata !"int", i32 0, i64 32, i64 32, i64 0, i32 0, i32 13} ; [ DW_TAG_base_type ] [int] [line 0, size 32, align 32, offset 0, enc DW_ATE_signed_fixed]
!9 = metadata !{i32 786478, metadata !1, metadata !10, metadata !"A", metadata !"A", metadata !"A", i32 0, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @A, null, null, metadata !2, i32 0} ; [ DW_TAG_subprogram ] [line 0] [def] [A]
!10 = metadata !{i32 786443, metadata !1, metadata !4, i32 2, i32 10, i32 0} ; [ DW_TAG_lexical_block ] [/Users/meister/Development/cando/brcl/src/TESTS/CORE//testcmp]
!11 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl2", metadata !"repl2", metadata !"repl2", i32 0, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl2, null, null, metadata !2, i32 0} ; [ DW_TAG_subprogram ] [line 0] [def] [repl2]
!12 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl3", metadata !"repl3", metadata !"repl3", i32 0, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl3, null, null, metadata !2, i32 0} ; [ DW_TAG_subprogram ] [line 0] [def] [repl3]
!13 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl4", metadata !"repl4", metadata !"repl4", i32 4, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl4, null, null, metadata !2, i32 4} ; [ DW_TAG_subprogram ] [line 4] [def] [repl4]
!14 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl5", metadata !"repl5", metadata !"repl5", i32 7, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl5, null, null, metadata !2, i32 7} ; [ DW_TAG_subprogram ] [line 7] [def] [repl5]
!15 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl6", metadata !"repl6", metadata !"repl6", i32 9, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl6, null, null, metadata !2, i32 9} ; [ DW_TAG_subprogram ] [line 9] [def] [repl6]
!16 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"__MAIN_testcmp", metadata !"__MAIN_testcmp", metadata !"__MAIN_testcmp", i32 0, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ()* @__MAIN_testcmp, null, null, metadata !2, i32 0} ; [ DW_TAG_subprogram ] [line 0] [def] [__MAIN_testcmp]
!17 = metadata !{i32 2, i32 10, metadata !10, null}
!18 = metadata !{i32 2, i32 16, metadata !19, null}
!19 = metadata !{i32 786443, metadata !1, metadata !20, i32 0, i32 0, i32 2} ; [ DW_TAG_lexical_block ] [/Users/meister/Development/cando/brcl/src/TESTS/CORE//testcmp]
!20 = metadata !{i32 786443, metadata !1, metadata !9, i32 0, i32 0, i32 1} ; [ DW_TAG_lexical_block ] [/Users/meister/Development/cando/brcl/src/TESTS/CORE//testcmp]
!21 = metadata !{i32 4, i32 1, metadata !22, null}
!22 = metadata !{i32 786443, metadata !1, metadata !13, i32 4, i32 1, i32 5} ; [ DW_TAG_lexical_block ] [/Users/meister/Development/cando/brcl/src/TESTS/CORE//testcmp]
!23 = metadata !{i32 4, i32 8, metadata !22, null}
!24 = metadata !{i32 7, i32 3, metadata !25, null}
!25 = metadata !{i32 786443, metadata !1, metadata !14, i32 7, i32 3, i32 6} ; [ DW_TAG_lexical_block ] [/Users/meister/Development/cando/brcl/src/TESTS/CORE//testcmp]
!26 = metadata !{i32 9, i32 1, metadata !27, null}
!27 = metadata !{i32 786443, metadata !1, metadata !15, i32 9, i32 1, i32 7} ; [ DW_TAG_lexical_block ] [/Users/meister/Development/cando/brcl/src/TESTS/CORE//testcmp]
