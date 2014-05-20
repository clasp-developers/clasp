; ModuleID = 'ltv2.bc'
target triple = "x86_64-apple-macosx"

@":::global-str-/Users/meister/Development/cando/brcl/src/tests/core/ltv2.lsp" = internal unnamed_addr constant [62 x i8] c"/Users/meister/Development/cando/brcl/src/tests/core/ltv2.lsp\00"
@load-time-value-vector = internal global { i32*, {}* }* null
@":::global-str-repl" = internal unnamed_addr constant [5 x i8] c"repl\00"
@":::symbol-name-XXX" = internal unnamed_addr constant [4 x i8] c"XXX\00"
@":::package-name-CORE" = internal unnamed_addr constant [5 x i8] c"CORE\00"
@":::str" = internal unnamed_addr constant [5 x i8] c"XXX-\00"
@":::symbol-name-/Users/meister/Development/cando/brcl/src/tests/core/ltv2.lsp" = internal unnamed_addr constant [62 x i8] c"/Users/meister/Development/cando/brcl/src/tests/core/ltv2.lsp\00"
@":::symbol-name-A" = internal unnamed_addr constant [2 x i8] c"A\00"
@":::symbol-name-B" = internal unnamed_addr constant [2 x i8] c"B\00"
@":::symbol-name-C" = internal unnamed_addr constant [2 x i8] c"C\00"
@":::symbol-name-T" = internal unnamed_addr constant [2 x i8] c"T\00"
@":::package-name-COMMON-LISP" = internal unnamed_addr constant [12 x i8] c"COMMON-LISP\00"
@":::symbol-name-COPY-XXX" = internal unnamed_addr constant [9 x i8] c"COPY-XXX\00"
@":::symbol-name-MAKE-XXX" = internal unnamed_addr constant [9 x i8] c"MAKE-XXX\00"
@":::symbol-name-XXX-P" = internal unnamed_addr constant [6 x i8] c"XXX-P\00"
@":::symbol-name-DEFINE-STRUCTURE" = internal unnamed_addr constant [17 x i8] c"DEFINE-STRUCTURE\00"
@":::global-str-MAKE-XXX" = internal unnamed_addr constant [9 x i8] c"MAKE-XXX\00"
@":::symbol-name-ALLOW-OTHER-KEYS" = internal unnamed_addr constant [17 x i8] c"ALLOW-OTHER-KEYS\00"
@":::package-name-KEYWORD" = internal unnamed_addr constant [8 x i8] c"KEYWORD\00"
@constant-array = internal constant [1 x i32] [i32 3]
@constant-array1 = internal constant [3 x i32] [i32 45, i32 46, i32 47]
@":::global-str-load-time-value-func" = internal unnamed_addr constant [21 x i8] c"load-time-value-func\00"
@":::symbol-name-FIND-CLASS" = internal unnamed_addr constant [11 x i8] c"FIND-CLASS\00"
@":::symbol-name-MAKE-STRUCTURE" = internal unnamed_addr constant [15 x i8] c"MAKE-STRUCTURE\00"
@_ZTIN4core10ReturnFromE = external constant i8
@":::symbol-name-*FSET" = internal unnamed_addr constant [6 x i8] c"*FSET\00"
@":::global-str-repl2" = internal unnamed_addr constant [6 x i8] c"repl2\00"
@":::global-str-repl3" = internal unnamed_addr constant [6 x i8] c"repl3\00"
@":::symbol-name-*A*" = internal unnamed_addr constant [4 x i8] c"*A*\00"
@":::symbol-name-*MAKE-SPECIAL" = internal unnamed_addr constant [14 x i8] c"*MAKE-SPECIAL\00"
@":::symbol-name-BOUNDP" = internal unnamed_addr constant [7 x i8] c"BOUNDP\00"
@":::symbol-name-NOT" = internal unnamed_addr constant [4 x i8] c"NOT\00"
@":::global-str-___user_ltv2" = internal unnamed_addr constant [13 x i8] c"___user_ltv2\00"
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
  call void @getLoadTimeValueArray({ i32*, {}* }** @load-time-value-vector, i8* getelementptr inbounds ([62 x i8]* @":::global-str-/Users/meister/Development/cando/brcl/src/tests/core/ltv2.lsp", i64 0, i64 0), i32 50, i32 9)
  %0 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  call void @sp_makeNil({ i32*, {}* }* %0)
  %1 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 1)
  call void @makeT({ i32*, {}* }* %1)
  %2 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 2)
  call void @internSymbol_tsp({ i32*, {}* }* %2, i8* getelementptr inbounds ([4 x i8]* @":::symbol-name-XXX", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %3 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 3)
  call void @makeString({ i32*, {}* }* %3, i8* getelementptr inbounds ([5 x i8]* @":::str", i64 0, i64 0))
  %4 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  call void @ltv_makeSourceCodeCons({ i32*, {}* }* %4, i8* getelementptr inbounds ([62 x i8]* @":::symbol-name-/Users/meister/Development/cando/brcl/src/tests/core/ltv2.lsp", i64 0, i64 0), i32 2, i32 15)
  %5 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 5)
  call void @internSymbol_tsp({ i32*, {}* }* %5, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-A", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %6 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 6)
  call void @ltv_makeSourceCodeCons({ i32*, {}* }* %6, i8* getelementptr inbounds ([62 x i8]* @":::symbol-name-/Users/meister/Development/cando/brcl/src/tests/core/ltv2.lsp", i64 0, i64 0), i32 2, i32 17)
  %7 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 7)
  call void @internSymbol_tsp({ i32*, {}* }* %7, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-B", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %8 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 8)
  call void @ltv_makeSourceCodeCons({ i32*, {}* }* %8, i8* getelementptr inbounds ([62 x i8]* @":::symbol-name-/Users/meister/Development/cando/brcl/src/tests/core/ltv2.lsp", i64 0, i64 0), i32 2, i32 19)
  %9 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 9)
  call void @internSymbol_tsp({ i32*, {}* }* %9, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-C", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %10 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 6)
  %11 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 7)
  %12 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 8)
  call void @rplaca({ i32*, {}* }* %10, { i32*, {}* }* %11)
  call void @rplacd({ i32*, {}* }* %10, { i32*, {}* }* %12)
  %13 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 8)
  %14 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 9)
  %15 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  call void @rplaca({ i32*, {}* }* %13, { i32*, {}* }* %14)
  call void @rplacd({ i32*, {}* }* %13, { i32*, {}* }* %15)
  %16 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  %17 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 5)
  %18 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 6)
  call void @rplaca({ i32*, {}* }* %16, { i32*, {}* }* %17)
  call void @rplacd({ i32*, {}* }* %16, { i32*, {}* }* %18)
  %19 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 10)
  call void @ltv_makeCons({ i32*, {}* }* %19)
  %20 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 11)
  call void @ltv_makeCons({ i32*, {}* }* %20)
  %21 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 12)
  call void @ltv_makeCons({ i32*, {}* }* %21)
  %22 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 13)
  call void @ltv_makeCons({ i32*, {}* }* %22)
  %23 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 14)
  call void @internSymbol_tsp({ i32*, {}* }* %23, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-T", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  %24 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 15)
  call void @ltv_makeCons({ i32*, {}* }* %24)
  %25 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 16)
  call void @ltv_makeCons({ i32*, {}* }* %25)
  %26 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 17)
  call void @makeFixnum({ i32*, {}* }* %26, i32 0)
  %27 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 18)
  call void @ltv_makeCons({ i32*, {}* }* %27)
  %28 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 19)
  call void @ltv_makeCons({ i32*, {}* }* %28)
  %29 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 20)
  call void @ltv_makeCons({ i32*, {}* }* %29)
  %30 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 21)
  call void @ltv_makeCons({ i32*, {}* }* %30)
  %31 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 22)
  call void @ltv_makeCons({ i32*, {}* }* %31)
  %32 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 23)
  call void @ltv_makeCons({ i32*, {}* }* %32)
  %33 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 24)
  call void @ltv_makeCons({ i32*, {}* }* %33)
  %34 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 25)
  call void @makeFixnum({ i32*, {}* }* %34, i32 1)
  %35 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 26)
  call void @ltv_makeCons({ i32*, {}* }* %35)
  %36 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 27)
  call void @ltv_makeCons({ i32*, {}* }* %36)
  %37 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 28)
  call void @ltv_makeCons({ i32*, {}* }* %37)
  %38 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 29)
  call void @ltv_makeCons({ i32*, {}* }* %38)
  %39 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 30)
  call void @ltv_makeCons({ i32*, {}* }* %39)
  %40 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 31)
  call void @ltv_makeCons({ i32*, {}* }* %40)
  %41 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 32)
  call void @ltv_makeCons({ i32*, {}* }* %41)
  %42 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 33)
  call void @makeFixnum({ i32*, {}* }* %42, i32 2)
  %43 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 34)
  call void @ltv_makeCons({ i32*, {}* }* %43)
  %44 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 10)
  %45 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 11)
  %46 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 19)
  call void @rplaca({ i32*, {}* }* %44, { i32*, {}* }* %45)
  call void @rplacd({ i32*, {}* }* %44, { i32*, {}* }* %46)
  %47 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 27)
  %48 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 28)
  %49 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  call void @rplaca({ i32*, {}* }* %47, { i32*, {}* }* %48)
  call void @rplacd({ i32*, {}* }* %47, { i32*, {}* }* %49)
  %50 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 24)
  %51 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 25)
  %52 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 26)
  call void @rplaca({ i32*, {}* }* %50, { i32*, {}* }* %51)
  call void @rplacd({ i32*, {}* }* %50, { i32*, {}* }* %52)
  %53 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 11)
  %54 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 5)
  %55 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 12)
  call void @rplaca({ i32*, {}* }* %53, { i32*, {}* }* %54)
  call void @rplacd({ i32*, {}* }* %53, { i32*, {}* }* %55)
  %56 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 28)
  %57 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 9)
  %58 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 29)
  call void @rplaca({ i32*, {}* }* %56, { i32*, {}* }* %57)
  call void @rplacd({ i32*, {}* }* %56, { i32*, {}* }* %58)
  %59 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 26)
  %60 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  %61 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  call void @rplaca({ i32*, {}* }* %59, { i32*, {}* }* %60)
  call void @rplacd({ i32*, {}* }* %59, { i32*, {}* }* %61)
  %62 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 12)
  %63 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  %64 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 13)
  call void @rplaca({ i32*, {}* }* %62, { i32*, {}* }* %63)
  call void @rplacd({ i32*, {}* }* %62, { i32*, {}* }* %64)
  %65 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 29)
  %66 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  %67 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 30)
  call void @rplaca({ i32*, {}* }* %65, { i32*, {}* }* %66)
  call void @rplacd({ i32*, {}* }* %65, { i32*, {}* }* %67)
  %68 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 19)
  %69 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 20)
  %70 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 27)
  call void @rplaca({ i32*, {}* }* %68, { i32*, {}* }* %69)
  call void @rplacd({ i32*, {}* }* %68, { i32*, {}* }* %70)
  %71 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 13)
  %72 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 14)
  %73 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 15)
  call void @rplaca({ i32*, {}* }* %71, { i32*, {}* }* %72)
  call void @rplacd({ i32*, {}* }* %71, { i32*, {}* }* %73)
  %74 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 30)
  %75 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 14)
  %76 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 31)
  call void @rplaca({ i32*, {}* }* %74, { i32*, {}* }* %75)
  call void @rplacd({ i32*, {}* }* %74, { i32*, {}* }* %76)
  %77 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 20)
  %78 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 7)
  %79 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 21)
  call void @rplaca({ i32*, {}* }* %77, { i32*, {}* }* %78)
  call void @rplacd({ i32*, {}* }* %77, { i32*, {}* }* %79)
  %80 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 31)
  %81 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  %82 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 32)
  call void @rplaca({ i32*, {}* }* %80, { i32*, {}* }* %81)
  call void @rplacd({ i32*, {}* }* %80, { i32*, {}* }* %82)
  %83 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 15)
  %84 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  %85 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 16)
  call void @rplaca({ i32*, {}* }* %83, { i32*, {}* }* %84)
  call void @rplacd({ i32*, {}* }* %83, { i32*, {}* }* %85)
  %86 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 21)
  %87 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  %88 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 22)
  call void @rplaca({ i32*, {}* }* %86, { i32*, {}* }* %87)
  call void @rplacd({ i32*, {}* }* %86, { i32*, {}* }* %88)
  %89 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 32)
  %90 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 33)
  %91 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 34)
  call void @rplaca({ i32*, {}* }* %89, { i32*, {}* }* %90)
  call void @rplacd({ i32*, {}* }* %89, { i32*, {}* }* %91)
  %92 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 16)
  %93 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 17)
  %94 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 18)
  call void @rplaca({ i32*, {}* }* %92, { i32*, {}* }* %93)
  call void @rplacd({ i32*, {}* }* %92, { i32*, {}* }* %94)
  %95 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 22)
  %96 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 14)
  %97 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 23)
  call void @rplaca({ i32*, {}* }* %95, { i32*, {}* }* %96)
  call void @rplacd({ i32*, {}* }* %95, { i32*, {}* }* %97)
  %98 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 34)
  %99 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  %100 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  call void @rplaca({ i32*, {}* }* %98, { i32*, {}* }* %99)
  call void @rplacd({ i32*, {}* }* %98, { i32*, {}* }* %100)
  %101 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 18)
  %102 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  %103 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  call void @rplaca({ i32*, {}* }* %101, { i32*, {}* }* %102)
  call void @rplacd({ i32*, {}* }* %101, { i32*, {}* }* %103)
  %104 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 23)
  %105 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  %106 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 24)
  call void @rplaca({ i32*, {}* }* %104, { i32*, {}* }* %105)
  call void @rplacd({ i32*, {}* }* %104, { i32*, {}* }* %106)
  %107 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 35)
  call void @internSymbol_tsp({ i32*, {}* }* %107, i8* getelementptr inbounds ([9 x i8]* @":::symbol-name-COPY-XXX", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %108 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 36)
  call void @ltv_makeCons({ i32*, {}* }* %108)
  %109 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 37)
  call void @internSymbol_tsp({ i32*, {}* }* %109, i8* getelementptr inbounds ([9 x i8]* @":::symbol-name-MAKE-XXX", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %110 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 36)
  %111 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 37)
  %112 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  call void @rplaca({ i32*, {}* }* %110, { i32*, {}* }* %111)
  call void @rplacd({ i32*, {}* }* %110, { i32*, {}* }* %112)
  %113 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 38)
  call void @makeFixnum({ i32*, {}* }* %113, i32 3)
  %114 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 39)
  call void @internSymbol_tsp({ i32*, {}* }* %114, i8* getelementptr inbounds ([6 x i8]* @":::symbol-name-XXX-P", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %115 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  call void @internSymbol_symsp({ i32*, i32* }* %115, i8* getelementptr inbounds ([17 x i8]* @":::symbol-name-DEFINE-STRUCTURE", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %116 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 40)
  call void @internSymbol_tsp({ i32*, {}* }* %116, i8* getelementptr inbounds ([17 x i8]* @":::symbol-name-ALLOW-OTHER-KEYS", i64 0, i64 0), i8* getelementptr inbounds ([8 x i8]* @":::package-name-KEYWORD", i64 0, i64 0))
  %117 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 41)
  call void @internSymbol_tsp({ i32*, {}* }* %117, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-A", i64 0, i64 0), i8* getelementptr inbounds ([8 x i8]* @":::package-name-KEYWORD", i64 0, i64 0))
  %118 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 42)
  call void @internSymbol_tsp({ i32*, {}* }* %118, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-B", i64 0, i64 0), i8* getelementptr inbounds ([8 x i8]* @":::package-name-KEYWORD", i64 0, i64 0))
  %119 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 43)
  call void @internSymbol_tsp({ i32*, {}* }* %119, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-C", i64 0, i64 0), i8* getelementptr inbounds ([8 x i8]* @":::package-name-KEYWORD", i64 0, i64 0))
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %array-element-type, { i32*, {}* }** @load-time-value-vector, i32 14)
  %120 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 44)
  call void @ltv_makeArrayObjects({ i32*, {}* }* %120, { i32*, {}* }* %array-element-type, i32 1, i32* getelementptr inbounds ([1 x i32]* @constant-array, i64 0, i64 0))
  %121 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 45)
  call void @makeSymbol_tsp({ i32*, {}* }* %121, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-A", i64 0, i64 0))
  %122 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 46)
  call void @makeSymbol_tsp({ i32*, {}* }* %122, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-B", i64 0, i64 0))
  %123 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 47)
  call void @makeSymbol_tsp({ i32*, {}* }* %123, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-C", i64 0, i64 0))
  %124 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 44)
  call void @ltv_initializeArrayObjectsRowMajorArefOrder({ i32*, {}* }* %124, { i32*, {}* }** @load-time-value-vector, i32* getelementptr inbounds ([3 x i32]* @constant-array1, i64 0, i64 0))
  %125 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 1)
  call void @internSymbol_symsp({ i32*, i32* }* %125, i8* getelementptr inbounds ([11 x i8]* @":::symbol-name-FIND-CLASS", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  %126 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 2)
  call void @internSymbol_symsp({ i32*, i32* }* %126, i8* getelementptr inbounds ([15 x i8]* @":::symbol-name-MAKE-STRUCTURE", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %127 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 3)
  call void @internSymbol_symsp({ i32*, i32* }* %127, i8* getelementptr inbounds ([6 x i8]* @":::symbol-name-*FSET", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %128 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 49)
  call void @internSymbol_tsp({ i32*, {}* }* %128, i8* getelementptr inbounds ([4 x i8]* @":::symbol-name-*A*", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %129 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  call void @internSymbol_symsp({ i32*, i32* }* %129, i8* getelementptr inbounds ([14 x i8]* @":::symbol-name-*MAKE-SPECIAL", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  %130 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 5)
  call void @internSymbol_symsp({ i32*, i32* }* %130, i8* getelementptr inbounds ([7 x i8]* @":::symbol-name-BOUNDP", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  %131 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 6)
  call void @internSymbol_symsp({ i32*, i32* }* %131, i8* getelementptr inbounds ([4 x i8]* @":::symbol-name-NOT", i64 0, i64 0), i8* getelementptr inbounds ([12 x i8]* @":::package-name-COMMON-LISP", i64 0, i64 0))
  %132 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 7)
  call void @internSymbol_symsp({ i32*, i32* }* %132, i8* getelementptr inbounds ([4 x i8]* @":::symbol-name-*A*", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  %133 = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 8)
  call void @internSymbol_symsp({ i32*, i32* }* %133, i8* getelementptr inbounds ([9 x i8]* @":::symbol-name-MAKE-XXX", i64 0, i64 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i64 0, i64 0))
  ret void
}

define internal void @repl({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %temp = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp)
  %call-args = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args)
  %temp1 = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp1)
  %call-args2 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args2)
  call void @trace_setLineNumberColumnForIHSTop(i32 2, i32 15)
  call void @makeValueFrame({ i32*, {}* }* %call-args, i32 15, i32 2000013)
  %0 = call { i32*, {}* }* @activationFrameNil()
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args, { i32*, {}* }* %0)
  %call-args-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 0)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-0, { i32*, {}* }** @load-time-value-vector, i32 2)
  %call-args-ref-1 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 1)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-1, { i32*, {}* }** @load-time-value-vector, i32 3)
  %call-args-ref-2 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 2)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-2, { i32*, {}* }** @load-time-value-vector, i32 0)
  %call-args-ref-3 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 3)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-3, { i32*, {}* }** @load-time-value-vector, i32 0)
  %call-args-ref-4 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 4)
  call void @sp_getLoadTimeValue({ i32*, {}* }* %call-args-ref-4, { i32*, {}* }** @load-time-value-vector, i32 4)
  %call-args-ref-5 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 5)
  call void @sp_getLoadTimeValue({ i32*, {}* }* %call-args-ref-5, { i32*, {}* }** @load-time-value-vector, i32 10)
  %call-args-ref-6 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 6)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-6, { i32*, {}* }** @load-time-value-vector, i32 35)
  %call-args-ref-7 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 7)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-7, { i32*, {}* }** @load-time-value-vector, i32 0)
  %call-args-ref-8 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 8)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-8, { i32*, {}* }** @load-time-value-vector, i32 0)
  %call-args-ref-9 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 9)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-9, { i32*, {}* }** @load-time-value-vector, i32 0)
  %call-args-ref-10 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 10)
  call void @sp_getLoadTimeValue({ i32*, {}* }* %call-args-ref-10, { i32*, {}* }** @load-time-value-vector, i32 36)
  %call-args-ref-11 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 11)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-11, { i32*, {}* }** @load-time-value-vector, i32 38)
  %call-args-ref-12 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 12)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-12, { i32*, {}* }** @load-time-value-vector, i32 0)
  %call-args-ref-13 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 13)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-13, { i32*, {}* }** @load-time-value-vector, i32 0)
  %call-args-ref-14 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 14)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-14, { i32*, {}* }** @load-time-value-vector, i32 39)
  call void @singleStepCallback()
  %"SYMBOL->DEFINE-STRUCTURE" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %temp, { i32*, i32* }* %"SYMBOL->DEFINE-STRUCTURE", { i32*, {}* }* %call-args)
          to label %normal-dest unwind label %func-cleanup-landing-pad

normal-dest:                                      ; preds = %entry
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %temp, { i32*, {}* }** @load-time-value-vector, i32 0)
  call void @makeValueFrame({ i32*, {}* }* %call-args2, i32 3, i32 2000014)
  %1 = call { i32*, {}* }* @activationFrameNil()
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args2, { i32*, {}* }* %1)
  %call-args2-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args2, i32 0)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args2-ref-0, { i32*, {}* }** @load-time-value-vector, i32 37)
  %call-args2-ref-1 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args2, i32 1)
  %"SYMBOL->nil" = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 0)
  %"SYMBOL->MAKE-XXX" = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 37)
  %2 = call { i32*, {}* }* @activationFrameNil()
  invoke void @sp_makeCompiledFunction({ i32*, {}* }* %call-args2-ref-1, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @MAKE-XXX, i8* getelementptr inbounds ([62 x i8]* @":::global-str-/Users/meister/Development/cando/brcl/src/tests/core/ltv2.lsp", i64 0, i64 0), { i32*, {}* }* %"SYMBOL->MAKE-XXX", { i32*, {}* }* %"SYMBOL->nil", { i32*, {}* }* %2)
          to label %normal-dest3 unwind label %func-cleanup-landing-pad

normal-dest3:                                     ; preds = %normal-dest
  %call-args2-ref-2 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args2, i32 2)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args2-ref-2, { i32*, {}* }** @load-time-value-vector, i32 0)
  call void @singleStepCallback()
  %"SYMBOL->*FSET" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 3)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %temp1, { i32*, i32* }* %"SYMBOL->*FSET", { i32*, {}* }* %call-args2)
          to label %normal-dest4 unwind label %func-cleanup-landing-pad

normal-dest4:                                     ; preds = %normal-dest3
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %temp1, { i32*, {}* }** @load-time-value-vector, i32 0)
  call void @mv_copyLoadTimeValue({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }** @load-time-value-vector, i32 37)
  ret void

func-cleanup-landing-pad:                         ; preds = %normal-dest3, %normal-dest, %entry
  %3 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %4 = extractvalue { i8*, i32 } %3, 0
  store i8* %4, i8** %exn.slot, align 8
  %5 = extractvalue { i8*, i32 } %3, 1
  store i32 %5, i32* %ehselector.slot, align 4
  resume { i8*, i32 } %3
}

define internal void @MAKE-XXX({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %lambda-args-14- = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %lambda-args-14-)
  %i8- = alloca i8, align 1
  store i8 0, i8* %i8-, align 1
  %i8-1 = alloca i8, align 1
  store i8 0, i8* %i8-1, align 1
  %i8-2 = alloca i8, align 1
  store i8 0, i8* %i8-2, align 1
  %temp = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp)
  %call-args = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args)
  %0 = call { i32*, {}* }* @activationFrameParentRef({ i32*, {}* }* %activation-frame-ptr)
  call void @makeValueFrame({ i32*, {}* }* %lambda-args-14-, i32 3, i32 2000015)
  call void @setParentOfActivationFrame({ i32*, {}* }* %lambda-args-14-, { i32*, {}* }* %0)
  %1 = call i32 @activationFrameSize({ i32*, {}* }* %activation-frame-ptr)
  %enough-args = icmp slt i32 %1, 0
  br i1 %enough-args, label %error, label %continue

error:                                            ; preds = %entry
  invoke void @throwNotEnoughArgumentsException(i8* getelementptr inbounds ([9 x i8]* @":::global-str-MAKE-XXX", i64 0, i64 0), { i32*, {}* }* %activation-frame-ptr, i32 %1, i32 0)
          to label %normal-dest unwind label %func-cleanup-landing-pad

normal-dest:                                      ; preds = %error
  unreachable

continue:                                         ; preds = %entry
  %"SYMBOL->:ALLOW-OTHER-KEYS" = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 40)
  %2 = icmp sgt i32 %1, 0
  br i1 %2, label %loop-kw-args, label %kw-exit-block

loop-kw-args:                                     ; preds = %advance-arg-idx-block, %continue
  %phi-saw-aok = phi i32 [ 0, %continue ], [ %phi-this-was-aok, %advance-arg-idx-block ]
  %phi-reg-arg-idx = phi i32 [ 0, %continue ], [ %23, %advance-arg-idx-block ]
  %phi-bad-kw-idx = phi i32 [ -1, %continue ], [ %phi.aok-bad-good.bad-kw-idx, %advance-arg-idx-block ]
  %3 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %activation-frame-ptr, i32 %phi-reg-arg-idx)
  invoke void @kw_throwIfNotKeyword({ i32*, {}* }* %3)
          to label %normal-dest3 unwind label %func-cleanup-landing-pad

normal-dest3:                                     ; preds = %loop-kw-args
  %4 = call i32 @compareTsp({ i32*, {}* }* %"SYMBOL->:ALLOW-OTHER-KEYS", { i32*, {}* }* %3)
  %5 = and i32 %4, 1
  %trunc = icmp eq i32 %5, 0
  br i1 %trunc, label %possible-kw-block, label %aok-block

aok-block:                                        ; preds = %normal-dest3
  %6 = call i32 @kw_allowOtherKeywords(i32 %phi-saw-aok, { i32*, {}* }* %activation-frame-ptr, i32 %phi-reg-arg-idx)
  br label %advance-arg-idx-block

possible-kw-block:                                ; preds = %normal-dest3
  %"SYMBOL->:A" = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 41)
  %7 = call i32 @matchKeywordOnce({ i32*, {}* }* %"SYMBOL->:A", { i32*, {}* }* %3, i8* %i8-)
  %8 = and i32 %7, 1
  %trunc4 = icmp eq i32 %8, 0
  br i1 %trunc4, label %next-kw-block, label %match-kw-block

match-kw-block:                                   ; preds = %possible-kw-block
  %9 = or i32 %phi-reg-arg-idx, 1
  %10 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %activation-frame-ptr, i32 %9)
  %11 = call { i32*, {}* }* @lexicalValueReference(i32 0, i32 0, { i32*, {}* }* %lambda-args-14-)
  call void @sp_copyTsp({ i32*, {}* }* %11, { i32*, {}* }* %10)
  store i8 1, i8* %i8-, align 1
  br label %advance-arg-idx-block

next-kw-block:                                    ; preds = %possible-kw-block
  %"SYMBOL->:B" = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 42)
  %12 = call i32 @matchKeywordOnce({ i32*, {}* }* %"SYMBOL->:B", { i32*, {}* }* %3, i8* %i8-1)
  %13 = and i32 %12, 1
  %trunc5 = icmp eq i32 %13, 0
  br i1 %trunc5, label %next-kw-block7, label %match-kw-block6

match-kw-block6:                                  ; preds = %next-kw-block
  %14 = or i32 %phi-reg-arg-idx, 1
  %15 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %activation-frame-ptr, i32 %14)
  %16 = call { i32*, {}* }* @lexicalValueReference(i32 0, i32 1, { i32*, {}* }* %lambda-args-14-)
  call void @sp_copyTsp({ i32*, {}* }* %16, { i32*, {}* }* %15)
  store i8 1, i8* %i8-1, align 1
  br label %advance-arg-idx-block

next-kw-block7:                                   ; preds = %next-kw-block
  %"SYMBOL->:C" = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 43)
  %17 = call i32 @matchKeywordOnce({ i32*, {}* }* %"SYMBOL->:C", { i32*, {}* }* %3, i8* %i8-2)
  %18 = and i32 %17, 1
  %trunc8 = icmp eq i32 %18, 0
  br i1 %trunc8, label %next-kw-block10, label %match-kw-block9

match-kw-block9:                                  ; preds = %next-kw-block7
  %19 = or i32 %phi-reg-arg-idx, 1
  %20 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %activation-frame-ptr, i32 %19)
  %21 = call { i32*, {}* }* @lexicalValueReference(i32 0, i32 2, { i32*, {}* }* %lambda-args-14-)
  call void @sp_copyTsp({ i32*, {}* }* %21, { i32*, {}* }* %20)
  store i8 1, i8* %i8-2, align 1
  br label %advance-arg-idx-block

next-kw-block10:                                  ; preds = %next-kw-block7
  %22 = call i32 @kw_trackFirstUnexpectedKeyword(i32 %phi-bad-kw-idx, i32 %phi-reg-arg-idx)
  br label %advance-arg-idx-block

advance-arg-idx-block:                            ; preds = %next-kw-block10, %match-kw-block9, %match-kw-block6, %match-kw-block, %aok-block
  %phi-this-was-aok = phi i32 [ %6, %aok-block ], [ %phi-saw-aok, %next-kw-block10 ], [ %phi-saw-aok, %match-kw-block9 ], [ %phi-saw-aok, %match-kw-block6 ], [ %phi-saw-aok, %match-kw-block ]
  %phi.aok-bad-good.bad-kw-idx = phi i32 [ %phi-bad-kw-idx, %aok-block ], [ %22, %next-kw-block10 ], [ %phi-bad-kw-idx, %match-kw-block9 ], [ %phi-bad-kw-idx, %match-kw-block6 ], [ %phi-bad-kw-idx, %match-kw-block ]
  %23 = add i32 %phi-reg-arg-idx, 2
  %24 = icmp slt i32 %23, %1
  br i1 %24, label %loop-kw-args, label %loop-cont

loop-cont:                                        ; preds = %advance-arg-idx-block
  invoke void @kw_throwIfBadKeywordArgument(i32 %phi-this-was-aok, i32 %phi.aok-bad-good.bad-kw-idx, { i32*, {}* }* %activation-frame-ptr)
          to label %kw-exit-block unwind label %func-cleanup-landing-pad

kw-exit-block:                                    ; preds = %loop-cont, %continue
  %phi-arg-idx-final = phi i32 [ 0, %continue ], [ %23, %loop-cont ]
  %25 = load i8* %i8-, align 1
  %26 = icmp eq i8 %25, 0
  br i1 %26, label %init-default-kw-block, label %next-kw-block12

init-default-kw-block:                            ; preds = %kw-exit-block
  %27 = call { i32*, {}* }* @lexicalValueReference(i32 0, i32 0, { i32*, {}* }* %lambda-args-14-)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %27, { i32*, {}* }** @load-time-value-vector, i32 0)
  br label %next-kw-block12

next-kw-block12:                                  ; preds = %init-default-kw-block, %kw-exit-block
  %28 = load i8* %i8-1, align 1
  %29 = icmp eq i8 %28, 0
  br i1 %29, label %init-default-kw-block13, label %next-kw-block14

init-default-kw-block13:                          ; preds = %next-kw-block12
  %30 = call { i32*, {}* }* @lexicalValueReference(i32 0, i32 1, { i32*, {}* }* %lambda-args-14-)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %30, { i32*, {}* }** @load-time-value-vector, i32 0)
  br label %next-kw-block14

next-kw-block14:                                  ; preds = %init-default-kw-block13, %next-kw-block12
  %31 = load i8* %i8-2, align 1
  %32 = icmp eq i8 %31, 0
  br i1 %32, label %init-default-kw-block15, label %next-kw-block16

init-default-kw-block15:                          ; preds = %next-kw-block14
  %33 = call { i32*, {}* }* @lexicalValueReference(i32 0, i32 2, { i32*, {}* }* %lambda-args-14-)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %33, { i32*, {}* }** @load-time-value-vector, i32 0)
  br label %next-kw-block16

next-kw-block16:                                  ; preds = %init-default-kw-block15, %next-kw-block14
  invoke void @throwIfExcessKeywordArguments(i8* getelementptr inbounds ([9 x i8]* @":::global-str-MAKE-XXX", i64 0, i64 0), { i32*, {}* }* %activation-frame-ptr, i32 %phi-arg-idx-final)
          to label %normal-dest17 unwind label %func-cleanup-landing-pad

normal-dest17:                                    ; preds = %next-kw-block16
  %value = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 44)
  call void @attachDebuggingInfoToValueFrame({ i32*, {}* }* %lambda-args-14-, { i32*, {}* }* %value)
  call void @trace_setActivationFrameForIHSTop({ i32*, {}* }* %lambda-args-14-)
  call void @makeValueFrame({ i32*, {}* }* %call-args, i32 4, i32 2000016)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args, { i32*, {}* }* %lambda-args-14-)
  %call-args-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 0)
  call void @sp_getLoadTimeValue({ i32*, {}* }* %call-args-ref-0, { i32*, {}* }** @load-time-value-vector, i32 48)
  %call-args-ref-1 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 1)
  call void @sp_lexicalValueRead({ i32*, {}* }* %call-args-ref-1, i32 0, i32 0, { i32*, {}* }* %lambda-args-14-)
  %call-args-ref-2 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 2)
  call void @sp_lexicalValueRead({ i32*, {}* }* %call-args-ref-2, i32 0, i32 1, { i32*, {}* }* %lambda-args-14-)
  %call-args-ref-3 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 3)
  call void @sp_lexicalValueRead({ i32*, {}* }* %call-args-ref-3, i32 0, i32 2, { i32*, {}* }* %lambda-args-14-)
  call void @singleStepCallback()
  %"SYMBOL->MAKE-STRUCTURE" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 2)
  invoke void @mv_invokePossibleMultipleValueSymbolFunction({ i32*, {}*, i32 }* %result-ptr, { i32*, i32* }* %"SYMBOL->MAKE-STRUCTURE", { i32*, {}* }* %call-args)
          to label %"(TRY-0).try-cont" unwind label %"(TRY-0).landing-pad"

"(TRY-0).landing-pad":                            ; preds = %normal-dest17
  %34 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
          catch i8* @_ZTIN4core10ReturnFromE
  %35 = extractvalue { i8*, i32 } %34, 0
  store i8* %35, i8** %exn.slot, align 8
  %36 = extractvalue { i8*, i32 } %34, 1
  store i32 %36, i32* %ehselector.slot, align 4
  %37 = call i32 @llvm.eh.typeid.for(i8* @_ZTIN4core10ReturnFromE)
  %38 = icmp eq i32 %36, %37
  br i1 %38, label %"(TRY-0).handler-block15750", label %func-ehcleanup

"(TRY-0).handler-block15750":                     ; preds = %"(TRY-0).landing-pad"
  %39 = call i8* @__cxa_begin_catch(i8* %35)
  invoke void @mv_blockHandleReturnFrom({ i32*, {}*, i32 }* %result-ptr, i8* %39)
          to label %"(TRY-0).normal-dest19" unwind label %"(TRY-0).landing-pad22"

"(TRY-0).normal-dest19":                          ; preds = %"(TRY-0).handler-block15750"
  invoke void @__cxa_end_catch()
          to label %"(TRY-0).try-cont" unwind label %"(TRY-0).landing-pad22"

"(TRY-0).try-cont":                               ; preds = %"(TRY-0).normal-dest19", %normal-dest17
  ret void

"(TRY-0).landing-pad22":                          ; preds = %"(TRY-0).normal-dest19", %"(TRY-0).handler-block15750"
  %40 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %41 = extractvalue { i8*, i32 } %40, 0
  store i8* %41, i8** %exn.slot, align 8
  %42 = extractvalue { i8*, i32 } %40, 1
  store i32 %42, i32* %ehselector.slot, align 4
  br label %func-ehcleanup

func-cleanup-landing-pad:                         ; preds = %next-kw-block16, %loop-cont, %loop-kw-args, %error
  %43 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %44 = extractvalue { i8*, i32 } %43, 0
  store i8* %44, i8** %exn.slot, align 8
  %45 = extractvalue { i8*, i32 } %43, 1
  store i32 %45, i32* %ehselector.slot, align 4
  br label %func-ehcleanup

func-ehcleanup:                                   ; preds = %func-cleanup-landing-pad, %"(TRY-0).landing-pad22", %"(TRY-0).landing-pad"
  %sel = phi i32 [ %45, %func-cleanup-landing-pad ], [ %42, %"(TRY-0).landing-pad22" ], [ %36, %"(TRY-0).landing-pad" ]
  %exn7 = phi i8* [ %44, %func-cleanup-landing-pad ], [ %41, %"(TRY-0).landing-pad22" ], [ %35, %"(TRY-0).landing-pad" ]
  %lpad.val = insertvalue { i8*, i32 } undef, i8* %exn7, 0
  %lpad.val8 = insertvalue { i8*, i32 } %lpad.val, i32 %sel, 1
  resume { i8*, i32 } %lpad.val8
}

define internal void @load-time-value-func({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %activation-frame-ptr) {
"(TRY-0).entry":
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %call-args = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args)
  %0 = call { i32*, {}* }* @loadTimeValueReference({ i32*, {}* }** @load-time-value-vector, i32 48)
  call void @makeValueFrame({ i32*, {}* }* %call-args, i32 1, i32 2000017)
  %1 = call { i32*, {}* }* @activationFrameNil()
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args, { i32*, {}* }* %1)
  %call-args-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 0)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-0, { i32*, {}* }** @load-time-value-vector, i32 2)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:FIND-CLASS" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 1)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %0, { i32*, i32* }* %"SYMBOL->COMMON-LISP:FIND-CLASS", { i32*, {}* }* %call-args)
          to label %"(TRY-0).normal-dest" unwind label %"(TRY-0).func-cleanup-landing-pad"

"(TRY-0).normal-dest":                            ; preds = %"(TRY-0).entry"
  call void @mv_copyTsp({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }* %0)
  ret void

"(TRY-0).func-cleanup-landing-pad":               ; preds = %"(TRY-0).entry"
  %2 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %3 = extractvalue { i8*, i32 } %2, 0
  store i8* %3, i8** %exn.slot, align 8
  %4 = extractvalue { i8*, i32 } %2, 1
  store i32 %4, i32* %ehselector.slot, align 4
  resume { i8*, i32 } %2
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
  %locally-env = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %locally-env)
  %temp = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp)
  %call-args = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args)
  %if-cond-tsp = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %if-cond-tsp)
  %call-args1 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args1)
  %call-args2 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args2)
  %temp5 = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %temp5)
  %tsetq = alloca { i32*, {}* }, align 8
  call void @newTsp({ i32*, {}* }* %tsetq)
  %call-args6 = alloca { i32*, {}* }, align 8
  call void @newAFsp({ i32*, {}* }* %call-args6)
  call void @makeValueFrame({ i32*, {}* }* %locally-env, i32 0, i32 2000018)
  %0 = call { i32*, {}* }* @activationFrameNil()
  call void @setParentOfActivationFrame({ i32*, {}* }* %locally-env, { i32*, {}* }* %0)
  call void @makeValueFrame({ i32*, {}* }* %call-args, i32 1, i32 2000019)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args, { i32*, {}* }* %locally-env)
  %call-args-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args, i32 0)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args-ref-0, { i32*, {}* }** @load-time-value-vector, i32 49)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:*MAKE-SPECIAL" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 4)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %temp, { i32*, i32* }* %"SYMBOL->COMMON-LISP:*MAKE-SPECIAL", { i32*, {}* }* %call-args)
          to label %normal-dest unwind label %func-cleanup-landing-pad

normal-dest:                                      ; preds = %entry
  call void @makeValueFrame({ i32*, {}* }* %call-args1, i32 1, i32 2000020)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args1, { i32*, {}* }* %locally-env)
  %call-args1-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args1, i32 0)
  call void @makeValueFrame({ i32*, {}* }* %call-args2, i32 1, i32 2000021)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args2, { i32*, {}* }* %locally-env)
  %call-args2-ref-0 = call { i32*, {}* }* @valueFrameReference({ i32*, {}* }* %call-args2, i32 0)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %call-args2-ref-0, { i32*, {}* }** @load-time-value-vector, i32 49)
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:BOUNDP" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 5)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %call-args1-ref-0, { i32*, i32* }* %"SYMBOL->COMMON-LISP:BOUNDP", { i32*, {}* }* %call-args2)
          to label %normal-dest3 unwind label %func-cleanup-landing-pad

normal-dest3:                                     ; preds = %normal-dest
  call void @singleStepCallback()
  %"SYMBOL->COMMON-LISP:NOT" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 6)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %if-cond-tsp, { i32*, i32* }* %"SYMBOL->COMMON-LISP:NOT", { i32*, {}* }* %call-args1)
          to label %normal-dest4 unwind label %func-cleanup-landing-pad

normal-dest4:                                     ; preds = %normal-dest3
  %1 = call i32 @isTrueTsp({ i32*, {}* }* %if-cond-tsp)
  %ifcond = icmp eq i32 %1, 1
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %normal-dest4
  %"SYMBOL->*A*" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 7)
  %"<special-var:*A*>" = call { i32*, {}* }* @symbolValueReference({ i32*, i32* }* %"SYMBOL->*A*")
  call void @trace_setLineNumberColumnForIHSTop(i32 4, i32 13)
  call void @makeValueFrame({ i32*, {}* }* %call-args6, i32 0, i32 2000022)
  call void @setParentOfActivationFrame({ i32*, {}* }* %call-args6, { i32*, {}* }* %locally-env)
  call void @singleStepCallback()
  %"SYMBOL->MAKE-XXX" = call { i32*, i32* }* @loadTimeSymbolReference({ i32*, {}* }** @load-time-value-vector, i32 8)
  invoke void @sp_invokePossibleMultipleValueSymbolFunction({ i32*, {}* }* %tsetq, { i32*, i32* }* %"SYMBOL->MAKE-XXX", { i32*, {}* }* %call-args6)
          to label %normal-dest7 unwind label %func-cleanup-landing-pad

normal-dest7:                                     ; preds = %then
  call void @sp_copyTsp({ i32*, {}* }* %"<special-var:*A*>", { i32*, {}* }* %tsetq)
  call void @sp_copyTsp({ i32*, {}* }* %temp, { i32*, {}* }* %tsetq)
  br label %ifcont

else:                                             ; preds = %normal-dest4
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %temp, { i32*, {}* }** @load-time-value-vector, i32 0)
  br label %ifcont

ifcont:                                           ; preds = %else, %normal-dest7
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %temp, { i32*, {}* }** @load-time-value-vector, i32 0)
  call void @sp_copyLoadTimeValue({ i32*, {}* }* %temp, { i32*, {}* }** @load-time-value-vector, i32 0)
  call void @mv_copyLoadTimeValue({ i32*, {}*, i32 }* %result-ptr, { i32*, {}* }** @load-time-value-vector, i32 49)
  ret void

func-cleanup-landing-pad:                         ; preds = %then, %normal-dest3, %normal-dest, %entry
  %2 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %3 = extractvalue { i8*, i32 } %2, 0
  store i8* %3, i8** %exn.slot, align 8
  %4 = extractvalue { i8*, i32 } %2, 1
  store i32 %4, i32* %ehselector.slot, align 4
  resume { i8*, i32 } %2
}

define void @___user_ltv2() {
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
  invoke void @invokeLlvmFunction({ i32*, {}*, i32 }* %result, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @load-time-value-func, { i32*, {}* }* %0)
          to label %normal-dest1 unwind label %func-cleanup-landing-pad

normal-dest1:                                     ; preds = %normal-dest
  %1 = call { i32*, {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ i32*, {}*, i32 }* %result, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl, { i32*, {}* }* %1)
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
  resume { i8*, i32 } %4
}

attributes #0 = { nounwind }
attributes #1 = { noreturn }
attributes #2 = { nounwind readnone }
attributes #3 = { noreturn nounwind }

!llvm.dbg.cu = !{!0}

!0 = metadata !{i32 786449, metadata !1, i32 32768, metadata !"brcl Common Lisp compiler", i1 false, metadata !"-v", i32 1, metadata !2, metadata !2, metadata !3, metadata !2, metadata !2, metadata !"split-name.log"} ; [ DW_TAG_compile_unit ] [/Users/meister/Development/cando/brcl/src/tests/core/ltv2.lsp] [DW_LANG_lo_user]
!1 = metadata !{metadata !"ltv2.lsp", metadata !"/Users/meister/Development/cando/brcl/src/tests/core"}
!2 = metadata !{i32 0}
!3 = metadata !{metadata !4, metadata !9, metadata !11, metadata !14, metadata !15, metadata !16}
!4 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl", metadata !"repl", metadata !"repl", i32 2, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl, null, null, metadata !2, i32 2} ; [ DW_TAG_subprogram ] [line 2] [def] [repl]
!5 = metadata !{i32 786473, metadata !1}          ; [ DW_TAG_file_type ] [/Users/meister/Development/cando/brcl/src/tests/core/ltv2.lsp]
!6 = metadata !{i32 786453, i32 0, i32 0, metadata !"", i32 0, i64 0, i64 0, i64 0, i32 0, null, metadata !7, i32 0, i32 0} ; [ DW_TAG_subroutine_type ] [line 0, size 0, align 0, offset 0] [from ]
!7 = metadata !{metadata !8}
!8 = metadata !{i32 786468, null, null, metadata !"int", i32 0, i64 32, i64 32, i64 0, i32 0, i32 13} ; [ DW_TAG_base_type ] [int] [line 0, size 32, align 32, offset 0, enc DW_ATE_signed_fixed]
!9 = metadata !{i32 786478, metadata !1, metadata !10, metadata !"MAKE-XXX", metadata !"MAKE-XXX", metadata !"MAKE-XXX", i32 0, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @MAKE-XXX, null, null, metadata !2, i32 0} ; [ DW_TAG_subprogram ] [line 0] [def] [MAKE-XXX]
!10 = metadata !{i32 786443, metadata !1, metadata !4, i32 2, i32 15, i32 0} ; [ DW_TAG_lexical_block ] [/Users/meister/Development/cando/brcl/src/tests/core/ltv2.lsp]
!11 = metadata !{i32 786478, metadata !1, metadata !12, metadata !"load-time-value-func", metadata !"load-time-value-func", metadata !"load-time-value-func", i32 0, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @load-time-value-func, null, null, metadata !2, i32 0} ; [ DW_TAG_subprogram ] [line 0] [def] [load-time-value-func]
!12 = metadata !{i32 786443, metadata !1, metadata !13, i32 0, i32 0, i32 2} ; [ DW_TAG_lexical_block ] [/Users/meister/Development/cando/brcl/src/tests/core/ltv2.lsp]
!13 = metadata !{i32 786443, metadata !1, metadata !9, i32 0, i32 0, i32 1} ; [ DW_TAG_lexical_block ] [/Users/meister/Development/cando/brcl/src/tests/core/ltv2.lsp]
!14 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl2", metadata !"repl2", metadata !"repl2", i32 0, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl2, null, null, metadata !2, i32 0} ; [ DW_TAG_subprogram ] [line 0] [def] [repl2]
!15 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"repl3", metadata !"repl3", metadata !"repl3", i32 4, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ({ i32*, {}*, i32 }*, { i32*, {}* }*)* @repl3, null, null, metadata !2, i32 4} ; [ DW_TAG_subprogram ] [line 4] [def] [repl3]
!16 = metadata !{i32 786478, metadata !1, metadata !5, metadata !"___user_ltv2", metadata !"___user_ltv2", metadata !"___user_ltv2", i32 0, metadata !6, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, void ()* @___user_ltv2, null, null, metadata !2, i32 0} ; [ DW_TAG_subprogram ] [line 0] [def] [___user_ltv2]
