; ModuleID = 'testup.bc'
target triple = "x86_64-apple-macosx10.7.0"

@":::global-str-/Users/meister/Development/new_cando/brcl/src/asttooling/testup.lsp" = internal unnamed_addr constant [68 x i8] c"/Users/meister/Development/new_cando/brcl/src/asttooling/testup.lsp\00"
@load-time-value-vector = internal global { {}* }* null
@":::global-str-repl" = internal unnamed_addr constant [5 x i8] c"repl\00"
@":::symbol-name-T" = internal unnamed_addr constant [2 x i8] c"T\00"
@":::package-name-CL" = internal unnamed_addr constant [3 x i8] c"CL\00"
@constant-array = internal constant [1 x i32] [i32 1]
@":::symbol-name-N" = internal unnamed_addr constant [2 x i8] c"N\00"
@":::package-name-COMMON-LISP-USER" = internal unnamed_addr constant [17 x i8] c"COMMON-LISP-USER\00"
@constant-array1 = internal constant [1 x i32] [i32 4]
@":::symbol-name--" = internal unnamed_addr constant [2 x i8] c"-\00"
@_ZTIN4core10ReturnFromE = external constant i8
@":::symbol-name-ZEROP" = internal unnamed_addr constant [6 x i8] c"ZEROP\00"
@":::symbol-name-NOT" = internal unnamed_addr constant [4 x i8] c"NOT\00"
@":::str" = internal unnamed_addr constant [14 x i8] c"n is not zero\00"
@":::symbol-name-PRINT" = internal unnamed_addr constant [6 x i8] c"PRINT\00"
@":::global-str-__MAIN_testup" = internal unnamed_addr constant [14 x i8] c"__MAIN_testup\00"
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
  call void @getOrCreateLoadTimeValueArray({ {}* }** @load-time-value-vector, i8* getelementptr inbounds ([68 x i8]* @":::global-str-/Users/meister/Development/new_cando/brcl/src/asttooling/testup.lsp", i64 0, i64 0), i32 8, i32 4)
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
  call void @internSymbol_tsp({ {}* }* %5, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-N", i64 0, i64 0), i8* getelementptr inbounds ([17 x i8]* @":::package-name-COMMON-LISP-USER", i64 0, i64 0))
  %6 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 3)
  call void @ltv_initializeArrayObjectsRowMajorArefOrder({ {}* }* %6, { {}* }** @load-time-value-vector, i32* getelementptr inbounds ([1 x i32]* @constant-array1, i64 0, i64 0))
  %7 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 5)
  call void @makeFixnum({ {}* }* %7, i32 1)
  %8 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 6)
  call void @makeFixnum({ {}* }* %8, i32 42)
  %9 = call { i32* }* @loadTimeSymbolReference({ {}* }** @load-time-value-vector, i32 0)
  call void @internSymbol_symsp({ i32* }* %9, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name--", i64 0, i64 0), i8* getelementptr inbounds ([3 x i8]* @":::package-name-CL", i64 0, i64 0))
  %10 = call { i32* }* @loadTimeSymbolReference({ {}* }** @load-time-value-vector, i32 1)
  call void @internSymbol_symsp({ i32* }* %10, i8* getelementptr inbounds ([6 x i8]* @":::symbol-name-ZEROP", i64 0, i64 0), i8* getelementptr inbounds ([3 x i8]* @":::package-name-CL", i64 0, i64 0))
  %11 = call { i32* }* @loadTimeSymbolReference({ {}* }** @load-time-value-vector, i32 2)
  call void @internSymbol_symsp({ i32* }* %11, i8* getelementptr inbounds ([4 x i8]* @":::symbol-name-NOT", i64 0, i64 0), i8* getelementptr inbounds ([3 x i8]* @":::package-name-CL", i64 0, i64 0))
  %12 = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 7)
  call void @makeString({ {}* }* %12, i8* getelementptr inbounds ([14 x i8]* @":::str", i64 0, i64 0))
  %13 = call { i32* }* @loadTimeSymbolReference({ {}* }** @load-time-value-vector, i32 3)
  call void @internSymbol_symsp({ i32* }* %13, i8* getelementptr inbounds ([6 x i8]* @":::symbol-name-PRINT", i64 0, i64 0), i8* getelementptr inbounds ([3 x i8]* @":::package-name-CL", i64 0, i64 0))
  %14 = call { {}* }* @activationFrameNil()
  invoke void @invokeLlvmFunction({ {}*, i32 }* %0, void ({ {}*, i32 }*, { {}* }*, i32, { {}* }*)* @repl, { {}* }* %14)
          to label %normal-dest unwind label %func-cleanup-landing-pad

normal-dest:                                      ; preds = %entry
  call void @destructTsp({ {}* }* %array-element-type)
  call void @destructTmv({ {}*, i32 }* %0)
  ret void

func-cleanup-landing-pad:                         ; preds = %entry
  %15 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %16 = extractvalue { i8*, i32 } %15, 0
  store i8* %16, i8** %exn.slot, align 8
  %17 = extractvalue { i8*, i32 } %15, 1
  store i32 %17, i32* %ehselector.slot, align 4
  call void @destructTsp({ {}* }* %array-element-type)
  call void @destructTmv({ {}*, i32 }* %0)
  resume { i8*, i32 } %15
}

define internal void @repl({ {}*, i32 }* %result-ptr, { {}* }* %closed-af-ptr, i32 %num-varargs, { {}* }* %va-list) {
entry:
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %temp = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp)
  %LET = alloca { {}* }, align 8
  call void @newAFsp({ {}* }* %LET)
  %temp2 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp2)
  %temp-mv-result = alloca { {}*, i32 }, align 8
  call void @newTmv({ {}*, i32 }* %temp-mv-result)
  %unwind-protect-saved-values = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %unwind-protect-saved-values)
  %temp-mv-result4 = alloca { {}*, i32 }, align 8
  call void @newTmv({ {}*, i32 }* %temp-mv-result4)
  %temp5 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp5)
  %tsetq = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %tsetq)
  %0 = alloca [2 x { {}* }], align 8
  %.sub = getelementptr inbounds [2 x { {}* }]* %0, i64 0, i64 0
  call void @newTsp({ {}* }* %.sub)
  %gep6 = getelementptr inbounds [2 x { {}* }]* %0, i64 0, i64 1
  call void @newTsp({ {}* }* %gep6)
  %func = alloca { i32* }, align 8
  call void @newFunction_sp({ i32* }* %func)
  %temp9 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp9)
  %tsetq10 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %tsetq10)
  %1 = alloca [2 x { {}* }], align 8
  %.sub58 = getelementptr inbounds [2 x { {}* }]* %1, i64 0, i64 0
  call void @newTsp({ {}* }* %.sub58)
  %gep12 = getelementptr inbounds [2 x { {}* }]* %1, i64 0, i64 1
  call void @newTsp({ {}* }* %gep12)
  %func15 = alloca { i32* }, align 8
  call void @newFunction_sp({ i32* }* %func15)
  %if-cond-tsp = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %if-cond-tsp)
  %2 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %2)
  %3 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %3)
  %func23 = alloca { i32* }, align 8
  call void @newFunction_sp({ i32* }* %func23)
  %func26 = alloca { i32* }, align 8
  call void @newFunction_sp({ i32* }* %func26)
  %4 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %4)
  %func31 = alloca { i32* }, align 8
  call void @newFunction_sp({ i32* }* %func31)
  %5 = call { {}* }* @activationFrameNil()
  call void @trace_setActivationFrameForIHSTop({ {}* }* %5)
  call void @trace_setActivationFrameForIHSTop({ {}* }* %LET)
  call void @makeValueFrame({ {}* }* %LET, i32 1, i32 2000044)
  %6 = call { {}* }* @activationFrameNil()
  call void @setParentOfActivationFrame({ {}* }* %LET, { {}* }* %6)
  %value = call { {}* }* @loadTimeValueReference({ {}* }** @load-time-value-vector, i32 3)
  call void @attachDebuggingInfoToValueFrame({ {}* }* %LET, { {}* }* %value)
  %7 = call { {}* }* @lexicalValueReference(i32 0, i32 0, { {}* }* %LET)
  call void @sp_copyLoadTimeValue({ {}* }* %7, { {}* }** @load-time-value-vector, i32 5)
  call void @trace_setActivationFrameForIHSTop({ {}* }* %LET)
  call void @singleStepCallback()
  call void @mv_copyLoadTimeValue({ {}*, i32 }* %temp-mv-result4, { {}* }** @load-time-value-vector, i32 6)
  invoke void @throwReturnFrom(i32 0, { {}*, i32 }* %temp-mv-result4)
          to label %"(TRY-0).normal-dest" unwind label %"(TRY-0).landing-pad"

"(TRY-0).normal-dest":                            ; preds = %entry
  call void @unreachableError()
  unreachable

"(TRY-0).landing-pad":                            ; preds = %entry
  %8 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
          catch i8* @_ZTIN4core10ReturnFromE
  %9 = extractvalue { i8*, i32 } %8, 0
  store i8* %9, i8** %exn.slot, align 8
  %10 = extractvalue { i8*, i32 } %8, 1
  store i32 %10, i32* %ehselector.slot, align 4
  %11 = call { {}* }* @lexicalValueReference(i32 0, i32 0, { {}* }* %LET)
  call void @trace_setLineNumberColumnForIHSTop(i32 2, i32 72)
  call void @sp_lexicalValueRead({ {}* }* %.sub58, i32 0, i32 0, { {}* }* %LET)
  call void @sp_copyLoadTimeValue({ {}* }* %gep12, { {}* }** @load-time-value-vector, i32 5)
  %"SYMBOL->CL:-16" = call { i32* }* @loadTimeSymbolReference({ {}* }** @load-time-value-vector, i32 0)
  invoke void @va_symbolFunction({ i32* }* %func15, { i32* }* %"SYMBOL->CL:-16")
          to label %"(TRY-0).normal-dest17" unwind label %"(TRY-0).landing-pad35"

"(TRY-0).normal-dest17":                          ; preds = %"(TRY-0).landing-pad"
  invoke void @sp_FUNCALL({ {}* }* %tsetq10, { i32* }* %func15, i32 2, { {}* }* %.sub58)
          to label %"(TRY-0).normal-dest18" unwind label %"(TRY-0).landing-pad35"

"(TRY-0).normal-dest18":                          ; preds = %"(TRY-0).normal-dest17"
  call void @sp_copyTsp({ {}* }* %11, { {}* }* %tsetq10)
  call void @sp_copyTsp({ {}* }* %temp9, { {}* }* %tsetq10)
  call void @loadValues({ {}*, i32 }* %temp-mv-result, { {}* }* %unwind-protect-saved-values)
  call void @sp_copyTmvOrSlice({ {}* }* %temp2, { {}*, i32 }* %temp-mv-result)
  br label %"(TRY-0).dispatch-header40"

"(TRY-0).landing-pad35":                          ; preds = %"(TRY-0).normal-dest17", %"(TRY-0).landing-pad"
  %12 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
          catch i8* @_ZTIN4core10ReturnFromE
  %13 = extractvalue { i8*, i32 } %12, 0
  store i8* %13, i8** %exn.slot, align 8
  %14 = extractvalue { i8*, i32 } %12, 1
  store i32 %14, i32* %ehselector.slot, align 4
  br label %"(TRY-0).dispatch-header40"

"(TRY-0).dispatch-header40":                      ; preds = %"(TRY-0).landing-pad35", %"(TRY-0).normal-dest18"
  %exn62 = phi i8* [ %13, %"(TRY-0).landing-pad35" ], [ %9, %"(TRY-0).normal-dest18" ]
  %ehselector-slot60 = phi i32 [ %14, %"(TRY-0).landing-pad35" ], [ %10, %"(TRY-0).normal-dest18" ]
  %15 = call i32 @llvm.eh.typeid.for(i8* @_ZTIN4core10ReturnFromE)
  %16 = icmp eq i32 %ehselector-slot60, %15
  br i1 %16, label %"(TRY-0).handler-block13712", label %func-ehcleanup

"(TRY-0).handler-block13712":                     ; preds = %"(TRY-0).dispatch-header40"
  %17 = call i8* @__cxa_begin_catch(i8* %exn62)
  invoke void @mv_blockHandleReturnFrom({ {}*, i32 }* %result-ptr, i8* %17)
          to label %"(TRY-0).normal-dest41" unwind label %func-cleanup-landing-pad

"(TRY-0).normal-dest41":                          ; preds = %"(TRY-0).handler-block13712"
  invoke void @__cxa_end_catch()
          to label %"(TRY-0).try-cont43" unwind label %func-cleanup-landing-pad

"(TRY-0).try-cont43":                             ; preds = %"(TRY-0).normal-dest41"
  call void @destructFunction_sp({ i32* }* %func31)
  call void @destructTsp({ {}* }* %4)
  call void @destructFunction_sp({ i32* }* %func26)
  call void @destructFunction_sp({ i32* }* %func23)
  call void @destructTsp({ {}* }* %3)
  call void @destructTsp({ {}* }* %2)
  call void @destructTsp({ {}* }* %if-cond-tsp)
  call void @destructFunction_sp({ i32* }* %func15)
  call void @destructTsp({ {}* }* %.sub58)
  call void @destructTsp({ {}* }* %gep12)
  call void @destructTsp({ {}* }* %tsetq10)
  call void @destructTsp({ {}* }* %temp9)
  call void @destructFunction_sp({ i32* }* %func)
  call void @destructTsp({ {}* }* %.sub)
  call void @destructTsp({ {}* }* %gep6)
  call void @destructTsp({ {}* }* %tsetq)
  call void @destructTsp({ {}* }* %temp5)
  call void @destructTmv({ {}*, i32 }* %temp-mv-result4)
  call void @destructTsp({ {}* }* %unwind-protect-saved-values)
  call void @destructTmv({ {}*, i32 }* %temp-mv-result)
  call void @destructTsp({ {}* }* %temp2)
  call void @destructAFsp({ {}* }* %LET)
  call void @destructTsp({ {}* }* %temp)
  ret void

func-cleanup-landing-pad:                         ; preds = %"(TRY-0).normal-dest41", %"(TRY-0).handler-block13712"
  %18 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
  %19 = extractvalue { i8*, i32 } %18, 0
  store i8* %19, i8** %exn.slot, align 8
  %20 = extractvalue { i8*, i32 } %18, 1
  store i32 %20, i32* %ehselector.slot, align 4
  br label %func-ehcleanup

func-ehcleanup:                                   ; preds = %func-cleanup-landing-pad, %"(TRY-0).dispatch-header40"
  %sel = phi i32 [ %20, %func-cleanup-landing-pad ], [ %ehselector-slot60, %"(TRY-0).dispatch-header40" ]
  %exn7 = phi i8* [ %19, %func-cleanup-landing-pad ], [ %exn62, %"(TRY-0).dispatch-header40" ]
  call void @destructFunction_sp({ i32* }* %func31)
  call void @destructTsp({ {}* }* %4)
  call void @destructFunction_sp({ i32* }* %func26)
  call void @destructFunction_sp({ i32* }* %func23)
  call void @destructTsp({ {}* }* %3)
  call void @destructTsp({ {}* }* %2)
  call void @destructTsp({ {}* }* %if-cond-tsp)
  call void @destructFunction_sp({ i32* }* %func15)
  call void @destructTsp({ {}* }* %.sub58)
  call void @destructTsp({ {}* }* %gep12)
  call void @destructTsp({ {}* }* %tsetq10)
  call void @destructTsp({ {}* }* %temp9)
  call void @destructFunction_sp({ i32* }* %func)
  call void @destructTsp({ {}* }* %.sub)
  call void @destructTsp({ {}* }* %gep6)
  call void @destructTsp({ {}* }* %tsetq)
  call void @destructTsp({ {}* }* %temp5)
  call void @destructTmv({ {}*, i32 }* %temp-mv-result4)
  call void @destructTsp({ {}* }* %unwind-protect-saved-values)
  call void @destructTmv({ {}*, i32 }* %temp-mv-result)
  call void @destructTsp({ {}* }* %temp2)
  call void @destructAFsp({ {}* }* %LET)
  call void @destructTsp({ {}* }* %temp)
  %lpad.val = insertvalue { i8*, i32 } undef, i8* %exn7, 0
  %lpad.val8 = insertvalue { i8*, i32 } %lpad.val, i32 %sel, 1
  resume { i8*, i32 } %lpad.val8
}

define void @__MAIN_testup() {
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
