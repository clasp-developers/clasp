
"#<LLVM-SYS::FUNCTION 
define internal void @A({ {}*, i32 }* %result-ptr, { {}* }* %closed-af-ptr, i32 %num-varargs, { {}* }* %va-list) {
\"(TRY-0).entry\":
  %exn.slot = alloca i8*, align 8
  %ehselector.slot = alloca i32, align 4
  store i32 0, i32* %ehselector.slot, align 4
  %lambda-args-35- = alloca { {}* }, align 8
  call void @newAFsp({ {}* }* %lambda-args-35-)
  %temp = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp)
  %0 = alloca [3 x { {}* }], align 8
  %.sub202 = bitcast [3 x { {}* }]* %0 to { {}* }*
  call void @newTsp({ {}* }* %.sub202)
  %gep6 = getelementptr inbounds [3 x { {}* }]* %0, i64 0, i64 1
  call void @newTsp({ {}* }* %gep6)
  %gep7 = getelementptr inbounds [3 x { {}* }]* %0, i64 0, i64 2
  call void @newTsp({ {}* }* %gep7)
  %temp9 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp9)
  %LET = alloca { {}* }, align 8
  call void @newAFsp({ {}* }* %LET)
  %temp12 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp12)
  %temp-mv-result = alloca { {}*, i32 }, align 8
  call void @newTmv({ {}*, i32 }* %temp-mv-result)
  %unwind-protect-saved-values = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %unwind-protect-saved-values)
  %temp-mv-result14 = alloca { {}*, i32 }, align 8
  call void @newTmv({ {}*, i32 }* %temp-mv-result14)
  %unwind-protect-saved-values15 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %unwind-protect-saved-values15)
  %temp-mv-result18 = alloca { {}*, i32 }, align 8
  call void @newTmv({ {}*, i32 }* %temp-mv-result18)
  %temp20 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp20)
  %\"LET*\" = alloca { {}* }, align 8
  call void @newAFsp({ {}* }* %\"LET*\")
  %temp23 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp23)
  %tsetq = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %tsetq)
  %1 = alloca [2 x { {}* }], align 8
  %.sub184203 = bitcast [2 x { {}* }]* %1 to { {}* }*
  call void @newTsp({ {}* }* %.sub184203)
  %gep25 = getelementptr inbounds [2 x { {}* }]* %1, i64 0, i64 1
  call void @newTsp({ {}* }* %gep25)
  %func = alloca { i32* }, align 8
  call void @newFunction_sp({ i32* }* %func)
  %temp33 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp33)
  %\"LET*34\" = alloca { {}* }, align 8
  call void @newAFsp({ {}* }* %\"LET*34\")
  %temp38 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp38)
  %tsetq39 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %tsetq39)
  %2 = alloca [2 x { {}* }], align 8
  %.sub185204 = bitcast [2 x { {}* }]* %2 to { {}* }*
  call void @newTsp({ {}* }* %.sub185204)
  %gep41 = getelementptr inbounds [2 x { {}* }]* %2, i64 0, i64 1
  call void @newTsp({ {}* }* %gep41)
  %func44 = alloca { i32* }, align 8
  call void @newFunction_sp({ i32* }* %func44)
  %temp54 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp54)
  %\"LET*55\" = alloca { {}* }, align 8
  call void @newAFsp({ {}* }* %\"LET*55\")
  %temp59 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp59)
  %tsetq60 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %tsetq60)
  %3 = alloca [2 x { {}* }], align 8
  %.sub186205 = bitcast [2 x { {}* }]* %3 to { {}* }*
  call void @newTsp({ {}* }* %.sub186205)
  %gep62 = getelementptr inbounds [2 x { {}* }]* %3, i64 0, i64 1
  call void @newTsp({ {}* }* %gep62)
  %func65 = alloca { i32* }, align 8
  call void @newFunction_sp({ i32* }* %func65)
  %temp75 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp75)
  %\"LET*76\" = alloca { {}* }, align 8
  call void @newAFsp({ {}* }* %\"LET*76\")
  %temp80 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp80)
  %tsetq81 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %tsetq81)
  %4 = alloca [2 x { {}* }], align 8
  %.sub187206 = bitcast [2 x { {}* }]* %4 to { {}* }*
  call void @newTsp({ {}* }* %.sub187206)
  %gep83 = getelementptr inbounds [2 x { {}* }]* %4, i64 0, i64 1
  call void @newTsp({ {}* }* %gep83)
  %func86 = alloca { i32* }, align 8
  call void @newFunction_sp({ i32* }* %func86)
  %tagbody-frame = alloca { {}* }, align 8
  call void @newAFsp({ {}* }* %tagbody-frame)
  %temp96 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp96)
  %temp99 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp99)
  %5 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %5)
  %func102 = alloca { i32* }, align 8
  call void @newFunction_sp({ i32* }* %func102)
  %temp105 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp105)
  %if-cond-tsp = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %if-cond-tsp)
  %6 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %6)
  %7 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %7)
  %func110 = alloca { i32* }, align 8
  call void @newFunction_sp({ i32* }* %func110)
  %func113 = alloca { i32* }, align 8
  call void @newFunction_sp({ i32* }* %func113)
  %temp116 = alloca { {}* }, align 8
  call void @newTsp({ {}* }* %temp116)
  %func139 = alloca { i32* }, align 8
  call void @newFunction_sp({ i32* }* %func139)
  call void @makeValueFrame({ {}* }* %lambda-args-35-, i32 0, i32 2000050)
  %correct-num-args = icmp eq i32 %num-varargs, 0
  br i1 %correct-num-args, label %\"(TRY-0).continue3\", label %\"(TRY-0).error\"

\"(TRY-0).error\":                                  ; preds = %\"(TRY-0).entry\"
  %enough-args = icmp slt i32 %num-varargs, 0
  br i1 %enough-args, label %\"(TRY-0).error1\", label %\"(TRY-0).continue\"

\"(TRY-0).error1\":                                 ; preds = %\"(TRY-0).error\"
  invoke void @va_throwNotEnoughArgumentsException(i8* getelementptr inbounds ([2 x i8]* @\":::global-str-A\", i64 0, i64 0), i32 %num-varargs, i32 0)
          to label %\"(TRY-0).normal-dest\" unwind label %\"(TRY-0).func-cleanup-landing-pad\"

\"(TRY-0).normal-dest\":                            ; preds = %\"(TRY-0).error1\"
  call void @unreachableError()
  unreachable

\"(TRY-0).continue\":                               ; preds = %\"(TRY-0).error\"
  invoke void @va_throwTooManyArgumentsException(i8* getelementptr inbounds ([2 x i8]* @\":::global-str-A\", i64 0, i64 0), i32 %num-varargs, i32 0)
          to label %\"(TRY-0).normal-dest2\" unwind label %\"(TRY-0).func-cleanup-landing-pad\"

\"(TRY-0).normal-dest2\":                           ; preds = %\"(TRY-0).continue\"
  call void @unreachableError()
  unreachable

\"(TRY-0).continue3\":                              ; preds = %\"(TRY-0).entry\"
  invoke void @va_fillActivationFrameWithRequiredVarargs({ {}* }* %lambda-args-35-, i32 0, { {}* }* %va-list)
          to label %\"(TRY-0).normal-dest4\" unwind label %\"(TRY-0).func-cleanup-landing-pad\"

\"(TRY-0).normal-dest4\":                           ; preds = %\"(TRY-0).continue3\"
  %8 = bitcast [3 x { {}* }]* %0 to { {}* }*
  call void @setParentOfActivationFrame({ {}* }* %lambda-args-35-, { {}* }* %closed-af-ptr)
  %value = call { {}* }* @loadTimeValueReference({ {}* }** @globalRunTimeValuesVector, i32 104)
  call void @attachDebuggingInfoToValueFrame({ {}* }* %lambda-args-35-, { {}* }* %value)
  call void @trace_setActivationFrameForIHSTop({ {}* }* %lambda-args-35-)
  call void @trace_setLineNumberColumnForIHSTop(i32 2, i32 13), !dbg !11
  %\"SYMBOL->CL:T\" = call { i32* }* @loadTimeSymbolReference({ {}* }** @globalRunTimeValuesVector, i32 21), !dbg !11
  call void @sp_symbolValueRead({ {}* }* %8, { i32* }* %\"SYMBOL->CL:T\"), !dbg !11
  call void @sp_copyLoadTimeValue({ {}* }* %gep6, { {}* }** @globalRunTimeValuesVector, i32 105), !dbg !11
  call void @trace_setActivationFrameForIHSTop({ {}* }* %lambda-args-35-), !dbg !11
  call void @trace_setActivationFrameForIHSTop({ {}* }* %LET), !dbg !11
  call void @makeValueFrame({ {}* }* %LET, i32 1, i32 2000051), !dbg !11
  call void @setParentOfActivationFrame({ {}* }* %LET, { {}* }* %lambda-args-35-), !dbg !11
  %value11 = call { {}* }* @loadTimeValueReference({ {}* }** @globalRunTimeValuesVector, i32 106), !dbg !11
  call void @attachDebuggingInfoToValueFrame({ {}* }* %LET, { {}* }* %value11), !dbg !11
  %9 = call { {}* }* @lexicalValueReference(i32 0, i32 0, { {}* }* %LET), !dbg !11
  call void @sp_copyLoadTimeValue({ {}* }* %9, { {}* }** @globalRunTimeValuesVector, i32 107), !dbg !11
  call void @trace_setActivationFrameForIHSTop({ {}* }* %LET), !dbg !11
  call void @singleStepCallback(), !dbg !11
  call void @mv_copyLoadTimeValue({ {}*, i32 }* %temp-mv-result18, { {}* }** @globalRunTimeValuesVector, i32 108), !dbg !14
  invoke void @throwReturnFrom(i32 0, { {}*, i32 }* %temp-mv-result18)
          to label %\"(TRY-0).normal-dest19\" unwind label %\"(TRY-0).landing-pad31\", !dbg !14

\"(TRY-0).normal-dest19\":                          ; preds = %\"(TRY-0).normal-dest4\"
  call void @unreachableError(), !dbg !14
  unreachable, !dbg !14

\"(TRY-0).landing-pad31\":                          ; preds = %\"(TRY-0).normal-dest4\"
  %10 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
          catch i8* @_ZTIN4core10ReturnFromE, !dbg !14
  %11 = extractvalue { i8*, i32 } %10, 0, !dbg !14
  store i8* %11, i8** %exn.slot, align 8, !dbg !14
  %12 = extractvalue { i8*, i32 } %10, 1, !dbg !14
  store i32 %12, i32* %ehselector.slot, align 4, !dbg !14
  br label %\"(TRY-0).dispatch-header32\", !dbg !14

\"(TRY-0).dispatch-header32\":                      ; preds = %\"(TRY-0).landing-pad49\", %\"(TRY-0).landing-pad31\"
  %exn135195 = phi i8* [ %18, %\"(TRY-0).landing-pad49\" ], [ %11, %\"(TRY-0).landing-pad31\" ]
  %ehselector-slot134191 = phi i32 [ %19, %\"(TRY-0).landing-pad49\" ], [ %12, %\"(TRY-0).landing-pad31\" ]
  %13 = bitcast [2 x { {}* }]* %2 to { {}* }*
  call void @trace_setActivationFrameForIHSTop({ {}* }* %\"LET*34\"), !dbg !14
  call void @makeValueFrame({ {}* }* %\"LET*34\", i32 1, i32 2000053), !dbg !14
  call void @setParentOfActivationFrame({ {}* }* %\"LET*34\", { {}* }* %LET), !dbg !14
  %value37 = call { {}* }* @loadTimeValueReference({ {}* }** @globalRunTimeValuesVector, i32 111), !dbg !14
  call void @attachDebuggingInfoToValueFrame({ {}* }* %\"LET*34\", { {}* }* %value37), !dbg !14
  %14 = call { {}* }* @lexicalValueReference(i32 0, i32 0, { {}* }* %\"LET*34\"), !dbg !14
  call void @sp_copyLoadTimeValue({ {}* }* %14, { {}* }** @globalRunTimeValuesVector, i32 112), !dbg !14
  call void @trace_setActivationFrameForIHSTop({ {}* }* %\"LET*34\"), !dbg !14
  call void @singleStepCallback(), !dbg !14
  %15 = call { {}* }* @lexicalValueReference(i32 1, i32 0, { {}* }* %\"LET*34\"), !dbg !14
  call void @sp_lexicalValueRead({ {}* }* %13, i32 1, i32 0, { {}* }* %\"LET*34\"), !dbg !14
  call void @sp_lexicalValueRead({ {}* }* %gep41, i32 0, i32 0, { {}* }* %\"LET*34\"), !dbg !14
  %\"SYMBOL->CL:-45\" = call { i32* }* @loadTimeSymbolReference({ {}* }** @globalRunTimeValuesVector, i32 51), !dbg !14
  invoke void @va_symbolFunction({ i32* }* %func44, { i32* }* %\"SYMBOL->CL:-45\")
          to label %\"(TRY-0).normal-dest46\" unwind label %\"(TRY-0).landing-pad49\", !dbg !14

\"(TRY-0).normal-dest46\":                          ; preds = %\"(TRY-0).dispatch-header32\"
  %16 = bitcast [2 x { {}* }]* %2 to { {}* }*
  invoke void @sp_FUNCALL({ {}* }* %tsetq39, { i32* }* %func44, i32 2, { {}* }* %16)
          to label %\"(TRY-0).normal-dest47\" unwind label %\"(TRY-0).landing-pad49\", !dbg !14

\"(TRY-0).normal-dest47\":                          ; preds = %\"(TRY-0).normal-dest46\"
  call void @sp_copyTsp({ {}* }* %15, { {}* }* %tsetq39), !dbg !14
  call void @sp_copyTsp({ {}* }* %temp33, { {}* }* %tsetq39), !dbg !14
  call void @loadValues({ {}*, i32 }* %temp-mv-result14, { {}* }* %unwind-protect-saved-values15), !dbg !14
  call void @mv_copyTmvOrSlice({ {}*, i32 }* %temp-mv-result, { {}*, i32 }* %temp-mv-result14), !dbg !14
  br label %\"(TRY-0).dispatch-header74\", !dbg !14

\"(TRY-0).landing-pad49\":                          ; preds = %\"(TRY-0).normal-dest46\", %\"(TRY-0).dispatch-header32\"
  %17 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
          catch i8* @_ZTIN4core10ReturnFromE, !dbg !14
  %18 = extractvalue { i8*, i32 } %17, 0, !dbg !14
  store i8* %18, i8** %exn.slot, align 8, !dbg !14
  %19 = extractvalue { i8*, i32 } %17, 1, !dbg !14
  store i32 %19, i32* %ehselector.slot, align 4, !dbg !14
  br label %\"(TRY-0).dispatch-header32\", !dbg !14

\"(TRY-0).dispatch-header74\":                      ; preds = %\"(TRY-0).landing-pad91\", %\"(TRY-0).normal-dest47\"
  %exn135194 = phi i8* [ %27, %\"(TRY-0).landing-pad91\" ], [ %exn135195, %\"(TRY-0).normal-dest47\" ]
  %ehselector-slot134190 = phi i32 [ %28, %\"(TRY-0).landing-pad91\" ], [ %ehselector-slot134191, %\"(TRY-0).normal-dest47\" ]
  %20 = bitcast [2 x { {}* }]* %4 to { {}* }*
  call void @trace_setActivationFrameForIHSTop({ {}* }* %\"LET*76\"), !dbg !14
  call void @makeValueFrame({ {}* }* %\"LET*76\", i32 1, i32 2000055), !dbg !14
  call void @setParentOfActivationFrame({ {}* }* %\"LET*76\", { {}* }* %LET), !dbg !14
  %value79 = call { {}* }* @loadTimeValueReference({ {}* }** @globalRunTimeValuesVector, i32 115), !dbg !14
  call void @attachDebuggingInfoToValueFrame({ {}* }* %\"LET*76\", { {}* }* %value79), !dbg !14
  %21 = call { {}* }* @lexicalValueReference(i32 0, i32 0, { {}* }* %\"LET*76\"), !dbg !14
  call void @sp_copyLoadTimeValue({ {}* }* %21, { {}* }** @globalRunTimeValuesVector, i32 116), !dbg !14
  call void @trace_setActivationFrameForIHSTop({ {}* }* %\"LET*76\"), !dbg !14
  call void @singleStepCallback(), !dbg !14
  %22 = call { {}* }* @lexicalValueReference(i32 1, i32 0, { {}* }* %\"LET*76\"), !dbg !14
  call void @sp_lexicalValueRead({ {}* }* %20, i32 1, i32 0, { {}* }* %\"LET*76\"), !dbg !14
  call void @sp_lexicalValueRead({ {}* }* %gep83, i32 0, i32 0, { {}* }* %\"LET*76\"), !dbg !14
  %\"SYMBOL->CL:-87\" = call { i32* }* @loadTimeSymbolReference({ {}* }** @globalRunTimeValuesVector, i32 51), !dbg !14
  invoke void @va_symbolFunction({ i32* }* %func86, { i32* }* %\"SYMBOL->CL:-87\")
          to label %\"(TRY-0).normal-dest88\" unwind label %\"(TRY-0).landing-pad91\", !dbg !14

\"(TRY-0).normal-dest88\":                          ; preds = %\"(TRY-0).dispatch-header74\"
  %23 = bitcast [2 x { {}* }]* %4 to { {}* }*
  invoke void @sp_FUNCALL({ {}* }* %tsetq81, { i32* }* %func86, i32 2, { {}* }* %23)
          to label %\"(TRY-0).normal-dest89\" unwind label %\"(TRY-0).landing-pad91\", !dbg !14

\"(TRY-0).normal-dest89\":                          ; preds = %\"(TRY-0).normal-dest88\"
  call void @sp_copyTsp({ {}* }* %22, { {}* }* %tsetq81), !dbg !14
  call void @sp_copyTsp({ {}* }* %temp75, { {}* }* %tsetq81), !dbg !14
  call void @loadValues({ {}*, i32 }* %temp-mv-result, { {}* }* %unwind-protect-saved-values), !dbg !14
  call void @sp_copyTmvOrSlice({ {}* }* %temp12, { {}*, i32 }* %temp-mv-result), !dbg !14
  %24 = call i32 @llvm.eh.typeid.for(i8* @_ZTIN4core10ReturnFromE), !dbg !14
  %25 = icmp eq i32 %ehselector-slot134190, %24, !dbg !14
  br i1 %25, label %\"(TRY-0).handler-block13712\", label %\"(TRY-0).dispatch-header144\", !dbg !14

\"(TRY-0).landing-pad91\":                          ; preds = %\"(TRY-0).normal-dest88\", %\"(TRY-0).dispatch-header74\"
  %26 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
          catch i8* @_ZTIN4core10ReturnFromE, !dbg !14
  %27 = extractvalue { i8*, i32 } %26, 0, !dbg !14
  store i8* %27, i8** %exn.slot, align 8, !dbg !14
  %28 = extractvalue { i8*, i32 } %26, 1, !dbg !14
  store i32 %28, i32* %ehselector.slot, align 4, !dbg !14
  br label %\"(TRY-0).dispatch-header74\", !dbg !14

\"(TRY-0).handler-block13712\":                     ; preds = %\"(TRY-0).normal-dest89\"
  %29 = call i8* @__cxa_begin_catch(i8* %exn135194), !dbg !14
  invoke void @sp_blockHandleReturnFrom({ {}* }* %gep7, i8* %29)
          to label %\"(TRY-0).normal-dest136\" unwind label %\"(TRY-0).landing-pad143\", !dbg !14

\"(TRY-0).normal-dest136\":                         ; preds = %\"(TRY-0).handler-block13712\"
  invoke void @__cxa_end_catch()
          to label %\"(TRY-0).try-cont138\" unwind label %\"(TRY-0).landing-pad143\", !dbg !14

\"(TRY-0).try-cont138\":                            ; preds = %\"(TRY-0).normal-dest136\"
  %\"SYMBOL->CL:FORMAT\" = call { i32* }* @loadTimeSymbolReference({ {}* }** @globalRunTimeValuesVector, i32 32), !dbg !14
  invoke void @va_symbolFunction({ i32* }* %func139, { i32* }* %\"SYMBOL->CL:FORMAT\")
          to label %\"(TRY-0).normal-dest140\" unwind label %\"(TRY-0).landing-pad143\", !dbg !14

\"(TRY-0).normal-dest140\":                         ; preds = %\"(TRY-0).try-cont138\"
  %30 = bitcast [3 x { {}* }]* %0 to { {}* }*
  invoke void @mv_FUNCALL({ {}*, i32 }* %result-ptr, { i32* }* %func139, i32 3, { {}* }* %30)
          to label %\"(TRY-0).try-cont151\" unwind label %\"(TRY-0).landing-pad143\", !dbg !14

\"(TRY-0).landing-pad143\":                         ; preds = %\"(TRY-0).normal-dest140\", %\"(TRY-0).try-cont138\", %\"(TRY-0).normal-dest136\", %\"(TRY-0).handler-block13712\"
  %31 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup
          catch i8* @_ZTIN4core10ReturnFromE, !dbg !14
  %32 = extractvalue { i8*, i32 } %31, 0, !dbg !14
  store i8* %32, i8** %exn.slot, align 8, !dbg !14
  %33 = extractvalue { i8*, i32 } %31, 1, !dbg !14
  store i32 %33, i32* %ehselector.slot, align 4, !dbg !14
  br label %\"(TRY-0).dispatch-header144\", !dbg !14

\"(TRY-0).dispatch-header144\":                     ; preds = %\"(TRY-0).landing-pad143\", %\"(TRY-0).normal-dest89\"
  %exn148 = phi i8* [ %32, %\"(TRY-0).landing-pad143\" ], [ %exn135194, %\"(TRY-0).normal-dest89\" ]
  %ehselector-slot146 = phi i32 [ %33, %\"(TRY-0).landing-pad143\" ], [ %ehselector-slot134190, %\"(TRY-0).normal-dest89\" ]
  %34 = icmp eq i32 %ehselector-slot146, %24, !dbg !14
  br i1 %34, label %\"(TRY-0).handler-block13712147\", label %\"(TRY-0).func-ehcleanup\", !dbg !14

\"(TRY-0).handler-block13712147\":                  ; preds = %\"(TRY-0).dispatch-header144\"
  %35 = call i8* @__cxa_begin_catch(i8* %exn148), !dbg !14
  invoke void @mv_blockHandleReturnFrom({ {}*, i32 }* %result-ptr, i8* %35)
          to label %\"(TRY-0).normal-dest149\" unwind label %\"(TRY-0).landing-pad153\", !dbg !14

\"(TRY-0).normal-dest149\":                         ; preds = %\"(TRY-0).handler-block13712147\"
  invoke void @__cxa_end_catch()
          to label %\"(TRY-0).try-cont151\" unwind label %\"(TRY-0).landing-pad153\", !dbg !14

\"(TRY-0).try-cont151\":                            ; preds = %\"(TRY-0).normal-dest149\", %\"(TRY-0).normal-dest140\"
  %36 = bitcast [2 x { {}* }]* %4 to { {}* }*
  %37 = bitcast [2 x { {}* }]* %3 to { {}* }*
  %38 = bitcast [2 x { {}* }]* %2 to { {}* }*
  %39 = bitcast [2 x { {}* }]* %1 to { {}* }*
  %40 = bitcast [3 x { {}* }]* %0 to { {}* }*
  call void @destructFunction_sp({ i32* }* %func139), !dbg !14
  call void @destructTsp({ {}* }* %temp116), !dbg !14
  call void @destructFunction_sp({ i32* }* %func113), !dbg !14
  call void @destructFunction_sp({ i32* }* %func110), !dbg !14
  call void @destructTsp({ {}* }* %7), !dbg !14
  call void @destructTsp({ {}* }* %6), !dbg !14
  call void @destructTsp({ {}* }* %if-cond-tsp), !dbg !14
  call void @destructTsp({ {}* }* %temp105), !dbg !14
  call void @destructFunction_sp({ i32* }* %func102), !dbg !14
  call void @destructTsp({ {}* }* %5), !dbg !14
  call void @destructTsp({ {}* }* %temp99), !dbg !14
  call void @destructTsp({ {}* }* %temp96), !dbg !14
  call void @destructAFsp({ {}* }* %tagbody-frame), !dbg !14
  call void @destructFunction_sp({ i32* }* %func86), !dbg !14
  call void @destructTsp({ {}* }* %36), !dbg !14
  call void @destructTsp({ {}* }* %gep83), !dbg !14
  call void @destructTsp({ {}* }* %tsetq81), !dbg !14
  call void @destructTsp({ {}* }* %temp80), !dbg !14
  call void @destructAFsp({ {}* }* %\"LET*76\"), !dbg !14
  call void @destructTsp({ {}* }* %temp75), !dbg !14
  call void @destructFunction_sp({ i32* }* %func65), !dbg !14
  call void @destructTsp({ {}* }* %37), !dbg !14
  call void @destructTsp({ {}* }* %gep62), !dbg !14
  call void @destructTsp({ {}* }* %tsetq60), !dbg !14
  call void @destructTsp({ {}* }* %temp59), !dbg !14
  call void @destructAFsp({ {}* }* %\"LET*55\"), !dbg !14
  call void @destructTsp({ {}* }* %temp54), !dbg !14
  call void @destructFunction_sp({ i32* }* %func44), !dbg !14
  call void @destructTsp({ {}* }* %38), !dbg !14
  call void @destructTsp({ {}* }* %gep41), !dbg !14
  call void @destructTsp({ {}* }* %tsetq39), !dbg !14
  call void @destructTsp({ {}* }* %temp38), !dbg !14
  call void @destructAFsp({ {}* }* %\"LET*34\"), !dbg !14
  call void @destructTsp({ {}* }* %temp33), !dbg !14
  call void @destructFunction_sp({ i32* }* %func), !dbg !14
  call void @destructTsp({ {}* }* %39), !dbg !14
  call void @destructTsp({ {}* }* %gep25), !dbg !14
  call void @destructTsp({ {}* }* %tsetq), !dbg !14
  call void @destructTsp({ {}* }* %temp23), !dbg !14
  call void @destructAFsp({ {}* }* %\"LET*\"), !dbg !14
  call void @destructTsp({ {}* }* %temp20), !dbg !14
  call void @destructTmv({ {}*, i32 }* %temp-mv-result18), !dbg !14
  call void @destructTsp({ {}* }* %unwind-protect-saved-values15), !dbg !14
  call void @destructTmv({ {}*, i32 }* %temp-mv-result14), !dbg !14
  call void @destructTsp({ {}* }* %unwind-protect-saved-values), !dbg !14
  call void @destructTmv({ {}*, i32 }* %temp-mv-result), !dbg !14
  call void @destructTsp({ {}* }* %temp12), !dbg !14
  call void @destructAFsp({ {}* }* %LET), !dbg !14
  call void @destructTsp({ {}* }* %temp9), !dbg !14
  call void @destructTsp({ {}* }* %40), !dbg !14
  call void @destructTsp({ {}* }* %gep6), !dbg !14
  call void @destructTsp({ {}* }* %gep7), !dbg !14
  call void @destructTsp({ {}* }* %temp), !dbg !14
  call void @destructAFsp({ {}* }* %lambda-args-35-), !dbg !14
  ret void, !dbg !14

\"(TRY-0).landing-pad153\":                         ; preds = %\"(TRY-0).normal-dest149\", %\"(TRY-0).handler-block13712147\"
  %41 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup, !dbg !14
  %42 = extractvalue { i8*, i32 } %41, 0, !dbg !14
  store i8* %42, i8** %exn.slot, align 8, !dbg !14
  %43 = extractvalue { i8*, i32 } %41, 1, !dbg !14
  store i32 %43, i32* %ehselector.slot, align 4, !dbg !14
  br label %\"(TRY-0).func-ehcleanup\", !dbg !14

\"(TRY-0).func-cleanup-landing-pad\":               ; preds = %\"(TRY-0).continue3\", %\"(TRY-0).continue\", %\"(TRY-0).error1\"
  %44 = landingpad { i8*, i32 } personality i32 (...)* @__gxx_personality_v0
          cleanup, !dbg !14
  %45 = extractvalue { i8*, i32 } %44, 0, !dbg !14
  store i8* %45, i8** %exn.slot, align 8, !dbg !14
  %46 = extractvalue { i8*, i32 } %44, 1, !dbg !14
  store i32 %46, i32* %ehselector.slot, align 4, !dbg !14
  br label %\"(TRY-0).func-ehcleanup\", !dbg !14

\"(TRY-0).func-ehcleanup\":                         ; preds = %\"(TRY-0).dispatch-header144\", %\"(TRY-0).landing-pad153\", %\"(TRY-0).func-cleanup-landing-pad\"
  %sel = phi i32 [ %46, %\"(TRY-0).func-cleanup-landing-pad\" ], [ %43, %\"(TRY-0).landing-pad153\" ], [ %ehselector-slot146, %\"(TRY-0).dispatch-header144\" ]
  %exn7 = phi i8* [ %45, %\"(TRY-0).func-cleanup-landing-pad\" ], [ %42, %\"(TRY-0).landing-pad153\" ], [ %exn148, %\"(TRY-0).dispatch-header144\" ]
  %47 = bitcast [2 x { {}* }]* %4 to { {}* }*
  %48 = bitcast [2 x { {}* }]* %3 to { {}* }*
  %49 = bitcast [2 x { {}* }]* %2 to { {}* }*
  %50 = bitcast [2 x { {}* }]* %1 to { {}* }*
  %51 = bitcast [3 x { {}* }]* %0 to { {}* }*
  call void @destructFunction_sp({ i32* }* %func139), !dbg !14
  call void @destructTsp({ {}* }* %temp116), !dbg !14
  call void @destructFunction_sp({ i32* }* %func113), !dbg !14
  call void @destructFunction_sp({ i32* }* %func110), !dbg !14
  call void @destructTsp({ {}* }* %7), !dbg !14
  call void @destructTsp({ {}* }* %6), !dbg !14
  call void @destructTsp({ {}* }* %if-cond-tsp), !dbg !14
  call void @destructTsp({ {}* }* %temp105), !dbg !14
  call void @destructFunction_sp({ i32* }* %func102), !dbg !14
  call void @destructTsp({ {}* }* %5), !dbg !14
  call void @destructTsp({ {}* }* %temp99), !dbg !14
  call void @destructTsp({ {}* }* %temp96), !dbg !14
  call void @destructAFsp({ {}* }* %tagbody-frame), !dbg !14
  call void @destructFunction_sp({ i32* }* %func86), !dbg !14
  call void @destructTsp({ {}* }* %47), !dbg !14
  call void @destructTsp({ {}* }* %gep83), !dbg !14
  call void @destructTsp({ {}* }* %tsetq81), !dbg !14
  call void @destructTsp({ {}* }* %temp80), !dbg !14
  call void @destructAFsp({ {}* }* %\"LET*76\"), !dbg !14
  call void @destructTsp({ {}* }* %temp75), !dbg !14
  call void @destructFunction_sp({ i32* }* %func65), !dbg !14
  call void @destructTsp({ {}* }* %48), !dbg !14
  call void @destructTsp({ {}* }* %gep62), !dbg !14
  call void @destructTsp({ {}* }* %tsetq60), !dbg !14
  call void @destructTsp({ {}* }* %temp59), !dbg !14
  call void @destructAFsp({ {}* }* %\"LET*55\"), !dbg !14
  call void @destructTsp({ {}* }* %temp54), !dbg !14
  call void @destructFunction_sp({ i32* }* %func44), !dbg !14
  call void @destructTsp({ {}* }* %49), !dbg !14
  call void @destructTsp({ {}* }* %gep41), !dbg !14
  call void @destructTsp({ {}* }* %tsetq39), !dbg !14
  call void @destructTsp({ {}* }* %temp38), !dbg !14
  call void @destructAFsp({ {}* }* %\"LET*34\"), !dbg !14
  call void @destructTsp({ {}* }* %temp33), !dbg !14
  call void @destructFunction_sp({ i32* }* %func), !dbg !14
  call void @destructTsp({ {}* }* %50), !dbg !14
  call void @destructTsp({ {}* }* %gep25), !dbg !14
  call void @destructTsp({ {}* }* %tsetq), !dbg !14
  call void @destructTsp({ {}* }* %temp23), !dbg !14
  call void @destructAFsp({ {}* }* %\"LET*\"), !dbg !14
  call void @destructTsp({ {}* }* %temp20), !dbg !14
  call void @destructTmv({ {}*, i32 }* %temp-mv-result18), !dbg !14
  call void @destructTsp({ {}* }* %unwind-protect-saved-values15), !dbg !14
  call void @destructTmv({ {}*, i32 }* %temp-mv-result14), !dbg !14
  call void @destructTsp({ {}* }* %unwind-protect-saved-values), !dbg !14
  call void @destructTmv({ {}*, i32 }* %temp-mv-result), !dbg !14
  call void @destructTsp({ {}* }* %temp12), !dbg !14
  call void @destructAFsp({ {}* }* %LET), !dbg !14
  call void @destructTsp({ {}* }* %temp9), !dbg !14
  call void @destructTsp({ {}* }* %51), !dbg !14
  call void @destructTsp({ {}* }* %gep6), !dbg !14
  call void @destructTsp({ {}* }* %gep7), !dbg !14
  call void @destructTsp({ {}* }* %temp), !dbg !14
  call void @destructAFsp({ {}* }* %lambda-args-35-), !dbg !14
  call void @_Unwind_Resume(i8* %exn7)
  unreachable
