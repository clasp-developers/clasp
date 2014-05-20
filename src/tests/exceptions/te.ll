; ModuleID = '<stdin>'

%Sym_sp = type { %Symbol_O*, %shared_count }
%Symbol_O = type opaque
%shared_count = type { %sp-counted-base-ty* }
%sp-counted-base-ty = type { i32**, i32, i32 }
%T_sp = type { %T_O*, %shared_count }
%T_O = type opaque
%ActivationFrame_sp = type { %ActivationFrame_O*, %shared_count }
%ActivationFrame_O = type opaque
%exception-struct = type { i8*, i32 }

@":::symbol-name-A" = private unnamed_addr constant [2 x i8] c"A\00"
@":::package-name-CORE" = private unnamed_addr constant [5 x i8] c"CORE\00"
@"CORE:T" = external constant %Sym_sp
@.str = private unnamed_addr constant [19 x i8] c"1. In catch forms\0A\00"
@"CORE:BFORMAT" = external constant %Sym_sp
@":::symbol-name-B" = private unnamed_addr constant [2 x i8] c"B\00"
@.str1 = private unnamed_addr constant [20 x i8] c"2. Last catch form\0A\00"
@typeid_core_CatchThrow = external constant i8
@.str3 = private unnamed_addr constant [22 x i8] c"3. Left catch forms n\00"

declare void @newTsp(%T_sp*) nounwind

declare void @resetTsp(%T_sp*)

declare void @copyTsp(%T_sp*, %T_sp*) nounwind

declare void @destructTsp(%T_sp*) nounwind

declare void @newAFsp(%ActivationFrame_sp*) nounwind

declare void @resetAFsp(%ActivationFrame_sp*)

declare void @copyAFsp(%ActivationFrame_sp*, %ActivationFrame_sp*) nounwind

declare void @destructAFsp(%ActivationFrame_sp*) nounwind

declare i32 @isNilTsp(%T_sp*)

declare i32 @isTrueTsp(%T_sp*)

declare void @internSymbol(%T_sp*, i8*, i8*)

declare void @makeNil(%T_sp*) nounwind

declare void @makeT(%T_sp*) nounwind

declare void @makeCons(%T_sp*, %T_sp*, %T_sp*) nounwind

declare void @makeFixnum(%T_sp*, i32) nounwind

declare void @makeBignum(%T_sp*, i8*) nounwind

declare void @makeShortFloat(%T_sp*, double) nounwind

declare void @makeSingleFloat(%T_sp*, double) nounwind

declare void @makeDoubleFloat(%T_sp*, double) nounwind

declare void @makeLongFloat(%T_sp*, double) nounwind

declare void @makeString(%T_sp*, i8*) nounwind

declare void @makeClosure(%T_sp*, void (%T_sp*, %ActivationFrame_sp*)*, %ActivationFrame_sp*)

declare void @fillRestTarget(%T_sp*, %ActivationFrame_sp*, i32)

declare i32 @checkForAllowOtherKeywords(i32, %ActivationFrame_sp*, i32)

declare i32 @lookupKeyword(%T_sp*, %Sym_sp*, %ActivationFrame_sp*, i32)

declare void @throwIfOtherKeywords(%ActivationFrame_sp*, i32)

declare void @symbolValueRead(%T_sp*, %Sym_sp*)

declare %T_sp* @symbolValueReference(%Sym_sp*)

declare %T_sp* @lexicalValueReference(i32, i32, %ActivationFrame_sp*)

declare void @lexicalValueRead(%T_sp*, i32, i32, %ActivationFrame_sp*)

declare void @symbolFunctionRead(%T_sp*, %Sym_sp*)

declare void @lexicalFunctionRead(%T_sp*, i32, i32)

declare void @makeValueFrameWithNilParent(%ActivationFrame_sp*, i32) nounwind

declare void @makeValueFrame(%ActivationFrame_sp*, i32, %ActivationFrame_sp*) nounwind

declare %T_sp* @valueFrameReference(%ActivationFrame_sp*, i32)

declare void @makeFunctionFrame(%ActivationFrame_sp*, i32, %ActivationFrame_sp*)

declare %T_sp* @functionFrameReference(%ActivationFrame_sp*, i32)

declare void @prependMultipleValues(%T_sp*, %T_sp*)

declare void @makeValueFrameFromReversedCons(%ActivationFrame_sp*, %T_sp*, %ActivationFrame_sp*)

declare void @firstValueIfMultipleValue(%T_sp*) nounwind

declare void @invokePossibleMultipleValueFunction(%T_sp*, %T_sp*, %ActivationFrame_sp*)

declare void @invokePossibleMultipleValueSymbolFunction(%T_sp*, %Sym_sp*, %ActivationFrame_sp*)

declare void @invokePossibleMultipleValueLexicalFunction(%T_sp*, i32, i32, %ActivationFrame_sp*)

declare void @invokeLlvmFunction(%T_sp*, void (%T_sp*, %ActivationFrame_sp*)*, %ActivationFrame_sp*)

declare %ActivationFrame_sp* @activationFrameNil() nounwind

declare i32 @activationFrameSize(%ActivationFrame_sp*)

declare %ActivationFrame_sp* @activationFrameParentRef(%ActivationFrame_sp*)

declare void @throwTooManyArgumentsException(i32, i32)

declare void @throwNotEnoughArgumentsException(i32, i32)

declare void @throwIfExcessKeywordArguments(%ActivationFrame_sp*, i32)

declare void @gdb() nounwind

declare void @debugInvoke() nounwind

declare void @debugInspectActivationFrame(%ActivationFrame_sp*) nounwind

declare void @debugInspectObject(%T_sp*) nounwind

declare void @debugTrace(i8*) nounwind

declare void @debugPrintObject(i8*, %T_sp*) nounwind

declare void @debugPrintI32(i32) nounwind

declare void @trace_enterFunctionScope(i8*, i32, i32)

declare void @trace_enterBlockScope(i8*, i32, i32)

declare void @trace_enterLetScope(i8*, i32, i32)

declare void @trace_enterLetSTARScope(i8*, i32, i32)

declare void @trace_lineNumberAndColumn(i32, i32)

declare void @trace_exitLexicalScope()

declare void @throwCatchThrow(%T_sp*, %T_sp*)

declare void @terminate() nounwind

declare i32 @__gxx_personality_v0(...) nounwind

declare i8* @__cxa_begin_catch(i8*) nounwind

declare void @__cxa_end_catch()

declare void @__cxa_rethrow()

declare i32 @llvm.eh.typeid.for(i8*) nounwind readnone

declare void @catchStoreTag(%T_sp*, %T_sp*) nounwind

declare i32 @catchTagMatches(%T_sp*, i8*) nounwind

declare void @catchUnwind(%T_sp*) nounwind

declare void @catchStoreResult(%T_sp*, i8*) nounwind

define void @repl(%T_sp* %result-ptr, %ActivationFrame_sp* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*, !dbg !0
  %ehselector.slot = alloca i32, !dbg !0
  %tag-store = alloca %T_sp, !dbg !0
  %tag-unwind-store = alloca %T_sp, !dbg !0
  %call-args = alloca %ActivationFrame_sp, !dbg !0
  %tag-store5 = alloca %T_sp, !dbg !0
  %result-form-store = alloca %T_sp, !dbg !0
  %call-args9 = alloca %ActivationFrame_sp, !dbg !0
  %":::alloca-end" = alloca i32, i32 0, !dbg !0
  call void @newTsp(%T_sp* %tag-store), !dbg !0
  call void @newTsp(%T_sp* %tag-unwind-store), !dbg !0
  call void @newAFsp(%ActivationFrame_sp* %call-args), !dbg !0
  call void @newTsp(%T_sp* %tag-store5), !dbg !0
  call void @newTsp(%T_sp* %result-form-store), !dbg !0
  call void @newAFsp(%ActivationFrame_sp* %call-args9), !dbg !0
  %":::new-end" = alloca i32, i32 0, !dbg !0
  %":::setup-end" = alloca i32, i32 0, !dbg !0
  invoke void @internSymbol(%T_sp* %tag-store, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-A", i32 0, i32 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i32 0, i32 0))
          to label %normal-dest unwind label %ehresume, !dbg !0

normal-dest:                                      ; preds = %entry
  call void @catchStoreTag(%T_sp* %tag-unwind-store, %T_sp* %tag-store), !dbg !0
  %0 = call %ActivationFrame_sp* @activationFrameNil(), !dbg !2
  call void @makeValueFrame(%ActivationFrame_sp* %call-args, i32 2, %ActivationFrame_sp* %0), !dbg !2
  %call-args-ref-0 = invoke %T_sp* @valueFrameReference(%ActivationFrame_sp* %call-args, i32 0)
          to label %normal-dest1 unwind label %catch-landing-pad, !dbg !2

normal-dest1:                                     ; preds = %normal-dest
  invoke void @symbolValueRead(%T_sp* %call-args-ref-0, %Sym_sp* @"CORE:T")
          to label %normal-dest2 unwind label %catch-landing-pad, !dbg !2

normal-dest2:                                     ; preds = %normal-dest1
  call void @firstValueIfMultipleValue(%T_sp* %call-args-ref-0), !dbg !2
  %call-args-ref-1 = invoke %T_sp* @valueFrameReference(%ActivationFrame_sp* %call-args, i32 1)
          to label %normal-dest3 unwind label %catch-landing-pad, !dbg !2

normal-dest3:                                     ; preds = %normal-dest2
  call void @makeString(%T_sp* %call-args-ref-1, i8* getelementptr inbounds ([19 x i8]* @.str, i32 0, i32 0)), !dbg !2
  call void @firstValueIfMultipleValue(%T_sp* %call-args-ref-1), !dbg !2
  invoke void @invokePossibleMultipleValueSymbolFunction(%T_sp* %result-ptr, %Sym_sp* @"CORE:BFORMAT", %ActivationFrame_sp* %call-args)
          to label %normal-dest4 unwind label %catch-landing-pad, !dbg !2

normal-dest4:                                     ; preds = %normal-dest3
  invoke void @internSymbol(%T_sp* %tag-store5, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-A", i32 0, i32 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i32 0, i32 0))
          to label %normal-dest6 unwind label %catch-landing-pad, !dbg !0

normal-dest6:                                     ; preds = %normal-dest4
  invoke void @internSymbol(%T_sp* %result-form-store, i8* getelementptr inbounds ([2 x i8]* @":::symbol-name-B", i32 0, i32 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i32 0, i32 0))
          to label %normal-dest7 unwind label %catch-landing-pad, !dbg !0

normal-dest7:                                     ; preds = %normal-dest6
  invoke void @throwCatchThrow(%T_sp* %tag-store5, %T_sp* %result-form-store)
          to label %normal-dest8 unwind label %catch-landing-pad, !dbg !0

normal-dest8:                                     ; preds = %normal-dest7
  %1 = call %ActivationFrame_sp* @activationFrameNil(), !dbg !2
  call void @makeValueFrame(%ActivationFrame_sp* %call-args9, i32 2, %ActivationFrame_sp* %1), !dbg !2
  %call-args9-ref-0 = invoke %T_sp* @valueFrameReference(%ActivationFrame_sp* %call-args9, i32 0)
          to label %normal-dest10 unwind label %catch-landing-pad, !dbg !2

normal-dest10:                                    ; preds = %normal-dest8
  invoke void @symbolValueRead(%T_sp* %call-args9-ref-0, %Sym_sp* @"CORE:T")
          to label %normal-dest11 unwind label %catch-landing-pad, !dbg !2

normal-dest11:                                    ; preds = %normal-dest10
  call void @firstValueIfMultipleValue(%T_sp* %call-args9-ref-0), !dbg !2
  %call-args9-ref-1 = invoke %T_sp* @valueFrameReference(%ActivationFrame_sp* %call-args9, i32 1)
          to label %normal-dest12 unwind label %catch-landing-pad, !dbg !2

normal-dest12:                                    ; preds = %normal-dest11
  call void @makeString(%T_sp* %call-args9-ref-1, i8* getelementptr inbounds ([20 x i8]* @.str1, i32 0, i32 0)), !dbg !2
  call void @firstValueIfMultipleValue(%T_sp* %call-args9-ref-1), !dbg !2
  invoke void @invokePossibleMultipleValueSymbolFunction(%T_sp* %result-ptr, %Sym_sp* @"CORE:BFORMAT", %ActivationFrame_sp* %call-args9)
          to label %catch-continue unwind label %catch-landing-pad, !dbg !2

catch-landing-pad:                                ; preds = %normal-dest12, %normal-dest11, %normal-dest10, %normal-dest8, %normal-dest7, %normal-dest6, %normal-dest4, %normal-dest3, %normal-dest2, %normal-dest1, %normal-dest
  %2 = landingpad %exception-struct personality i32 (...)* @__gxx_personality_v0
          catch i8* @typeid_core_CatchThrow
          catch i8* null, !dbg !2
  %3 = extractvalue %exception-struct %2, 0, !dbg !2
  store i8* %3, i8** %exn.slot, !dbg !2
  %4 = extractvalue %exception-struct %2, 1, !dbg !2
  store i32 %4, i32* %ehselector.slot, !dbg !2
  %ehselector.slot14 = load i32* %ehselector.slot, !dbg !2
  %5 = call i32 @llvm.eh.typeid.for(i8* @typeid_core_CatchThrow), !dbg !2
  %6 = icmp eq i32 %ehselector.slot14, %5, !dbg !2
  br i1 %6, label %catch-throw, label %catch-all, !dbg !2

catch-throw:                                      ; preds = %catch-landing-pad
  %exn = load i8** %exn.slot, !dbg !2
  %7 = call i8* @__cxa_begin_catch(i8* %exn), !dbg !2
  call void @debugPrintI32(i32 1001), !dbg !2
  call void @gdb(), !dbg !2
  %8 = call i32 @catchTagMatches(%T_sp* %tag-unwind-store, i8* %7), !dbg !2
  %9 = icmp eq i32 %8, 1, !dbg !2
  br i1 %9, label %tag-matches, label %unwind-catch-all, !dbg !2

tag-matches:                                      ; preds = %catch-throw
  call void @catchStoreResult(%T_sp* %result-ptr, i8* %7), !dbg !2
  invoke void @__cxa_end_catch()
          to label %catch-continue unwind label %ehresume, !dbg !2

catch-all:                                        ; preds = %catch-landing-pad
  %exn16 = load i8** %exn.slot, !dbg !2
  %10 = call i8* @__cxa_begin_catch(i8* %exn16), !dbg !2
  call void @debugPrintI32(i32 1002), !dbg !2
  br label %unwind-catch-all, !dbg !2

unwind-catch-all:                                 ; preds = %catch-all, %catch-throw
  call void @gdb(), !dbg !2
  call void @catchUnwind(%T_sp* %tag-unwind-store), !dbg !2
  invoke void @__cxa_rethrow()
          to label %normal-dest17 unwind label %ehresume, !dbg !3

normal-dest17:                                    ; preds = %unwind-catch-all
  invoke void @__cxa_end_catch()
          to label %normal-dest18 unwind label %ehresume, !dbg !3

normal-dest18:                                    ; preds = %normal-dest17
  unreachable, !dbg !3

catch-continue:                                   ; preds = %tag-matches, %normal-dest12
  call void @destructAFsp(%ActivationFrame_sp* %call-args9), !dbg !5
  call void @destructTsp(%T_sp* %result-form-store), !dbg !5
  call void @destructTsp(%T_sp* %tag-store5), !dbg !5
  call void @destructAFsp(%ActivationFrame_sp* %call-args), !dbg !5
  call void @destructTsp(%T_sp* %tag-unwind-store), !dbg !5
  call void @destructTsp(%T_sp* %tag-store), !dbg !5
  ret void, !dbg !5

ehresume:                                         ; preds = %normal-dest17, %unwind-catch-all, %tag-matches, %entry
  %11 = landingpad %exception-struct personality i32 (...)* @__gxx_personality_v0
          cleanup, !dbg !5
  %12 = extractvalue %exception-struct %11, 0, !dbg !5
  store i8* %12, i8** %exn.slot, !dbg !5
  %13 = extractvalue %exception-struct %11, 1, !dbg !5
  store i32 %13, i32* %ehselector.slot, !dbg !5
  call void @debugPrintI32(i32 100), !dbg !5
  call void @destructAFsp(%ActivationFrame_sp* %call-args9), !dbg !5
  call void @destructTsp(%T_sp* %result-form-store), !dbg !5
  call void @destructTsp(%T_sp* %tag-store5), !dbg !5
  call void @destructAFsp(%ActivationFrame_sp* %call-args), !dbg !5
  call void @destructTsp(%T_sp* %tag-unwind-store), !dbg !5
  call void @destructTsp(%T_sp* %tag-store), !dbg !5
  call void @debugPrintI32(i32 101), !dbg !5
  %exn7 = load i8** %exn.slot, !dbg !6
  call void @debugPrintI32(i32 90), !dbg !6
  call void @debugPrintI32(i32 91), !dbg !6
  call void @_Unwind_Resume(i8* %exn7)
  unreachable
}

declare void @_Unwind_Resume(i8*)

define void @repl2(%T_sp* %result-ptr, %ActivationFrame_sp* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*, !dbg !0
  %ehselector.slot = alloca i32, !dbg !0
  %call-args = alloca %ActivationFrame_sp, !dbg !0
  %":::alloca-end" = alloca i32, i32 0, !dbg !0
  call void @newAFsp(%ActivationFrame_sp* %call-args), !dbg !0
  %":::new-end" = alloca i32, i32 0, !dbg !0
  %":::setup-end" = alloca i32, i32 0, !dbg !0
  %0 = call %ActivationFrame_sp* @activationFrameNil(), !dbg !2
  call void @makeValueFrame(%ActivationFrame_sp* %call-args, i32 2, %ActivationFrame_sp* %0), !dbg !2
  %call-args-ref-0 = invoke %T_sp* @valueFrameReference(%ActivationFrame_sp* %call-args, i32 0)
          to label %normal-dest unwind label %cleanup-landing-pad, !dbg !2

normal-dest:                                      ; preds = %entry
  invoke void @symbolValueRead(%T_sp* %call-args-ref-0, %Sym_sp* @"CORE:T")
          to label %normal-dest1 unwind label %cleanup-landing-pad, !dbg !2

normal-dest1:                                     ; preds = %normal-dest
  call void @firstValueIfMultipleValue(%T_sp* %call-args-ref-0), !dbg !2
  %call-args-ref-1 = invoke %T_sp* @valueFrameReference(%ActivationFrame_sp* %call-args, i32 1)
          to label %normal-dest2 unwind label %cleanup-landing-pad, !dbg !2

normal-dest2:                                     ; preds = %normal-dest1
  call void @makeString(%T_sp* %call-args-ref-1, i8* getelementptr inbounds ([22 x i8]* @.str3, i32 0, i32 0)), !dbg !2
  call void @firstValueIfMultipleValue(%T_sp* %call-args-ref-1), !dbg !2
  invoke void @invokePossibleMultipleValueSymbolFunction(%T_sp* %result-ptr, %Sym_sp* @"CORE:BFORMAT", %ActivationFrame_sp* %call-args)
          to label %normal-dest3 unwind label %cleanup-landing-pad, !dbg !2

normal-dest3:                                     ; preds = %normal-dest2
  call void @destructAFsp(%ActivationFrame_sp* %call-args), !dbg !5
  ret void, !dbg !5

cleanup-landing-pad:                              ; preds = %normal-dest2, %normal-dest1, %normal-dest, %entry
  %1 = landingpad %exception-struct personality i32 (...)* @__gxx_personality_v0
          cleanup, !dbg !5
  %2 = extractvalue %exception-struct %1, 0, !dbg !5
  store i8* %2, i8** %exn.slot, !dbg !5
  %3 = extractvalue %exception-struct %1, 1, !dbg !5
  store i32 %3, i32* %ehselector.slot, !dbg !5
  call void @debugPrintI32(i32 100), !dbg !5
  br label %ehcleanup, !dbg !5

ehcleanup:                                        ; preds = %cleanup-landing-pad
  call void @destructAFsp(%ActivationFrame_sp* %call-args), !dbg !5
  br label %ehresume, !dbg !5

ehresume:                                         ; preds = %ehcleanup
  call void @debugPrintI32(i32 101), !dbg !5
  %exn7 = load i8** %exn.slot, !dbg !6
  call void @debugPrintI32(i32 90), !dbg !6
  call void @debugPrintI32(i32 91), !dbg !6
  call void @_Unwind_Resume(i8* %exn7)
  unreachable
}

define void @___main___(%T_sp* %result-ptr, %ActivationFrame_sp* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*, !dbg !6
  %ehselector.slot = alloca i32, !dbg !6
  %":::alloca-end" = alloca i32, i32 0, !dbg !6
  %":::new-end" = alloca i32, i32 0, !dbg !6
  %":::setup-end" = alloca i32, i32 0, !dbg !6
  %0 = call %ActivationFrame_sp* @activationFrameNil(), !dbg !6
  invoke void @invokeLlvmFunction(%T_sp* %result-ptr, void (%T_sp*, %ActivationFrame_sp*)* @repl, %ActivationFrame_sp* %0)
          to label %normal-dest unwind label %cleanup-landing-pad, !dbg !6

normal-dest:                                      ; preds = %entry
  %1 = call %ActivationFrame_sp* @activationFrameNil(), !dbg !6
  invoke void @invokeLlvmFunction(%T_sp* %result-ptr, void (%T_sp*, %ActivationFrame_sp*)* @repl2, %ActivationFrame_sp* %1)
          to label %normal-dest1 unwind label %cleanup-landing-pad, !dbg !6

normal-dest1:                                     ; preds = %normal-dest
  ret void, !dbg !6

cleanup-landing-pad:                              ; preds = %normal-dest, %entry
  %2 = landingpad %exception-struct personality i32 (...)* @__gxx_personality_v0
          cleanup, !dbg !6
  %3 = extractvalue %exception-struct %2, 0, !dbg !6
  store i8* %3, i8** %exn.slot, !dbg !6
  %4 = extractvalue %exception-struct %2, 1, !dbg !6
  store i32 %4, i32* %ehselector.slot, !dbg !6
  call void @debugPrintI32(i32 100), !dbg !6
  br label %ehcleanup, !dbg !6

ehcleanup:                                        ; preds = %cleanup-landing-pad
  br label %ehresume, !dbg !6

ehresume:                                         ; preds = %ehcleanup
  call void @debugPrintI32(i32 101), !dbg !6
  %exn7 = load i8** %exn.slot, !dbg !6
  %sel = load i32* %ehselector.slot, !dbg !6
  %lpad.val = insertvalue %exception-struct undef, i8* %exn7, 0, !dbg !6
  call void @debugPrintI32(i32 90), !dbg !6
  %lpad.val8 = insertvalue %exception-struct %lpad.val, i32 %sel, 1, !dbg !6
  call void @debugPrintI32(i32 91), !dbg !6
  resume %exception-struct %lpad.val8, !dbg !6

terminate-lpad:                                   ; No predecessors!
  %5 = landingpad %exception-struct personality i32 (...)* @__gxx_personality_v0
          catch i8* null, !dbg !6
  call void @debugPrintI32(i32 9999), !dbg !6
  call void @terminate(), !dbg !6
  unreachable, !dbg !6
}

!0 = metadata !{i32 975, i32 4, metadata !1, null}
!1 = metadata !{i32 524329, metadata !"compiler.lsp", metadata !"/Users/meister/Development/cando/build/cando.app/Contents/Resources/csc/CANDO/init/compiler", metadata !"unused"}
!2 = metadata !{i32 276, i32 4, metadata !1, null}
!3 = metadata !{i32 39, i32 4, metadata !4, null}
!4 = metadata !{i32 524329, metadata !"llvm-ir.lsp", metadata !"/Users/meister/Development/cando/build/cando.app/Contents/Resources/csc/CANDO/init/compiler", metadata !"unused"}
!5 = metadata !{i32 985, i32 6, metadata !1, null}
!6 = metadata !{i32 28, i32 4, metadata !4, null}
