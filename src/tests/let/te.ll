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

@":::str" = private unnamed_addr constant [7 x i8] c"te.lsp\00"
@":::symbol-name-CONSTANTLY" = private unnamed_addr constant [11 x i8] c"CONSTANTLY\00"
@":::package-name-CORE" = private unnamed_addr constant [5 x i8] c"CORE\00"
@"CORE:FSET" = external constant %Sym_sp

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

declare i32 @activationFrameSize(%ActivationFrame_sp*) nounwind

declare %ActivationFrame_sp* @activationFrameParentRef(%ActivationFrame_sp*) nounwind

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

declare i32 @trace_enterFunctionScope(i8*, i32, i32, %ActivationFrame_sp*) nounwind

declare i32 @trace_enterLetScope(i8*, i32, i32, %ActivationFrame_sp*) nounwind

declare i32 @trace_enterLetSTARScope(i8*, i32, i32, %ActivationFrame_sp*) nounwind

declare i32 @trace_enterFletScope(i8*, i32, i32, %ActivationFrame_sp*) nounwind

declare i32 @trace_enterLabelsScope(i8*, i32, i32, %ActivationFrame_sp*) nounwind

declare i32 @trace_enterCallScope(i8*, i32, i32, %ActivationFrame_sp*) nounwind

declare void @trace_exitLexicalScope(i32) nounwind

declare void @throwCatchThrow(%T_sp*, %T_sp*)

declare void @catchStoreTag(%T_sp*, %T_sp*) nounwind

declare i32 @catchTagMatches(%T_sp*, i8*) nounwind

declare void @catchUnwind(%T_sp*) nounwind

declare void @catchStoreResult(%T_sp*, i8*) nounwind

declare void @terminate() nounwind

declare i32 @__gxx_personality_v0(...) nounwind

declare i8* @__cxa_begin_catch(i8*) nounwind

declare void @__cxa_end_catch()

declare void @__cxa_rethrow()

declare i32 @llvm.eh.typeid.for(i8*) nounwind readnone

declare void @_Unwind_Resume(i8*)

define void @repl(%T_sp* %result-ptr, %ActivationFrame_sp* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*, !dbg !0
  %ehselector.slot = alloca i32, !dbg !0
  %call-args = alloca %ActivationFrame_sp, !dbg !0
  %":::alloca-end" = alloca i32, i32 0, !dbg !0
  call void @newAFsp(%ActivationFrame_sp* %call-args), !dbg !0
  %":::new-end" = alloca i32, i32 0, !dbg !0
  %":::setup-end" = alloca i32, i32 0, !dbg !0
  %0 = call %ActivationFrame_sp* @activationFrameNil(), !dbg !2
  %trace-call = call i32 @trace_enterCallScope(i8* getelementptr inbounds ([7 x i8]* @":::str", i32 0, i32 0), i32 1, i32 3, %ActivationFrame_sp* %0), !dbg !2
  %1 = call %ActivationFrame_sp* @activationFrameNil(), !dbg !3
  call void @makeValueFrame(%ActivationFrame_sp* %call-args, i32 2, %ActivationFrame_sp* %1), !dbg !3
  %call-args-ref-0 = invoke %T_sp* @valueFrameReference(%ActivationFrame_sp* %call-args, i32 0)
          to label %normal-dest unwind label %call-unwind-landing-pad, !dbg !3

normal-dest:                                      ; preds = %entry
  invoke void @internSymbol(%T_sp* %call-args-ref-0, i8* getelementptr inbounds ([11 x i8]* @":::symbol-name-CONSTANTLY", i32 0, i32 0), i8* getelementptr inbounds ([5 x i8]* @":::package-name-CORE", i32 0, i32 0))
          to label %normal-dest1 unwind label %call-unwind-landing-pad, !dbg !3

normal-dest1:                                     ; preds = %normal-dest
  call void @firstValueIfMultipleValue(%T_sp* %call-args-ref-0), !dbg !3
  %call-args-ref-1 = invoke %T_sp* @valueFrameReference(%ActivationFrame_sp* %call-args, i32 1)
          to label %normal-dest2 unwind label %call-unwind-landing-pad, !dbg !3

normal-dest2:                                     ; preds = %normal-dest1
  %2 = call %ActivationFrame_sp* @activationFrameNil(), !dbg !4
  invoke void @makeClosure(%T_sp* %call-args-ref-1, void (%T_sp*, %ActivationFrame_sp*)* @CONSTANTLY, %ActivationFrame_sp* %2)
          to label %normal-dest3 unwind label %call-unwind-landing-pad, !dbg !4

normal-dest3:                                     ; preds = %normal-dest2
  call void @firstValueIfMultipleValue(%T_sp* %call-args-ref-1), !dbg !4
  invoke void @invokePossibleMultipleValueSymbolFunction(%T_sp* %result-ptr, %Sym_sp* @"CORE:FSET", %ActivationFrame_sp* %call-args)
          to label %call-cont-block unwind label %call-unwind-landing-pad, !dbg !4

call-unwind-landing-pad:                          ; preds = %normal-dest3, %normal-dest2, %normal-dest1, %normal-dest, %entry
  %3 = landingpad %exception-struct personality i32 (...)* @__gxx_personality_v0
          catch i8* null, !dbg !4
  %4 = extractvalue %exception-struct %3, 0, !dbg !4
  store i8* %4, i8** %exn.slot, !dbg !4
  %5 = extractvalue %exception-struct %3, 1, !dbg !4
  store i32 %5, i32* %ehselector.slot, !dbg !4
  %exn = load i8** %exn.slot, !dbg !4
  %6 = call i8* @__cxa_begin_catch(i8* %exn), !dbg !4
  call void @trace_exitLexicalScope(i32 %trace-call), !dbg !4
  invoke void @__cxa_rethrow()
          to label %normal-dest5 unwind label %cleanup-landing-pad, !dbg !5

normal-dest5:                                     ; preds = %call-unwind-landing-pad
  invoke void @__cxa_end_catch()
          to label %ehresume unwind label %cleanup-landing-pad, !dbg !5

call-cont-block:                                  ; preds = %normal-dest3
  call void @trace_exitLexicalScope(i32 %trace-call), !dbg !5
  call void @destructAFsp(%ActivationFrame_sp* %call-args), !dbg !7
  ret void, !dbg !7

cleanup-landing-pad:                              ; preds = %normal-dest5, %call-unwind-landing-pad
  %7 = landingpad %exception-struct personality i32 (...)* @__gxx_personality_v0
          cleanup, !dbg !7
  %8 = extractvalue %exception-struct %7, 0, !dbg !7
  store i8* %8, i8** %exn.slot, !dbg !7
  %9 = extractvalue %exception-struct %7, 1, !dbg !7
  store i32 %9, i32* %ehselector.slot, !dbg !7
  br label %ehresume, !dbg !7

ehresume:                                         ; preds = %cleanup-landing-pad, %normal-dest5
  call void @destructAFsp(%ActivationFrame_sp* %call-args), !dbg !7
  %exn7 = load i8** %exn.slot, !dbg !8
  invoke void @_Unwind_Resume(i8* %exn7)
          to label %normal-dest7 unwind label %terminate-lpad, !dbg !8

normal-dest7:                                     ; preds = %ehresume
  unreachable, !dbg !8

terminate-lpad:                                   ; preds = %ehresume
  %10 = landingpad %exception-struct personality i32 (...)* @__gxx_personality_v0
          catch i8* null, !dbg !8
  call void @terminate(), !dbg !8
  unreachable, !dbg !8
}

define void @CONSTANTLY(%T_sp* %result-ptr, %ActivationFrame_sp* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*, !dbg !9
  %ehselector.slot = alloca i32, !dbg !9
  %lambda-args = alloca %ActivationFrame_sp, !dbg !9
  %":::alloca-end" = alloca i32, i32 0, !dbg !9
  call void @newAFsp(%ActivationFrame_sp* %lambda-args), !dbg !9
  %":::new-end" = alloca i32, i32 0, !dbg !9
  %":::setup-end" = alloca i32, i32 0, !dbg !9
  %trace-FN = call i32 @trace_enterFunctionScope(i8* getelementptr inbounds ([7 x i8]* @":::str", i32 0, i32 0), i32 2, i32 10, %ActivationFrame_sp* %lambda-args), !dbg !9
  %given-num-args = call i32 @activationFrameSize(%ActivationFrame_sp* %activation-frame-ptr), !dbg !9
  %correct-num-args = icmp ne i32 %given-num-args, 1, !dbg !9
  br i1 %correct-num-args, label %error, label %continue3, !dbg !9

error:                                            ; preds = %entry
  %enough-args = icmp slt i32 %given-num-args, 1, !dbg !9
  br i1 %enough-args, label %error1, label %continue, !dbg !9

error1:                                           ; preds = %error
  invoke void @throwNotEnoughArgumentsException(i32 %given-num-args, i32 1)
          to label %normal-dest unwind label %ehresume, !dbg !9

normal-dest:                                      ; preds = %error1
  unreachable, !dbg !9

continue:                                         ; preds = %error
  invoke void @throwTooManyArgumentsException(i32 %given-num-args, i32 1)
          to label %normal-dest2 unwind label %ehresume, !dbg !9

normal-dest2:                                     ; preds = %continue
  unreachable, !dbg !9

continue3:                                        ; preds = %entry
  call void @copyAFsp(%ActivationFrame_sp* %activation-frame-ptr, %ActivationFrame_sp* %lambda-args), !dbg !9
  invoke void @makeClosure(%T_sp* %result-ptr, void (%T_sp*, %ActivationFrame_sp*)* @std-function, %ActivationFrame_sp* %lambda-args)
          to label %return-from-cont unwind label %ehresume, !dbg !4

return-from-cont:                                 ; preds = %continue3
  call void @trace_exitLexicalScope(i32 %trace-FN), !dbg !4
  call void @destructAFsp(%ActivationFrame_sp* %lambda-args), !dbg !4
  ret void, !dbg !4

ehresume:                                         ; preds = %continue3, %continue, %error1
  %0 = landingpad %exception-struct personality i32 (...)* @__gxx_personality_v0
          cleanup, !dbg !4
  %1 = extractvalue %exception-struct %0, 0, !dbg !4
  store i8* %1, i8** %exn.slot, !dbg !4
  %2 = extractvalue %exception-struct %0, 1, !dbg !4
  store i32 %2, i32* %ehselector.slot, !dbg !4
  call void @trace_exitLexicalScope(i32 %trace-FN), !dbg !4
  call void @destructAFsp(%ActivationFrame_sp* %lambda-args), !dbg !4
  %exn7 = load i8** %exn.slot, !dbg !8
  invoke void @_Unwind_Resume(i8* %exn7)
          to label %normal-dest5 unwind label %terminate-lpad, !dbg !8

normal-dest5:                                     ; preds = %ehresume
  unreachable, !dbg !8

terminate-lpad:                                   ; preds = %ehresume
  %3 = landingpad %exception-struct personality i32 (...)* @__gxx_personality_v0
          catch i8* null, !dbg !8
  call void @terminate(), !dbg !8
  unreachable, !dbg !8
}

define void @std-function(%T_sp* %result-ptr, %ActivationFrame_sp* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*, !dbg !9
  %ehselector.slot = alloca i32, !dbg !9
  %lambda-args = alloca %ActivationFrame_sp, !dbg !9
  %arg-idx = alloca i32, !dbg !9
  %temp-rest = alloca %T_sp, !dbg !9
  %":::alloca-end" = alloca i32, i32 0, !dbg !9
  call void @newAFsp(%ActivationFrame_sp* %lambda-args), !dbg !9
  store i32 0, i32* %arg-idx, !dbg !9
  call void @newTsp(%T_sp* %temp-rest), !dbg !9
  %":::new-end" = alloca i32, i32 0, !dbg !9
  %":::setup-end" = alloca i32, i32 0, !dbg !9
  %trace-FN = call i32 @trace_enterFunctionScope(i8* getelementptr inbounds ([7 x i8]* @":::str", i32 0, i32 0), i32 2, i32 58, %ActivationFrame_sp* %lambda-args), !dbg !9
  %0 = call %ActivationFrame_sp* @activationFrameParentRef(%ActivationFrame_sp* %activation-frame-ptr), !dbg !9
  call void @makeValueFrame(%ActivationFrame_sp* %lambda-args, i32 1, %ActivationFrame_sp* %0), !dbg !9
  %1 = call i32 @activationFrameSize(%ActivationFrame_sp* %activation-frame-ptr), !dbg !9
  %enough-args = icmp slt i32 %1, 0, !dbg !9
  br i1 %enough-args, label %error, label %rest-arguments-start, !dbg !9

error:                                            ; preds = %entry
  invoke void @throwNotEnoughArgumentsException(i32 %1, i32 0)
          to label %normal-dest unwind label %cleanup-landing-pad, !dbg !9

normal-dest:                                      ; preds = %error
  unreachable, !dbg !9

rest-arguments-start:                             ; preds = %entry
  %arg-idx-val = load i32* %arg-idx, !dbg !10
  invoke void @fillRestTarget(%T_sp* %temp-rest, %ActivationFrame_sp* %activation-frame-ptr, i32 %arg-idx-val)
          to label %normal-dest1 unwind label %cleanup-landing-pad, !dbg !10

normal-dest1:                                     ; preds = %rest-arguments-start
  %dest-frame-0 = invoke %T_sp* @valueFrameReference(%ActivationFrame_sp* %lambda-args, i32 0)
          to label %normal-dest2 unwind label %cleanup-landing-pad, !dbg !10

normal-dest2:                                     ; preds = %normal-dest1
  call void @copyTsp(%T_sp* %temp-rest, %T_sp* %dest-frame-0), !dbg !12
  invoke void @lexicalValueRead(%T_sp* %result-ptr, i32 1, i32 0, %ActivationFrame_sp* %lambda-args)
          to label %normal-dest3 unwind label %cleanup-landing-pad, !dbg !13

normal-dest3:                                     ; preds = %normal-dest2
  call void @destructTsp(%T_sp* %temp-rest), !dbg !13
  call void @trace_exitLexicalScope(i32 %trace-FN), !dbg !13
  call void @destructAFsp(%ActivationFrame_sp* %lambda-args), !dbg !13
  ret void, !dbg !13

cleanup-landing-pad:                              ; preds = %normal-dest2, %normal-dest1, %rest-arguments-start, %error
  %2 = landingpad %exception-struct personality i32 (...)* @__gxx_personality_v0
          cleanup, !dbg !13
  %3 = extractvalue %exception-struct %2, 0, !dbg !13
  store i8* %3, i8** %exn.slot, !dbg !13
  %4 = extractvalue %exception-struct %2, 1, !dbg !13
  store i32 %4, i32* %ehselector.slot, !dbg !13
  br label %ehcleanup, !dbg !13

ehcleanup:                                        ; preds = %cleanup-landing-pad
  call void @destructTsp(%T_sp* %temp-rest), !dbg !13
  call void @trace_exitLexicalScope(i32 %trace-FN), !dbg !13
  call void @destructAFsp(%ActivationFrame_sp* %lambda-args), !dbg !13
  br label %ehresume, !dbg !13

ehresume:                                         ; preds = %ehcleanup
  %exn7 = load i8** %exn.slot, !dbg !8
  invoke void @_Unwind_Resume(i8* %exn7)
          to label %normal-dest4 unwind label %terminate-lpad, !dbg !8

normal-dest4:                                     ; preds = %ehresume
  unreachable, !dbg !8

terminate-lpad:                                   ; preds = %ehresume
  %5 = landingpad %exception-struct personality i32 (...)* @__gxx_personality_v0
          catch i8* null, !dbg !8
  call void @terminate(), !dbg !8
  unreachable, !dbg !8
}

define void @___main___(%T_sp* %result-ptr, %ActivationFrame_sp* %activation-frame-ptr) {
entry:
  %exn.slot = alloca i8*, !dbg !8
  %ehselector.slot = alloca i32, !dbg !8
  %":::alloca-end" = alloca i32, i32 0, !dbg !8
  %":::new-end" = alloca i32, i32 0, !dbg !8
  %":::setup-end" = alloca i32, i32 0, !dbg !8
  %0 = call %ActivationFrame_sp* @activationFrameNil(), !dbg !8
  invoke void @invokeLlvmFunction(%T_sp* %result-ptr, void (%T_sp*, %ActivationFrame_sp*)* @repl, %ActivationFrame_sp* %0)
          to label %normal-dest unwind label %cleanup-landing-pad, !dbg !8

normal-dest:                                      ; preds = %entry
  ret void, !dbg !8

cleanup-landing-pad:                              ; preds = %entry
  %1 = landingpad %exception-struct personality i32 (...)* @__gxx_personality_v0
          cleanup, !dbg !8
  %2 = extractvalue %exception-struct %1, 0, !dbg !8
  store i8* %2, i8** %exn.slot, !dbg !8
  %3 = extractvalue %exception-struct %1, 1, !dbg !8
  store i32 %3, i32* %ehselector.slot, !dbg !8
  br label %ehcleanup, !dbg !8

ehcleanup:                                        ; preds = %cleanup-landing-pad
  br label %ehresume, !dbg !8

ehresume:                                         ; preds = %ehcleanup
  %exn7 = load i8** %exn.slot, !dbg !8
  invoke void @_Unwind_Resume(i8* %exn7)
          to label %normal-dest1 unwind label %terminate-lpad, !dbg !8

normal-dest1:                                     ; preds = %ehresume
  unreachable, !dbg !8

terminate-lpad:                                   ; preds = %ehresume
  %4 = landingpad %exception-struct personality i32 (...)* @__gxx_personality_v0
          catch i8* null, !dbg !8
  call void @terminate(), !dbg !8
  unreachable, !dbg !8
}

!0 = metadata !{i32 1045, i32 4, metadata !1, null}
!1 = metadata !{i32 524329, metadata !"compiler.lsp", metadata !"/Users/meister/Development/cando/build/cando.app/Contents/Resources/csc/CANDO/init/compiler", metadata !"unused"}
!2 = metadata !{i32 1053, i32 6, metadata !1, null}
!3 = metadata !{i32 268, i32 4, metadata !1, null}
!4 = metadata !{i32 224, i32 5, metadata !1, null}
!5 = metadata !{i32 51, i32 4, metadata !6, null}
!6 = metadata !{i32 524329, metadata !"llvm-ir.lsp", metadata !"/Users/meister/Development/cando/build/cando.app/Contents/Resources/csc/CANDO/init/compiler", metadata !"unused"}
!7 = metadata !{i32 1055, i32 6, metadata !1, null}
!8 = metadata !{i32 32, i32 4, metadata !6, null}
!9 = metadata !{i32 208, i32 8, metadata !1, null}
!10 = metadata !{i32 137, i32 6, metadata !11, null}
!11 = metadata !{i32 524329, metadata !"lambda-list.lsp", metadata !"/Users/meister/Development/cando/build/cando.app/Contents/Resources/csc/CANDO/init/compiler", metadata !"unused"}
!12 = metadata !{i32 49, i32 6, metadata !11, null}
!13 = metadata !{i32 27, i32 6, metadata !14, null}
!14 = metadata !{i32 524329, metadata !"compile-var-lookups.lsp", metadata !"/Users/meister/Development/cando/build/cando.app/Contents/Resources/csc/CANDO/init/compiler", metadata !"unused"}
