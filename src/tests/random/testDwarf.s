	.section	__TEXT,__text,regular,pure_instructions
	.section	__DWARF,__debug_info,regular,debug
Lsection_info:
	.section	__DWARF,__debug_abbrev,regular,debug
Lsection_abbrev:
	.section	__DWARF,__debug_aranges,regular,debug
	.section	__DWARF,__debug_macinfo,regular,debug
	.section	__DWARF,__debug_line,regular,debug
Lsection_line:
	.section	__DWARF,__debug_loc,regular,debug
	.section	__DWARF,__debug_str,regular,debug
Linfo_string:
	.section	__DWARF,__debug_ranges,regular,debug
Ldebug_range:
	.section	__DWARF,__debug_loc,regular,debug
Lsection_debug_loc:
	.section	__TEXT,__text,regular,pure_instructions
Ltext_begin:
	.section	__DATA,__data
	.section	__TEXT,__text,regular,pure_instructions
	.align	4, 0x90
____loadTimeDataInitializer:            ## @___loadTimeDataInitializer
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
Leh_func_begin0:
	.cfi_lsda 16, Lexception0
## BB#0:                                ## %entry
	pushq	%r15
Ltmp19:
	.cfi_def_cfa_offset 16
	pushq	%r14
Ltmp20:
	.cfi_def_cfa_offset 24
	pushq	%r13
Ltmp21:
	.cfi_def_cfa_offset 32
	pushq	%r12
Ltmp22:
	.cfi_def_cfa_offset 40
	pushq	%rbx
Ltmp23:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
Ltmp24:
	.cfi_def_cfa_offset 128
Ltmp25:
	.cfi_offset %rbx, -48
Ltmp26:
	.cfi_offset %r12, -40
Ltmp27:
	.cfi_offset %r13, -32
Ltmp28:
	.cfi_offset %r14, -24
Ltmp29:
	.cfi_offset %r15, -16
	movl	$0, 68(%rsp)
	leaq	40(%rsp), %r14
	movq	%r14, %rdi
	callq	_newTmv
	leaq	24(%rsp), %r15
	movq	%r15, %rdi
	callq	_newTsp
	leaq	8(%rsp), %rdi
	callq	_newTsp
	leaq	"_load-time-value-vector"(%rip), %rbx
	leaq	"_:::global-str-/Users/meister/Development/cando/brcl/src/tests/random/testDwarf.lsp"(%rip), %rsi
	movq	%rbx, %rdi
	movl	$9, %edx
	movl	$3, %ecx
	callq	_getOrCreateLoadTimeValueArray
	movq	%rbx, %rdi
	xorl	%esi, %esi
	callq	_loadTimeValueReference
	movq	%rax, %rdi
	callq	_sp_makeNil
	movq	%rbx, %rdi
	movl	$1, %esi
	callq	_loadTimeValueReference
	movq	%rax, %rdi
	callq	_makeT
	movq	%rbx, %rdi
	movl	$2, %esi
	callq	_loadTimeValueReference
	leaq	"_:::symbol-name-A"(%rip), %rsi
	leaq	"_:::package-name-CORE"(%rip), %r12
	movq	%rax, %rdi
	movq	%r12, %rdx
	callq	_internSymbol_tsp
	movq	%rbx, %rdi
	movl	$3, %esi
	callq	_loadTimeValueReference
	leaq	"_:::symbol-name-T"(%rip), %rsi
	leaq	"_:::package-name-COMMON-LISP"(%rip), %r13
	movq	%rax, %rdi
	movq	%r13, %rdx
	callq	_internSymbol_tsp
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movl	$3, %edx
	callq	_sp_copyLoadTimeValue
	movq	%rbx, %rdi
	movl	$4, %esi
	callq	_loadTimeValueReference
	leaq	"_constant-array"(%rip), %rcx
	movq	%rax, %rdi
	movq	%r15, %rsi
	movl	$1, %edx
	callq	_ltv_makeArrayObjects
	movq	%rbx, %rdi
	movl	$4, %esi
	callq	_loadTimeValueReference
	leaq	"_constant-array1"(%rip), %rdx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	_ltv_initializeArrayObjectsRowMajorArefOrder
	movq	%rbx, %rdi
	movl	$5, %esi
	callq	_loadTimeValueReference
	leaq	"_:::str"(%rip), %rsi
	movq	%rax, %rdi
	callq	_makeString
	movq	%rbx, %rdi
	xorl	%esi, %esi
	callq	_loadTimeSymbolReference
	leaq	"_:::symbol-name-PRINT"(%rip), %rsi
	movq	%rax, %rdi
	movq	%r13, %rdx
	callq	_internSymbol_symsp
	movq	%rbx, %rdi
	movl	$1, %esi
	callq	_loadTimeSymbolReference
	leaq	"_:::symbol-name-*FSET"(%rip), %rsi
	movq	%rax, %rdi
	movq	%r12, %rdx
	callq	_internSymbol_symsp
	callq	_activationFrameNil
	leaq	_repl(%rip), %rsi
Ltmp0:
	movq	%r14, %rdi
	movq	%rax, %rdx
	callq	_invokeLlvmFunction
Ltmp1:
## BB#1:                                ## %normal-dest
	callq	_activationFrameNil
Ltmp2:
	leaq	40(%rsp), %rdi
	leaq	_repl2(%rip), %rsi
	movq	%rax, %rdx
	callq	_invokeLlvmFunction
Ltmp3:
## BB#2:                                ## %normal-dest1
	callq	_activationFrameNil
Ltmp4:
	leaq	40(%rsp), %rdi
	leaq	_repl3(%rip), %rsi
	movq	%rax, %rdx
	callq	_invokeLlvmFunction
Ltmp5:
## BB#3:                                ## %normal-dest2
	leaq	"_load-time-value-vector"(%rip), %rbx
	movq	%rbx, %rdi
	movl	$6, %esi
	callq	_loadTimeValueReference
	leaq	"_:::symbol-name-B"(%rip), %rsi
	leaq	"_:::package-name-CORE"(%rip), %r14
	movq	%rax, %rdi
	movq	%r14, %rdx
	callq	_internSymbol_tsp
	leaq	8(%rsp), %r15
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movl	$3, %edx
	callq	_sp_copyLoadTimeValue
	movq	%rbx, %rdi
	movl	$7, %esi
	callq	_loadTimeValueReference
	leaq	"_constant-array5"(%rip), %rcx
	movq	%rax, %rdi
	movq	%r15, %rsi
	movl	$1, %edx
	callq	_ltv_makeArrayObjects
	movq	%rbx, %rdi
	movl	$7, %esi
	callq	_loadTimeValueReference
	leaq	"_constant-array6"(%rip), %rdx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	_ltv_initializeArrayObjectsRowMajorArefOrder
	movq	%rbx, %rdi
	movl	$2, %esi
	callq	_loadTimeSymbolReference
	leaq	"_:::symbol-name-A"(%rip), %rsi
	movq	%rax, %rdi
	movq	%r14, %rdx
	callq	_internSymbol_symsp
	movq	%rbx, %rdi
	movl	$8, %esi
	callq	_loadTimeValueReference
	leaq	"_:::str7"(%rip), %rsi
	movq	%rax, %rdi
	callq	_makeString
	callq	_activationFrameNil
	leaq	40(%rsp), %rdi
	leaq	_repl4(%rip), %rsi
Ltmp6:
	movq	%rax, %rdx
	callq	_invokeLlvmFunction
Ltmp7:
## BB#4:                                ## %normal-dest4
	callq	_activationFrameNil
Ltmp8:
	leaq	40(%rsp), %rdi
	leaq	_repl8(%rip), %rsi
	movq	%rax, %rdx
	callq	_invokeLlvmFunction
Ltmp9:
## BB#5:                                ## %normal-dest5
	callq	_activationFrameNil
Ltmp10:
	leaq	40(%rsp), %rdi
	leaq	_repl9(%rip), %rsi
	movq	%rax, %rdx
	callq	_invokeLlvmFunction
Ltmp11:
## BB#6:                                ## %normal-dest6
	leaq	8(%rsp), %rdi
	callq	_destructTsp
	leaq	24(%rsp), %rdi
	callq	_destructTsp
	leaq	40(%rsp), %rdi
	callq	_destructTmv
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
LBB0_7:                                 ## %func-cleanup-landing-pad
Ltmp12:
	movq	%rdx, %rcx
	movq	%rax, %rbx
	movq	%rbx, 72(%rsp)
	movl	%ecx, 68(%rsp)
	leaq	8(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	24(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	40(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTmv
	movq	%rbx, %rdi
	callq	__Unwind_Resume
	.cfi_endproc
Leh_func_end0:
	.section	__TEXT,__gcc_except_tab
	.align	2
GCC_except_table0:
Lexception0:
	.byte	255                     ## @LPStart Encoding = omit
	.byte	155                     ## @TType Encoding = indirect pcrel sdata4
	.asciz	"\234"                  ## @TType base offset
	.byte	3                       ## Call site Encoding = udata4
	.byte	26                      ## Call site table length
Lset0 = Ltmp0-Leh_func_begin0           ## >> Call Site 1 <<
	.long	Lset0
Lset1 = Ltmp11-Ltmp0                    ##   Call between Ltmp0 and Ltmp11
	.long	Lset1
Lset2 = Ltmp12-Leh_func_begin0          ##     jumps to Ltmp12
	.long	Lset2
	.byte	0                       ##   On action: cleanup
Lset3 = Ltmp11-Leh_func_begin0          ## >> Call Site 2 <<
	.long	Lset3
Lset4 = Leh_func_end0-Ltmp11            ##   Call between Ltmp11 and Leh_func_end0
	.long	Lset4
	.long	0                       ##     has no landing pad
	.byte	0                       ##   On action: cleanup
	.align	2

	.section	__TEXT,__text,regular,pure_instructions
	.align	4, 0x90
_repl:                                  ## @repl
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
Leh_func_begin1:
	.cfi_lsda 16, Lexception1
Lfunc_begin1:
## BB#0:                                ## %entry
Ltmp41:
	pushq	%r15
Ltmp42:
	.cfi_def_cfa_offset 16
	pushq	%r14
Ltmp43:
	.cfi_def_cfa_offset 24
	pushq	%r13
Ltmp44:
	.cfi_def_cfa_offset 32
	pushq	%r12
Ltmp45:
	.cfi_def_cfa_offset 40
	pushq	%rbx
Ltmp46:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
Ltmp47:
	.cfi_def_cfa_offset 80
Ltmp48:
	.cfi_offset %rbx, -48
Ltmp49:
	.cfi_offset %r12, -40
Ltmp50:
	.cfi_offset %r13, -32
Ltmp51:
	.cfi_offset %r14, -24
Ltmp52:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movl	$0, 20(%rsp)
	leaq	(%rsp), %rbx
	movq	%rbx, %rdi
	callq	_newAFsp
	movl	$2, %edi
	movl	$13, %esi
Ltmp53:
Ltmp54:
	callq	_trace_setLineNumberColumnForIHSTop
	movq	%rbx, %rdi
	movl	$3, %esi
	movl	$2000000, %edx          ## imm = 0x1E8480
	callq	_makeValueFrame
	callq	_activationFrameNil
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	_setParentOfActivationFrame
	movq	%rbx, %rdi
	xorl	%esi, %esi
	callq	_valueFrameReference
	leaq	"_load-time-value-vector"(%rip), %r15
	movq	%rax, %rdi
	movq	%r15, %rsi
	movl	$2, %edx
	callq	_sp_copyLoadTimeValue
	movq	%rbx, %rdi
	movl	$1, %esi
	callq	_valueFrameReference
	movq	%rax, %r12
	movq	%r15, %rdi
	xorl	%esi, %esi
	callq	_loadTimeValueReference
	movq	%rax, %r13
	movq	%r15, %rdi
	movl	$2, %esi
	callq	_loadTimeValueReference
	movq	%rax, %rbx
	callq	_activationFrameNil
	leaq	_A(%rip), %rsi
	leaq	"_:::global-str-/Users/meister/Development/cando/brcl/src/tests/random/testDwarf.lsp"(%rip), %rdx
Ltmp30:
	movq	%r12, %rdi
	movq	%rbx, %rcx
	movq	%r13, %r8
	movq	%rax, %r9
	callq	_sp_makeCompiledFunction
Ltmp31:
## BB#1:                                ## %normal-dest
	leaq	(%rsp), %r15
	movq	%r15, %rdi
	movl	$2, %esi
	callq	_valueFrameReference
	leaq	"_load-time-value-vector"(%rip), %rbx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	xorl	%edx, %edx
	callq	_sp_copyLoadTimeValue
	callq	_singleStepCallback
	movq	%rbx, %rdi
	movl	$1, %esi
	callq	_loadTimeSymbolReference
Ltmp32:
	movq	%r14, %rdi
	movq	%rax, %rsi
	movq	%r15, %rdx
	callq	_mv_invokePossibleMultipleValueSymbolFunction
Ltmp33:
## BB#2:                                ## %normal-dest1
	leaq	(%rsp), %rdi
	callq	_destructAFsp
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
LBB1_3:                                 ## %func-cleanup-landing-pad
Ltmp34:
	movq	%rdx, %rcx
	movq	%rax, %rbx
	movq	%rbx, 24(%rsp)
	movl	%ecx, 20(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	movq	%rbx, %rdi
	callq	__Unwind_Resume
Ltmp55:
Lfunc_end1:
	.cfi_endproc
Leh_func_end1:
	.section	__TEXT,__gcc_except_tab
	.align	2
GCC_except_table1:
Lexception1:
	.byte	255                     ## @LPStart Encoding = omit
	.byte	155                     ## @TType Encoding = indirect pcrel sdata4
	.asciz	"\234"                  ## @TType base offset
	.byte	3                       ## Call site Encoding = udata4
	.byte	26                      ## Call site table length
Lset5 = Ltmp30-Leh_func_begin1          ## >> Call Site 1 <<
	.long	Lset5
Lset6 = Ltmp33-Ltmp30                   ##   Call between Ltmp30 and Ltmp33
	.long	Lset6
Lset7 = Ltmp34-Leh_func_begin1          ##     jumps to Ltmp34
	.long	Lset7
	.byte	0                       ##   On action: cleanup
Lset8 = Ltmp33-Leh_func_begin1          ## >> Call Site 2 <<
	.long	Lset8
Lset9 = Leh_func_end1-Ltmp33            ##   Call between Ltmp33 and Leh_func_end1
	.long	Lset9
	.long	0                       ##     has no landing pad
	.byte	0                       ##   On action: cleanup
	.align	2

	.section	__TEXT,__text,regular,pure_instructions
	.align	4, 0x90
_A:                                     ## @A
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
Leh_func_begin2:
	.cfi_lsda 16, Lexception2
Lfunc_begin2:
## BB#0:                                ## %entry
Ltmp74:
	pushq	%r15
Ltmp75:
	.cfi_def_cfa_offset 16
	pushq	%r14
Ltmp76:
	.cfi_def_cfa_offset 24
	pushq	%r12
Ltmp77:
	.cfi_def_cfa_offset 32
	pushq	%rbx
Ltmp78:
	.cfi_def_cfa_offset 40
	subq	$72, %rsp
Ltmp79:
	.cfi_def_cfa_offset 112
Ltmp80:
	.cfi_offset %rbx, -40
Ltmp81:
	.cfi_offset %r12, -32
Ltmp82:
	.cfi_offset %r14, -24
Ltmp83:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movl	$0, 60(%rsp)
	leaq	40(%rsp), %rdi
	callq	_newAFsp
	leaq	24(%rsp), %rdi
	callq	_newTsp
	leaq	8(%rsp), %rdi
	callq	_newAFsp
	movq	%rbx, %rdi
	callq	_activationFrameSize
	testl	%eax, %eax
	jne	LBB2_1
LBB2_4:                                 ## %continue3
	leaq	40(%rsp), %r15
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	_copyAFsp
	leaq	"_load-time-value-vector"(%rip), %r12
	movq	%r12, %rdi
	movl	$4, %esi
	callq	_loadTimeValueReference
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	_attachDebuggingInfoToValueFrame
	movq	%r15, %rdi
	callq	_trace_setActivationFrameForIHSTop
	movl	$2, %edi
	movl	$13, %esi
Ltmp84:
Ltmp85:
	callq	_trace_setLineNumberColumnForIHSTop
	leaq	8(%rsp), %rbx
	movq	%rbx, %rdi
	movl	$1, %esi
	movl	$2000001, %edx          ## imm = 0x1E8481
	callq	_makeValueFrame
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	_setParentOfActivationFrame
	movq	%rbx, %rdi
	xorl	%esi, %esi
	callq	_valueFrameReference
	movq	%rax, %rdi
	movq	%r12, %rsi
	movl	$5, %edx
	callq	_sp_copyLoadTimeValue
	callq	_singleStepCallback
	movq	%r12, %rdi
	xorl	%esi, %esi
	callq	_loadTimeSymbolReference
Ltmp61:
	movq	%r14, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	_mv_invokePossibleMultipleValueSymbolFunction
Ltmp62:
LBB2_8:                                 ## %(TRY-0).try-cont
	leaq	8(%rsp), %rdi
	callq	_destructAFsp
	leaq	24(%rsp), %rdi
	callq	_destructTsp
	leaq	40(%rsp), %rdi
	callq	_destructAFsp
	addq	$72, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	ret
LBB2_1:                                 ## %error
	testl	%eax, %eax
	jns	LBB2_3
## BB#2:                                ## %error1
Ltmp58:
	leaq	"_:::global-str-A"(%rip), %rcx
	movq	%rcx, %rdi
	movq	%rbx, %rsi
	movl	%eax, %edx
	xorl	%ecx, %ecx
	callq	_throwNotEnoughArgumentsException
Ltmp59:
LBB2_3:                                 ## %continue
Ltmp56:
	leaq	"_:::global-str-A"(%rip), %rcx
	movq	%rcx, %rdi
	movq	%rbx, %rsi
	movl	%eax, %edx
	xorl	%ecx, %ecx
	callq	_throwTooManyArgumentsException
Ltmp57:
	jmp	LBB2_4
LBB2_10:                                ## %func-cleanup-landing-pad
Ltmp60:
	jmp	LBB2_11
LBB2_5:                                 ## %(TRY-0).landing-pad
Ltmp63:
	movq	%rdx, %rcx
	movq	%rax, %rbx
	movq	%rbx, 64(%rsp)
	movl	%ecx, 60(%rsp)
	cmpl	$1, %ecx
	jne	LBB2_12
## BB#6:                                ## %(TRY-0).handler-block6064
	movq	%rbx, %rdi
	callq	___cxa_begin_catch
Ltmp64:
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	_mv_blockHandleReturnFrom
Ltmp65:
## BB#7:                                ## %(TRY-0).normal-dest5
Ltmp66:
	callq	___cxa_end_catch
Ltmp67:
	jmp	LBB2_8
LBB2_9:                                 ## %(TRY-0).landing-pad8
Ltmp68:
LBB2_11:                                ## %func-ehcleanup
	movq	%rdx, %rcx
	movq	%rax, %rbx
	movq	%rbx, 64(%rsp)
	movl	%ecx, 60(%rsp)
LBB2_12:                                ## %func-ehcleanup
	leaq	8(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	leaq	24(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	40(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	movq	%rbx, %rdi
	callq	__Unwind_Resume
Ltmp86:
Lfunc_end2:
	.cfi_endproc
Leh_func_end2:
	.section	__TEXT,__gcc_except_tab
	.align	2
GCC_except_table2:
Lexception2:
	.byte	255                     ## @LPStart Encoding = omit
	.byte	155                     ## @TType Encoding = indirect pcrel sdata4
	.asciz	"\276\200\200"          ## @TType base offset
	.byte	3                       ## Call site Encoding = udata4
	.byte	52                      ## Call site table length
Lset10 = Ltmp61-Leh_func_begin2         ## >> Call Site 1 <<
	.long	Lset10
Lset11 = Ltmp62-Ltmp61                  ##   Call between Ltmp61 and Ltmp62
	.long	Lset11
Lset12 = Ltmp63-Leh_func_begin2         ##     jumps to Ltmp63
	.long	Lset12
	.byte	3                       ##   On action: 2
Lset13 = Ltmp58-Leh_func_begin2         ## >> Call Site 2 <<
	.long	Lset13
Lset14 = Ltmp57-Ltmp58                  ##   Call between Ltmp58 and Ltmp57
	.long	Lset14
Lset15 = Ltmp60-Leh_func_begin2         ##     jumps to Ltmp60
	.long	Lset15
	.byte	0                       ##   On action: cleanup
Lset16 = Ltmp64-Leh_func_begin2         ## >> Call Site 3 <<
	.long	Lset16
Lset17 = Ltmp67-Ltmp64                  ##   Call between Ltmp64 and Ltmp67
	.long	Lset17
Lset18 = Ltmp68-Leh_func_begin2         ##     jumps to Ltmp68
	.long	Lset18
	.byte	0                       ##   On action: cleanup
Lset19 = Ltmp67-Leh_func_begin2         ## >> Call Site 4 <<
	.long	Lset19
Lset20 = Leh_func_end2-Ltmp67           ##   Call between Ltmp67 and Leh_func_end2
	.long	Lset20
	.long	0                       ##     has no landing pad
	.byte	0                       ##   On action: cleanup
	.byte	0                       ## >> Action Record 1 <<
                                        ##   Cleanup
	.byte	0                       ##   No further actions
	.byte	1                       ## >> Action Record 2 <<
                                        ##   Catch TypeInfo 1
	.byte	125                     ##   Continue to action 1
                                        ## >> Catch TypeInfos <<
	.long	__ZTIN4core10ReturnFromE@GOTPCREL+4 ## TypeInfo 1
	.align	2

	.section	__TEXT,__text,regular,pure_instructions
	.align	4, 0x90
_repl2:                                 ## @repl2
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%rax
Ltmp88:
	.cfi_def_cfa_offset 16
	leaq	"_load-time-value-vector"(%rip), %rsi
	xorl	%edx, %edx
	callq	_mv_copyLoadTimeValue
	popq	%rax
	ret
	.cfi_endproc

	.align	4, 0x90
_repl3:                                 ## @repl3
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%rax
Ltmp90:
	.cfi_def_cfa_offset 16
	leaq	"_load-time-value-vector"(%rip), %rsi
	movl	$2, %edx
	callq	_mv_copyLoadTimeValue
	popq	%rax
	ret
	.cfi_endproc

	.align	4, 0x90
_repl4:                                 ## @repl4
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
Leh_func_begin5:
	.cfi_lsda 16, Lexception5
Lfunc_begin5:
## BB#0:                                ## %entry
Ltmp102:
	pushq	%r15
Ltmp103:
	.cfi_def_cfa_offset 16
	pushq	%r14
Ltmp104:
	.cfi_def_cfa_offset 24
	pushq	%r13
Ltmp105:
	.cfi_def_cfa_offset 32
	pushq	%r12
Ltmp106:
	.cfi_def_cfa_offset 40
	pushq	%rbx
Ltmp107:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
Ltmp108:
	.cfi_def_cfa_offset 80
Ltmp109:
	.cfi_offset %rbx, -48
Ltmp110:
	.cfi_offset %r12, -40
Ltmp111:
	.cfi_offset %r13, -32
Ltmp112:
	.cfi_offset %r14, -24
Ltmp113:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movl	$0, 20(%rsp)
	leaq	(%rsp), %rbx
	movq	%rbx, %rdi
	callq	_newAFsp
	movl	$505, %edi              ## imm = 0x1F9
	movl	$3, %esi
Ltmp114:
Ltmp115:
	callq	_trace_setLineNumberColumnForIHSTop
	movq	%rbx, %rdi
	movl	$3, %esi
	movl	$2000002, %edx          ## imm = 0x1E8482
	callq	_makeValueFrame
	callq	_activationFrameNil
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	_setParentOfActivationFrame
	movq	%rbx, %rdi
	xorl	%esi, %esi
	callq	_valueFrameReference
	leaq	"_load-time-value-vector"(%rip), %r15
	movq	%rax, %rdi
	movq	%r15, %rsi
	movl	$6, %edx
	callq	_sp_copyLoadTimeValue
	movq	%rbx, %rdi
	movl	$1, %esi
	callq	_valueFrameReference
	movq	%rax, %r12
	movq	%r15, %rdi
	xorl	%esi, %esi
	callq	_loadTimeValueReference
	movq	%rax, %r13
	movq	%r15, %rdi
	movl	$6, %esi
	callq	_loadTimeValueReference
	movq	%rax, %rbx
	callq	_activationFrameNil
	leaq	_B(%rip), %rsi
	leaq	"_:::global-str-/Users/meister/Development/cando/brcl/src/tests/random/testDwarf.lsp"(%rip), %rdx
Ltmp91:
	movq	%r12, %rdi
	movq	%rbx, %rcx
	movq	%r13, %r8
	movq	%rax, %r9
	callq	_sp_makeCompiledFunction
Ltmp92:
## BB#1:                                ## %normal-dest
	leaq	(%rsp), %r15
	movq	%r15, %rdi
	movl	$2, %esi
	callq	_valueFrameReference
	leaq	"_load-time-value-vector"(%rip), %rbx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	xorl	%edx, %edx
	callq	_sp_copyLoadTimeValue
	callq	_singleStepCallback
	movq	%rbx, %rdi
	movl	$1, %esi
	callq	_loadTimeSymbolReference
Ltmp93:
	movq	%r14, %rdi
	movq	%rax, %rsi
	movq	%r15, %rdx
	callq	_mv_invokePossibleMultipleValueSymbolFunction
Ltmp94:
## BB#2:                                ## %normal-dest1
	leaq	(%rsp), %rdi
	callq	_destructAFsp
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
LBB5_3:                                 ## %func-cleanup-landing-pad
Ltmp95:
	movq	%rdx, %rcx
	movq	%rax, %rbx
	movq	%rbx, 24(%rsp)
	movl	%ecx, 20(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	movq	%rbx, %rdi
	callq	__Unwind_Resume
Ltmp116:
Lfunc_end5:
	.cfi_endproc
Leh_func_end5:
	.section	__TEXT,__gcc_except_tab
	.align	2
GCC_except_table5:
Lexception5:
	.byte	255                     ## @LPStart Encoding = omit
	.byte	155                     ## @TType Encoding = indirect pcrel sdata4
	.asciz	"\234"                  ## @TType base offset
	.byte	3                       ## Call site Encoding = udata4
	.byte	26                      ## Call site table length
Lset21 = Ltmp91-Leh_func_begin5         ## >> Call Site 1 <<
	.long	Lset21
Lset22 = Ltmp94-Ltmp91                  ##   Call between Ltmp91 and Ltmp94
	.long	Lset22
Lset23 = Ltmp95-Leh_func_begin5         ##     jumps to Ltmp95
	.long	Lset23
	.byte	0                       ##   On action: cleanup
Lset24 = Ltmp94-Leh_func_begin5         ## >> Call Site 2 <<
	.long	Lset24
Lset25 = Leh_func_end5-Ltmp94           ##   Call between Ltmp94 and Leh_func_end5
	.long	Lset25
	.long	0                       ##     has no landing pad
	.byte	0                       ##   On action: cleanup
	.align	2

	.section	__TEXT,__text,regular,pure_instructions
	.align	4, 0x90
_B:                                     ## @B
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
Leh_func_begin6:
	.cfi_lsda 16, Lexception6
Lfunc_begin6:
## BB#0:                                ## %entry
Ltmp137:
	pushq	%r15
Ltmp138:
	.cfi_def_cfa_offset 16
	pushq	%r14
Ltmp139:
	.cfi_def_cfa_offset 24
	pushq	%r12
Ltmp140:
	.cfi_def_cfa_offset 32
	pushq	%rbx
Ltmp141:
	.cfi_def_cfa_offset 40
	subq	$88, %rsp
Ltmp142:
	.cfi_def_cfa_offset 128
Ltmp143:
	.cfi_offset %rbx, -40
Ltmp144:
	.cfi_offset %r12, -32
Ltmp145:
	.cfi_offset %r14, -24
Ltmp146:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movl	$0, 76(%rsp)
	leaq	56(%rsp), %rdi
	callq	_newAFsp
	leaq	40(%rsp), %rdi
	callq	_newTsp
	leaq	24(%rsp), %rdi
	callq	_newAFsp
	leaq	8(%rsp), %rdi
	callq	_newAFsp
	movq	%rbx, %rdi
	callq	_activationFrameSize
	testl	%eax, %eax
	jne	LBB6_1
LBB6_4:                                 ## %continue3
	leaq	56(%rsp), %r15
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	_copyAFsp
	leaq	"_load-time-value-vector"(%rip), %r12
	movq	%r12, %rdi
	movl	$7, %esi
	callq	_loadTimeValueReference
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	_attachDebuggingInfoToValueFrame
	movq	%r15, %rdi
	callq	_trace_setActivationFrameForIHSTop
	movl	$5, %edi
	movl	$3, %esi
Ltmp147:
Ltmp148:
	callq	_trace_setLineNumberColumnForIHSTop
	leaq	24(%rsp), %rbx
	movq	%rbx, %rdi
	xorl	%esi, %esi
	movl	$2000003, %edx          ## imm = 0x1E8483
	callq	_makeValueFrame
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	_setParentOfActivationFrame
	callq	_singleStepCallback
	movq	%r12, %rdi
	movl	$2, %esi
	callq	_loadTimeSymbolReference
	leaq	40(%rsp), %rdi
Ltmp122:
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	_sp_invokePossibleMultipleValueSymbolFunction
Ltmp123:
## BB#5:                                ## %(TRY-0).normal-dest
	movl	$6, %edi
	movl	$3, %esi
Ltmp149:
	callq	_trace_setLineNumberColumnForIHSTop
	leaq	8(%rsp), %r15
	movq	%r15, %rdi
	movl	$1, %esi
	movl	$2000004, %edx          ## imm = 0x1E8484
	callq	_makeValueFrame
	leaq	56(%rsp), %rsi
	movq	%r15, %rdi
	callq	_setParentOfActivationFrame
	movq	%r15, %rdi
	xorl	%esi, %esi
	callq	_valueFrameReference
	leaq	"_load-time-value-vector"(%rip), %rbx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	movl	$8, %edx
	callq	_sp_copyLoadTimeValue
	callq	_singleStepCallback
	movq	%rbx, %rdi
	xorl	%esi, %esi
	callq	_loadTimeSymbolReference
Ltmp124:
	movq	%r14, %rdi
	movq	%rax, %rsi
	movq	%r15, %rdx
	callq	_mv_invokePossibleMultipleValueSymbolFunction
Ltmp125:
LBB6_9:                                 ## %(TRY-0).try-cont
	leaq	8(%rsp), %rdi
	callq	_destructAFsp
	leaq	24(%rsp), %rdi
	callq	_destructAFsp
	leaq	40(%rsp), %rdi
	callq	_destructTsp
	leaq	56(%rsp), %rdi
	callq	_destructAFsp
	addq	$88, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	ret
LBB6_1:                                 ## %error
	testl	%eax, %eax
	jns	LBB6_3
## BB#2:                                ## %error1
Ltmp119:
	leaq	"_:::global-str-B"(%rip), %rcx
	movq	%rcx, %rdi
	movq	%rbx, %rsi
	movl	%eax, %edx
	xorl	%ecx, %ecx
	callq	_throwNotEnoughArgumentsException
Ltmp120:
LBB6_3:                                 ## %continue
Ltmp117:
	leaq	"_:::global-str-B"(%rip), %rcx
	movq	%rcx, %rdi
	movq	%rbx, %rsi
	movl	%eax, %edx
	xorl	%ecx, %ecx
	callq	_throwTooManyArgumentsException
Ltmp118:
	jmp	LBB6_4
LBB6_11:                                ## %func-cleanup-landing-pad
Ltmp121:
	jmp	LBB6_12
LBB6_6:                                 ## %(TRY-0).landing-pad
Ltmp126:
	movq	%rdx, %rcx
	movq	%rax, %rbx
	movq	%rbx, 80(%rsp)
	movl	%ecx, 76(%rsp)
	cmpl	$1, %ecx
	jne	LBB6_13
## BB#7:                                ## %(TRY-0).handler-block6882
	movq	%rbx, %rdi
	callq	___cxa_begin_catch
Ltmp127:
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	_mv_blockHandleReturnFrom
Ltmp128:
## BB#8:                                ## %(TRY-0).normal-dest7
Ltmp129:
	callq	___cxa_end_catch
Ltmp130:
	jmp	LBB6_9
LBB6_10:                                ## %(TRY-0).landing-pad10
Ltmp131:
LBB6_12:                                ## %func-ehcleanup
	movq	%rdx, %rcx
	movq	%rax, %rbx
	movq	%rbx, 80(%rsp)
	movl	%ecx, 76(%rsp)
LBB6_13:                                ## %func-ehcleanup
	leaq	8(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	leaq	24(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	leaq	40(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	56(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	movq	%rbx, %rdi
	callq	__Unwind_Resume
Ltmp150:
Lfunc_end6:
	.cfi_endproc
Leh_func_end6:
	.section	__TEXT,__gcc_except_tab
	.align	2
GCC_except_table6:
Lexception6:
	.byte	255                     ## @LPStart Encoding = omit
	.byte	155                     ## @TType Encoding = indirect pcrel sdata4
	.asciz	"\276\200\200"          ## @TType base offset
	.byte	3                       ## Call site Encoding = udata4
	.byte	52                      ## Call site table length
Lset26 = Ltmp122-Leh_func_begin6        ## >> Call Site 1 <<
	.long	Lset26
Lset27 = Ltmp125-Ltmp122                ##   Call between Ltmp122 and Ltmp125
	.long	Lset27
Lset28 = Ltmp126-Leh_func_begin6        ##     jumps to Ltmp126
	.long	Lset28
	.byte	3                       ##   On action: 2
Lset29 = Ltmp119-Leh_func_begin6        ## >> Call Site 2 <<
	.long	Lset29
Lset30 = Ltmp118-Ltmp119                ##   Call between Ltmp119 and Ltmp118
	.long	Lset30
Lset31 = Ltmp121-Leh_func_begin6        ##     jumps to Ltmp121
	.long	Lset31
	.byte	0                       ##   On action: cleanup
Lset32 = Ltmp127-Leh_func_begin6        ## >> Call Site 3 <<
	.long	Lset32
Lset33 = Ltmp130-Ltmp127                ##   Call between Ltmp127 and Ltmp130
	.long	Lset33
Lset34 = Ltmp131-Leh_func_begin6        ##     jumps to Ltmp131
	.long	Lset34
	.byte	0                       ##   On action: cleanup
Lset35 = Ltmp130-Leh_func_begin6        ## >> Call Site 4 <<
	.long	Lset35
Lset36 = Leh_func_end6-Ltmp130          ##   Call between Ltmp130 and Leh_func_end6
	.long	Lset36
	.long	0                       ##     has no landing pad
	.byte	0                       ##   On action: cleanup
	.byte	0                       ## >> Action Record 1 <<
                                        ##   Cleanup
	.byte	0                       ##   No further actions
	.byte	1                       ## >> Action Record 2 <<
                                        ##   Catch TypeInfo 1
	.byte	125                     ##   Continue to action 1
                                        ## >> Catch TypeInfos <<
	.long	__ZTIN4core10ReturnFromE@GOTPCREL+4 ## TypeInfo 1
	.align	2

	.section	__TEXT,__text,regular,pure_instructions
	.align	4, 0x90
_repl8:                                 ## @repl8
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%rax
Ltmp152:
	.cfi_def_cfa_offset 16
	leaq	"_load-time-value-vector"(%rip), %rsi
	xorl	%edx, %edx
	callq	_mv_copyLoadTimeValue
	popq	%rax
	ret
	.cfi_endproc

	.align	4, 0x90
_repl9:                                 ## @repl9
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%rax
Ltmp154:
	.cfi_def_cfa_offset 16
	leaq	"_load-time-value-vector"(%rip), %rsi
	movl	$6, %edx
	callq	_mv_copyLoadTimeValue
	popq	%rax
	ret
	.cfi_endproc

	.globl	___MAIN_testDwarf
	.align	4, 0x90
___MAIN_testDwarf:                      ## @__MAIN_testDwarf
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
Leh_func_begin9:
	.cfi_lsda 16, Lexception9
## BB#0:                                ## %entry
	subq	$24, %rsp
Ltmp159:
	.cfi_def_cfa_offset 32
	movl	$0, 12(%rsp)
Ltmp155:
	leaq	____loadTimeDataInitializer(%rip), %rdi
	callq	_invokeLlvmFunctionVoid
Ltmp156:
## BB#1:                                ## %normal-dest
	addq	$24, %rsp
	ret
LBB9_2:                                 ## %func-cleanup-landing-pad
Ltmp157:
	movq	%rdx, %rcx
	movq	%rax, 16(%rsp)
	movl	%ecx, 12(%rsp)
	movq	%rax, %rdi
	callq	__Unwind_Resume
	.cfi_endproc
Leh_func_end9:
	.section	__TEXT,__gcc_except_tab
	.align	2
GCC_except_table9:
Lexception9:
	.byte	255                     ## @LPStart Encoding = omit
	.byte	155                     ## @TType Encoding = indirect pcrel sdata4
	.asciz	"\234"                  ## @TType base offset
	.byte	3                       ## Call site Encoding = udata4
	.byte	26                      ## Call site table length
Lset37 = Ltmp155-Leh_func_begin9        ## >> Call Site 1 <<
	.long	Lset37
Lset38 = Ltmp156-Ltmp155                ##   Call between Ltmp155 and Ltmp156
	.long	Lset38
Lset39 = Ltmp157-Leh_func_begin9        ##     jumps to Ltmp157
	.long	Lset39
	.byte	0                       ##   On action: cleanup
Lset40 = Ltmp156-Leh_func_begin9        ## >> Call Site 2 <<
	.long	Lset40
Lset41 = Leh_func_end9-Ltmp156          ##   Call between Ltmp156 and Leh_func_end9
	.long	Lset41
	.long	0                       ##     has no landing pad
	.byte	0                       ##   On action: cleanup
	.align	2

	.section	__TEXT,__cstring,cstring_literals
	.align	4                       ## @":::global-str-/Users/meister/Development/cando/brcl/src/tests/random/testDwarf.lsp"
"_:::global-str-/Users/meister/Development/cando/brcl/src/tests/random/testDwarf.lsp":
	.asciz	"/Users/meister/Development/cando/brcl/src/tests/random/testDwarf.lsp"

.zerofill __DATA,__bss,"_load-time-value-vector",8,3 ## @load-time-value-vector
"_:::global-str-repl":                  ## @":::global-str-repl"
	.asciz	"repl"

"_:::symbol-name-A":                    ## @":::symbol-name-A"
	.asciz	"A"

"_:::package-name-CORE":                ## @":::package-name-CORE"
	.asciz	"CORE"

"_:::global-str-A":                     ## @":::global-str-A"
	.asciz	"A"

"_:::symbol-name-T":                    ## @":::symbol-name-T"
	.asciz	"T"

"_:::package-name-COMMON-LISP":         ## @":::package-name-COMMON-LISP"
	.asciz	"COMMON-LISP"

	.section	__TEXT,__const
	.align	2                       ## @constant-array
"_constant-array":
	.space	4

	.align	2                       ## @constant-array1
"_constant-array1":
	.byte	0

	.section	__TEXT,__cstring,cstring_literals
"_:::str":                              ## @":::str"
	.asciz	"Hi from a"

"_:::symbol-name-PRINT":                ## @":::symbol-name-PRINT"
	.asciz	"PRINT"

"_:::symbol-name-*FSET":                ## @":::symbol-name-*FSET"
	.asciz	"*FSET"

"_:::global-str-repl2":                 ## @":::global-str-repl2"
	.asciz	"repl2"

"_:::global-str-repl3":                 ## @":::global-str-repl3"
	.asciz	"repl3"

"_:::global-str-repl4":                 ## @":::global-str-repl4"
	.asciz	"repl4"

"_:::symbol-name-B":                    ## @":::symbol-name-B"
	.asciz	"B"

"_:::global-str-B":                     ## @":::global-str-B"
	.asciz	"B"

	.section	__TEXT,__const
	.align	2                       ## @constant-array5
"_constant-array5":
	.space	4

	.align	2                       ## @constant-array6
"_constant-array6":
	.byte	0

	.section	__TEXT,__cstring,cstring_literals
"_:::str7":                             ## @":::str7"
	.asciz	"Hi from b"

"_:::global-str-repl8":                 ## @":::global-str-repl8"
	.asciz	"repl8"

"_:::global-str-repl9":                 ## @":::global-str-repl9"
	.asciz	"repl9"

	.align	4                       ## @":::global-str-__MAIN_testDwarf"
"_:::global-str-__MAIN_testDwarf":
	.asciz	"__MAIN_testDwarf"

	.align	4                       ## @":::global-str-___loadTimeDataInitializer"
"_:::global-str-___loadTimeDataInitializer":
	.asciz	"___loadTimeDataInitializer"

	.section	__TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
	.section	__TEXT,__text,regular,pure_instructions
Ldebug_end0:
	.section	__DWARF,__debug_str,regular,debug
Linfo_string0:
	.asciz	"brcl Common Lisp compiler"
Linfo_string1:
	.asciz	"testDwarf"
Linfo_string2:
	.asciz	"/Users/meister/Development/cando/brcl/src/tests/random/"
Linfo_string3:
	.asciz	"-v"
Linfo_string4:
	.asciz	"repl"
Linfo_string5:
	.asciz	"int"
Linfo_string6:
	.asciz	"A"
Linfo_string7:
	.asciz	"repl2"
Linfo_string8:
	.asciz	"repl3"
Linfo_string9:
	.asciz	"repl4"
Linfo_string10:
	.asciz	"B"
Linfo_string11:
	.asciz	"repl8"
Linfo_string12:
	.asciz	"repl9"
Linfo_string13:
	.asciz	"__MAIN_testDwarf"
	.section	__DWARF,__debug_info,regular,debug
L__DWARF__debug_info_begin0:
	.long	241                     ## Length of Compilation Unit Info
	.short	4                       ## DWARF version number
Lset42 = L__DWARF__debug_abbrev_begin-Lsection_abbrev ## Offset Into Abbrev. Section
	.long	Lset42
	.byte	8                       ## Address Size (in bytes)
	.byte	1                       ## Abbrev [1] 0xb:0xea DW_TAG_compile_unit
Lset43 = Linfo_string0-Linfo_string     ## DW_AT_producer
	.long	Lset43
	.short	32768                   ## DW_AT_language
Lset44 = Linfo_string1-Linfo_string     ## DW_AT_name
	.long	Lset44
	.quad	0                       ## DW_AT_low_pc
	.long	0                       ## DW_AT_stmt_list
Lset45 = Linfo_string2-Linfo_string     ## DW_AT_comp_dir
	.long	Lset45
Lset46 = Linfo_string3-Linfo_string     ## DW_AT_APPLE_flags
	.long	Lset46
	.byte	1                       ## DW_AT_APPLE_major_runtime_vers
	.byte	2                       ## Abbrev [2] 0x2b:0x21 DW_TAG_subprogram
Lset47 = Linfo_string4-Linfo_string     ## DW_AT_MIPS_linkage_name
	.long	Lset47
Lset48 = Linfo_string4-Linfo_string     ## DW_AT_name
	.long	Lset48
	.byte	1                       ## DW_AT_decl_file
	.byte	2                       ## DW_AT_decl_line
	.long	76                      ## DW_AT_type
                                        ## DW_AT_external
	.quad	Lfunc_begin1            ## DW_AT_low_pc
	.quad	Lfunc_end1              ## DW_AT_high_pc
	.byte	1                       ## DW_AT_frame_base
	.byte	87
                                        ## DW_AT_APPLE_omit_frame_ptr
	.byte	3                       ## Abbrev [3] 0x4c:0x7 DW_TAG_base_type
Lset49 = Linfo_string5-Linfo_string     ## DW_AT_name
	.long	Lset49
	.byte	13                      ## DW_AT_encoding
	.byte	4                       ## DW_AT_byte_size
	.byte	4                       ## Abbrev [4] 0x53:0x1f DW_TAG_subprogram
Lset50 = Linfo_string6-Linfo_string     ## DW_AT_MIPS_linkage_name
	.long	Lset50
Lset51 = Linfo_string6-Linfo_string     ## DW_AT_name
	.long	Lset51
	.long	76                      ## DW_AT_type
                                        ## DW_AT_external
	.quad	Lfunc_begin2            ## DW_AT_low_pc
	.quad	Lfunc_end2              ## DW_AT_high_pc
	.byte	1                       ## DW_AT_frame_base
	.byte	87
                                        ## DW_AT_APPLE_omit_frame_ptr
	.byte	5                       ## Abbrev [5] 0x72:0xd DW_TAG_subprogram
Lset52 = Linfo_string7-Linfo_string     ## DW_AT_MIPS_linkage_name
	.long	Lset52
Lset53 = Linfo_string7-Linfo_string     ## DW_AT_name
	.long	Lset53
	.long	76                      ## DW_AT_type
                                        ## DW_AT_external
	.byte	5                       ## Abbrev [5] 0x7f:0xd DW_TAG_subprogram
Lset54 = Linfo_string8-Linfo_string     ## DW_AT_MIPS_linkage_name
	.long	Lset54
Lset55 = Linfo_string8-Linfo_string     ## DW_AT_name
	.long	Lset55
	.long	76                      ## DW_AT_type
                                        ## DW_AT_external
	.byte	6                       ## Abbrev [6] 0x8c:0x22 DW_TAG_subprogram
Lset56 = Linfo_string9-Linfo_string     ## DW_AT_MIPS_linkage_name
	.long	Lset56
Lset57 = Linfo_string9-Linfo_string     ## DW_AT_name
	.long	Lset57
	.byte	1                       ## DW_AT_decl_file
	.short	505                     ## DW_AT_decl_line
	.long	76                      ## DW_AT_type
                                        ## DW_AT_external
	.quad	Lfunc_begin5            ## DW_AT_low_pc
	.quad	Lfunc_end5              ## DW_AT_high_pc
	.byte	1                       ## DW_AT_frame_base
	.byte	87
                                        ## DW_AT_APPLE_omit_frame_ptr
	.byte	4                       ## Abbrev [4] 0xae:0x1f DW_TAG_subprogram
Lset58 = Linfo_string10-Linfo_string    ## DW_AT_MIPS_linkage_name
	.long	Lset58
Lset59 = Linfo_string10-Linfo_string    ## DW_AT_name
	.long	Lset59
	.long	76                      ## DW_AT_type
                                        ## DW_AT_external
	.quad	Lfunc_begin6            ## DW_AT_low_pc
	.quad	Lfunc_end6              ## DW_AT_high_pc
	.byte	1                       ## DW_AT_frame_base
	.byte	87
                                        ## DW_AT_APPLE_omit_frame_ptr
	.byte	5                       ## Abbrev [5] 0xcd:0xd DW_TAG_subprogram
Lset60 = Linfo_string11-Linfo_string    ## DW_AT_MIPS_linkage_name
	.long	Lset60
Lset61 = Linfo_string11-Linfo_string    ## DW_AT_name
	.long	Lset61
	.long	76                      ## DW_AT_type
                                        ## DW_AT_external
	.byte	5                       ## Abbrev [5] 0xda:0xd DW_TAG_subprogram
Lset62 = Linfo_string12-Linfo_string    ## DW_AT_MIPS_linkage_name
	.long	Lset62
Lset63 = Linfo_string12-Linfo_string    ## DW_AT_name
	.long	Lset63
	.long	76                      ## DW_AT_type
                                        ## DW_AT_external
	.byte	5                       ## Abbrev [5] 0xe7:0xd DW_TAG_subprogram
Lset64 = Linfo_string13-Linfo_string    ## DW_AT_MIPS_linkage_name
	.long	Lset64
Lset65 = Linfo_string13-Linfo_string    ## DW_AT_name
	.long	Lset65
	.long	76                      ## DW_AT_type
                                        ## DW_AT_external
	.byte	0                       ## End Of Children Mark
L__DWARF__debug_info_end0:
	.section	__DWARF,__debug_abbrev,regular,debug
L__DWARF__debug_abbrev_begin:
	.byte	1                       ## Abbreviation Code
	.byte	17                      ## DW_TAG_compile_unit
	.byte	1                       ## DW_CHILDREN_yes
	.byte	37                      ## DW_AT_producer
	.byte	14                      ## DW_FORM_strp
	.byte	19                      ## DW_AT_language
	.byte	5                       ## DW_FORM_data2
	.byte	3                       ## DW_AT_name
	.byte	14                      ## DW_FORM_strp
	.byte	17                      ## DW_AT_low_pc
	.byte	1                       ## DW_FORM_addr
	.byte	16                      ## DW_AT_stmt_list
	.byte	6                       ## DW_FORM_data4
	.byte	27                      ## DW_AT_comp_dir
	.byte	14                      ## DW_FORM_strp
	.ascii	"\342\177"              ## DW_AT_APPLE_flags
	.byte	14                      ## DW_FORM_strp
	.ascii	"\345\177"              ## DW_AT_APPLE_major_runtime_vers
	.byte	11                      ## DW_FORM_data1
	.byte	0                       ## EOM(1)
	.byte	0                       ## EOM(2)
	.byte	2                       ## Abbreviation Code
	.byte	46                      ## DW_TAG_subprogram
	.byte	0                       ## DW_CHILDREN_no
	.ascii	"\207@"                 ## DW_AT_MIPS_linkage_name
	.byte	14                      ## DW_FORM_strp
	.byte	3                       ## DW_AT_name
	.byte	14                      ## DW_FORM_strp
	.byte	58                      ## DW_AT_decl_file
	.byte	11                      ## DW_FORM_data1
	.byte	59                      ## DW_AT_decl_line
	.byte	11                      ## DW_FORM_data1
	.byte	73                      ## DW_AT_type
	.byte	19                      ## DW_FORM_ref4
	.byte	63                      ## DW_AT_external
	.byte	25                      ## DW_FORM_flag_present
	.byte	17                      ## DW_AT_low_pc
	.byte	1                       ## DW_FORM_addr
	.byte	18                      ## DW_AT_high_pc
	.byte	1                       ## DW_FORM_addr
	.byte	64                      ## DW_AT_frame_base
	.byte	10                      ## DW_FORM_block1
	.ascii	"\347\177"              ## DW_AT_APPLE_omit_frame_ptr
	.byte	25                      ## DW_FORM_flag_present
	.byte	0                       ## EOM(1)
	.byte	0                       ## EOM(2)
	.byte	3                       ## Abbreviation Code
	.byte	36                      ## DW_TAG_base_type
	.byte	0                       ## DW_CHILDREN_no
	.byte	3                       ## DW_AT_name
	.byte	14                      ## DW_FORM_strp
	.byte	62                      ## DW_AT_encoding
	.byte	11                      ## DW_FORM_data1
	.byte	11                      ## DW_AT_byte_size
	.byte	11                      ## DW_FORM_data1
	.byte	0                       ## EOM(1)
	.byte	0                       ## EOM(2)
	.byte	4                       ## Abbreviation Code
	.byte	46                      ## DW_TAG_subprogram
	.byte	0                       ## DW_CHILDREN_no
	.ascii	"\207@"                 ## DW_AT_MIPS_linkage_name
	.byte	14                      ## DW_FORM_strp
	.byte	3                       ## DW_AT_name
	.byte	14                      ## DW_FORM_strp
	.byte	73                      ## DW_AT_type
	.byte	19                      ## DW_FORM_ref4
	.byte	63                      ## DW_AT_external
	.byte	25                      ## DW_FORM_flag_present
	.byte	17                      ## DW_AT_low_pc
	.byte	1                       ## DW_FORM_addr
	.byte	18                      ## DW_AT_high_pc
	.byte	1                       ## DW_FORM_addr
	.byte	64                      ## DW_AT_frame_base
	.byte	10                      ## DW_FORM_block1
	.ascii	"\347\177"              ## DW_AT_APPLE_omit_frame_ptr
	.byte	25                      ## DW_FORM_flag_present
	.byte	0                       ## EOM(1)
	.byte	0                       ## EOM(2)
	.byte	5                       ## Abbreviation Code
	.byte	46                      ## DW_TAG_subprogram
	.byte	0                       ## DW_CHILDREN_no
	.ascii	"\207@"                 ## DW_AT_MIPS_linkage_name
	.byte	14                      ## DW_FORM_strp
	.byte	3                       ## DW_AT_name
	.byte	14                      ## DW_FORM_strp
	.byte	73                      ## DW_AT_type
	.byte	19                      ## DW_FORM_ref4
	.byte	63                      ## DW_AT_external
	.byte	25                      ## DW_FORM_flag_present
	.byte	0                       ## EOM(1)
	.byte	0                       ## EOM(2)
	.byte	6                       ## Abbreviation Code
	.byte	46                      ## DW_TAG_subprogram
	.byte	0                       ## DW_CHILDREN_no
	.ascii	"\207@"                 ## DW_AT_MIPS_linkage_name
	.byte	14                      ## DW_FORM_strp
	.byte	3                       ## DW_AT_name
	.byte	14                      ## DW_FORM_strp
	.byte	58                      ## DW_AT_decl_file
	.byte	11                      ## DW_FORM_data1
	.byte	59                      ## DW_AT_decl_line
	.byte	5                       ## DW_FORM_data2
	.byte	73                      ## DW_AT_type
	.byte	19                      ## DW_FORM_ref4
	.byte	63                      ## DW_AT_external
	.byte	25                      ## DW_FORM_flag_present
	.byte	17                      ## DW_AT_low_pc
	.byte	1                       ## DW_FORM_addr
	.byte	18                      ## DW_AT_high_pc
	.byte	1                       ## DW_FORM_addr
	.byte	64                      ## DW_AT_frame_base
	.byte	10                      ## DW_FORM_block1
	.ascii	"\347\177"              ## DW_AT_APPLE_omit_frame_ptr
	.byte	25                      ## DW_FORM_flag_present
	.byte	0                       ## EOM(1)
	.byte	0                       ## EOM(2)
	.byte	0                       ## EOM(3)
L__DWARF__debug_abbrev_end:
	.section	__DWARF,__debug_aranges,regular,debug
	.long	44                      ## Length of ARange Set
	.short	2                       ## DWARF Arange version number
Lset66 = L__DWARF__debug_info_begin0-Lsection_info ## Offset Into Debug Info Section
	.long	Lset66
	.byte	8                       ## Address Size (in bytes)
	.byte	0                       ## Segment Size (in bytes)
	.byte	255
	.byte	255
	.byte	255
	.byte	255
	.quad	Lfunc_begin1
Lset67 = Ldebug_end0-Lfunc_begin1
	.quad	Lset67
	.quad	0                       ## ARange terminator
	.quad	0
	.section	__DWARF,__debug_ranges,regular,debug
	.section	__DWARF,__debug_macinfo,regular,debug
	.section	__DWARF,__apple_names,regular,debug
Lnames_begin:
	.long	1212240712              ## Header Magic
	.short	1                       ## Header Version
	.short	0                       ## Header Hash Function
	.long	4                       ## Header Bucket Count
	.long	4                       ## Header Hash Count
	.long	12                      ## Header Data Length
	.long	0                       ## HeaderData Die Offset Base
	.long	1                       ## HeaderData Atom Count
	.short	1                       ## DW_ATOM_die_offset
	.short	6                       ## DW_FORM_data4
	.long	0                       ## Bucket 0
	.long	-1                      ## Bucket 1
	.long	2                       ## Bucket 2
	.long	3                       ## Bucket 3
	.long	2090684216              ## Hash in Bucket 0
	.long	273102444               ## Hash in Bucket 0
	.long	177638                  ## Hash in Bucket 2
	.long	177639                  ## Hash in Bucket 3
	.long	LNames2-Lnames_begin    ## Offset in Bucket 0
	.long	LNames3-Lnames_begin    ## Offset in Bucket 0
	.long	LNames0-Lnames_begin    ## Offset in Bucket 2
	.long	LNames1-Lnames_begin    ## Offset in Bucket 3
LNames2:
Lset68 = Linfo_string4-Linfo_string     ## repl
	.long	Lset68
	.long	1                       ## Num DIEs
	.long	43
	.long	0
LNames3:
Lset69 = Linfo_string9-Linfo_string     ## repl4
	.long	Lset69
	.long	1                       ## Num DIEs
	.long	140
	.long	0
LNames0:
Lset70 = Linfo_string6-Linfo_string     ## A
	.long	Lset70
	.long	1                       ## Num DIEs
	.long	83
	.long	0
LNames1:
Lset71 = Linfo_string10-Linfo_string    ## B
	.long	Lset71
	.long	1                       ## Num DIEs
	.long	174
	.long	0
	.section	__DWARF,__apple_objc,regular,debug
Lobjc_begin:
	.long	1212240712              ## Header Magic
	.short	1                       ## Header Version
	.short	0                       ## Header Hash Function
	.long	1                       ## Header Bucket Count
	.long	0                       ## Header Hash Count
	.long	12                      ## Header Data Length
	.long	0                       ## HeaderData Die Offset Base
	.long	1                       ## HeaderData Atom Count
	.short	1                       ## DW_ATOM_die_offset
	.short	6                       ## DW_FORM_data4
	.long	-1                      ## Bucket 0
	.section	__DWARF,__apple_namespac,regular,debug
Lnamespac_begin:
	.long	1212240712              ## Header Magic
	.short	1                       ## Header Version
	.short	0                       ## Header Hash Function
	.long	1                       ## Header Bucket Count
	.long	0                       ## Header Hash Count
	.long	12                      ## Header Data Length
	.long	0                       ## HeaderData Die Offset Base
	.long	1                       ## HeaderData Atom Count
	.short	1                       ## DW_ATOM_die_offset
	.short	6                       ## DW_FORM_data4
	.long	-1                      ## Bucket 0
	.section	__DWARF,__apple_types,regular,debug
Ltypes_begin:
	.long	1212240712              ## Header Magic
	.short	1                       ## Header Version
	.short	0                       ## Header Hash Function
	.long	1                       ## Header Bucket Count
	.long	1                       ## Header Hash Count
	.long	20                      ## Header Data Length
	.long	0                       ## HeaderData Die Offset Base
	.long	3                       ## HeaderData Atom Count
	.short	1                       ## DW_ATOM_die_offset
	.short	6                       ## DW_FORM_data4
	.short	3                       ## DW_ATOM_die_tag
	.short	5                       ## DW_FORM_data2
	.short	4                       ## DW_ATOM_type_flags
	.short	11                      ## DW_FORM_data1
	.long	0                       ## Bucket 0
	.long	193495088               ## Hash in Bucket 0
	.long	Ltypes0-Ltypes_begin    ## Offset in Bucket 0
Ltypes0:
Lset72 = Linfo_string5-Linfo_string     ## int
	.long	Lset72
	.long	1                       ## Num DIEs
	.long	76
	.short	36
	.byte	0
	.long	0

.subsections_via_symbols
	.section	__DWARF,__debug_line,regular,debug
Lline_table_start0:
Ltmp161 = (Ltmp160-Lline_table_start0)-4
	.long	Ltmp161
	.short	2
Ltmp163 = (Ltmp162-Lline_table_start0)-10
	.long	Ltmp163
	.byte	1
	.byte	1
	.byte	-5
	.byte	14
	.byte	13
	.byte	0
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.ascii	"testDwarf"
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
Ltmp162:
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp41
	.byte	19
	.byte	5
	.byte	13
	.byte	10
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp54
	.byte	1
	.byte	5
	.byte	0
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp74
	.byte	16
	.byte	5
	.byte	13
	.byte	10
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp85
	.byte	20
	.byte	5
	.byte	0
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp102
	.ascii	"\003\367\003\001"
	.byte	5
	.byte	3
	.byte	10
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp115
	.byte	1
	.byte	5
	.byte	0
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp137
	.ascii	"\003\207|\001"
	.byte	5
	.byte	3
	.byte	10
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp148
	.byte	23
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp149
	.byte	19
	.section	__TEXT,__text,regular,pure_instructions
Ltmp164:
	.section	__DWARF,__debug_line,regular,debug
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp164
	.ascii	"\002\000\000\001\001"
Ltmp160:
