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
_lambda:                                ## @lambda
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
Leh_func_begin0:
	.cfi_lsda 16, Lexception0
Lfunc_begin0:
## BB#0:                                ## %entry
Ltmp16:
	pushq	%r15
Ltmp17:
	.cfi_def_cfa_offset 16
	pushq	%r14
Ltmp18:
	.cfi_def_cfa_offset 24
	pushq	%r13
Ltmp19:
	.cfi_def_cfa_offset 32
	pushq	%r12
Ltmp20:
	.cfi_def_cfa_offset 40
	pushq	%rbx
Ltmp21:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
Ltmp22:
	.cfi_def_cfa_offset 128
Ltmp23:
	.cfi_offset %rbx, -48
Ltmp24:
	.cfi_offset %r12, -40
Ltmp25:
	.cfi_offset %r13, -32
Ltmp26:
	.cfi_offset %r14, -24
Ltmp27:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movl	$0, 68(%rsp)
	leaq	48(%rsp), %rdi
	callq	_newAFsp
	leaq	32(%rsp), %rdi
	callq	_newTsp
	leaq	16(%rsp), %rdi
	callq	_newTsp
	leaq	(%rsp), %rdi
	callq	_newAFsp
	movq	%rbx, %rdi
	callq	_activationFrameSize
	testl	%eax, %eax
	jne	LBB0_1
LBB0_4:                                 ## %continue3
	leaq	48(%rsp), %r15
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	_copyAFsp
	movq	_globalRunTimeValuesVector@GOTPCREL(%rip), %r12
	movq	%r12, %rdi
	movl	$36, %esi
	callq	_loadTimeValueReference
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	_attachDebuggingInfoToValueFrame
	movl	$2, %edi
	movl	$25, %esi
Ltmp28:
Ltmp29:
	callq	_trace_setLineNumberColumnForIHSTop
	leaq	(%rsp), %rbx
	movq	%rbx, %rdi
	movl	$3, %esi
	movl	$2000049, %edx          ## imm = 0x1E84B1
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
	movq	%rbx, %rdi
	movl	$1, %esi
	callq	_valueFrameReference
	movq	%rax, %r13
	movq	%r12, %rdi
	movl	$44, %esi
	callq	_loadTimeValueReference
	movq	%rax, %rbx
	movq	%r12, %rdi
	movl	$5, %esi
	callq	_loadTimeValueReference
	leaq	"_MATCH-DIMENSIONS"(%rip), %rsi
	leaq	"_:::global-str-/Users/meister/DEVELOPMENT/CANDO/BRCL/SRC/LISP/KERNEL/LSP/test_matchdim.lsp"(%rip), %rdx
Ltmp5:
	movq	%r13, %rdi
	movq	%rax, %rcx
	movq	%rbx, %r8
	movq	%r15, %r9
	callq	_sp_makeCompiledFunction
Ltmp6:
## BB#5:                                ## %(TRY-0).normal-dest
	leaq	(%rsp), %r15
	movq	%r15, %rdi
	movl	$2, %esi
	callq	_valueFrameReference
	movq	_globalRunTimeValuesVector@GOTPCREL(%rip), %rbx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	xorl	%edx, %edx
	callq	_sp_copyLoadTimeValue
	callq	_singleStepCallback
	movq	%rbx, %rdi
	movl	$13, %esi
	callq	_loadTimeSymbolReference
	leaq	16(%rsp), %rdi
Ltmp7:
	movq	%rax, %rsi
	movq	%r15, %rdx
	callq	_sp_invokePossibleMultipleValueSymbolFunction
Ltmp8:
## BB#6:                                ## %(TRY-0).normal-dest5
	leaq	16(%rsp), %r15
	movq	_globalRunTimeValuesVector@GOTPCREL(%rip), %rbx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	xorl	%edx, %edx
	callq	_sp_copyLoadTimeValue
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movl	$5, %edx
	callq	_mv_copyLoadTimeValue
	leaq	(%rsp), %rdi
	callq	_destructAFsp
	movq	%r15, %rdi
	callq	_destructTsp
	leaq	32(%rsp), %rdi
	callq	_destructTsp
	leaq	48(%rsp), %rdi
	callq	_destructAFsp
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
LBB0_1:                                 ## %error
	testl	%eax, %eax
	jns	LBB0_3
## BB#2:                                ## %error1
Ltmp2:
	leaq	"_:::global-str-lambda"(%rip), %rcx
	movq	%rcx, %rdi
	movq	%rbx, %rsi
	movl	%eax, %edx
	xorl	%ecx, %ecx
	callq	_throwNotEnoughArgumentsException
Ltmp3:
LBB0_3:                                 ## %continue
Ltmp0:
	leaq	"_:::global-str-lambda"(%rip), %rcx
	movq	%rcx, %rdi
	movq	%rbx, %rsi
	movl	%eax, %edx
	xorl	%ecx, %ecx
	callq	_throwTooManyArgumentsException
Ltmp1:
	jmp	LBB0_4
LBB0_8:                                 ## %func-cleanup-landing-pad
Ltmp4:
	jmp	LBB0_9
LBB0_7:                                 ## %(TRY-0).landing-pad
Ltmp9:
LBB0_9:                                 ## %func-ehcleanup
	movq	%rdx, %rcx
	movq	%rax, %rbx
	movq	%rbx, 72(%rsp)
	movl	%ecx, 68(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	leaq	16(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	32(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	48(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	movl	$90, %edi
	callq	_debugPrintI32
	movl	$91, %edi
	callq	_debugPrintI32
	movq	%rbx, %rdi
	callq	__Unwind_Resume
Ltmp30:
Lfunc_end0:
	.cfi_endproc
Leh_func_end0:
	.section	__TEXT,__gcc_except_tab
	.align	2
GCC_except_table0:
Lexception0:
	.byte	255                     ## @LPStart Encoding = omit
	.byte	155                     ## @TType Encoding = indirect pcrel sdata4
	.byte	41                      ## @TType base offset
	.byte	3                       ## Call site Encoding = udata4
	.byte	39                      ## Call site table length
Lset0 = Ltmp5-Leh_func_begin0           ## >> Call Site 1 <<
	.long	Lset0
Lset1 = Ltmp8-Ltmp5                     ##   Call between Ltmp5 and Ltmp8
	.long	Lset1
Lset2 = Ltmp9-Leh_func_begin0           ##     jumps to Ltmp9
	.long	Lset2
	.byte	0                       ##   On action: cleanup
Lset3 = Ltmp2-Leh_func_begin0           ## >> Call Site 2 <<
	.long	Lset3
Lset4 = Ltmp1-Ltmp2                     ##   Call between Ltmp2 and Ltmp1
	.long	Lset4
Lset5 = Ltmp4-Leh_func_begin0           ##     jumps to Ltmp4
	.long	Lset5
	.byte	0                       ##   On action: cleanup
Lset6 = Ltmp1-Leh_func_begin0           ## >> Call Site 3 <<
	.long	Lset6
Lset7 = Leh_func_end0-Ltmp1             ##   Call between Ltmp1 and Leh_func_end0
	.long	Lset7
	.long	0                       ##     has no landing pad
	.byte	0                       ##   On action: cleanup
	.align	2

	.section	__TEXT,__text,regular,pure_instructions
	.align	4, 0x90
"_MATCH-DIMENSIONS":                    ## @MATCH-DIMENSIONS
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
Leh_func_begin1:
	.cfi_lsda 16, Lexception1
Lfunc_begin1:
## BB#0:                                ## %(TRY-0).entry
Ltmp90:
	pushq	%rbp
Ltmp91:
	.cfi_def_cfa_offset 16
	pushq	%r15
Ltmp92:
	.cfi_def_cfa_offset 24
	pushq	%r14
Ltmp93:
	.cfi_def_cfa_offset 32
	pushq	%r13
Ltmp94:
	.cfi_def_cfa_offset 40
	pushq	%r12
Ltmp95:
	.cfi_def_cfa_offset 48
	pushq	%rbx
Ltmp96:
	.cfi_def_cfa_offset 56
	subq	$616, %rsp              ## imm = 0x268
Ltmp97:
	.cfi_def_cfa_offset 672
Ltmp98:
	.cfi_offset %rbx, -56
Ltmp99:
	.cfi_offset %r12, -48
Ltmp100:
	.cfi_offset %r13, -40
Ltmp101:
	.cfi_offset %r14, -32
Ltmp102:
	.cfi_offset %r15, -24
Ltmp103:
	.cfi_offset %rbp, -16
	movq	%rsi, %rbx
	movq	%rdi, %r13
	movl	$0, 604(%rsp)
	leaq	584(%rsp), %rdi
	callq	_newAFsp
	leaq	568(%rsp), %rdi
	callq	_newTsp
	leaq	552(%rsp), %rdi
	callq	_newAFsp
	leaq	536(%rsp), %rdi
	callq	_newAFsp
	leaq	520(%rsp), %rdi
	callq	_newTsp
	leaq	504(%rsp), %rdi
	callq	_newTsp
	leaq	488(%rsp), %rdi
	callq	_newAFsp
	leaq	472(%rsp), %rdi
	callq	_newAFsp
	leaq	456(%rsp), %rdi
	callq	_newTsp
	leaq	440(%rsp), %rdi
	callq	_newTsp
	leaq	424(%rsp), %rdi
	callq	_newAFsp
	leaq	408(%rsp), %rdi
	callq	_newAFsp
	leaq	392(%rsp), %rdi
	callq	_newTsp
	leaq	376(%rsp), %rdi
	callq	_newAFsp
	leaq	360(%rsp), %rdi
	callq	_newTsp
	leaq	344(%rsp), %rdi
	callq	_newAFsp
	leaq	328(%rsp), %rdi
	callq	_newTsp
	leaq	312(%rsp), %rdi
	callq	_newAFsp
	leaq	296(%rsp), %rdi
	callq	_newTsp
	leaq	280(%rsp), %rdi
	callq	_newTsp
	leaq	264(%rsp), %rdi
	callq	_newTsp
	leaq	248(%rsp), %rdi
	callq	_newAFsp
	leaq	232(%rsp), %rdi
	callq	_newTsp
	leaq	216(%rsp), %rdi
	callq	_newAFsp
	leaq	200(%rsp), %rdi
	callq	_newTsp
	leaq	184(%rsp), %rdi
	callq	_newAFsp
	leaq	168(%rsp), %rdi
	callq	_newTsp
	leaq	152(%rsp), %rdi
	callq	_newTsp
	leaq	136(%rsp), %rdi
	callq	_newAFsp
	leaq	120(%rsp), %rdi
	callq	_newTsp
	leaq	104(%rsp), %rdi
	callq	_newAFsp
	leaq	88(%rsp), %rdi
	callq	_newTsp
	leaq	72(%rsp), %rdi
	callq	_newAFsp
	leaq	56(%rsp), %rdi
	callq	_newAFsp
	movq	%rbx, %rdi
	callq	_activationFrameSize
	cmpl	$2, %eax
	jne	LBB1_1
LBB1_4:                                 ## %(TRY-0).continue3
	leaq	584(%rsp), %rbp
	movq	%rbp, %rdi
	movq	%rbx, %rsi
	callq	_copyAFsp
	movq	_globalRunTimeValuesVector@GOTPCREL(%rip), %r14
	movq	%r14, %rdi
	movl	$37, %esi
	callq	_loadTimeValueReference
	movq	%rbp, %rdi
	movq	%rax, %rsi
	callq	_attachDebuggingInfoToValueFrame
	movq	%rbp, %rdi
	callq	_trace_setActivationFrameForIHSTop
	leaq	552(%rsp), %rbx
Ltmp104:
Ltmp105:
	movq	%rbx, %rdi
	callq	_trace_setActivationFrameForIHSTop
	movq	%rbx, %rdi
	movl	$1, %esi
	movl	$2000050, %edx          ## imm = 0x1E84B2
	callq	_makeValueFrame
	movq	%rbx, %rdi
	movq	%rbp, %rsi
	callq	_setParentOfActivationFrame
	movq	%r14, %rdi
	movl	$38, %esi
	callq	_loadTimeValueReference
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	_attachDebuggingInfoToValueFrame
	xorl	%edi, %edi
	xorl	%esi, %esi
	movq	%rbx, %rdx
	callq	_lexicalValueReference
	movq	%rax, %r15
	movl	$3, %edi
	movl	$7, %esi
Ltmp106:
Ltmp107:
	callq	_trace_setLineNumberColumnForIHSTop
	leaq	536(%rsp), %rbx
	movq	%rbx, %rdi
	movl	$2, %esi
	movl	$2000051, %edx          ## imm = 0x1E84B3
	callq	_makeValueFrame
	movq	%rbx, %rdi
	movq	%rbp, %rsi
	callq	_setParentOfActivationFrame
	movq	%rbx, %rdi
	xorl	%esi, %esi
	callq	_valueFrameReference
	movq	%rax, %rdi
	xorl	%esi, %esi
	movl	$1, %edx
	movq	%rbp, %rcx
	callq	_sp_lexicalValueRead
	movq	%rbx, %rdi
	movl	$1, %esi
	callq	_valueFrameReference
	movq	%rax, %rdi
	movq	%r14, %rsi
	movl	$39, %edx
	callq	_sp_copyLoadTimeValue
	callq	_singleStepCallback
	movq	%r14, %rdi
	movl	$15, %esi
	callq	_loadTimeSymbolReference
Ltmp36:
	movq	%r15, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	_sp_invokePossibleMultipleValueSymbolFunction
Ltmp37:
## BB#5:                                ## %(TRY-0).normal-dest7
	leaq	552(%rsp), %rbx
	movq	%rbx, %rdi
	callq	_trace_setActivationFrameForIHSTop
	callq	_singleStepCallback
	leaq	504(%rsp), %rbp
	movq	%rbp, %rdi
	xorl	%esi, %esi
	xorl	%edx, %edx
	movq	%rbx, %rcx
	callq	_sp_lexicalValueRead
	movq	%rbp, %rdi
	callq	_isTrueTsp
	cmpl	$1, %eax
	jne	LBB1_7
## BB#6:                                ## %(TRY-0).then
	leaq	552(%rsp), %rcx
	movq	%r13, %rdi
	xorl	%esi, %esi
	xorl	%edx, %edx
	callq	_mv_lexicalValueRead
	jmp	LBB1_46
LBB1_7:                                 ## %(TRY-0).else
	leaq	488(%rsp), %rbx
Ltmp108:
	movq	%rbx, %rdi
	callq	_trace_setActivationFrameForIHSTop
	movq	%rbx, %rdi
	movl	$1, %esi
	movl	$2000052, %edx          ## imm = 0x1E84B4
	callq	_makeValueFrame
	leaq	552(%rsp), %r15
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	_setParentOfActivationFrame
	movq	_globalRunTimeValuesVector@GOTPCREL(%rip), %r14
	movq	%r14, %rdi
	movl	$40, %esi
	callq	_loadTimeValueReference
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	_attachDebuggingInfoToValueFrame
	xorl	%edi, %edi
	xorl	%esi, %esi
	movq	%rbx, %rdx
	callq	_lexicalValueReference
	movq	%rax, %rbx
	movl	$4, %edi
	movl	$19, %esi
Ltmp109:
Ltmp110:
	callq	_trace_setLineNumberColumnForIHSTop
	leaq	472(%rsp), %rbp
	movq	%rbp, %rdi
	movl	$1, %esi
	movl	$2000053, %edx          ## imm = 0x1E84B5
	callq	_makeValueFrame
	movq	%rbp, %rdi
	movq	%r15, %rsi
	callq	_setParentOfActivationFrame
	movq	%rbp, %rdi
	xorl	%esi, %esi
	callq	_valueFrameReference
	movq	%rax, %rdi
	movl	$1, %esi
	xorl	%edx, %edx
	movq	%r15, %rcx
	callq	_sp_lexicalValueRead
	callq	_singleStepCallback
	movq	%r14, %rdi
	movl	$1, %esi
	callq	_loadTimeSymbolReference
Ltmp39:
	movq	%rbx, %rdi
	movq	%rax, %rsi
	movq	%rbp, %rdx
	callq	_sp_invokePossibleMultipleValueSymbolFunction
Ltmp40:
## BB#8:                                ## %(TRY-0).normal-dest14
	leaq	488(%rsp), %rbx
	movq	%rbx, %rdi
	callq	_trace_setActivationFrameForIHSTop
	callq	_singleStepCallback
	movl	$5, %edi
	movl	$9, %esi
Ltmp111:
	callq	_trace_setLineNumberColumnForIHSTop
	leaq	424(%rsp), %rbp
	movq	%rbp, %rdi
	movl	$1, %esi
	movl	$2000054, %edx          ## imm = 0x1E84B6
	callq	_makeValueFrame
	movq	%rbp, %rdi
	movq	%rbx, %rsi
	callq	_setParentOfActivationFrame
	movq	%rbp, %rdi
	xorl	%esi, %esi
	callq	_valueFrameReference
	movq	%rax, %rdi
	movl	$2, %esi
	movl	$1, %edx
	movq	%rbx, %rcx
	callq	_sp_lexicalValueRead
	callq	_singleStepCallback
	movq	_globalRunTimeValuesVector@GOTPCREL(%rip), %rdi
	movl	$2, %esi
	callq	_loadTimeSymbolReference
	leaq	440(%rsp), %rdi
Ltmp41:
	movq	%rax, %rsi
	movq	%rbp, %rdx
	callq	_sp_invokePossibleMultipleValueSymbolFunction
Ltmp42:
## BB#9:                                ## %(TRY-0).normal-dest18
	leaq	440(%rsp), %rdi
	callq	_isTrueTsp
	cmpl	$1, %eax
	jne	LBB1_11
## BB#10:                               ## %(TRY-0).then20
	movl	$5, %edi
	movl	$23, %esi
Ltmp112:
	callq	_trace_setLineNumberColumnForIHSTop
	leaq	408(%rsp), %rbx
	movq	%rbx, %rdi
	movl	$2, %esi
	movl	$2000055, %edx          ## imm = 0x1E84B7
	callq	_makeValueFrame
	leaq	488(%rsp), %rbp
	movq	%rbx, %rdi
	movq	%rbp, %rsi
	callq	_setParentOfActivationFrame
	movq	%rbx, %rdi
	xorl	%esi, %esi
	callq	_valueFrameReference
	movq	%rax, %rdi
	xorl	%esi, %esi
	xorl	%edx, %edx
	movq	%rbp, %rcx
	callq	_sp_lexicalValueRead
	movq	%rbx, %rdi
	movl	$1, %esi
	callq	_valueFrameReference
	movq	%rax, %rdi
	movl	$2, %esi
	movl	$1, %edx
	movq	%rbp, %rcx
	callq	_sp_lexicalValueRead
	callq	_singleStepCallback
	movq	_globalRunTimeValuesVector@GOTPCREL(%rip), %rdi
	movl	$3, %esi
	callq	_loadTimeSymbolReference
Ltmp75:
	movq	%r13, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	_mv_invokePossibleMultipleValueSymbolFunction
Ltmp76:
	jmp	LBB1_46
LBB1_11:                                ## %(TRY-0).else23
	movl	$6, %edi
	movl	$9, %esi
Ltmp113:
	callq	_trace_setLineNumberColumnForIHSTop
	leaq	376(%rsp), %rbx
	movq	%rbx, %rdi
	movl	$1, %esi
	movl	$2000056, %edx          ## imm = 0x1E84B8
	callq	_makeValueFrame
	leaq	488(%rsp), %rbp
	movq	%rbx, %rdi
	movq	%rbp, %rsi
	callq	_setParentOfActivationFrame
	movq	%rbx, %rdi
	xorl	%esi, %esi
	callq	_valueFrameReference
	movq	%rax, %rdi
	movl	$2, %esi
	movl	$1, %edx
	movq	%rbp, %rcx
	callq	_sp_lexicalValueRead
	callq	_singleStepCallback
	movq	_globalRunTimeValuesVector@GOTPCREL(%rip), %rdi
	movl	$4, %esi
	callq	_loadTimeSymbolReference
	leaq	392(%rsp), %rdi
Ltmp43:
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	_sp_invokePossibleMultipleValueSymbolFunction
Ltmp44:
## BB#12:                               ## %(TRY-0).normal-dest26
	leaq	392(%rsp), %rdi
	callq	_isTrueTsp
	cmpl	$1, %eax
	jne	LBB1_37
## BB#13:                               ## %(TRY-0).then28
	movq	%r13, 48(%rsp)          ## 8-byte Spill
	leaq	488(%rsp), %rbx
Ltmp114:
	movq	%rbx, %rdi
	callq	_trace_setActivationFrameForIHSTop
	leaq	344(%rsp), %rbp
	movq	%rbp, %rdi
	callq	_trace_setActivationFrameForIHSTop
	movq	%rbp, %rdi
	movl	$2, %esi
	movl	$2000057, %edx          ## imm = 0x1E84B9
	callq	_makeValueFrame
	movq	%rbp, %rdi
	movq	%rbx, %rsi
	callq	_setParentOfActivationFrame
	movq	_globalRunTimeValuesVector@GOTPCREL(%rip), %rbx
	movq	%rbx, %rdi
	movl	$41, %esi
	callq	_loadTimeValueReference
	movq	%rbp, %rdi
	movq	%rax, %rsi
	callq	_attachDebuggingInfoToValueFrame
	xorl	%edi, %edi
	xorl	%esi, %esi
	movq	%rbp, %rdx
	callq	_lexicalValueReference
	movq	%rax, %rdi
	movl	$1, %esi
	xorl	%edx, %edx
	movq	%rbp, %rcx
	callq	_sp_lexicalValueRead
	xorl	%edi, %edi
	movl	$1, %esi
	movq	%rbp, %rdx
	callq	_lexicalValueReference
	movq	%rax, %rdi
	movq	%rbx, %rsi
	movq	%rbx, %r13
	movl	$42, %edx
	callq	_sp_copyLoadTimeValue
	movq	%rbp, %rdi
	callq	_trace_setActivationFrameForIHSTop
	callq	_singleStepCallback
	leaq	312(%rsp), %r15
	movq	%r15, %rdi
	callq	_makeTagbodyFrame
	movq	%r15, %rdi
	movq	%rbp, %rsi
	callq	_setParentOfActivationFrame
	leaq	248(%rsp), %rbx
	jmp	LBB1_14
LBB1_37:                                ## %(TRY-0).else78
	movl	$14, %edi
	movl	$9, %esi
Ltmp115:
	callq	_trace_setLineNumberColumnForIHSTop
	leaq	72(%rsp), %rbx
	movq	%rbx, %rdi
	movl	$1, %esi
	movl	$2000063, %edx          ## imm = 0x1E84BF
	callq	_makeValueFrame
	leaq	488(%rsp), %rbp
	movq	%rbx, %rdi
	movq	%rbp, %rsi
	callq	_setParentOfActivationFrame
	movq	%rbx, %rdi
	xorl	%esi, %esi
	callq	_valueFrameReference
	movq	%rax, %rdi
	movl	$2, %esi
	movl	$1, %edx
	movq	%rbp, %rcx
	callq	_sp_lexicalValueRead
	callq	_singleStepCallback
	movq	_globalRunTimeValuesVector@GOTPCREL(%rip), %rdi
	movl	$11, %esi
	callq	_loadTimeSymbolReference
	leaq	88(%rsp), %rdi
Ltmp45:
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	_sp_invokePossibleMultipleValueSymbolFunction
Ltmp46:
## BB#38:                               ## %(TRY-0).normal-dest81
	leaq	88(%rsp), %rdi
	callq	_isTrueTsp
	cmpl	$1, %eax
	jne	LBB1_52
## BB#39:                               ## %(TRY-0).then83
	movl	$15, %edi
	movl	$9, %esi
Ltmp116:
	callq	_trace_setLineNumberColumnForIHSTop
	leaq	56(%rsp), %rbx
	movq	%rbx, %rdi
	movl	$2, %esi
	movl	$2000064, %edx          ## imm = 0x1E84C0
	callq	_makeValueFrame
	leaq	488(%rsp), %r14
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	_setParentOfActivationFrame
	movq	%rbx, %rdi
	xorl	%esi, %esi
	callq	_valueFrameReference
	movq	_globalRunTimeValuesVector@GOTPCREL(%rip), %rbp
	movq	%rax, %rdi
	movq	%rbp, %rsi
	movl	$43, %edx
	callq	_sp_copyLoadTimeValue
	movq	%rbx, %rdi
	movl	$1, %esi
	callq	_valueFrameReference
	movq	%rax, %rdi
	movl	$2, %esi
	movl	$1, %edx
	movq	%r14, %rcx
	callq	_sp_lexicalValueRead
	callq	_singleStepCallback
	movq	%rbp, %rdi
	movl	$12, %esi
	callq	_loadTimeSymbolReference
Ltmp47:
	movq	%r13, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	_mv_invokePossibleMultipleValueSymbolFunction
Ltmp48:
	jmp	LBB1_46
LBB1_26:                                ## %(TRY-0).handler-block22136
                                        ##   in Loop: Header=BB1_14 Depth=1
Ltmp117:
Ltmp118:
	movq	%r14, %rdi
	callq	___cxa_begin_catch
Ltmp52:
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	_tagbodyDynamicGoIndexElseRethrow
Ltmp53:
## BB#27:                               ## %(TRY-0).normal-dest60
                                        ##   in Loop: Header=BB1_14 Depth=1
	cmpl	$1, %eax
	je	LBB1_15
## BB#28:                               ## %(TRY-0).normal-dest60
                                        ##   in Loop: Header=BB1_14 Depth=1
	cmpl	$2, %eax
	jne	LBB1_29
LBB1_19:                                ## %(TRY-0).tagbody-#:G2291-2
                                        ##   in Loop: Header=BB1_14 Depth=1
	leaq	136(%rsp), %rbp
	movq	%rbp, %rdi
	movl	$2, %esi
	movl	$2000061, %edx          ## imm = 0x1E84BD
	callq	_makeValueFrame
	movq	%rbp, %rdi
	movq	%r15, %rsi
	callq	_setParentOfActivationFrame
	movq	%rbp, %rdi
	xorl	%esi, %esi
	callq	_valueFrameReference
	movq	%rax, %rdi
	movl	$1, %esi
	movl	$1, %edx
	movq	%r15, %rcx
	callq	_sp_lexicalValueRead
	movq	%rbp, %rdi
	movl	$1, %esi
	callq	_valueFrameReference
	movq	%rax, %rdi
	movl	$1, %esi
	xorl	%edx, %edx
	movq	%r15, %rcx
	callq	_sp_lexicalValueRead
	callq	_singleStepCallback
	movq	%r13, %rdi
	movl	$9, %esi
	callq	_loadTimeSymbolReference
Ltmp61:
	leaq	152(%rsp), %rdi
	movq	%rax, %rsi
	movq	%rbp, %rdx
	callq	_sp_invokePossibleMultipleValueSymbolFunction
Ltmp62:
## BB#20:                               ## %(TRY-0).normal-dest52
                                        ##   in Loop: Header=BB1_14 Depth=1
	leaq	152(%rsp), %rdi
	callq	_isTrueTsp
	cmpl	$1, %eax
	jne	LBB1_22
## BB#21:                               ## %(TRY-0).then54
                                        ##   in Loop: Header=BB1_14 Depth=1
Ltmp65:
	xorl	%edi, %edi
	movl	$1, %esi
	movq	%r15, %rdx
	callq	_throw_DynamicGo
Ltmp66:
	jmp	LBB1_22
LBB1_29:                                ## %(TRY-0).normal-dest60
                                        ##   in Loop: Header=BB1_14 Depth=1
	testl	%eax, %eax
	je	LBB1_14
	jmp	LBB1_30
LBB1_15:                                ## %(TRY-0).tagbody-#:G2290-1
                                        ##   in Loop: Header=BB1_14 Depth=1
Ltmp119:
	movq	%rbx, %rdi
	movl	$1, %esi
	movl	$2000058, %edx          ## imm = 0x1E84BA
	callq	_makeValueFrame
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	_setParentOfActivationFrame
	movq	%rbx, %rdi
	xorl	%esi, %esi
	callq	_valueFrameReference
	movq	%rax, %r14
	movq	%r13, %rbp
	movq	%rbp, %rdi
	movl	$5, %esi
	callq	_loadTimeSymbolReference
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	_sp_symbolValueRead
	callq	_singleStepCallback
	movq	%rbp, %rdi
	movq	%rbp, %r13
	movl	$6, %esi
	callq	_loadTimeSymbolReference
Ltmp55:
	leaq	264(%rsp), %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	_sp_invokePossibleMultipleValueSymbolFunction
Ltmp56:
## BB#16:                               ## %(TRY-0).normal-dest40
                                        ##   in Loop: Header=BB1_14 Depth=1
	leaq	264(%rsp), %rdi
	callq	_isTrueTsp
	leaq	280(%rsp), %rdi
	movq	%r13, %r12
	movq	%r12, %rsi
	xorl	%edx, %edx
	callq	_sp_copyLoadTimeValue
	movl	$4, %edi
	movl	$1, %esi
Ltmp120:
	movq	%r15, %rdx
	callq	_lexicalValueReference
	movq	%rax, %r14
	movl	$11, %edi
	movl	$14, %esi
Ltmp121:
	callq	_trace_setLineNumberColumnForIHSTop
	leaq	216(%rsp), %rbp
	movq	%rbp, %rdi
	movl	$1, %esi
	movl	$2000059, %edx          ## imm = 0x1E84BB
	callq	_makeValueFrame
	movq	%rbp, %rdi
	movq	%r15, %rsi
	callq	_setParentOfActivationFrame
	movq	%rbp, %rdi
	xorl	%esi, %esi
	callq	_valueFrameReference
	movq	%rax, %rdi
	movl	$4, %esi
	movl	$1, %edx
	movq	%r15, %rcx
	callq	_sp_lexicalValueRead
	callq	_singleStepCallback
	movq	%r12, %rdi
	movq	%r12, %r13
	movl	$7, %esi
	callq	_loadTimeSymbolReference
Ltmp57:
	leaq	232(%rsp), %rdi
	movq	%rax, %rsi
	movq	%rbp, %rdx
	callq	_sp_invokePossibleMultipleValueSymbolFunction
Ltmp58:
## BB#17:                               ## %(TRY-0).normal-dest45
                                        ##   in Loop: Header=BB1_14 Depth=1
	movq	%r14, %rdi
	leaq	232(%rsp), %rbp
	movq	%rbp, %rsi
	callq	_sp_copyTsp
	leaq	280(%rsp), %rdi
	movq	%rbp, %rsi
	callq	_sp_copyTsp
	movl	$1, %edi
	movl	$1, %esi
	movq	%r15, %rdx
	callq	_lexicalValueReference
	movq	%rax, %r14
	leaq	184(%rsp), %rbp
	movq	%rbp, %rdi
	movl	$1, %esi
	movl	$2000060, %edx          ## imm = 0x1E84BC
	callq	_makeValueFrame
	movq	%rbp, %rdi
	movq	%r15, %rsi
	callq	_setParentOfActivationFrame
	movq	%rbp, %rdi
	xorl	%esi, %esi
	callq	_valueFrameReference
	movq	%rax, %rdi
	movl	$1, %esi
	movl	$1, %edx
	movq	%r15, %rcx
	callq	_sp_lexicalValueRead
	callq	_singleStepCallback
	movq	%r13, %rdi
	movl	$8, %esi
	callq	_loadTimeSymbolReference
Ltmp59:
	leaq	200(%rsp), %rdi
	movq	%rax, %rsi
	movq	%rbp, %rdx
	callq	_sp_invokePossibleMultipleValueSymbolFunction
Ltmp60:
## BB#18:                               ## %(TRY-0).normal-dest48
                                        ##   in Loop: Header=BB1_14 Depth=1
	movq	%r14, %rdi
	leaq	200(%rsp), %rbp
	movq	%rbp, %rsi
	callq	_sp_copyTsp
	leaq	328(%rsp), %rdi
	movq	%rbp, %rsi
	callq	_sp_copyTsp
	jmp	LBB1_19
Ltmp122:
LBB1_23:                                ## %(TRY-0).landing-pad.split-lp
                                        ##   in Loop: Header=BB1_14 Depth=1
Ltmp67:
	jmp	LBB1_24
LBB1_14:                                ## %(TRY-0).tagbody-#:G2292-0
                                        ## =>This Inner Loop Header: Depth=1
Ltmp49:
Ltmp123:
	xorl	%edi, %edi
	movl	$2, %esi
	movq	%r15, %rdx
	callq	_throw_DynamicGo
Ltmp50:
	jmp	LBB1_15
LBB1_51:                                ## %(TRY-0).landing-pad.preheader
                                        ##   in Loop: Header=BB1_14 Depth=1
Ltmp51:
LBB1_24:                                ## %(TRY-0).landing-pad
                                        ##   in Loop: Header=BB1_14 Depth=1
	movq	%rdx, %r12
	movq	%rax, %r14
Ltmp124:
Ltmp125:
	movq	%r14, 608(%rsp)
	movl	%r12d, 604(%rsp)
	leaq	328(%rsp), %rdi
	movq	%r13, %rsi
	xorl	%edx, %edx
	callq	_sp_copyLoadTimeValue
	cmpl	$2, %r12d
	je	LBB1_26
Ltmp126:
## BB#25:
	movq	48(%rsp), %r13          ## 8-byte Reload
	jmp	LBB1_34
LBB1_52:                                ## %(TRY-0).else86
Ltmp127:
	movq	_globalRunTimeValuesVector@GOTPCREL(%rip), %rsi
	movq	%r13, %rdi
	xorl	%edx, %edx
	callq	_mv_copyLoadTimeValue
LBB1_46:                                ## %(TRY-0).try-cont108
	leaq	56(%rsp), %rdi
	callq	_destructAFsp
	leaq	72(%rsp), %rdi
	callq	_destructAFsp
	leaq	88(%rsp), %rdi
	callq	_destructTsp
	leaq	104(%rsp), %rdi
	callq	_destructAFsp
	leaq	120(%rsp), %rdi
	callq	_destructTsp
	leaq	136(%rsp), %rdi
	callq	_destructAFsp
	leaq	152(%rsp), %rdi
	callq	_destructTsp
	leaq	168(%rsp), %rdi
	callq	_destructTsp
	leaq	184(%rsp), %rdi
	callq	_destructAFsp
	leaq	200(%rsp), %rdi
	callq	_destructTsp
	leaq	216(%rsp), %rdi
	callq	_destructAFsp
	leaq	232(%rsp), %rdi
	callq	_destructTsp
	leaq	248(%rsp), %rdi
	callq	_destructAFsp
	leaq	264(%rsp), %rdi
	callq	_destructTsp
	leaq	280(%rsp), %rdi
	callq	_destructTsp
	leaq	296(%rsp), %rdi
	callq	_destructTsp
	leaq	312(%rsp), %rdi
	callq	_destructAFsp
	leaq	328(%rsp), %rdi
	callq	_destructTsp
	leaq	344(%rsp), %rdi
	callq	_destructAFsp
	leaq	360(%rsp), %rdi
	callq	_destructTsp
	leaq	376(%rsp), %rdi
	callq	_destructAFsp
	leaq	392(%rsp), %rdi
	callq	_destructTsp
	leaq	408(%rsp), %rdi
	callq	_destructAFsp
	leaq	424(%rsp), %rdi
	callq	_destructAFsp
	leaq	440(%rsp), %rdi
	callq	_destructTsp
	leaq	456(%rsp), %rdi
	callq	_destructTsp
	leaq	472(%rsp), %rdi
	callq	_destructAFsp
	leaq	488(%rsp), %rdi
	callq	_destructAFsp
	leaq	504(%rsp), %rdi
	callq	_destructTsp
	leaq	520(%rsp), %rdi
	callq	_destructTsp
	leaq	536(%rsp), %rdi
	callq	_destructAFsp
	leaq	552(%rsp), %rdi
	callq	_destructAFsp
	leaq	568(%rsp), %rdi
	callq	_destructTsp
	leaq	584(%rsp), %rdi
	callq	_destructAFsp
	addq	$616, %rsp              ## imm = 0x268
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	ret
LBB1_30:                                ## %(TRY-0).switch-default
Ltmp128:
Ltmp68:
Ltmp129:
	movl	%eax, %edi
	movl	$3, %esi
	movq	48(%rsp), %r13          ## 8-byte Reload
	callq	_throwIllegalSwitchValue
Ltmp69:
LBB1_32:                                ## %(TRY-0).landing-pad67.nonloopexit
Ltmp70:
	movq	%rdx, %r12
	movq	%rax, %r14
	jmp	LBB1_33
LBB1_31:                                ## %(TRY-0).landing-pad67.loopexit
Ltmp54:
	movq	%rdx, %r12
	movq	%rax, %r14
	movq	48(%rsp), %r13          ## 8-byte Reload
LBB1_33:                                ## %(TRY-0).landing-pad67
Ltmp130:
	movq	%r14, 608(%rsp)
	movl	%r12d, 604(%rsp)
LBB1_34:                                ## %(TRY-0).dispatch-header72
	cmpl	$1, %r12d
	jne	LBB1_43
## BB#35:                               ## %(TRY-0).handler-block24051
	movq	%r14, %rdi
	callq	___cxa_begin_catch
Ltmp71:
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	_mv_blockHandleReturnFrom
Ltmp72:
## BB#36:                               ## %(TRY-0).normal-dest75
Ltmp73:
	callq	___cxa_end_catch
Ltmp74:
	jmp	LBB1_46
Ltmp131:
LBB1_40:                                ## %(TRY-0).landing-pad91
Ltmp77:
	jmp	LBB1_42
LBB1_1:                                 ## %(TRY-0).error
	cmpl	$1, %eax
	jg	LBB1_3
## BB#2:                                ## %(TRY-0).error1
Ltmp33:
	leaq	"_:::global-str-MATCH-DIMENSIONS"(%rip), %rcx
	movq	%rcx, %rdi
	movq	%rbx, %rsi
	movl	%eax, %edx
	movl	$2, %ecx
	callq	_throwNotEnoughArgumentsException
Ltmp34:
LBB1_3:                                 ## %(TRY-0).continue
Ltmp31:
	leaq	"_:::global-str-MATCH-DIMENSIONS"(%rip), %rcx
	movq	%rcx, %rdi
	movq	%rbx, %rsi
	movl	%eax, %edx
	movl	$2, %ecx
	callq	_throwTooManyArgumentsException
Ltmp32:
	jmp	LBB1_4
LBB1_48:                                ## %(TRY-0).func-cleanup-landing-pad
Ltmp35:
	jmp	LBB1_49
LBB1_41:                                ## %(TRY-0).landing-pad96
Ltmp38:
LBB1_42:                                ## %(TRY-0).dispatch-header101
	movq	%rdx, %r12
	movq	%rax, %r14
Ltmp132:
	movq	%r14, 608(%rsp)
	movl	%r12d, 604(%rsp)
LBB1_43:                                ## %(TRY-0).dispatch-header101
	cmpl	$1, %r12d
	jne	LBB1_50
## BB#44:                               ## %(TRY-0).handler-block24051104
	movq	%r14, %rdi
	callq	___cxa_begin_catch
Ltmp78:
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	_mv_blockHandleReturnFrom
Ltmp79:
## BB#45:                               ## %(TRY-0).normal-dest106
Ltmp80:
	callq	___cxa_end_catch
Ltmp81:
	jmp	LBB1_46
LBB1_47:                                ## %(TRY-0).landing-pad110
Ltmp82:
LBB1_49:                                ## %(TRY-0).func-ehcleanup
	movq	%rdx, %rcx
	movq	%rax, %r14
	movq	%r14, 608(%rsp)
	movl	%ecx, 604(%rsp)
LBB1_50:                                ## %(TRY-0).func-ehcleanup
	leaq	56(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	leaq	72(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	leaq	88(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	104(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	leaq	120(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	136(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	leaq	152(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	168(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	184(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	leaq	200(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	216(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	leaq	232(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	248(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	leaq	264(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	280(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	296(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	312(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	leaq	328(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	344(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	leaq	360(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	376(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	leaq	392(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	408(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	leaq	424(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	leaq	440(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	456(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	472(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	leaq	488(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	leaq	504(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	520(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	536(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	leaq	552(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	leaq	568(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	584(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	movl	$90, %edi
	callq	_debugPrintI32
	movl	$91, %edi
	callq	_debugPrintI32
	movq	%r14, %rdi
	callq	__Unwind_Resume
LBB1_22:                                ## %(TRY-0).else58
	leaq	328(%rsp), %rbx
Ltmp133:
Ltmp134:
	movq	_globalRunTimeValuesVector@GOTPCREL(%rip), %r14
	movq	%rbx, %rdi
	movq	%r14, %rsi
	xorl	%edx, %edx
	callq	_sp_copyLoadTimeValue
	movq	%rbx, %rdi
	movq	%r14, %rsi
	xorl	%edx, %edx
	callq	_sp_copyLoadTimeValue
	movl	$7, %edi
	movl	$26, %esi
Ltmp135:
	callq	_trace_setLineNumberColumnForIHSTop
	leaq	104(%rsp), %rbx
	movq	%rbx, %rdi
	movl	$1, %esi
	movl	$2000062, %edx          ## imm = 0x1E84BE
	callq	_makeValueFrame
	leaq	344(%rsp), %rbp
	movq	%rbx, %rdi
	movq	%rbp, %rsi
	callq	_setParentOfActivationFrame
	movq	%rbx, %rdi
	xorl	%esi, %esi
	callq	_valueFrameReference
	movq	%rax, %rdi
	movl	$3, %esi
	movl	$1, %edx
	movq	%rbp, %rcx
	callq	_sp_lexicalValueRead
	callq	_singleStepCallback
	movq	%r14, %rdi
	movl	$10, %esi
	callq	_loadTimeSymbolReference
Ltmp63:
	movq	48(%rsp), %r13          ## 8-byte Reload
	movq	%r13, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	_mv_invokePossibleMultipleValueSymbolFunction
Ltmp64:
	jmp	LBB1_46
Ltmp136:
Lfunc_end1:
	.cfi_endproc
Leh_func_end1:
	.section	__TEXT,__gcc_except_tab
	.align	2
GCC_except_table1:
Lexception1:
	.byte	255                     ## @LPStart Encoding = omit
	.byte	155                     ## @TType Encoding = indirect pcrel sdata4
	.ascii	"\240\001"              ## @TType base offset
	.byte	3                       ## Call site Encoding = udata4
	.ascii	"\217\001"              ## Call site table length
Lset8 = Ltmp36-Leh_func_begin1          ## >> Call Site 1 <<
	.long	Lset8
Lset9 = Ltmp37-Ltmp36                   ##   Call between Ltmp36 and Ltmp37
	.long	Lset9
Lset10 = Ltmp38-Leh_func_begin1         ##     jumps to Ltmp38
	.long	Lset10
	.byte	3                       ##   On action: 2
Lset11 = Ltmp39-Leh_func_begin1         ## >> Call Site 2 <<
	.long	Lset11
Lset12 = Ltmp48-Ltmp39                  ##   Call between Ltmp39 and Ltmp48
	.long	Lset12
Lset13 = Ltmp77-Leh_func_begin1         ##     jumps to Ltmp77
	.long	Lset13
	.byte	3                       ##   On action: 2
Lset14 = Ltmp52-Leh_func_begin1         ## >> Call Site 3 <<
	.long	Lset14
Lset15 = Ltmp53-Ltmp52                  ##   Call between Ltmp52 and Ltmp53
	.long	Lset15
Lset16 = Ltmp54-Leh_func_begin1         ##     jumps to Ltmp54
	.long	Lset16
	.byte	3                       ##   On action: 2
Lset17 = Ltmp61-Leh_func_begin1         ## >> Call Site 4 <<
	.long	Lset17
Lset18 = Ltmp60-Ltmp61                  ##   Call between Ltmp61 and Ltmp60
	.long	Lset18
Lset19 = Ltmp67-Leh_func_begin1         ##     jumps to Ltmp67
	.long	Lset19
	.byte	5                       ##   On action: 3
Lset20 = Ltmp49-Leh_func_begin1         ## >> Call Site 5 <<
	.long	Lset20
Lset21 = Ltmp50-Ltmp49                  ##   Call between Ltmp49 and Ltmp50
	.long	Lset21
Lset22 = Ltmp51-Leh_func_begin1         ##     jumps to Ltmp51
	.long	Lset22
	.byte	5                       ##   On action: 3
Lset23 = Ltmp68-Leh_func_begin1         ## >> Call Site 6 <<
	.long	Lset23
Lset24 = Ltmp69-Ltmp68                  ##   Call between Ltmp68 and Ltmp69
	.long	Lset24
Lset25 = Ltmp70-Leh_func_begin1         ##     jumps to Ltmp70
	.long	Lset25
	.byte	3                       ##   On action: 2
Lset26 = Ltmp71-Leh_func_begin1         ## >> Call Site 7 <<
	.long	Lset26
Lset27 = Ltmp74-Ltmp71                  ##   Call between Ltmp71 and Ltmp74
	.long	Lset27
Lset28 = Ltmp77-Leh_func_begin1         ##     jumps to Ltmp77
	.long	Lset28
	.byte	3                       ##   On action: 2
Lset29 = Ltmp33-Leh_func_begin1         ## >> Call Site 8 <<
	.long	Lset29
Lset30 = Ltmp32-Ltmp33                  ##   Call between Ltmp33 and Ltmp32
	.long	Lset30
Lset31 = Ltmp35-Leh_func_begin1         ##     jumps to Ltmp35
	.long	Lset31
	.byte	0                       ##   On action: cleanup
Lset32 = Ltmp78-Leh_func_begin1         ## >> Call Site 9 <<
	.long	Lset32
Lset33 = Ltmp81-Ltmp78                  ##   Call between Ltmp78 and Ltmp81
	.long	Lset33
Lset34 = Ltmp82-Leh_func_begin1         ##     jumps to Ltmp82
	.long	Lset34
	.byte	0                       ##   On action: cleanup
Lset35 = Ltmp81-Leh_func_begin1         ## >> Call Site 10 <<
	.long	Lset35
Lset36 = Ltmp63-Ltmp81                  ##   Call between Ltmp81 and Ltmp63
	.long	Lset36
	.long	0                       ##     has no landing pad
	.byte	0                       ##   On action: cleanup
Lset37 = Ltmp63-Leh_func_begin1         ## >> Call Site 11 <<
	.long	Lset37
Lset38 = Ltmp64-Ltmp63                  ##   Call between Ltmp63 and Ltmp64
	.long	Lset38
Lset39 = Ltmp70-Leh_func_begin1         ##     jumps to Ltmp70
	.long	Lset39
	.byte	3                       ##   On action: 2
	.byte	0                       ## >> Action Record 1 <<
                                        ##   Cleanup
	.byte	0                       ##   No further actions
	.byte	1                       ## >> Action Record 2 <<
                                        ##   Catch TypeInfo 1
	.byte	125                     ##   Continue to action 1
	.byte	2                       ## >> Action Record 3 <<
                                        ##   Catch TypeInfo 2
	.byte	125                     ##   Continue to action 2
                                        ## >> Catch TypeInfos <<
	.long	__ZTIN4core9DynamicGoE@GOTPCREL+4 ## TypeInfo 2
	.long	__ZTIN4core10ReturnFromE@GOTPCREL+4 ## TypeInfo 1
	.align	2

	.section	__TEXT,__cstring,cstring_literals
	.align	4                       ## @":::global-str-/Users/meister/DEVELOPMENT/CANDO/BRCL/SRC/LISP/KERNEL/LSP/test_matchdim.lsp"
"_:::global-str-/Users/meister/DEVELOPMENT/CANDO/BRCL/SRC/LISP/KERNEL/LSP/test_matchdim.lsp":
	.asciz	"/Users/meister/DEVELOPMENT/CANDO/BRCL/SRC/LISP/KERNEL/LSP/test_matchdim.lsp"

"_:::global-str-lambda":                ## @":::global-str-lambda"
	.asciz	"lambda"

	.align	4                       ## @":::global-str-MATCH-DIMENSIONS"
"_:::global-str-MATCH-DIMENSIONS":
	.asciz	"MATCH-DIMENSIONS"

	.section	__TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
	.section	__TEXT,__text,regular,pure_instructions
Ldebug_end0:
	.section	__DWARF,__debug_str,regular,debug
Linfo_string0:
	.asciz	"brcl Common Lisp compiler"
Linfo_string1:
	.asciz	"test_matchdim"
Linfo_string2:
	.asciz	"/Users/meister/DEVELOPMENT/CANDO/BRCL/SRC/LISP/KERNEL/LSP/"
Linfo_string3:
	.asciz	"-v"
Linfo_string4:
	.asciz	"lambda"
Linfo_string5:
	.asciz	"int"
Linfo_string6:
	.asciz	"MATCH-DIMENSIONS"
	.section	__DWARF,__debug_info,regular,debug
L__DWARF__debug_info_begin0:
	.long	113                     ## Length of Compilation Unit Info
	.short	2                       ## DWARF version number
Lset40 = L__DWARF__debug_abbrev_begin-Lsection_abbrev ## Offset Into Abbrev. Section
	.long	Lset40
	.byte	8                       ## Address Size (in bytes)
	.byte	1                       ## Abbrev [1] 0xb:0x6a DW_TAG_compile_unit
Lset41 = Linfo_string0-Linfo_string     ## DW_AT_producer
	.long	Lset41
	.short	2                       ## DW_AT_language
Lset42 = Linfo_string1-Linfo_string     ## DW_AT_name
	.long	Lset42
	.quad	0                       ## DW_AT_low_pc
	.long	0                       ## DW_AT_stmt_list
Lset43 = Linfo_string2-Linfo_string     ## DW_AT_comp_dir
	.long	Lset43
Lset44 = Linfo_string3-Linfo_string     ## DW_AT_APPLE_flags
	.long	Lset44
	.byte	1                       ## DW_AT_APPLE_major_runtime_vers
	.byte	2                       ## Abbrev [2] 0x2b:0x21 DW_TAG_subprogram
Lset45 = Linfo_string4-Linfo_string     ## DW_AT_MIPS_linkage_name
	.long	Lset45
Lset46 = Linfo_string4-Linfo_string     ## DW_AT_name
	.long	Lset46
	.long	76                      ## DW_AT_type
	.byte	1                       ## DW_AT_external
	.quad	Lfunc_begin0            ## DW_AT_low_pc
	.quad	Lfunc_end0              ## DW_AT_high_pc
	.byte	1                       ## DW_AT_frame_base
	.byte	87
	.byte	1                       ## DW_AT_APPLE_omit_frame_ptr
	.byte	3                       ## Abbrev [3] 0x4c:0x7 DW_TAG_base_type
Lset47 = Linfo_string5-Linfo_string     ## DW_AT_name
	.long	Lset47
	.byte	13                      ## DW_AT_encoding
	.byte	4                       ## DW_AT_byte_size
	.byte	2                       ## Abbrev [2] 0x53:0x21 DW_TAG_subprogram
Lset48 = Linfo_string6-Linfo_string     ## DW_AT_MIPS_linkage_name
	.long	Lset48
Lset49 = Linfo_string6-Linfo_string     ## DW_AT_name
	.long	Lset49
	.long	76                      ## DW_AT_type
	.byte	1                       ## DW_AT_external
	.quad	Lfunc_begin1            ## DW_AT_low_pc
	.quad	Lfunc_end1              ## DW_AT_high_pc
	.byte	1                       ## DW_AT_frame_base
	.byte	87
	.byte	1                       ## DW_AT_APPLE_omit_frame_ptr
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
	.byte	73                      ## DW_AT_type
	.byte	19                      ## DW_FORM_ref4
	.byte	63                      ## DW_AT_external
	.byte	12                      ## DW_FORM_flag
	.byte	17                      ## DW_AT_low_pc
	.byte	1                       ## DW_FORM_addr
	.byte	18                      ## DW_AT_high_pc
	.byte	1                       ## DW_FORM_addr
	.byte	64                      ## DW_AT_frame_base
	.byte	10                      ## DW_FORM_block1
	.ascii	"\347\177"              ## DW_AT_APPLE_omit_frame_ptr
	.byte	12                      ## DW_FORM_flag
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
	.byte	0                       ## EOM(3)
L__DWARF__debug_abbrev_end:
	.section	__DWARF,__debug_aranges,regular,debug
	.long	44                      ## Length of ARange Set
	.short	2                       ## DWARF Arange version number
Lset50 = L__DWARF__debug_info_begin0-Lsection_info ## Offset Into Debug Info Section
	.long	Lset50
	.byte	8                       ## Address Size (in bytes)
	.byte	0                       ## Segment Size (in bytes)
	.byte	255
	.byte	255
	.byte	255
	.byte	255
	.quad	Lfunc_begin0
Lset51 = Ldebug_end0-Lfunc_begin0
	.quad	Lset51
	.quad	0                       ## ARange terminator
	.quad	0
	.section	__DWARF,__debug_ranges,regular,debug
	.section	__DWARF,__debug_macinfo,regular,debug
	.section	__DWARF,__apple_names,regular,debug
Lnames_begin:
	.long	1212240712              ## Header Magic
	.short	1                       ## Header Version
	.short	0                       ## Header Hash Function
	.long	2                       ## Header Bucket Count
	.long	2                       ## Header Hash Count
	.long	12                      ## Header Data Length
	.long	0                       ## HeaderData Die Offset Base
	.long	1                       ## HeaderData Atom Count
	.short	1                       ## DW_ATOM_die_offset
	.short	6                       ## DW_FORM_data4
	.long	0                       ## Bucket 0
	.long	-1                      ## Bucket 1
	.long	182772998               ## Hash in Bucket 0
	.long	555690424               ## Hash in Bucket 0
	.long	LNames0-Lnames_begin    ## Offset in Bucket 0
	.long	LNames1-Lnames_begin    ## Offset in Bucket 0
LNames0:
Lset52 = Linfo_string4-Linfo_string     ## lambda
	.long	Lset52
	.long	1                       ## Num DIEs
	.long	43
	.long	0
LNames1:
Lset53 = Linfo_string6-Linfo_string     ## MATCH-DIMENSIONS
	.long	Lset53
	.long	1                       ## Num DIEs
	.long	83
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
Lset54 = Linfo_string5-Linfo_string     ## int
	.long	Lset54
	.long	1                       ## Num DIEs
	.long	76
	.short	36
	.byte	0
	.long	0

.subsections_via_symbols
	.section	__DWARF,__debug_line,regular,debug
Lline_table_start0:
Ltmp138 = (Ltmp137-Lline_table_start0)-4
	.long	Ltmp138
	.short	2
Ltmp140 = (Ltmp139-Lline_table_start0)-10
	.long	Ltmp140
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
	.ascii	"test_matchdim"
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
Ltmp139:
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp16
	.byte	17
	.byte	5
	.byte	25
	.byte	10
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp29
	.byte	20
	.byte	5
	.byte	0
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp90
	.byte	16
	.byte	5
	.byte	3
	.byte	10
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp105
	.byte	21
	.byte	5
	.byte	7
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp107
	.byte	1
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp108
	.byte	19
	.byte	5
	.byte	19
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp110
	.byte	1
	.byte	5
	.byte	9
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp111
	.byte	19
	.byte	5
	.byte	23
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp112
	.byte	1
	.byte	5
	.byte	9
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp113
	.byte	19
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp114
	.byte	19
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp115
	.byte	25
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp116
	.byte	19
	.byte	5
	.byte	14
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp118
	.byte	14
	.byte	5
	.byte	4
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp119
	.byte	15
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp120
	.byte	21
	.byte	5
	.byte	14
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp121
	.byte	1
	.byte	5
	.byte	9
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp123
	.byte	14
	.byte	5
	.byte	14
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp125
	.byte	22
	.byte	5
	.byte	9
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp127
	.byte	22
	.byte	5
	.byte	14
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp129
	.byte	14
	.byte	5
	.byte	26
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp130
	.byte	14
	.byte	5
	.byte	9
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp132
	.byte	26
	.byte	5
	.byte	14
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp134
	.byte	14
	.byte	5
	.byte	26
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp135
	.byte	14
	.section	__TEXT,__text,regular,pure_instructions
Ltmp141:
	.section	__DWARF,__debug_line,regular,debug
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp141
	.ascii	"\002\000\000\001\001"
Ltmp137:
