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
	movl	$2, %esi
	callq	_loadTimeValueReference
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	_attachDebuggingInfoToValueFrame
	movl	$505, %edi              ## imm = 0x1F9
	movl	$3, %esi
Ltmp28:
Ltmp29:
	callq	_trace_setLineNumberColumnForIHSTop
	leaq	(%rsp), %rbx
	movq	%rbx, %rdi
	movl	$3, %esi
	movl	$2000000, %edx          ## imm = 0x1E8480
	callq	_makeValueFrame
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	_setParentOfActivationFrame
	movq	%rbx, %rdi
	xorl	%esi, %esi
	callq	_valueFrameReference
	movq	%rax, %rdi
	movq	%r12, %rsi
	movl	$3, %edx
	callq	_sp_copyLoadTimeValue
	movq	%rbx, %rdi
	movl	$1, %esi
	callq	_valueFrameReference
	movq	%rax, %r13
	movq	%r12, %rdi
	movl	$6, %esi
	callq	_loadTimeValueReference
	movq	%rax, %rbx
	movq	%r12, %rdi
	movl	$3, %esi
	callq	_loadTimeValueReference
	leaq	_A(%rip), %rsi
	leaq	"_:::global-str-compile-in-env"(%rip), %rdx
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
	movl	$1, %esi
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
	movl	$3, %edx
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
_A:                                     ## @A
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
Leh_func_begin1:
	.cfi_lsda 16, Lexception1
Lfunc_begin1:
## BB#0:                                ## %(TRY-0).entry
Ltmp49:
	pushq	%r15
Ltmp50:
	.cfi_def_cfa_offset 16
	pushq	%r14
Ltmp51:
	.cfi_def_cfa_offset 24
	pushq	%r12
Ltmp52:
	.cfi_def_cfa_offset 32
	pushq	%rbx
Ltmp53:
	.cfi_def_cfa_offset 40
	subq	$72, %rsp
Ltmp54:
	.cfi_def_cfa_offset 112
Ltmp55:
	.cfi_offset %rbx, -40
Ltmp56:
	.cfi_offset %r12, -32
Ltmp57:
	.cfi_offset %r14, -24
Ltmp58:
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
	jne	LBB1_1
LBB1_4:                                 ## %(TRY-0).continue3
	leaq	40(%rsp), %r15
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	_copyAFsp
	movq	_globalRunTimeValuesVector@GOTPCREL(%rip), %r12
	movq	%r12, %rdi
	movl	$4, %esi
	callq	_loadTimeValueReference
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	_attachDebuggingInfoToValueFrame
	movq	%r15, %rdi
	callq	_trace_setActivationFrameForIHSTop
	movl	$1, %edi
	movl	$13, %esi
Ltmp59:
Ltmp60:
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
Ltmp36:
	movq	%r14, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	_mv_invokePossibleMultipleValueSymbolFunction
Ltmp37:
LBB1_8:                                 ## %(TRY-0).try-cont
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
LBB1_1:                                 ## %(TRY-0).error
	testl	%eax, %eax
	jns	LBB1_3
## BB#2:                                ## %(TRY-0).error1
Ltmp33:
	leaq	"_:::global-str-A"(%rip), %rcx
	movq	%rcx, %rdi
	movq	%rbx, %rsi
	movl	%eax, %edx
	xorl	%ecx, %ecx
	callq	_throwNotEnoughArgumentsException
Ltmp34:
LBB1_3:                                 ## %(TRY-0).continue
Ltmp31:
	leaq	"_:::global-str-A"(%rip), %rcx
	movq	%rcx, %rdi
	movq	%rbx, %rsi
	movl	%eax, %edx
	xorl	%ecx, %ecx
	callq	_throwTooManyArgumentsException
Ltmp32:
	jmp	LBB1_4
LBB1_10:                                ## %(TRY-0).func-cleanup-landing-pad
Ltmp35:
	jmp	LBB1_11
LBB1_5:                                 ## %(TRY-0).landing-pad
Ltmp38:
	movq	%rdx, %rcx
	movq	%rax, %rbx
	movq	%rbx, 64(%rsp)
	movl	%ecx, 60(%rsp)
	cmpl	$1, %ecx
	jne	LBB1_12
## BB#6:                                ## %(TRY-0).handler-block6118
	movq	%rbx, %rdi
	callq	___cxa_begin_catch
Ltmp39:
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	_mv_blockHandleReturnFrom
Ltmp40:
## BB#7:                                ## %(TRY-0).normal-dest6
Ltmp41:
	callq	___cxa_end_catch
Ltmp42:
	jmp	LBB1_8
LBB1_9:                                 ## %(TRY-0).landing-pad9
Ltmp43:
LBB1_11:                                ## %(TRY-0).func-ehcleanup
	movq	%rdx, %rcx
	movq	%rax, %rbx
	movq	%rbx, 64(%rsp)
	movl	%ecx, 60(%rsp)
LBB1_12:                                ## %(TRY-0).func-ehcleanup
	leaq	8(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	leaq	24(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructTsp
	leaq	40(%rsp), %rax
	movq	%rax, %rdi
	callq	_destructAFsp
	movl	$90, %edi
	callq	_debugPrintI32
	movl	$91, %edi
	callq	_debugPrintI32
	movq	%rbx, %rdi
	callq	__Unwind_Resume
Ltmp61:
Lfunc_end1:
	.cfi_endproc
Leh_func_end1:
	.section	__TEXT,__gcc_except_tab
	.align	2
GCC_except_table1:
Lexception1:
	.byte	255                     ## @LPStart Encoding = omit
	.byte	155                     ## @TType Encoding = indirect pcrel sdata4
	.asciz	"\276\200\200"          ## @TType base offset
	.byte	3                       ## Call site Encoding = udata4
	.byte	52                      ## Call site table length
Lset8 = Ltmp36-Leh_func_begin1          ## >> Call Site 1 <<
	.long	Lset8
Lset9 = Ltmp37-Ltmp36                   ##   Call between Ltmp36 and Ltmp37
	.long	Lset9
Lset10 = Ltmp38-Leh_func_begin1         ##     jumps to Ltmp38
	.long	Lset10
	.byte	3                       ##   On action: 2
Lset11 = Ltmp33-Leh_func_begin1         ## >> Call Site 2 <<
	.long	Lset11
Lset12 = Ltmp32-Ltmp33                  ##   Call between Ltmp33 and Ltmp32
	.long	Lset12
Lset13 = Ltmp35-Leh_func_begin1         ##     jumps to Ltmp35
	.long	Lset13
	.byte	0                       ##   On action: cleanup
Lset14 = Ltmp39-Leh_func_begin1         ## >> Call Site 3 <<
	.long	Lset14
Lset15 = Ltmp42-Ltmp39                  ##   Call between Ltmp39 and Ltmp42
	.long	Lset15
Lset16 = Ltmp43-Leh_func_begin1         ##     jumps to Ltmp43
	.long	Lset16
	.byte	0                       ##   On action: cleanup
Lset17 = Ltmp42-Leh_func_begin1         ## >> Call Site 4 <<
	.long	Lset17
Lset18 = Leh_func_end1-Ltmp42           ##   Call between Ltmp42 and Leh_func_end1
	.long	Lset18
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

	.section	__TEXT,__cstring,cstring_literals
"_:::global-str-compile-in-env":        ## @":::global-str-compile-in-env"
	.asciz	"compile-in-env"

"_:::global-str-lambda":                ## @":::global-str-lambda"
	.asciz	"lambda"

"_:::global-str-A":                     ## @":::global-str-A"
	.asciz	"A"

	.section	__TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
	.section	__TEXT,__text,regular,pure_instructions
Ldebug_end0:
	.section	__DWARF,__debug_str,regular,debug
Linfo_string0:
	.asciz	"brcl Common Lisp compiler"
Linfo_string1:
	.asciz	"compile-in-env"
Linfo_string2:
	.asciz	"-v"
Linfo_string3:
	.asciz	"lambda"
Linfo_string4:
	.asciz	"int"
Linfo_string5:
	.asciz	"A"
	.section	__DWARF,__debug_info,regular,debug
L__DWARF__debug_info_begin0:
	.long	110                     ## Length of Compilation Unit Info
	.short	4                       ## DWARF version number
Lset19 = L__DWARF__debug_abbrev_begin-Lsection_abbrev ## Offset Into Abbrev. Section
	.long	Lset19
	.byte	8                       ## Address Size (in bytes)
	.byte	1                       ## Abbrev [1] 0xb:0x67 DW_TAG_compile_unit
Lset20 = Linfo_string0-Linfo_string     ## DW_AT_producer
	.long	Lset20
	.short	32768                   ## DW_AT_language
Lset21 = Linfo_string1-Linfo_string     ## DW_AT_name
	.long	Lset21
	.quad	0                       ## DW_AT_low_pc
	.long	0                       ## DW_AT_stmt_list
Lset22 = Linfo_string2-Linfo_string     ## DW_AT_APPLE_flags
	.long	Lset22
	.byte	1                       ## DW_AT_APPLE_major_runtime_vers
	.byte	2                       ## Abbrev [2] 0x27:0x1f DW_TAG_subprogram
Lset23 = Linfo_string3-Linfo_string     ## DW_AT_MIPS_linkage_name
	.long	Lset23
Lset24 = Linfo_string3-Linfo_string     ## DW_AT_name
	.long	Lset24
	.long	70                      ## DW_AT_type
                                        ## DW_AT_external
	.quad	Lfunc_begin0            ## DW_AT_low_pc
	.quad	Lfunc_end0              ## DW_AT_high_pc
	.byte	1                       ## DW_AT_frame_base
	.byte	87
                                        ## DW_AT_APPLE_omit_frame_ptr
	.byte	3                       ## Abbrev [3] 0x46:0x7 DW_TAG_base_type
Lset25 = Linfo_string4-Linfo_string     ## DW_AT_name
	.long	Lset25
	.byte	13                      ## DW_AT_encoding
	.byte	4                       ## DW_AT_byte_size
	.byte	4                       ## Abbrev [4] 0x4d:0xd DW_TAG_subprogram
Lset26 = Linfo_string5-Linfo_string     ## DW_AT_MIPS_linkage_name
	.long	Lset26
Lset27 = Linfo_string5-Linfo_string     ## DW_AT_name
	.long	Lset27
	.long	70                      ## DW_AT_type
                                        ## DW_AT_external
                                        ## DW_AT_declaration
	.byte	5                       ## Abbrev [5] 0x5a:0x17 DW_TAG_subprogram
	.long	77                      ## DW_AT_specification
	.quad	Lfunc_begin1            ## DW_AT_low_pc
	.quad	Lfunc_end1              ## DW_AT_high_pc
	.byte	1                       ## DW_AT_frame_base
	.byte	87
                                        ## DW_AT_APPLE_omit_frame_ptr
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
	.byte	60                      ## DW_AT_declaration
	.byte	25                      ## DW_FORM_flag_present
	.byte	0                       ## EOM(1)
	.byte	0                       ## EOM(2)
	.byte	5                       ## Abbreviation Code
	.byte	46                      ## DW_TAG_subprogram
	.byte	0                       ## DW_CHILDREN_no
	.byte	71                      ## DW_AT_specification
	.byte	19                      ## DW_FORM_ref4
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
Lset28 = L__DWARF__debug_info_begin0-Lsection_info ## Offset Into Debug Info Section
	.long	Lset28
	.byte	8                       ## Address Size (in bytes)
	.byte	0                       ## Segment Size (in bytes)
	.byte	255
	.byte	255
	.byte	255
	.byte	255
	.quad	Lfunc_begin0
Lset29 = Ldebug_end0-Lfunc_begin0
	.quad	Lset29
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
	.long	177638                  ## Hash in Bucket 0
	.long	LNames0-Lnames_begin    ## Offset in Bucket 0
	.long	LNames1-Lnames_begin    ## Offset in Bucket 0
LNames0:
Lset30 = Linfo_string3-Linfo_string     ## lambda
	.long	Lset30
	.long	1                       ## Num DIEs
	.long	39
	.long	0
LNames1:
Lset31 = Linfo_string5-Linfo_string     ## A
	.long	Lset31
	.long	1                       ## Num DIEs
	.long	90
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
Lset32 = Linfo_string4-Linfo_string     ## int
	.long	Lset32
	.long	1                       ## Num DIEs
	.long	70
	.short	36
	.byte	0
	.long	0

.subsections_via_symbols
	.section	__DWARF,__debug_line,regular,debug
Lline_table_start0:
Ltmp63 = (Ltmp62-Lline_table_start0)-4
	.long	Ltmp63
	.short	2
Ltmp65 = (Ltmp64-Lline_table_start0)-10
	.long	Ltmp65
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
	.ascii	"compile-in-env"
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
Ltmp64:
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp16
	.byte	17
	.byte	5
	.byte	3
	.byte	10
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp29
	.ascii	"\003\371\003\001"
	.byte	5
	.byte	0
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp49
	.ascii	"\003\207|\001"
	.byte	5
	.byte	13
	.byte	10
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp60
	.byte	19
	.section	__TEXT,__text,regular,pure_instructions
Ltmp66:
	.section	__DWARF,__debug_line,regular,debug
	.byte	0
	.byte	9
	.byte	2
	.quad	Ltmp66
	.ascii	"\002\000\000\001\001"
Ltmp62:
