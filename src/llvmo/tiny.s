	.section	__TEXT,__text,regular,pure_instructions
	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
Leh_func_begin0:
	.cfi_lsda 16, Lexception0
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp20:
	.cfi_def_cfa_offset 16
Ltmp21:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp22:
	.cfi_def_cfa_register %rbp
	subq	$96, %rsp
	movl	$0, -4(%rbp)
	movl	%edi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	leaq	-24(%rbp), %rdi
	callq	__ZN9TestClassC1Ev
	leaq	-32(%rbp), %rsi
	movq	%rsi, %rdi
	movq	%rsi, -64(%rbp)         ## 8-byte Spill
	callq	__ZN9TestClassC1Ev
Ltmp0:
	movq	-64(%rbp), %rdi         ## 8-byte Reload
	callq	__ZN9TestClassD1Ev
Ltmp1:
	jmp	LBB0_1
LBB0_1:                                 ## %invoke.cont
	movl	$8, %edi
	callq	___cxa_allocate_exception
	movq	$0, (%rax)
	movq	%rax, %rdi
	movq	%rax, -72(%rbp)         ## 8-byte Spill
	callq	__ZN11MyExceptionC1Ev
Ltmp2:
	movq	__ZTI11MyException@GOTPCREL(%rip), %rsi
	movq	__ZN11MyExceptionD1Ev@GOTPCREL(%rip), %rdx
	movq	-72(%rbp), %rdi         ## 8-byte Reload
	callq	___cxa_throw
Ltmp3:
	jmp	LBB0_16
LBB0_2:                                 ## %lpad
Ltmp4:
	movl	%edx, %ecx
	movq	%rax, -40(%rbp)
	movl	%ecx, -44(%rbp)
## BB#3:                                ## %catch.dispatch
	movl	-44(%rbp), %eax
	movl	$1, %ecx
	cmpl	%ecx, %eax
	jne	LBB0_12
## BB#4:                                ## %catch
	movq	-40(%rbp), %rdi
	callq	___cxa_begin_catch
	movq	%rax, -56(%rbp)
Ltmp5:
	xorb	%al, %al
	leaq	L_.str(%rip), %rdi
	callq	_printf
Ltmp6:
	movl	%eax, -76(%rbp)         ## 4-byte Spill
	jmp	LBB0_5
LBB0_5:                                 ## %invoke.cont2
Ltmp10:
	callq	___cxa_end_catch
Ltmp11:
	jmp	LBB0_6
LBB0_6:                                 ## %invoke.cont4
	jmp	LBB0_7
LBB0_7:                                 ## %try.cont
Ltmp12:
	xorb	%al, %al
	leaq	L_.str1(%rip), %rdi
	callq	_printf
Ltmp13:
	movl	%eax, -80(%rbp)         ## 4-byte Spill
	jmp	LBB0_8
LBB0_8:                                 ## %invoke.cont6
	leaq	-24(%rbp), %rdi
	callq	__ZN9TestClassD1Ev
	movl	-4(%rbp), %eax
	addq	$96, %rsp
	popq	%rbp
	ret
LBB0_9:                                 ## %lpad1
Ltmp7:
	movl	%edx, %ecx
	movq	%rax, -40(%rbp)
	movl	%ecx, -44(%rbp)
Ltmp8:
	callq	___cxa_end_catch
Ltmp9:
	jmp	LBB0_11
LBB0_10:                                ## %lpad3
Ltmp14:
	movl	%edx, %ecx
	movq	%rax, -40(%rbp)
	movl	%ecx, -44(%rbp)
	jmp	LBB0_12
LBB0_11:                                ## %invoke.cont5
	jmp	LBB0_12
LBB0_12:                                ## %ehcleanup
Ltmp15:
	leaq	-24(%rbp), %rdi
	callq	__ZN9TestClassD1Ev
Ltmp16:
	jmp	LBB0_13
LBB0_13:                                ## %invoke.cont8
	jmp	LBB0_14
LBB0_14:                                ## %eh.resume
	movq	-40(%rbp), %rdi
	callq	__Unwind_Resume
LBB0_15:                                ## %terminate.lpad
Ltmp17:
	movl	%edx, %ecx
	movq	%rax, %rdi
	movl	%ecx, -84(%rbp)         ## 4-byte Spill
	callq	___clang_call_terminate
LBB0_16:                                ## %unreachable
	.cfi_endproc
Leh_func_end0:
	.section	__TEXT,__gcc_except_tab
	.align	2
GCC_except_table0:
Lexception0:
	.byte	255                     ## @LPStart Encoding = omit
	.byte	155                     ## @TType Encoding = indirect pcrel sdata4
	.asciz	 "\205\201\200\200"     ## @TType base offset
	.byte	3                       ## Call site Encoding = udata4
	.byte	117                     ## Call site table length
Lset0 = Ltmp0-Leh_func_begin0           ## >> Call Site 1 <<
	.long	Lset0
Lset1 = Ltmp1-Ltmp0                     ##   Call between Ltmp0 and Ltmp1
	.long	Lset1
Lset2 = Ltmp4-Leh_func_begin0           ##     jumps to Ltmp4
	.long	Lset2
	.byte	3                       ##   On action: 2
Lset3 = Ltmp1-Leh_func_begin0           ## >> Call Site 2 <<
	.long	Lset3
Lset4 = Ltmp2-Ltmp1                     ##   Call between Ltmp1 and Ltmp2
	.long	Lset4
	.long	0                       ##     has no landing pad
	.byte	0                       ##   On action: cleanup
Lset5 = Ltmp2-Leh_func_begin0           ## >> Call Site 3 <<
	.long	Lset5
Lset6 = Ltmp3-Ltmp2                     ##   Call between Ltmp2 and Ltmp3
	.long	Lset6
Lset7 = Ltmp4-Leh_func_begin0           ##     jumps to Ltmp4
	.long	Lset7
	.byte	3                       ##   On action: 2
Lset8 = Ltmp3-Leh_func_begin0           ## >> Call Site 4 <<
	.long	Lset8
Lset9 = Ltmp5-Ltmp3                     ##   Call between Ltmp3 and Ltmp5
	.long	Lset9
	.long	0                       ##     has no landing pad
	.byte	0                       ##   On action: cleanup
Lset10 = Ltmp5-Leh_func_begin0          ## >> Call Site 5 <<
	.long	Lset10
Lset11 = Ltmp6-Ltmp5                    ##   Call between Ltmp5 and Ltmp6
	.long	Lset11
Lset12 = Ltmp7-Leh_func_begin0          ##     jumps to Ltmp7
	.long	Lset12
	.byte	0                       ##   On action: cleanup
Lset13 = Ltmp10-Leh_func_begin0         ## >> Call Site 6 <<
	.long	Lset13
Lset14 = Ltmp13-Ltmp10                  ##   Call between Ltmp10 and Ltmp13
	.long	Lset14
Lset15 = Ltmp14-Leh_func_begin0         ##     jumps to Ltmp14
	.long	Lset15
	.byte	0                       ##   On action: cleanup
Lset16 = Ltmp13-Leh_func_begin0         ## >> Call Site 7 <<
	.long	Lset16
Lset17 = Ltmp8-Ltmp13                   ##   Call between Ltmp13 and Ltmp8
	.long	Lset17
	.long	0                       ##     has no landing pad
	.byte	0                       ##   On action: cleanup
Lset18 = Ltmp8-Leh_func_begin0          ## >> Call Site 8 <<
	.long	Lset18
Lset19 = Ltmp16-Ltmp8                   ##   Call between Ltmp8 and Ltmp16
	.long	Lset19
Lset20 = Ltmp17-Leh_func_begin0         ##     jumps to Ltmp17
	.long	Lset20
	.byte	5                       ##   On action: 3
Lset21 = Ltmp16-Leh_func_begin0         ## >> Call Site 9 <<
	.long	Lset21
Lset22 = Leh_func_end0-Ltmp16           ##   Call between Ltmp16 and Leh_func_end0
	.long	Lset22
	.long	0                       ##     has no landing pad
	.byte	0                       ##   On action: cleanup
	.byte	0                       ## >> Action Record 1 <<
                                        ##   Cleanup
	.byte	0                       ##   No further actions
	.byte	1                       ## >> Action Record 2 <<
                                        ##   Catch TypeInfo 1
	.byte	125                     ##   Continue to action 1
	.byte	2                       ## >> Action Record 3 <<
                                        ##   Catch TypeInfo 2
	.byte	0                       ##   No further actions
                                        ## >> Catch TypeInfos <<
	.long	0                       ## TypeInfo 2
	.long	__ZTI11MyException@GOTPCREL+4 ## TypeInfo 1
	.align	2

	.section	__TEXT,__textcoal_nt,coalesced,pure_instructions
	.globl	__ZN9TestClassC1Ev
	.weak_definition	__ZN9TestClassC1Ev
	.align	4, 0x90
__ZN9TestClassC1Ev:                     ## @_ZN9TestClassC1Ev
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp25:
	.cfi_def_cfa_offset 16
Ltmp26:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp27:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	callq	__ZN9TestClassC2Ev
	addq	$16, %rsp
	popq	%rbp
	ret
	.cfi_endproc

	.globl	__ZN9TestClassD1Ev
	.weak_definition	__ZN9TestClassD1Ev
	.align	4, 0x90
__ZN9TestClassD1Ev:                     ## @_ZN9TestClassD1Ev
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp30:
	.cfi_def_cfa_offset 16
Ltmp31:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp32:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	callq	__ZN9TestClassD2Ev
	addq	$16, %rsp
	popq	%rbp
	ret
	.cfi_endproc

	.globl	__ZN11MyExceptionC1Ev
	.weak_definition	__ZN11MyExceptionC1Ev
	.align	4, 0x90
__ZN11MyExceptionC1Ev:                  ## @_ZN11MyExceptionC1Ev
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp35:
	.cfi_def_cfa_offset 16
Ltmp36:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp37:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	callq	__ZN11MyExceptionC2Ev
	addq	$16, %rsp
	popq	%rbp
	ret
	.cfi_endproc

	.globl	__ZN11MyExceptionD1Ev
	.weak_definition	__ZN11MyExceptionD1Ev
	.align	4, 0x90
__ZN11MyExceptionD1Ev:                  ## @_ZN11MyExceptionD1Ev
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp40:
	.cfi_def_cfa_offset 16
Ltmp41:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp42:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	callq	__ZN11MyExceptionD2Ev
	addq	$16, %rsp
	popq	%rbp
	ret
	.cfi_endproc

	.private_extern	___clang_call_terminate
	.globl	___clang_call_terminate
	.weak_definition	___clang_call_terminate
	.align	4, 0x90
___clang_call_terminate:                ## @__clang_call_terminate
## BB#0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	callq	___cxa_begin_catch
	movq	%rax, -8(%rbp)          ## 8-byte Spill
	callq	__ZSt9terminatev

	.globl	__ZN11MyExceptionD2Ev
	.weak_definition	__ZN11MyExceptionD2Ev
	.align	4, 0x90
__ZN11MyExceptionD2Ev:                  ## @_ZN11MyExceptionD2Ev
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp45:
	.cfi_def_cfa_offset 16
Ltmp46:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp47:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	popq	%rbp
	ret
	.cfi_endproc

	.globl	__ZN11MyExceptionC2Ev
	.weak_definition	__ZN11MyExceptionC2Ev
	.align	4, 0x90
__ZN11MyExceptionC2Ev:                  ## @_ZN11MyExceptionC2Ev
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp50:
	.cfi_def_cfa_offset 16
Ltmp51:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp52:
	.cfi_def_cfa_register %rbp
	movq	__ZTV11MyException@GOTPCREL(%rip), %rax
	addq	$16, %rax
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	movq	%rax, (%rdi)
	popq	%rbp
	ret
	.cfi_endproc

	.globl	__ZN11MyExceptionD0Ev
	.weak_definition	__ZN11MyExceptionD0Ev
	.align	4, 0x90
__ZN11MyExceptionD0Ev:                  ## @_ZN11MyExceptionD0Ev
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
Leh_func_begin8:
	.cfi_lsda 16, Lexception8
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp58:
	.cfi_def_cfa_offset 16
Ltmp59:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp60:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rdi, %rax
Ltmp53:
	movq	%rax, -32(%rbp)         ## 8-byte Spill
	callq	__ZN11MyExceptionD1Ev
Ltmp54:
	jmp	LBB8_1
LBB8_1:                                 ## %invoke.cont
	movq	-32(%rbp), %rax         ## 8-byte Reload
	movq	%rax, %rdi
	callq	__ZdlPv
	addq	$32, %rsp
	popq	%rbp
	ret
LBB8_2:                                 ## %lpad
Ltmp55:
	movl	%edx, %ecx
	movq	%rax, -16(%rbp)
	movl	%ecx, -20(%rbp)
	movq	-32(%rbp), %rax         ## 8-byte Reload
	movq	%rax, %rdi
	callq	__ZdlPv
## BB#3:                                ## %eh.resume
	movq	-16(%rbp), %rdi
	callq	__Unwind_Resume
	.cfi_endproc
Leh_func_end8:
	.section	__TEXT,__gcc_except_tab
	.align	2
GCC_except_table8:
Lexception8:
	.byte	255                     ## @LPStart Encoding = omit
	.byte	155                     ## @TType Encoding = indirect pcrel sdata4
	.asciz	 "\234"                 ## @TType base offset
	.byte	3                       ## Call site Encoding = udata4
	.byte	26                      ## Call site table length
Lset23 = Ltmp53-Leh_func_begin8         ## >> Call Site 1 <<
	.long	Lset23
Lset24 = Ltmp54-Ltmp53                  ##   Call between Ltmp53 and Ltmp54
	.long	Lset24
Lset25 = Ltmp55-Leh_func_begin8         ##     jumps to Ltmp55
	.long	Lset25
	.byte	0                       ##   On action: cleanup
Lset26 = Ltmp54-Leh_func_begin8         ## >> Call Site 2 <<
	.long	Lset26
Lset27 = Leh_func_end8-Ltmp54           ##   Call between Ltmp54 and Leh_func_end8
	.long	Lset27
	.long	0                       ##     has no landing pad
	.byte	0                       ##   On action: cleanup
	.align	2

	.section	__TEXT,__textcoal_nt,coalesced,pure_instructions
	.globl	__ZN9TestClassD2Ev
	.weak_definition	__ZN9TestClassD2Ev
	.align	4, 0x90
__ZN9TestClassD2Ev:                     ## @_ZN9TestClassD2Ev
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp63:
	.cfi_def_cfa_offset 16
Ltmp64:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp65:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	popq	%rbp
	ret
	.cfi_endproc

	.globl	__ZN9TestClassC2Ev
	.weak_definition	__ZN9TestClassC2Ev
	.align	4, 0x90
__ZN9TestClassC2Ev:                     ## @_ZN9TestClassC2Ev
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp68:
	.cfi_def_cfa_offset 16
Ltmp69:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp70:
	.cfi_def_cfa_register %rbp
	movq	__ZTV9TestClass@GOTPCREL(%rip), %rax
	addq	$16, %rax
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	movq	%rax, (%rdi)
	popq	%rbp
	ret
	.cfi_endproc

	.globl	__ZN9TestClassD0Ev
	.weak_definition	__ZN9TestClassD0Ev
	.align	4, 0x90
__ZN9TestClassD0Ev:                     ## @_ZN9TestClassD0Ev
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
Leh_func_begin11:
	.cfi_lsda 16, Lexception11
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp76:
	.cfi_def_cfa_offset 16
Ltmp77:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp78:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rdi, %rax
Ltmp71:
	movq	%rax, -32(%rbp)         ## 8-byte Spill
	callq	__ZN9TestClassD1Ev
Ltmp72:
	jmp	LBB11_1
LBB11_1:                                ## %invoke.cont
	movq	-32(%rbp), %rax         ## 8-byte Reload
	movq	%rax, %rdi
	callq	__ZdlPv
	addq	$32, %rsp
	popq	%rbp
	ret
LBB11_2:                                ## %lpad
Ltmp73:
	movl	%edx, %ecx
	movq	%rax, -16(%rbp)
	movl	%ecx, -20(%rbp)
	movq	-32(%rbp), %rax         ## 8-byte Reload
	movq	%rax, %rdi
	callq	__ZdlPv
## BB#3:                                ## %eh.resume
	movq	-16(%rbp), %rdi
	callq	__Unwind_Resume
	.cfi_endproc
Leh_func_end11:
	.section	__TEXT,__gcc_except_tab
	.align	2
GCC_except_table11:
Lexception11:
	.byte	255                     ## @LPStart Encoding = omit
	.byte	155                     ## @TType Encoding = indirect pcrel sdata4
	.asciz	 "\234"                 ## @TType base offset
	.byte	3                       ## Call site Encoding = udata4
	.byte	26                      ## Call site table length
Lset28 = Ltmp71-Leh_func_begin11        ## >> Call Site 1 <<
	.long	Lset28
Lset29 = Ltmp72-Ltmp71                  ##   Call between Ltmp71 and Ltmp72
	.long	Lset29
Lset30 = Ltmp73-Leh_func_begin11        ##     jumps to Ltmp73
	.long	Lset30
	.byte	0                       ##   On action: cleanup
Lset31 = Ltmp72-Leh_func_begin11        ## >> Call Site 2 <<
	.long	Lset31
Lset32 = Leh_func_end11-Ltmp72          ##   Call between Ltmp72 and Leh_func_end11
	.long	Lset32
	.long	0                       ##     has no landing pad
	.byte	0                       ##   On action: cleanup
	.align	2

	.section	__TEXT,__const_coal,coalesced
	.globl	__ZTS11MyException      ## @_ZTS11MyException
	.weak_definition	__ZTS11MyException
__ZTS11MyException:
	.asciz	 "11MyException"

	.section	__DATA,__datacoal_nt,coalesced
	.globl	__ZTI11MyException      ## @_ZTI11MyException
	.weak_definition	__ZTI11MyException
	.align	3
__ZTI11MyException:
	.quad	__ZTVN10__cxxabiv117__class_type_infoE+16
	.quad	__ZTS11MyException

	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	 "Caught MyException\n"

L_.str1:                                ## @.str1
	.asciz	 "Done\n"

	.section	__DATA,__datacoal_nt,coalesced
	.globl	__ZTV11MyException      ## @_ZTV11MyException
	.weak_definition	__ZTV11MyException
	.align	4
__ZTV11MyException:
	.quad	0
	.quad	__ZTI11MyException
	.quad	__ZN11MyExceptionD1Ev
	.quad	__ZN11MyExceptionD0Ev

	.globl	__ZTV9TestClass         ## @_ZTV9TestClass
	.weak_definition	__ZTV9TestClass
	.align	4
__ZTV9TestClass:
	.quad	0
	.quad	__ZTI9TestClass
	.quad	__ZN9TestClassD1Ev
	.quad	__ZN9TestClassD0Ev

	.section	__TEXT,__const_coal,coalesced
	.globl	__ZTS9TestClass         ## @_ZTS9TestClass
	.weak_definition	__ZTS9TestClass
__ZTS9TestClass:
	.asciz	 "9TestClass"

	.section	__DATA,__datacoal_nt,coalesced
	.globl	__ZTI9TestClass         ## @_ZTI9TestClass
	.weak_definition	__ZTI9TestClass
	.align	3
__ZTI9TestClass:
	.quad	__ZTVN10__cxxabiv117__class_type_infoE+16
	.quad	__ZTS9TestClass

	.section	__TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support

.subsections_via_symbols
