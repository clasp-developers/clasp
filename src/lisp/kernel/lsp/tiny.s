	.section	__TEXT,__text,regular,pure_instructions
	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
Ltmp21:
Leh_func_begin0:
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp22:
Ltmp23:
	movq	%rsp, %rbp
Ltmp24:
	pushq	%rbx
	subq	$56, %rsp
Ltmp25:
	movl	$0, -12(%rbp)
	movl	%edi, -16(%rbp)
	movq	%rsi, -24(%rbp)
	leaq	-32(%rbp), %rdi
	callq	__ZN9TestClassC1Ev
	leaq	-40(%rbp), %rbx
	movq	%rbx, %rdi
	callq	__ZN9TestClassC1Ev
Ltmp0:
	movq	%rbx, %rdi
	callq	__ZN9TestClassD1Ev
Ltmp1:
## BB#1:                                ## %invoke.cont
	movl	$8, %edi
	callq	___cxa_allocate_exception
	movq	%rax, %rbx
	movq	$0, (%rbx)
	movq	%rbx, %rdi
	callq	__ZN11MyExceptionC1Ev
Ltmp2:
	movq	__ZTI11MyException@GOTPCREL(%rip), %rsi
	movq	__ZN11MyExceptionD1Ev@GOTPCREL(%rip), %rdx
	movq	%rbx, %rdi
	callq	___cxa_throw
Ltmp3:
	jmp	LBB0_12
LBB0_2:                                 ## %catch.dispatch
Ltmp4:
	movq	%rax, -48(%rbp)
	movl	%edx, -52(%rbp)
	cmpl	$1, %edx
	jne	LBB0_9
## BB#3:                                ## %catch
	movq	-48(%rbp), %rdi
	callq	___cxa_begin_catch
	movq	%rax, -64(%rbp)
Ltmp5:
	leaq	L_.str(%rip), %rdi
	xorb	%al, %al
	callq	_printf
Ltmp6:
## BB#4:                                ## %invoke.cont2
Ltmp10:
	callq	___cxa_end_catch
Ltmp11:
## BB#5:                                ## %try.cont
Ltmp12:
	leaq	L_.str1(%rip), %rdi
	xorb	%al, %al
	callq	_printf
Ltmp13:
## BB#6:                                ## %invoke.cont6
	leaq	-32(%rbp), %rdi
	callq	__ZN9TestClassD1Ev
	movl	-12(%rbp), %eax
	addq	$56, %rsp
	popq	%rbx
	popq	%rbp
	ret
LBB0_12:                                ## %unreachable
LBB0_7:                                 ## %lpad1
Ltmp7:
	movq	%rax, -48(%rbp)
	movl	%edx, -52(%rbp)
Ltmp8:
	callq	___cxa_end_catch
Ltmp9:
	jmp	LBB0_9
LBB0_8:                                 ## %lpad3
Ltmp14:
	movq	%rax, -48(%rbp)
	movl	%edx, -52(%rbp)
LBB0_9:                                 ## %ehcleanup
Ltmp15:
	leaq	-32(%rbp), %rdi
	callq	__ZN9TestClassD1Ev
Ltmp16:
## BB#10:                               ## %eh.resume
	movq	-48(%rbp), %rdi
	callq	__Unwind_Resume
LBB0_11:                                ## %terminate.lpad
Ltmp17:
	movq	%rax, %rdi
	callq	___clang_call_terminate
Ltmp26:
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
Ltmp29:
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp30:
Ltmp31:
	movq	%rsp, %rbp
Ltmp32:
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	callq	__ZN9TestClassC2Ev
	addq	$16, %rsp
	popq	%rbp
	ret
Ltmp33:

	.globl	__ZN9TestClassD1Ev
	.weak_definition	__ZN9TestClassD1Ev
	.align	4, 0x90
__ZN9TestClassD1Ev:                     ## @_ZN9TestClassD1Ev
Ltmp36:
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp37:
Ltmp38:
	movq	%rsp, %rbp
Ltmp39:
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	callq	__ZN9TestClassD2Ev
	addq	$16, %rsp
	popq	%rbp
	ret
Ltmp40:

	.globl	__ZN11MyExceptionC1Ev
	.weak_definition	__ZN11MyExceptionC1Ev
	.align	4, 0x90
__ZN11MyExceptionC1Ev:                  ## @_ZN11MyExceptionC1Ev
Ltmp43:
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp44:
Ltmp45:
	movq	%rsp, %rbp
Ltmp46:
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	callq	__ZN11MyExceptionC2Ev
	addq	$16, %rsp
	popq	%rbp
	ret
Ltmp47:

	.globl	__ZN11MyExceptionD1Ev
	.weak_definition	__ZN11MyExceptionD1Ev
	.align	4, 0x90
__ZN11MyExceptionD1Ev:                  ## @_ZN11MyExceptionD1Ev
Ltmp50:
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp51:
Ltmp52:
	movq	%rsp, %rbp
Ltmp53:
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	callq	__ZN11MyExceptionD2Ev
	addq	$16, %rsp
	popq	%rbp
	ret
Ltmp54:

	.private_extern	___clang_call_terminate
	.globl	___clang_call_terminate
	.weak_definition	___clang_call_terminate
	.align	4, 0x90
___clang_call_terminate:                ## @__clang_call_terminate
## BB#0:
	pushq	%rbp
	movq	%rsp, %rbp
	callq	___cxa_begin_catch
	callq	__ZSt9terminatev

	.globl	__ZN11MyExceptionD2Ev
	.weak_definition	__ZN11MyExceptionD2Ev
	.align	4, 0x90
__ZN11MyExceptionD2Ev:                  ## @_ZN11MyExceptionD2Ev
Ltmp57:
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp58:
Ltmp59:
	movq	%rsp, %rbp
Ltmp60:
	movq	%rdi, -8(%rbp)
	popq	%rbp
	ret
Ltmp61:

	.globl	__ZN11MyExceptionC2Ev
	.weak_definition	__ZN11MyExceptionC2Ev
	.align	4, 0x90
__ZN11MyExceptionC2Ev:                  ## @_ZN11MyExceptionC2Ev
Ltmp64:
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp65:
Ltmp66:
	movq	%rsp, %rbp
Ltmp67:
	movq	%rdi, -8(%rbp)
	movq	__ZTV11MyException@GOTPCREL(%rip), %rax
	addq	$16, %rax
	movq	%rax, (%rdi)
	popq	%rbp
	ret
Ltmp68:

	.globl	__ZN11MyExceptionD0Ev
	.weak_definition	__ZN11MyExceptionD0Ev
	.align	4, 0x90
__ZN11MyExceptionD0Ev:                  ## @_ZN11MyExceptionD0Ev
Ltmp75:
Leh_func_begin8:
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp76:
Ltmp77:
	movq	%rsp, %rbp
Ltmp78:
	pushq	%rbx
	subq	$24, %rsp
Ltmp79:
	movq	%rdi, %rbx
	movq	%rbx, -16(%rbp)
Ltmp69:
	callq	__ZN11MyExceptionD1Ev
Ltmp70:
## BB#1:                                ## %invoke.cont
	movq	%rbx, %rdi
	callq	__ZdlPv
	addq	$24, %rsp
	popq	%rbx
	popq	%rbp
	ret
LBB8_2:                                 ## %lpad
Ltmp71:
	movq	%rax, -24(%rbp)
	movl	%edx, -28(%rbp)
	movq	%rbx, %rdi
	callq	__ZdlPv
	movq	-24(%rbp), %rdi
	callq	__Unwind_Resume
Ltmp80:
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
Lset23 = Ltmp69-Leh_func_begin8         ## >> Call Site 1 <<
	.long	Lset23
Lset24 = Ltmp70-Ltmp69                  ##   Call between Ltmp69 and Ltmp70
	.long	Lset24
Lset25 = Ltmp71-Leh_func_begin8         ##     jumps to Ltmp71
	.long	Lset25
	.byte	0                       ##   On action: cleanup
Lset26 = Ltmp70-Leh_func_begin8         ## >> Call Site 2 <<
	.long	Lset26
Lset27 = Leh_func_end8-Ltmp70           ##   Call between Ltmp70 and Leh_func_end8
	.long	Lset27
	.long	0                       ##     has no landing pad
	.byte	0                       ##   On action: cleanup
	.align	2

	.section	__TEXT,__textcoal_nt,coalesced,pure_instructions
	.globl	__ZN9TestClassD2Ev
	.weak_definition	__ZN9TestClassD2Ev
	.align	4, 0x90
__ZN9TestClassD2Ev:                     ## @_ZN9TestClassD2Ev
Ltmp83:
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp84:
Ltmp85:
	movq	%rsp, %rbp
Ltmp86:
	movq	%rdi, -8(%rbp)
	popq	%rbp
	ret
Ltmp87:

	.globl	__ZN9TestClassC2Ev
	.weak_definition	__ZN9TestClassC2Ev
	.align	4, 0x90
__ZN9TestClassC2Ev:                     ## @_ZN9TestClassC2Ev
Ltmp90:
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp91:
Ltmp92:
	movq	%rsp, %rbp
Ltmp93:
	movq	%rdi, -8(%rbp)
	movq	__ZTV9TestClass@GOTPCREL(%rip), %rax
	addq	$16, %rax
	movq	%rax, (%rdi)
	popq	%rbp
	ret
Ltmp94:

	.globl	__ZN9TestClassD0Ev
	.weak_definition	__ZN9TestClassD0Ev
	.align	4, 0x90
__ZN9TestClassD0Ev:                     ## @_ZN9TestClassD0Ev
Ltmp101:
Leh_func_begin11:
## BB#0:                                ## %entry
	pushq	%rbp
Ltmp102:
Ltmp103:
	movq	%rsp, %rbp
Ltmp104:
	pushq	%rbx
	subq	$24, %rsp
Ltmp105:
	movq	%rdi, %rbx
	movq	%rbx, -16(%rbp)
Ltmp95:
	callq	__ZN9TestClassD1Ev
Ltmp96:
## BB#1:                                ## %invoke.cont
	movq	%rbx, %rdi
	callq	__ZdlPv
	addq	$24, %rsp
	popq	%rbx
	popq	%rbp
	ret
LBB11_2:                                ## %lpad
Ltmp97:
	movq	%rax, -24(%rbp)
	movl	%edx, -28(%rbp)
	movq	%rbx, %rdi
	callq	__ZdlPv
	movq	-24(%rbp), %rdi
	callq	__Unwind_Resume
Ltmp106:
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
Lset28 = Ltmp95-Leh_func_begin11        ## >> Call Site 1 <<
	.long	Lset28
Lset29 = Ltmp96-Ltmp95                  ##   Call between Ltmp95 and Ltmp96
	.long	Lset29
Lset30 = Ltmp97-Leh_func_begin11        ##     jumps to Ltmp97
	.long	Lset30
	.byte	0                       ##   On action: cleanup
Lset31 = Ltmp96-Leh_func_begin11        ## >> Call Site 2 <<
	.long	Lset31
Lset32 = Leh_func_end11-Ltmp96          ##   Call between Ltmp96 and Leh_func_end11
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
	.section	__LD,__compact_unwind,regular,debug
	.align	3
	.quad	_main                   ## Range Start
Ltmp107 = (Ltmp26-Ltmp21)-0             ## Range Length
	.long	Ltmp107
	.long	1090584577              ## Compact Unwind Encoding: 0x41010001
	.quad	___gxx_personality_v0   ## Personality Function
	.quad	Lexception0             ## LSDA
	.quad	__ZN9TestClassC1Ev      ## Range Start
Ltmp108 = (Ltmp33-Ltmp29)-0             ## Range Length
	.long	Ltmp108
	.long	16777216                ## Compact Unwind Encoding: 0x1000000
	.quad	0                       ## Personality Function
	.quad	0                       ## LSDA
	.quad	__ZN9TestClassD1Ev      ## Range Start
Ltmp109 = (Ltmp40-Ltmp36)-0             ## Range Length
	.long	Ltmp109
	.long	16777216                ## Compact Unwind Encoding: 0x1000000
	.quad	0                       ## Personality Function
	.quad	0                       ## LSDA
	.quad	__ZN11MyExceptionC1Ev   ## Range Start
Ltmp110 = (Ltmp47-Ltmp43)-0             ## Range Length
	.long	Ltmp110
	.long	16777216                ## Compact Unwind Encoding: 0x1000000
	.quad	0                       ## Personality Function
	.quad	0                       ## LSDA
	.quad	__ZN11MyExceptionD1Ev   ## Range Start
Ltmp111 = (Ltmp54-Ltmp50)-0             ## Range Length
	.long	Ltmp111
	.long	16777216                ## Compact Unwind Encoding: 0x1000000
	.quad	0                       ## Personality Function
	.quad	0                       ## LSDA
	.quad	__ZN11MyExceptionD2Ev   ## Range Start
Ltmp112 = (Ltmp61-Ltmp57)-0             ## Range Length
	.long	Ltmp112
	.long	16777216                ## Compact Unwind Encoding: 0x1000000
	.quad	0                       ## Personality Function
	.quad	0                       ## LSDA
	.quad	__ZN11MyExceptionC2Ev   ## Range Start
Ltmp113 = (Ltmp68-Ltmp64)-0             ## Range Length
	.long	Ltmp113
	.long	16777216                ## Compact Unwind Encoding: 0x1000000
	.quad	0                       ## Personality Function
	.quad	0                       ## LSDA
	.quad	__ZN11MyExceptionD0Ev   ## Range Start
Ltmp114 = (Ltmp80-Ltmp75)-0             ## Range Length
	.long	Ltmp114
	.long	1090584577              ## Compact Unwind Encoding: 0x41010001
	.quad	___gxx_personality_v0   ## Personality Function
	.quad	Lexception8             ## LSDA
	.quad	__ZN9TestClassD2Ev      ## Range Start
Ltmp115 = (Ltmp87-Ltmp83)-0             ## Range Length
	.long	Ltmp115
	.long	16777216                ## Compact Unwind Encoding: 0x1000000
	.quad	0                       ## Personality Function
	.quad	0                       ## LSDA
	.quad	__ZN9TestClassC2Ev      ## Range Start
Ltmp116 = (Ltmp94-Ltmp90)-0             ## Range Length
	.long	Ltmp116
	.long	16777216                ## Compact Unwind Encoding: 0x1000000
	.quad	0                       ## Personality Function
	.quad	0                       ## LSDA
	.quad	__ZN9TestClassD0Ev      ## Range Start
Ltmp117 = (Ltmp106-Ltmp101)-0           ## Range Length
	.long	Ltmp117
	.long	1090584577              ## Compact Unwind Encoding: 0x41010001
	.quad	___gxx_personality_v0   ## Personality Function
	.quad	Lexception11            ## LSDA
	.section	__TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
Ltmp118:
EH_frame0:
Ltmp120 = (Ltmp119-EH_frame0)-4         ## CIE Length
	.long	Ltmp120
	.long	0                       ## CIE ID Tag
	.byte	1                       ## DW_CIE_VERSION
	.ascii	 "zPLR"                 ## CIE Augmentation
	.byte	0
	.byte	1                       ## CIE Code Alignment Factor
	.byte	120                     ## CIE Data Alignment Factor
	.byte	16                      ## CIE Return Address Column
	.byte	7                       ## Augmentation Size
	.byte	155                     ## Personality Encoding = indirect pcrel sdata4
	.long	___gxx_personality_v0@GOTPCREL+4 ## Personality
	.byte	16                      ## LSDA Encoding = pcrel
	.byte	16                      ## FDE Encoding = pcrel
	.byte	12                      ## DW_CFA_def_cfa
	.byte	7                       ## Reg 7
	.byte	8                       ## Offset 8
	.byte	144                     ## DW_CFA_offset + Reg(16)
	.byte	1                       ## Offset 1
	.align	2
Ltmp119:
	.globl	_main.eh
_main.eh:
Ltmp123 = (Ltmp122-Ltmp121)-0           ## FDE Length
	.long	Ltmp123
Ltmp121:
Ltmp124 = (Ltmp121-EH_frame0)-0         ## FDE CIE Offset
	.long	Ltmp124
Ltmp125:
Ltmp126 = Ltmp21-Ltmp125                ## FDE initial location
	.quad	Ltmp126
Ltmp127 = (Ltmp26-Ltmp21)-0             ## FDE address range
	.quad	Ltmp127
	.byte	8                       ## Augmentation size
Ltmp128:
Ltmp129 = Lexception0-Ltmp128           ## Language Specific Data Area
	.quad	Ltmp129
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp130 = Ltmp22-Ltmp21
	.long	Ltmp130
	.byte	14                      ## DW_CFA_def_cfa_offset
	.byte	16                      ## Offset 16
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp131 = Ltmp23-Ltmp22
	.long	Ltmp131
	.byte	134                     ## DW_CFA_offset + Reg(6)
	.byte	2                       ## Offset 2
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp132 = Ltmp24-Ltmp23
	.long	Ltmp132
	.byte	13                      ## DW_CFA_def_cfa_register
	.byte	6                       ## Reg 6
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp133 = Ltmp25-Ltmp24
	.long	Ltmp133
	.byte	131                     ## DW_CFA_offset + Reg(3)
	.byte	3                       ## Offset 3
	.align	3
Ltmp122:
EH_frame1:
Ltmp135 = (Ltmp134-EH_frame1)-4         ## CIE Length
	.long	Ltmp135
	.long	0                       ## CIE ID Tag
	.byte	1                       ## DW_CIE_VERSION
	.ascii	 "zR"                   ## CIE Augmentation
	.byte	0
	.byte	1                       ## CIE Code Alignment Factor
	.byte	120                     ## CIE Data Alignment Factor
	.byte	16                      ## CIE Return Address Column
	.byte	1                       ## Augmentation Size
	.byte	16                      ## FDE Encoding = pcrel
	.byte	12                      ## DW_CFA_def_cfa
	.byte	7                       ## Reg 7
	.byte	8                       ## Offset 8
	.byte	144                     ## DW_CFA_offset + Reg(16)
	.byte	1                       ## Offset 1
	.align	2
Ltmp134:
	.globl	__ZN9TestClassC1Ev.eh
	.weak_definition	__ZN9TestClassC1Ev.eh
__ZN9TestClassC1Ev.eh:
Ltmp138 = (Ltmp137-Ltmp136)-0           ## FDE Length
	.long	Ltmp138
Ltmp136:
Ltmp139 = (Ltmp136-EH_frame1)-0         ## FDE CIE Offset
	.long	Ltmp139
Ltmp140:
Ltmp141 = Ltmp29-Ltmp140                ## FDE initial location
	.quad	Ltmp141
Ltmp142 = (Ltmp33-Ltmp29)-0             ## FDE address range
	.quad	Ltmp142
	.byte	0                       ## Augmentation size
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp143 = Ltmp30-Ltmp29
	.long	Ltmp143
	.byte	14                      ## DW_CFA_def_cfa_offset
	.byte	16                      ## Offset 16
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp144 = Ltmp31-Ltmp30
	.long	Ltmp144
	.byte	134                     ## DW_CFA_offset + Reg(6)
	.byte	2                       ## Offset 2
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp145 = Ltmp32-Ltmp31
	.long	Ltmp145
	.byte	13                      ## DW_CFA_def_cfa_register
	.byte	6                       ## Reg 6
	.align	3
Ltmp137:
	.globl	__ZN9TestClassD1Ev.eh
	.weak_definition	__ZN9TestClassD1Ev.eh
__ZN9TestClassD1Ev.eh:
Ltmp148 = (Ltmp147-Ltmp146)-0           ## FDE Length
	.long	Ltmp148
Ltmp146:
Ltmp149 = (Ltmp146-EH_frame1)-0         ## FDE CIE Offset
	.long	Ltmp149
Ltmp150:
Ltmp151 = Ltmp36-Ltmp150                ## FDE initial location
	.quad	Ltmp151
Ltmp152 = (Ltmp40-Ltmp36)-0             ## FDE address range
	.quad	Ltmp152
	.byte	0                       ## Augmentation size
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp153 = Ltmp37-Ltmp36
	.long	Ltmp153
	.byte	14                      ## DW_CFA_def_cfa_offset
	.byte	16                      ## Offset 16
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp154 = Ltmp38-Ltmp37
	.long	Ltmp154
	.byte	134                     ## DW_CFA_offset + Reg(6)
	.byte	2                       ## Offset 2
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp155 = Ltmp39-Ltmp38
	.long	Ltmp155
	.byte	13                      ## DW_CFA_def_cfa_register
	.byte	6                       ## Reg 6
	.align	3
Ltmp147:
	.globl	__ZN11MyExceptionC1Ev.eh
	.weak_definition	__ZN11MyExceptionC1Ev.eh
__ZN11MyExceptionC1Ev.eh:
Ltmp158 = (Ltmp157-Ltmp156)-0           ## FDE Length
	.long	Ltmp158
Ltmp156:
Ltmp159 = (Ltmp156-EH_frame1)-0         ## FDE CIE Offset
	.long	Ltmp159
Ltmp160:
Ltmp161 = Ltmp43-Ltmp160                ## FDE initial location
	.quad	Ltmp161
Ltmp162 = (Ltmp47-Ltmp43)-0             ## FDE address range
	.quad	Ltmp162
	.byte	0                       ## Augmentation size
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp163 = Ltmp44-Ltmp43
	.long	Ltmp163
	.byte	14                      ## DW_CFA_def_cfa_offset
	.byte	16                      ## Offset 16
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp164 = Ltmp45-Ltmp44
	.long	Ltmp164
	.byte	134                     ## DW_CFA_offset + Reg(6)
	.byte	2                       ## Offset 2
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp165 = Ltmp46-Ltmp45
	.long	Ltmp165
	.byte	13                      ## DW_CFA_def_cfa_register
	.byte	6                       ## Reg 6
	.align	3
Ltmp157:
	.globl	__ZN11MyExceptionD1Ev.eh
	.weak_definition	__ZN11MyExceptionD1Ev.eh
__ZN11MyExceptionD1Ev.eh:
Ltmp168 = (Ltmp167-Ltmp166)-0           ## FDE Length
	.long	Ltmp168
Ltmp166:
Ltmp169 = (Ltmp166-EH_frame1)-0         ## FDE CIE Offset
	.long	Ltmp169
Ltmp170:
Ltmp171 = Ltmp50-Ltmp170                ## FDE initial location
	.quad	Ltmp171
Ltmp172 = (Ltmp54-Ltmp50)-0             ## FDE address range
	.quad	Ltmp172
	.byte	0                       ## Augmentation size
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp173 = Ltmp51-Ltmp50
	.long	Ltmp173
	.byte	14                      ## DW_CFA_def_cfa_offset
	.byte	16                      ## Offset 16
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp174 = Ltmp52-Ltmp51
	.long	Ltmp174
	.byte	134                     ## DW_CFA_offset + Reg(6)
	.byte	2                       ## Offset 2
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp175 = Ltmp53-Ltmp52
	.long	Ltmp175
	.byte	13                      ## DW_CFA_def_cfa_register
	.byte	6                       ## Reg 6
	.align	3
Ltmp167:
	.globl	__ZN11MyExceptionD2Ev.eh
	.weak_definition	__ZN11MyExceptionD2Ev.eh
__ZN11MyExceptionD2Ev.eh:
Ltmp178 = (Ltmp177-Ltmp176)-0           ## FDE Length
	.long	Ltmp178
Ltmp176:
Ltmp179 = (Ltmp176-EH_frame1)-0         ## FDE CIE Offset
	.long	Ltmp179
Ltmp180:
Ltmp181 = Ltmp57-Ltmp180                ## FDE initial location
	.quad	Ltmp181
Ltmp182 = (Ltmp61-Ltmp57)-0             ## FDE address range
	.quad	Ltmp182
	.byte	0                       ## Augmentation size
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp183 = Ltmp58-Ltmp57
	.long	Ltmp183
	.byte	14                      ## DW_CFA_def_cfa_offset
	.byte	16                      ## Offset 16
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp184 = Ltmp59-Ltmp58
	.long	Ltmp184
	.byte	134                     ## DW_CFA_offset + Reg(6)
	.byte	2                       ## Offset 2
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp185 = Ltmp60-Ltmp59
	.long	Ltmp185
	.byte	13                      ## DW_CFA_def_cfa_register
	.byte	6                       ## Reg 6
	.align	3
Ltmp177:
	.globl	__ZN11MyExceptionC2Ev.eh
	.weak_definition	__ZN11MyExceptionC2Ev.eh
__ZN11MyExceptionC2Ev.eh:
Ltmp188 = (Ltmp187-Ltmp186)-0           ## FDE Length
	.long	Ltmp188
Ltmp186:
Ltmp189 = (Ltmp186-EH_frame1)-0         ## FDE CIE Offset
	.long	Ltmp189
Ltmp190:
Ltmp191 = Ltmp64-Ltmp190                ## FDE initial location
	.quad	Ltmp191
Ltmp192 = (Ltmp68-Ltmp64)-0             ## FDE address range
	.quad	Ltmp192
	.byte	0                       ## Augmentation size
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp193 = Ltmp65-Ltmp64
	.long	Ltmp193
	.byte	14                      ## DW_CFA_def_cfa_offset
	.byte	16                      ## Offset 16
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp194 = Ltmp66-Ltmp65
	.long	Ltmp194
	.byte	134                     ## DW_CFA_offset + Reg(6)
	.byte	2                       ## Offset 2
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp195 = Ltmp67-Ltmp66
	.long	Ltmp195
	.byte	13                      ## DW_CFA_def_cfa_register
	.byte	6                       ## Reg 6
	.align	3
Ltmp187:
	.globl	__ZN11MyExceptionD0Ev.eh
	.weak_definition	__ZN11MyExceptionD0Ev.eh
__ZN11MyExceptionD0Ev.eh:
Ltmp198 = (Ltmp197-Ltmp196)-0           ## FDE Length
	.long	Ltmp198
Ltmp196:
Ltmp199 = (Ltmp196-EH_frame0)-0         ## FDE CIE Offset
	.long	Ltmp199
Ltmp200:
Ltmp201 = Ltmp75-Ltmp200                ## FDE initial location
	.quad	Ltmp201
Ltmp202 = (Ltmp80-Ltmp75)-0             ## FDE address range
	.quad	Ltmp202
	.byte	8                       ## Augmentation size
Ltmp203:
Ltmp204 = Lexception8-Ltmp203           ## Language Specific Data Area
	.quad	Ltmp204
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp205 = Ltmp76-Ltmp75
	.long	Ltmp205
	.byte	14                      ## DW_CFA_def_cfa_offset
	.byte	16                      ## Offset 16
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp206 = Ltmp77-Ltmp76
	.long	Ltmp206
	.byte	134                     ## DW_CFA_offset + Reg(6)
	.byte	2                       ## Offset 2
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp207 = Ltmp78-Ltmp77
	.long	Ltmp207
	.byte	13                      ## DW_CFA_def_cfa_register
	.byte	6                       ## Reg 6
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp208 = Ltmp79-Ltmp78
	.long	Ltmp208
	.byte	131                     ## DW_CFA_offset + Reg(3)
	.byte	3                       ## Offset 3
	.align	3
Ltmp197:
	.globl	__ZN9TestClassD2Ev.eh
	.weak_definition	__ZN9TestClassD2Ev.eh
__ZN9TestClassD2Ev.eh:
Ltmp211 = (Ltmp210-Ltmp209)-0           ## FDE Length
	.long	Ltmp211
Ltmp209:
Ltmp212 = (Ltmp209-EH_frame1)-0         ## FDE CIE Offset
	.long	Ltmp212
Ltmp213:
Ltmp214 = Ltmp83-Ltmp213                ## FDE initial location
	.quad	Ltmp214
Ltmp215 = (Ltmp87-Ltmp83)-0             ## FDE address range
	.quad	Ltmp215
	.byte	0                       ## Augmentation size
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp216 = Ltmp84-Ltmp83
	.long	Ltmp216
	.byte	14                      ## DW_CFA_def_cfa_offset
	.byte	16                      ## Offset 16
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp217 = Ltmp85-Ltmp84
	.long	Ltmp217
	.byte	134                     ## DW_CFA_offset + Reg(6)
	.byte	2                       ## Offset 2
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp218 = Ltmp86-Ltmp85
	.long	Ltmp218
	.byte	13                      ## DW_CFA_def_cfa_register
	.byte	6                       ## Reg 6
	.align	3
Ltmp210:
	.globl	__ZN9TestClassC2Ev.eh
	.weak_definition	__ZN9TestClassC2Ev.eh
__ZN9TestClassC2Ev.eh:
Ltmp221 = (Ltmp220-Ltmp219)-0           ## FDE Length
	.long	Ltmp221
Ltmp219:
Ltmp222 = (Ltmp219-EH_frame1)-0         ## FDE CIE Offset
	.long	Ltmp222
Ltmp223:
Ltmp224 = Ltmp90-Ltmp223                ## FDE initial location
	.quad	Ltmp224
Ltmp225 = (Ltmp94-Ltmp90)-0             ## FDE address range
	.quad	Ltmp225
	.byte	0                       ## Augmentation size
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp226 = Ltmp91-Ltmp90
	.long	Ltmp226
	.byte	14                      ## DW_CFA_def_cfa_offset
	.byte	16                      ## Offset 16
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp227 = Ltmp92-Ltmp91
	.long	Ltmp227
	.byte	134                     ## DW_CFA_offset + Reg(6)
	.byte	2                       ## Offset 2
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp228 = Ltmp93-Ltmp92
	.long	Ltmp228
	.byte	13                      ## DW_CFA_def_cfa_register
	.byte	6                       ## Reg 6
	.align	3
Ltmp220:
	.globl	__ZN9TestClassD0Ev.eh
	.weak_definition	__ZN9TestClassD0Ev.eh
__ZN9TestClassD0Ev.eh:
Ltmp231 = (Ltmp230-Ltmp229)-0           ## FDE Length
	.long	Ltmp231
Ltmp229:
Ltmp232 = (Ltmp229-EH_frame0)-0         ## FDE CIE Offset
	.long	Ltmp232
Ltmp233:
Ltmp234 = Ltmp101-Ltmp233               ## FDE initial location
	.quad	Ltmp234
Ltmp235 = (Ltmp106-Ltmp101)-0           ## FDE address range
	.quad	Ltmp235
	.byte	8                       ## Augmentation size
Ltmp236:
Ltmp237 = Lexception11-Ltmp236          ## Language Specific Data Area
	.quad	Ltmp237
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp238 = Ltmp102-Ltmp101
	.long	Ltmp238
	.byte	14                      ## DW_CFA_def_cfa_offset
	.byte	16                      ## Offset 16
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp239 = Ltmp103-Ltmp102
	.long	Ltmp239
	.byte	134                     ## DW_CFA_offset + Reg(6)
	.byte	2                       ## Offset 2
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp240 = Ltmp104-Ltmp103
	.long	Ltmp240
	.byte	13                      ## DW_CFA_def_cfa_register
	.byte	6                       ## Reg 6
	.byte	4                       ## DW_CFA_advance_loc4
Ltmp241 = Ltmp105-Ltmp104
	.long	Ltmp241
	.byte	131                     ## DW_CFA_offset + Reg(3)
	.byte	3                       ## Offset 3
	.align	3
	.align	3
Ltmp230:
