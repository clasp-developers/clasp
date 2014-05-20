dnl  AMD64 calling conventions checking.

dnl  Copyright 2000, 2003, 2004, 2006, 2007 Free Software Foundation, Inc.
dnl
dnl  This file is part of the GNU MP Library.
dnl
dnl  The GNU MP Library is free software; you can redistribute it and/or
dnl  modify it under the terms of the GNU Lesser General Public License as
dnl  published by the Free Software Foundation; either version 3 of the
dnl  License, or (at your option) any later version.
dnl
dnl  The GNU MP Library is distributed in the hope that it will be useful,
dnl  but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl  Lesser General Public License for more details.
dnl
dnl  You should have received a copy of the GNU Lesser General Public License
dnl  along with the GNU MP Library.  If not, see http://www.gnu.org/licenses/.


include(`../config.m4')


C void x86_fldcw (unsigned short cw);
C
C Execute an fldcw, setting the x87 control word to cw.

PROLOGUE(x86_fldcw)
	movq	%rdi, -8(%rsp)
	fldcw	-8(%rsp)
	ret
EPILOGUE()


C unsigned short x86_fstcw (void);
C
C Execute an fstcw, returning the current x87 control word.

PROLOGUE(x86_fstcw)
        movq	$0, -8(%rsp)
        fstcw	-8(%rsp)
        movq	-8(%rsp), %rax
	ret
EPILOGUE()


dnl  Instrumented profiling won't come out quite right below, since we don't
dnl  do an actual "ret".  There's only a few instructions here, so there's
dnl  no great need to get them separately accounted, just let them get
dnl  attributed to the caller.

ifelse(WANT_PROFILING,instrument,
`define(`WANT_PROFILING',no)')


C int calling_conventions (...);
C
C The global variable "calling_conventions_function" is the function to
C call, with the arguments as passed here.
C
C Perhaps the finit should be done only if the tags word isn't clear, but
C nothing uses the rounding mode or anything at the moment.

define(`WANT_RBX', eval(8*0)($1))
define(`WANT_RBP', eval(8*1)($1))
define(`WANT_R12', eval(8*2)($1))
define(`WANT_R13', eval(8*3)($1))
define(`WANT_R14', eval(8*4)($1))
define(`WANT_R15', eval(8*5)($1))

define(`JUNK_RAX', eval(8*6)($1))
define(`JUNK_R10', eval(8*7)($1))
define(`JUNK_R11', eval(8*8)($1))

define(`SAVE_RBX', eval(8*9)($1))
define(`SAVE_RBP', eval(8*10)($1))
define(`SAVE_R12', eval(8*11)($1))
define(`SAVE_R13', eval(8*12)($1))
define(`SAVE_R14', eval(8*13)($1))
define(`SAVE_R15', eval(8*14)($1))

define(`RETADDR',  eval(8*15)($1))

define(`RBX',	   eval(8*16)($1))
define(`RBP',	   eval(8*17)($1))
define(`R12',	   eval(8*18)($1))
define(`R13',	   eval(8*19)($1))
define(`R14',	   eval(8*20)($1))
define(`R15',	   eval(8*21)($1))
define(`RFLAGS',   eval(8*22)($1))


define(G,
m4_assert_numargs(1)
`GSYM_PREFIX`'$1')

	TEXT
	ALIGN(32)
PROLOGUE(calling_conventions)
	push	%rdi
	movq	G(calling_conventions_values)@GOTPCREL(%rip), %rdi

	movq	8(%rsp), %rax
	movq	%rax, RETADDR(%rdi)

	leaq	L(return)(%rip), %rax
	movq	%rax, 8(%rsp)

	movq	%rbx, SAVE_RBX(%rdi)
	movq	%rbp, SAVE_RBP(%rdi)
	movq	%r12, SAVE_R12(%rdi)
	movq	%r13, SAVE_R13(%rdi)
	movq	%r14, SAVE_R14(%rdi)
	movq	%r15, SAVE_R15(%rdi)

	C values we expect to see unchanged, as per amd64check.c
	movq	WANT_RBX(%rdi), %rbx
	movq	WANT_RBP(%rdi), %rbp
	movq	WANT_R12(%rdi), %r12
	movq	WANT_R13(%rdi), %r13
	movq	WANT_R14(%rdi), %r14
	movq	WANT_R15(%rdi), %r15

	C Try to provoke a problem by starting with junk in the registers,
	C especially %rax which will be the return value.
	C
	C ENHANCE-ME: If we knew how many of the parameter registers were
	C actually being used we could put junk in the rest.  Maybe we could
	C get try.c to communicate this to us.
C	movq	JUNK_RAX(%rdi), %rax		C overwritten below anyway
	movq	JUNK_R10(%rdi), %r10
	movq	JUNK_R11(%rdi), %r11

	movq	G(calling_conventions_function)@GOTPCREL(%rip), %rax
	pop	%rdi
	jmp	*(%rax)

L(return):
	movq	G(calling_conventions_values)@GOTPCREL(%rip), %rdi

	movq	%rbx, RBX(%rdi)
	movq	%rbp, RBP(%rdi)
	movq	%r12, R12(%rdi)
	movq	%r13, R13(%rdi)
	movq	%r14, R14(%rdi)
	movq	%r15, R15(%rdi)

	pushfq
	popq	%rbx
	movq	%rbx, RFLAGS(%rdi)

	movq	G(calling_conventions_fenv)@GOTPCREL(%rip), %rbx
	fstenv	(%rbx)
	finit

	movq	SAVE_RBX(%rdi), %rbx
	movq	SAVE_RBP(%rdi), %rbp
	movq	SAVE_R12(%rdi), %r12
	movq	SAVE_R13(%rdi), %r13
	movq	SAVE_R14(%rdi), %r14
	movq	SAVE_R15(%rdi), %r15

	jmp	*RETADDR(%rdi)

EPILOGUE()
