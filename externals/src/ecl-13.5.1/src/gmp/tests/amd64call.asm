dnl  AMD64 calling conventions checking.

dnl  Copyright 2000, 2003, 2004 Free Software Foundation, Inc.
dnl 
dnl  This file is part of the GNU MP Library.
dnl 
dnl  The GNU MP Library is free software; you can redistribute it and/or
dnl  modify it under the terms of the GNU Lesser General Public License as
dnl  published by the Free Software Foundation; either version 2.1 of the
dnl  License, or (at your option) any later version.
dnl 
dnl  The GNU MP Library is distributed in the hope that it will be useful,
dnl  but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl  Lesser General Public License for more details.
dnl 
dnl  You should have received a copy of the GNU Lesser General Public
dnl  License along with the GNU MP Library; see the file COPYING.LIB.  If
dnl  not, write to the Free Software Foundation, Inc., 51 Franklin Street,
dnl  Fifth Floor, Boston, MA 02110-1301, USA.


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

define(G,
m4_assert_numargs(1)
`GSYM_PREFIX`'$1')

	TEXT
	ALIGN(32)
PROLOGUE(calling_conventions)
	movq	(%rsp), %rax
	movq	%rax, G(calling_conventions_retaddr)

	movq	$L(return), (%rsp)

	movq	%rbx, G(calling_conventions_save_rbx)
	movq	%rbp, G(calling_conventions_save_rbp)
	movq	%r12, G(calling_conventions_save_r12)
	movq	%r13, G(calling_conventions_save_r13)
	movq	%r14, G(calling_conventions_save_r14)
	movq	%r15, G(calling_conventions_save_r15)

	movq	G(calling_conventions_save_rbx), %rbx
	movq	G(calling_conventions_save_rbp), %rbp
	movq	G(calling_conventions_save_r12), %r12
	movq	G(calling_conventions_save_r13), %r13
	movq	G(calling_conventions_save_r14), %r14
	movq	G(calling_conventions_save_r15), %r15

	C values we expect to see unchanged, as per amd64check.c
	movq	$0x1234567887654321, %rbx
	movq	$0x89ABCDEFFEDCBA98, %rbp
	movq	$0xDEADBEEFBADECAFE, %r12
	movq	$0xFFEEDDCCBBAA9988, %r13
	movq	$0x0011223344556677, %r14
	movq	$0x1234432156788765, %r15

	C Try to provoke a problem by starting with junk in the registers,
	C especially %rax which will be the return value.
	C
	C ENHANCE-ME: If we knew how many of the parameter registers were
	C actually being used we could put junk in the rest.  Maybe we could
	C get try.c to communicate this to us.
	C 
	movq	$0xFEEDABBACAAFBEED, %rax
	movq	$0xAB78DE89FF5125BB, %r10
	movq	$0x1238901890189031, %r11

	jmpq	*G(calling_conventions_function)

L(return):
	movq	%rbx, G(calling_conventions_rbx)
	movq	%rbp, G(calling_conventions_rbp)
	movq	%r12, G(calling_conventions_r12)
	movq	%r13, G(calling_conventions_r13)
	movq	%r14, G(calling_conventions_r14)
	movq	%r15, G(calling_conventions_r15)

	pushfq
	popq	%rbx
	movq	%rbx, G(calling_conventions_rflags)

	fstenv	G(calling_conventions_fenv)
	finit

	movq	G(calling_conventions_save_rbx), %rbx
	movq	G(calling_conventions_save_rbp), %rbp
	movq	G(calling_conventions_save_r12), %r12
	movq	G(calling_conventions_save_r13), %r13
	movq	G(calling_conventions_save_r14), %r14
	movq	G(calling_conventions_save_r15), %r15

	jmpq	*G(calling_conventions_retaddr)

EPILOGUE()
