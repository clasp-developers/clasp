dnl  AMD64 mpn_mul_1 -- Multiply a limb vector with a limb and store the result
dnl  in a second limb vector.

dnl  Copyright 2004 Free Software Foundation, Inc.

dnl  This file is part of the GNU MP Library.

dnl  The GNU MP Library is free software; you can redistribute it and/or modify
dnl  it under the terms of the GNU Lesser General Public License as published
dnl  by the Free Software Foundation; either version 2.1 of the License, or (at
dnl  your option) any later version.

dnl  The GNU MP Library is distributed in the hope that it will be useful, but
dnl  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
dnl  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
dnl  License for more details.

dnl  You should have received a copy of the GNU Lesser General Public License
dnl  along with the GNU MP Library; see the file COPYING.LIB.  If not, write
dnl  to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
dnl  Boston, MA 02110-1301, USA.

include(`../config.m4')


C         cycles/limb
C Hammer:     3.0


C INPUT PARAMETERS
C rp	rdi
C up	rsi
C n	rdx
C vl	rcx

	TEXT
	ALIGN(16)
	.byte	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
ASM_START()
PROLOGUE(mpn_mul_1)
	movq	%rdx, %r11
	leaq	(%rsi,%rdx,8), %rsi
	leaq	(%rdi,%rdx,8), %rdi
	negq	%r11
	xorl	%r8d, %r8d

.Loop:	movq	(%rsi,%r11,8), %rax
	mulq	%rcx
	addq	%r8, %rax
	movl	$0, %r8d
	adcq	%rdx, %r8
	movq	%rax, (%rdi,%r11,8)
	incq	%r11
	jne	.Loop

	movq	%r8, %rax
	ret
EPILOGUE()
