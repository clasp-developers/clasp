dnl  AMD64 mpn_com.

dnl  Copyright 2004, 2005, 2006 Free Software Foundation, Inc.

dnl  This file is part of the GNU MP Library.

dnl  The GNU MP Library is free software; you can redistribute it and/or modify
dnl  it under the terms of the GNU Lesser General Public License as published
dnl  by the Free Software Foundation; either version 3 of the License, or (at
dnl  your option) any later version.

dnl  The GNU MP Library is distributed in the hope that it will be useful, but
dnl  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
dnl  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
dnl  License for more details.

dnl  You should have received a copy of the GNU Lesser General Public License
dnl  along with the GNU MP Library.  If not, see http://www.gnu.org/licenses/.

include(`../config.m4')


C	    cycles/limb
C K8,K9:	1.25
C K10:		1.25
C P4:		2.78
C P6-15:	1.1

C INPUT PARAMETERS
define(`rp',`%rdi')
define(`up',`%rsi')
define(`n',`%rdx')


ASM_START()
	TEXT
	ALIGN(32)
PROLOGUE(mpn_com)
	movq	(up), %r8
	movl	%edx, %eax
	leaq	(up,n,8), up
	leaq	(rp,n,8), rp
	negq	n
	andl	$3, %eax
	je	L(b00)
	cmpl	$2, %eax
	jc	L(b01)
	je	L(b10)

L(b11):	notq	%r8
	movq	%r8, (rp,n,8)
	decq	n
	jmp	L(e11)
L(b10):	addq	$-2, n
	jmp	L(e10)
	.byte	0x90,0x90,0x90,0x90,0x90,0x90
L(b01):	notq	%r8
	movq	%r8, (rp,n,8)
	incq	n
	jz	L(ret)

L(oop):	movq	(up,n,8), %r8
L(b00):	movq	8(up,n,8), %r9
	notq	%r8
	notq	%r9
	movq	%r8, (rp,n,8)
	movq	%r9, 8(rp,n,8)
L(e11):	movq	16(up,n,8), %r8
L(e10):	movq	24(up,n,8), %r9
	notq	%r8
	notq	%r9
	movq	%r8, 16(rp,n,8)
	movq	%r9, 24(rp,n,8)
	addq	$4, n
	jnc	L(oop)
L(ret):	ret
EPILOGUE()
