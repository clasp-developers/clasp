dnl  x86-64 mpn_rshift optimized for Pentium 4.

dnl  Copyright 2003, 2005, 2007, 2008 Free Software Foundation, Inc.
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


C	     cycles/limb
C K8,K9:	 2.5
C K10:		 ?
C P4:		 3.29
C P6-15 (Core2): 2.1 (fluctuates, presumably cache related)
C P6-28 (Atom):	14.3

C INPUT PARAMETERS
define(`rp',`%rdi')
define(`up',`%rsi')
define(`n',`%rdx')
define(`cnt',`%cl')

ASM_START()
	TEXT
	ALIGN(32)
PROLOGUE(mpn_rshift)
	mov	(up), %rax
	movd	%ecx, %mm4
	neg	%ecx			C put lsh count in cl
	and	$63, %ecx
	movd	%ecx, %mm5

	lea	-8(up,n,8), up
	lea	-8(rp,n,8), rp
	lea	1(n), %r8d
	neg	n

	shl	%cl, %rax		C function return value

	and	$3, %r8d
	je	L(rol)			C jump for n = 3, 7, 11, ...

	dec	%r8d
	jne	L(1)
C	n = 4, 8, 12, ...
	movq	8(up,n,8), %mm2
	psrlq	%mm4, %mm2
	movq	16(up,n,8), %mm0
	psllq	%mm5, %mm0
	por	%mm0, %mm2
	movq	%mm2, 8(rp,n,8)
	inc	n
	jmp	L(rol)

L(1):	dec	%r8d
	je	L(1x)			C jump for n = 1, 5, 9, 13, ...
C	n = 2, 6, 10, 16, ...
	movq	8(up,n,8), %mm2
	psrlq	%mm4, %mm2
	movq	16(up,n,8), %mm0
	psllq	%mm5, %mm0
	por	%mm0, %mm2
	movq	%mm2, 8(rp,n,8)
	inc	n
L(1x):
	cmp	$-1, n
	je	L(ast)
	movq	8(up,n,8), %mm2
	psrlq	%mm4, %mm2
	movq	16(up,n,8), %mm3
	psrlq	%mm4, %mm3
	movq	16(up,n,8), %mm0
	movq	24(up,n,8), %mm1
	psllq	%mm5, %mm0
	por	%mm0, %mm2
	psllq	%mm5, %mm1
	por	%mm1, %mm3
	movq	%mm2, 8(rp,n,8)
	movq	%mm3, 16(rp,n,8)
	add	$2, n

L(rol):	movq	8(up,n,8), %mm2
	psrlq	%mm4, %mm2
	movq	16(up,n,8), %mm3
	psrlq	%mm4, %mm3

	add	$4, n			C				      4
	jb	L(end)			C				      2
	ALIGN(32)
L(top):
	C finish stuff from lsh block
	movq	-16(up,n,8), %mm0
	movq	-8(up,n,8), %mm1
	psllq	%mm5, %mm0
	por	%mm0, %mm2
	psllq	%mm5, %mm1
	movq	(up,n,8), %mm0
	por	%mm1, %mm3
	movq	8(up,n,8), %mm1
	movq	%mm2, -24(rp,n,8)
	movq	%mm3, -16(rp,n,8)
	C start two new rsh
	psllq	%mm5, %mm0
	psllq	%mm5, %mm1

	C finish stuff from rsh block
	movq	-8(up,n,8), %mm2
	movq	(up,n,8), %mm3
	psrlq	%mm4, %mm2
	por	%mm2, %mm0
	psrlq	%mm4, %mm3
	movq	8(up,n,8), %mm2
	por	%mm3, %mm1
	movq	16(up,n,8), %mm3
	movq	%mm0, -8(rp,n,8)
	movq	%mm1, (rp,n,8)
	C start two new lsh
	add	$4, n
	psrlq	%mm4, %mm2
	psrlq	%mm4, %mm3

	jae	L(top)			C				      2
L(end):
	movq	-16(up,n,8), %mm0
	psllq	%mm5, %mm0
	por	%mm0, %mm2
	movq	-8(up,n,8), %mm1
	psllq	%mm5, %mm1
	por	%mm1, %mm3
	movq	%mm2, -24(rp,n,8)
	movq	%mm3, -16(rp,n,8)

L(ast):	movq	(up), %mm2
	psrlq	%mm4, %mm2
	movq	%mm2, (rp)
	emms
	ret
EPILOGUE()
