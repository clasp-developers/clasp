dnl  AMD64 mpn_lshift

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
C Hammer:     2.5


C INPUT PARAMETERS
C rp	rdi
C up	rsi
C n	rdx
C cnt	rcx

ASM_START()
PROLOGUE(mpn_lshift)
	movq	-8(%rsi,%rdx,8), %mm7
	movd	%ecx, %mm1
	movl	$64, %eax
	subl	%ecx, %eax
	movd	%eax, %mm0
	movq	%mm7, %mm3
	psrlq	%mm0, %mm7
	movd	%mm7, %rax
	subq	$2, %rdx
	jl	.Lendo

	ALIGN(4)			C minimal alignment for claimed speed
.Loop:	movq	(%rsi,%rdx,8), %mm6
	movq	%mm6, %mm2
	psrlq	%mm0, %mm6
	psllq	%mm1, %mm3
	por	%mm6, %mm3
	movq	%mm3, 8(%rdi,%rdx,8)
	je	.Lende
	movq	-8(%rsi,%rdx,8), %mm7
	movq	%mm7, %mm3
	psrlq	%mm0, %mm7
	psllq	%mm1, %mm2
	por	%mm7, %mm2
	movq	%mm2, (%rdi,%rdx,8)
	subq	$2, %rdx
	jge	.Loop

.Lendo:	movq	%mm3, %mm2
.Lende:	psllq	%mm1, %mm2
	movq	%mm2, (%rdi)
	emms
	ret
EPILOGUE()
