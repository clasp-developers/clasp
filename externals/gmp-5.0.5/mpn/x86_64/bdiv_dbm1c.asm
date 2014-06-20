dnl  x86_64 mpn_bdiv_dbm1.

dnl  Copyright 2008 Free Software Foundation, Inc.

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

C	     cycles/limb
C K8,K9:	 2.25
C K10:		  ?
C P4:		12.5
C P6 core2: 	 4.0
C P6 corei7: 	 3.8
C P6 atom:	20

C TODO
C  * Do proper 4-way feed-in instead of the current epilogue

C INPUT PARAMETERS shared
define(`qp',	`%rdi')
define(`up',	`%rsi')
define(`n',	`%rdx')
define(`bd',	`%rcx')
define(`cy',	`%r8')


ASM_START()
	TEXT
	ALIGN(16)
PROLOGUE(mpn_bdiv_dbm1c)
	mov	(%rsi), %rax
	mov	%rdx, %r9		C n

	mul	%rcx
	sub	%rax, %r8
	mov	%r8, (%rdi)
	sbb	%rdx, %r8

	lea	(%rsi,%r9,8), %rsi
	lea	(%rdi,%r9,8), %rdi
	neg	%r9
	add	$4, %r9
	jns	L(end)
	ALIGN(16)
L(top):
	mov	-24(%rsi,%r9,8), %rax
	mul	%rcx
	sub	%rax, %r8
	mov	%r8, -24(%rdi,%r9,8)
	sbb	%rdx, %r8
L(3):
	mov	-16(%rsi,%r9,8), %rax
	mul	%rcx
	sub	%rax, %r8
	mov	%r8, -16(%rdi,%r9,8)
	sbb	%rdx, %r8
L(2):
	mov	-8(%rsi,%r9,8), %rax
	mul	%rcx
	sub	%rax, %r8
	mov	%r8, -8(%rdi,%r9,8)
	sbb	%rdx, %r8
L(1):
	mov	(%rsi,%r9,8), %rax
	mul	%rcx
	sub	%rax, %r8
	mov	%r8, (%rdi,%r9,8)
	sbb	%rdx, %r8

	add	$4, %r9
	js	L(top)
L(end):
	je	L(3x)
	cmp	$2, %r9
	jg	L(ret)
	mov	$-1, %r9
	je	L(1)
	jmp	L(2)
L(3x):
	dec	%r9
	jmp	L(3)

L(ret):	mov	%r8, %rax
	ret
EPILOGUE()
