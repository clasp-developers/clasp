dnl  x86-64 mpn_addlsh1_n and mpn_sublsh1_n, optimized for "Core" 2.

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
C K8,K9:	 4.25
C K10:		 ?
C P4:		 ?
C P6-15:	 3

C INPUT PARAMETERS
define(`rp',`%rdi')
define(`up',`%rsi')
define(`vp',`%rdx')
define(`n', `%rcx')

ifdef(`OPERATION_addlsh1_n', `
	define(ADDSUB,	add)
	define(ADCSBB,	adc)
	define(func,	mpn_addlsh1_n)')
ifdef(`OPERATION_sublsh1_n', `
	define(ADDSUB,	sub)
	define(ADCSBB,	sbb)
	define(func,	mpn_sublsh1_n)')

MULFUNC_PROLOGUE(mpn_addlsh1_n mpn_sublsh1_n)

ASM_START()
	TEXT
	ALIGN(8)
PROLOGUE(func)
	push	%rbx
	push	%r12

	mov	R32(%rcx), R32(%rax)
	lea	24(up,n,8), up
	lea	24(vp,n,8), vp
	lea	24(rp,n,8), rp
	neg	n

	xor	R32(%r11), R32(%r11)

	mov	-24(vp,n,8), %r8	C do first limb early
	shrd	$63, %r8, %r11

	and	$3, R32(%rax)
	je	L(b0)
	cmp	$2, R32(%rax)
	jc	L(b1)
	je	L(b2)

L(b3):	mov	-16(vp,n,8), %r9
	shrd	$63, %r9, %r8
	mov	-8(vp,n,8), %r10
	shrd	$63, %r10, %r9
	mov	-24(up,n,8), %r12
	ADDSUB	%r11, %r12
	mov	%r12, -24(rp,n,8)
	mov	-16(up,n,8), %r12
	ADCSBB	%r8, %r12
	mov	%r12, -16(rp,n,8)
	mov	-8(up,n,8), %r12
	ADCSBB	%r9, %r12
	mov	%r12, -8(rp,n,8)
	mov	%r10, %r11
	sbb	R32(%rax), R32(%rax)	C save cy
	add	$3, n
	js	L(top)
	jmp	L(end)

L(b1):	mov	-24(up,n,8), %r12
	ADDSUB	%r11, %r12
	mov	%r12, -24(rp,n,8)
	mov	%r8, %r11
	sbb	R32(%rax), R32(%rax)	C save cy
	inc	n
	js	L(top)
	jmp	L(end)

L(b2):	mov	-16(vp,n,8), %r9
	shrd	$63, %r9, %r8
	mov	-24(up,n,8), %r12
	ADDSUB	%r11, %r12
	mov	%r12, -24(rp,n,8)
	mov	-16(up,n,8), %r12
	ADCSBB	%r8, %r12
	mov	%r12, -16(rp,n,8)
	mov	%r9, %r11
	sbb	R32(%rax), R32(%rax)	C save cy
	add	$2, n
	js	L(top)
	jmp	L(end)

	ALIGN(16)
L(top):	mov	-24(vp,n,8), %r8
	shrd	$63, %r8, %r11
L(b0):	mov	-16(vp,n,8), %r9
	shrd	$63, %r9, %r8
	mov	-8(vp,n,8), %r10
	shrd	$63, %r10, %r9
	mov	(vp,n,8), %rbx
	shrd	$63, %rbx, %r10

	add	R32(%rax), R32(%rax)	C restore cy

	mov	-24(up,n,8), %r12
	ADCSBB	%r11, %r12
	mov	%r12, -24(rp,n,8)

	mov	-16(up,n,8), %r12
	ADCSBB	%r8, %r12
	mov	%r12, -16(rp,n,8)

	mov	-8(up,n,8), %r12
	ADCSBB	%r9, %r12
	mov	%r12, -8(rp,n,8)

	mov	(up,n,8), %r12
	ADCSBB	%r10, %r12
	mov	%r12, (rp,n,8)

	mov	%rbx, %r11
	sbb	R32(%rax), R32(%rax)	C save cy

	add	$4, n
	js	L(top)

L(end):	add	%r11, %r11
	pop	%r12
	pop	%rbx
	sbb	$0, R32(%rax)
	neg	R32(%rax)
	ret
EPILOGUE()
