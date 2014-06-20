dnl  X86-64 mpn_add_n, mpn_sub_n, optimized for Intel Atom.

dnl  Copyright 2003, 2004, 2005, 2007, 2008, 2010 Free Software Foundation, Inc.

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
C K8,K9:	 1.85
C K10:		 ?
C P4:		 ?
C P6-15 (Core2): ?
C P6-28 (Atom):	 3

C INPUT PARAMETERS
define(`rp',	`%rdi')
define(`up',	`%rsi')
define(`vp',	`%rdx')
define(`n',	`%rcx')
define(`cy',	`%r8')		C (only for mpn_add_nc)

ifdef(`OPERATION_add_n', `
	define(ADCSBB,	      adc)
	define(func,	      mpn_add_n)
	define(func_nc,	      mpn_add_nc)')
ifdef(`OPERATION_sub_n', `
	define(ADCSBB,	      sbb)
	define(func,	      mpn_sub_n)
	define(func_nc,	      mpn_sub_nc)')

MULFUNC_PROLOGUE(mpn_add_n mpn_add_nc mpn_sub_n mpn_sub_nc)

ASM_START()
	TEXT
	ALIGN(16)
PROLOGUE(func_nc)
	jmp	L(ent)
EPILOGUE()
PROLOGUE(func)
	xor	%r8, %r8
L(ent):
	mov	R32(%rcx), R32(%rax)
	shr	$2, %rcx
	and	$3, R32(%rax)
	jz	L(b0)
	cmp	$2, R32(%rax)
	jz	L(b2)
	jg	L(b3)

L(b1):	mov	(%rsi), %r10
	test	%rcx, %rcx
	jnz	L(gt1)
	shr	R32(%r8)			C Set CF from argument
	ADCSBB	(%rdx), %r10
	mov	%r10, (%rdi)
	mov	R32(%rcx), R32(%rax)		C zero rax
	adc	R32(%rax), R32(%rax)
	ret
L(gt1):	shr	R32(%r8)
	ADCSBB	(%rdx), %r10
	mov	8(%rsi), %r11
	lea	16(%rsi), %rsi
	lea	-16(%rdx), %rdx
	lea	-16(%rdi), %rdi
	jmp	L(m1)

L(b2):	mov	(%rsi), %r9
	mov	8(%rsi), %r10
	lea	-8(%rdx), %rdx
	test	%rcx, %rcx
	jnz	L(gt2)
	shr	R32(%r8)
	lea	-40(%rdi), %rdi
	jmp	L(e2)
L(gt2):	shr	R32(%r8)
	ADCSBB	8(%rdx), %r9
	mov	16(%rsi), %r11
	lea	-8(%rsi), %rsi
	lea	-8(%rdi), %rdi
	jmp	L(m2)

L(b3):	mov	(%rsi), %rax
	mov	8(%rsi), %r9
	mov	16(%rsi), %r10
	test	%rcx, %rcx
	jnz	L(gt3)
	shr	R32(%r8)
	lea	-32(%rdi), %rdi
	jmp	L(e3)
L(gt3):	shr	R32(%r8)
	ADCSBB	(%rdx), %rax
	jmp	L(m3)

L(b0):	mov	(%rsi), %r11
	neg	R32(%r8)
	lea	-24(%rdx), %rdx
	lea	-24(%rdi), %rdi
	lea	8(%rsi), %rsi
	jmp	L(m0)

	ALIGN(8)
L(top):	mov	%r11, 24(%rdi)
	ADCSBB	(%rdx), %rax
	lea	32(%rdi), %rdi
L(m3):	mov	%rax, (%rdi)
	ADCSBB	8(%rdx), %r9
	mov	24(%rsi), %r11
L(m2):	mov	%r9, 8(%rdi)
	ADCSBB	16(%rdx), %r10
	lea	32(%rsi), %rsi
L(m1):	mov	%r10, 16(%rdi)
L(m0):	ADCSBB	24(%rdx), %r11
	mov	(%rsi), %rax
	mov	8(%rsi), %r9
	lea	32(%rdx), %rdx
	dec	%rcx
	mov	16(%rsi), %r10
	jnz	L(top)

	mov	%r11, 24(%rdi)
L(e3):	ADCSBB	(%rdx), %rax
	mov	%rax, 32(%rdi)
L(e2):	ADCSBB	8(%rdx), %r9
	mov	%r9, 40(%rdi)
L(e1):	ADCSBB	16(%rdx), %r10
	mov	%r10, 48(%rdi)
	mov	R32(%rcx), R32(%rax)		C zero rax
	adc	R32(%rax), R32(%rax)
	ret
EPILOGUE()
