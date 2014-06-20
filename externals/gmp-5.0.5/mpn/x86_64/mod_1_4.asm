dnl  AMD64 mpn_mod_1s_4p

dnl  Contributed to the GNU project by Torbjorn Granlund.

dnl  Copyright 2009 Free Software Foundation, Inc.

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
C K8,K9:	 3.0
C K10:		 3.0
C P4:		14.5
C P6 core2:	 5.0
C P6 corei7:	 4.3
C P6 atom:	25.0

ASM_START()
	TEXT
	ALIGN(16)
PROLOGUE(mpn_mod_1s_4p)
	push	%r14
	push	%r13
	push	%r12
	push	%rbp
	push	%rbx

	mov	%rdx, -16(%rsp)
	mov	%rcx, %r14
	mov	16(%rcx), %r11
	mov	24(%rcx), %rbx
	mov	32(%rcx), %rbp
	mov	40(%rcx), %r13
	mov	48(%rcx), %r12
	xor	R32(%r8), R32(%r8)
	mov	R32(%rsi), R32(%rdx)
	and	$3, R32(%rdx)
	je	L(b0)
	cmp	$2, R32(%rdx)
	jc	L(b1)
	je	L(b2)

L(b3):	lea	-24(%rdi,%rsi,8), %rdi
	mov	8(%rdi), %rax
	mul	%r11
	mov	(%rdi), %r9
	add	%rax, %r9
	adc	%rdx, %r8
	mov	16(%rdi), %rax
	mul	%rbx
	jmp	L(m0)

	ALIGN(8)
L(b0):	lea	-32(%rdi,%rsi,8), %rdi
	mov	8(%rdi), %rax
	mul	%r11
	mov	(%rdi), %r9
	add	%rax, %r9
	adc	%rdx, %r8
	mov	16(%rdi), %rax
	mul	%rbx
	add	%rax, %r9
	adc	%rdx, %r8
	mov	24(%rdi), %rax
	mul	%rbp
	jmp	L(m0)

	ALIGN(8)
L(b1):	lea	-8(%rdi,%rsi,8), %rdi
	mov	(%rdi), %r9
	jmp	L(m1)

	ALIGN(8)
L(b2):	lea	-16(%rdi,%rsi,8), %rdi
	mov	8(%rdi), %rax
	mul	%r11
	mov	(%rdi), %r9
	jmp	L(m0)

	ALIGN(16)
L(top):	mov	-24(%rdi), %rax
	mov	-32(%rdi), %r10
	mul	%r11
	add	%rax, %r10
	mov	-16(%rdi), %rax
	mov	%rdx, %rcx
	adc	$0, %rcx
	mul	%rbx
	add	%rax, %r10
	mov	-8(%rdi), %rax
	adc	%rdx, %rcx
	sub	$32, %rdi
	mul	%rbp
	add	%rax, %r10
	mov	%r9, %rax
	adc	%rdx, %rcx
	mul	%r13
	add	%rax, %r10
	mov	%r8, %rax
	adc	%rdx, %rcx
	mul	%r12
	mov	%r10, %r9
	mov	%rcx, %r8
L(m0):	add	%rax, %r9
	adc	%rdx, %r8
L(m1):	sub	$4, %rsi
	ja	L(top)

L(end):	mov	8(%r14), R32(%rsi)
	mov	%r8, %rax
	mul	%r11
	mov	%rax, %r8
	add	%r9, %r8
	adc	$0, %rdx
	xor	R32(%rcx), R32(%rcx)
	sub	R32(%rsi), R32(%rcx)
	mov	%r8, %rdi
	shr	R8(%rcx), %rdi
	mov	R32(%rsi), R32(%rcx)
	sal	R8(%rcx), %rdx
	or	%rdx, %rdi
	mov	%rdi, %rax
	mulq	(%r14)
	mov	-16(%rsp), %rbx
	mov	%rax, %r9
	sal	R8(%rcx), %r8
	inc	%rdi
	add	%r8, %r9
	adc	%rdi, %rdx
	imul	%rbx, %rdx
	sub	%rdx, %r8
	lea	(%r8,%rbx), %rax
	cmp	%r8, %r9
	cmovb	%rax, %r8
	mov	%r8, %rax
	sub	%rbx, %rax
	cmovb	%r8, %rax
	shr	R8(%rcx), %rax
	pop	%rbx
	pop	%rbp
	pop	%r12
	pop	%r13
	pop	%r14
	ret
EPILOGUE()

	ALIGN(16)
PROLOGUE(mpn_mod_1s_4p_cps)
	push	%r12
	bsr	%rsi, %rcx
	push	%rbp
	xor	$63, R32(%rcx)
	mov	%rsi, %rbp
	mov	R32(%rcx), R32(%r12)
	sal	R8(%rcx), %rbp
	push	%rbx
	mov	%rdi, %rbx
	mov	%rbp, %rdi
	CALL(	mpn_invert_limb)
	mov	R32(%r12), R32(%rcx)
	mov	$1, R32(%r10)
	sal	R8(%rcx), %r10
	mov	$64, R32(%rcx)
	mov	%rax, %r9
	sub	R32(%r12), R32(%rcx)
	mov	%r9, (%rbx)
	shr	R8(%rcx), %rax
	mov	R32(%r12), R32(%rcx)
	or	%rax, %r10
	mov	%rbp, %rax
	neg	%rax
	imul	%rax, %r10
	mov	%r10, %rax
	mul	%r9
	lea	1(%r10,%rdx), %r8
	neg	%r8
	imul	%rbp, %r8
	cmp	%r8, %rax
	lea	(%r8,%rbp), %rdx
	cmovb	%rdx, %r8
	mov	%r8, %rax
	mul	%r9
	lea	1(%r8,%rdx), %rdi
	neg	%rdi
	imul	%rbp, %rdi
	cmp	%rdi, %rax
	lea	(%rdi,%rbp), %rdx
	cmovb	%rdx, %rdi
	mov	%rdi, %rax
	mul	%r9
	lea	1(%rdi,%rdx), %rsi
	neg	%rsi
	imul	%rbp, %rsi
	cmp	%rsi, %rax
	lea	(%rsi,%rbp), %rdx
	cmovb	%rdx, %rsi
	mov	%rsi, %rax
	mul	%r9
	lea	1(%rsi,%rdx), %rdx
	neg	%rdx
	imul	%rbp, %rdx
	cmp	%rdx, %rax
	lea	(%rdx,%rbp), %rbp
	movslq	R32(%r12), %rax
	cmovae	%rdx, %rbp
	shr	R8(%rcx), %r10
	shr	R8(%rcx), %r8
	shr	R8(%rcx), %rbp
	shr	R8(%rcx), %rdi
	shr	R8(%rcx), %rsi
	mov	%rbp, 48(%rbx)
	mov	%rax, 8(%rbx)
	mov	%r10, 16(%rbx)
	mov	%r8, 24(%rbx)
	mov	%rdi, 32(%rbx)
	mov	%rsi, 40(%rbx)
	pop	%rbx
	pop	%rbp
	pop	%r12
	ret
EPILOGUE()
