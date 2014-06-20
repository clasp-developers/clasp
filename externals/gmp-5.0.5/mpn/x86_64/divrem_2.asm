dnl  x86-64 mpn_divrem_2 -- Divide an mpn number by a normalized 2-limb number.

dnl  Copyright 2007, 2008, 2010 Free Software Foundation, Inc.

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


C		norm	frac
C K8		20	20
C P4		73	73
C P6 core2	37	37
C P6 corei7	33	33

C TODO
C  * Perhaps compute the inverse without relying on divq?  Could either use
C    Newton's method and mulq, or perhaps the faster fdiv.
C  * The loop has not been carefully tuned, nor analysed for critical path
C    length.  It seems that 20 c/l is a bit long, compared to the 13 c/l for
C    mpn_divrem_1.
C  * Clean up.  This code is really crude.


C INPUT PARAMETERS
define(`qp',		`%rdi')
define(`fn',		`%rsi')
define(`up_param',	`%rdx')
define(`un_param',	`%rcx')
define(`dp',		`%r8')

define(`dinv',		`%r9')


C rax rbx rcx rdx rsi rdi rbp r8  r9  r10 r11 r12 r13 r14 r15
C         cnt         qp      d  dinv

ASM_START()
	TEXT
	ALIGN(16)
PROLOGUE(mpn_divrem_2)

	push	%r15
	lea	(%rdx,%rcx,8), %rax
	push	%r14
	push	%r13
	mov	%rsi, %r13
	push	%r12
	lea	-24(%rax), %r12
	push	%rbp
	mov	%rdi, %rbp
	push	%rbx
	mov	8(%r8), %r11
	mov	-8(%rax), %r9
	mov	(%r8), %r8
	mov	-16(%rax), %r10
	xor	R32(%r15), R32(%r15)
	cmp	%r9, %r11
	ja	L(2)
	setb	%dl
	cmp	%r10, %r8
	setbe	%al
	orb	%al, %dl
	jne	L(23)
L(2):
	lea	-3(%rcx,%r13), %rbx	C un + fn - 3
	test	%rbx, %rbx
	js	L(6)
	mov	%r11, %rdx
	mov	$-1, %rax
	not	%rdx
	div	%r11
	mov	%r11, %rdx
	mov	%rax, %rdi
	imul	%rax, %rdx
	mov	%rdx, %r14
	mul	%r8
	mov	%rdx, %rcx
	mov	$-1, %rdx
	add	%r8, %r14
	adc	$0, %rdx
	add	%rcx, %r14
	adc	$0, %rdx
	js	L(8)
L(18):
	dec	%rdi
	sub	%r11, %r14
	sbb	$0, %rdx
	jns	L(18)
L(8):

C rax rbx rcx rdx rsi rdi rbp r8 r9 r10 r11 r12 r13 r14 r15
C n2      un      n1 dinv qp  d0        d1  up  fn      msl
C     n2  un     -d1      n1    dinv XX              XX

ifdef(`NEW',`
	lea	(%rbp,%rbx,8), %rbp
	mov	%rbx, %rcx		C un
	mov	%r9, %rbx
	mov	%rdi, %r9		C di
	mov	%r10, %r14
	mov	%r11, %rsi
	neg	%rsi			C -d1
	ALIGN(16)
L(loop):
	mov	%r9, %rax		C di		ncp
	mul	%rbx			C		0, 18
	add	%r14, %rax		C		4
	mov	%rax, %r10		C q0		5
	adc	%rbx, %rdx		C		5
	mov	%rdx, %rdi		C q		6
	imul	%rsi, %rdx		C		6
	mov	%r8, %rax		C		ncp
	lea	(%rdx, %r14), %rbx	C n1 -= ...	7
	mul	%rdi			C		7
	xor	R32(%r14), R32(%r14)	C
	cmp	%rcx, %r13		C
	jg	L(19)			C
	mov	(%r12), %r14		C
	sub	$8, %r12		C
L(19):	sub	%r8, %r14		C		ncp
	sbb	%r11, %rbx		C		9
	sub	%rax, %r14		C		11
	sbb	%rdx, %rbx		C		12
	inc	%rdi			C		7
	xor	R32(%rdx), R32(%rdx)	C
	cmp	%r10, %rbx		C		13
	mov	%r8, %rax		C d0		ncp
	adc	$-1, %rdx		C mask		14
	add	%rdx, %rdi		C q--		15
	and	%rdx, %rax		C d0 or 0	15
	and	%r11, %rdx		C d1 or 0	15
	add	%rax, %r14		C		16
	adc	%rdx, %rbx		C		16
	cmp	%r11, %rbx		C		17
	jae	L(fix)			C
L(bck):	mov	%rdi, (%rbp)		C
	sub	$8, %rbp		C
	dec	%rcx
	jns	L(loop)

	mov	%r14, %r10
	mov	%rbx, %r9
',`
	lea	(%rbp,%rbx,8), %rbp
	mov	%rbx, %rcx
	mov	%r9, %rax
	mov	%r10, %rsi
	ALIGN(16)
L(loop):
	mov	%rax, %r14		C		0, 19
	mul	%rdi			C		0
	mov	%r11, %r9		C		1
	add	%rsi, %rax		C		4
	mov	%rax, %rbx		C q0		5
	adc	%r14, %rdx		C q		5
	lea	1(%rdx), %r10		C		6
	mov	%rdx, %rax		C		6
	imul	%rdx, %r9		C		6
	sub	%r9, %rsi		C		10
	xor	R32(%r9), R32(%r9)	C
	mul	%r8			C		7
	cmp	%rcx, %r13		C
	jg	L(13)			C
	mov	(%r12), %r9		C
	sub	$8, %r12		C
L(13):	sub	%r8, %r9		C		ncp
	sbb	%r11, %rsi		C		11
	sub	%rax, %r9		C		11
	sbb	%rdx, %rsi		C		12
	cmp	%rbx, %rsi		C		13
	sbb	%rax, %rax		C		14
	not	%rax			C		15
	add	%rax, %r10		C		16
	mov	%r8, %rbx		C		ncp
	and	%rax, %rbx		C		16
	and	%r11, %rax		C		16
	add	%rbx, %r9		C		17
	adc	%rsi, %rax		C		18
	cmp	%rax, %r11		C		19
	jbe	L(fix)			C
L(bck):	mov	%r10, (%rbp)		C
	sub	$8, %rbp		C
	mov	%r9, %rsi		C		18
	dec	%rcx
	jns	L(loop)

	mov	%rsi, %r10
	mov	%rax, %r9
')
L(6):
	mov	%r10, 8(%r12)
	mov	%r9, 16(%r12)
	pop	%rbx
	pop	%rbp
	pop	%r12
	pop	%r13
	pop	%r14
	mov	%r15, %rax
	pop	%r15
	ret

L(23):	inc	R32(%r15)
	sub	%r8, %r10
	sbb	%r11, %r9
	jmp	L(2)

ifdef(`NEW',`
L(fix):	seta	%dl
	cmp	%r8, %r14
	setae	%al
	orb	%dl, %al
	je	L(bck)
	inc	%rdi
	sub	%r8, %r14
	sbb	%r11, %rbx
	jmp	L(bck)
',`
L(fix):	jb	L(88)
	cmp	%r8, %r9
	jb	L(bck)
L(88):	inc	%r10
	sub	%r8, %r9
	sbb	%r11, %rax
	jmp	L(bck)
')
EPILOGUE()
