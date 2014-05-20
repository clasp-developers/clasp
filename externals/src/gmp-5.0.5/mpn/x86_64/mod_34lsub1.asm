dnl  AMD64 mpn_mod_34lsub1 -- remainder modulo 2^48-1.

dnl  Copyright 2000, 2001, 2002, 2004, 2005, 2007 Free Software Foundation,
dnl  Inc.
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
C K8,K9:	 1.0
C K10:		 1.12
C P4:		 3.25
C P6-15 (Core2): 1.5
C P6-28 (Atom):	 2.5


C INPUT PARAMETERS
C up	rdi
C n	rsi

C mp_limb_t mpn_mod_34lsub1 (mp_srcptr up, mp_size_t n)

C TODO
C  * Apply the movzwl tricks to the x86/k7 code
C  * Review feed-in and wind-down code.  In particular, try to avoid adc and
C    sbb to placate Pentium4.
C  * More unrolling and/or index addressing could bring time to under 1 c/l
C    for Athlon64, approaching 0.67 c/l seems possible.
C  * There are recurrencies on the carry registers (r8, r9, r10) that might
C    be the limiting factor for the Pentium4 speed.  Splitting these into 6
C    registers would help.
C  * For ultimate Athlon64 performance, a sequence like this might be best.
C    It should reach 0.5 c/l (limited by L1 cache bandwidth).
C
C	add	(%rdi), %rax
C	adc	8(%rdi), %rcx
C	adc	16(%rdi), %rdx
C	adc	$0, %r8
C	add	24(%rdi), %rax
C	adc	32(%rdi), %rcx
C	adc	40(%rdi), %rdx
C	adc	$0, %r8
C	...


ASM_START()
	TEXT
	ALIGN(32)
PROLOGUE(mpn_mod_34lsub1)

	mov	$0x0000FFFFFFFFFFFF, %r11

	sub	$2, %rsi
	ja	L(gt2)

	mov	(%rdi), %rax
	nop
	jb	L(1)

	mov	8(%rdi), %rsi
	mov	%rax, %rdx
	shr	$48, %rax		C src[0] low

	and	%r11, %rdx		C src[0] high
	add	%rdx, %rax
	mov	%esi, %edx

	shr	$32, %rsi		C src[1] high
	add	%rsi, %rax

	shl	$16, %rdx		C src[1] low
	add	%rdx, %rax

L(1):	ret


	ALIGN(16)
L(gt2):	xor	%eax, %eax
	xor	%ecx, %ecx
	xor	%edx, %edx
	xor	%r8, %r8
	xor	%r9, %r9
	xor	%r10, %r10

L(top):	add	(%rdi), %rax
	adc	$0, %r10
	add	8(%rdi), %rcx
	adc	$0, %r8
	add	16(%rdi), %rdx
	adc	$0, %r9

	sub	$3,%rsi
	jng	L(end)

	add	24(%rdi), %rax
	adc	$0, %r10
	add	32(%rdi), %rcx
	adc	$0, %r8
	add	40(%rdi), %rdx
	lea	48(%rdi), %rdi
	adc	$0, %r9

	sub	$3,%rsi
	jg	L(top)


	add	$-24, %rdi
L(end):	add	%r9, %rax
	adc	%r10, %rcx
	adc	%r8, %rdx

	inc	%rsi
	mov	$0x1, %r10d
	js	L(combine)

	mov	$0x10000, %r10d
	adc	24(%rdi), %rax
	dec	%rsi
	js	L(combine)

	adc	32(%rdi), %rcx
	mov	$0x100000000, %r10

L(combine):
	sbb	%rsi, %rsi		C carry
	mov	%rax, %rdi		C 0mod3
	shr	$48, %rax		C 0mod3 high

	and	%r10, %rsi		C carry masked
	and	%r11, %rdi		C 0mod3 low
	mov	%ecx, %r10d		C 1mod3

	add	%rsi, %rax		C apply carry
	shr	$32, %rcx		C 1mod3 high

	add	%rdi, %rax		C apply 0mod3 low
	movzwl	%dx, %edi		C 2mod3
	shl	$16, %r10		C 1mod3 low

	add	%rcx, %rax		C apply 1mod3 high
	shr	$16, %rdx		C 2mod3 high

	add	%r10, %rax		C apply 1mod3 low
	shl	$32, %rdi		C 2mod3 low

	add	%rdx, %rax		C apply 2mod3 high
	add	%rdi, %rax		C apply 2mod3 low

	ret
EPILOGUE()
