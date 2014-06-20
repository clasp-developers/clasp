dnl  x86-32 mpn_mod_1s_4p, requiring cmov.

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

C                           cycles/limb
C P5:
C P6 model 0-8,10-12)
C P6 model 9  (Banias)
C P6 model 13 (Dothan)		 6.0
C P4 model 0  (Willamette)
C P4 model 1  (?)
C P4 model 2  (Northwood)	15.5
C P4 model 3  (Prescott)
C P4 model 4  (Nocona)
C K6:
C K7:                            4.75
C K8:


C Ths inner loop was manually written, it ought to be loopmixed.
C Presumably, we could get to 4 c/l for K7.

C The cps function was compiler generated.  It can clearly be optimized.


ASM_START()
	TEXT

	ALIGN(16)
PROLOGUE(mpn_mod_1s_4p)
	push	%ebp
	push	%edi
	push	%esi
	push	%ebx
	sub	$28, %esp
	mov	60(%esp), %edi		C cps
	mov	8(%edi), %eax
	mov	12(%edi), %edx
	mov	16(%edi), %ecx
	mov	20(%edi), %esi
	mov	24(%edi), %edi
	mov	%eax, 4(%esp)
	mov	%edx, 8(%esp)
	mov	%ecx, 12(%esp)
	mov	%esi, 16(%esp)
	mov	%edi, 20(%esp)
	mov	52(%esp), %eax		C n
	xor	%edi, %edi
	mov	48(%esp), %esi		C up
	lea	-12(%esi,%eax,4), %esi
	and	$3, %eax
	je	L(b0)
	cmp	$2, %eax
	jc	L(b1)
	je	L(b2)

L(b3):	mov	4(%esi), %eax
	mull	4(%esp)
	mov	(%esi), %ebp
	add	%eax, %ebp
	adc	%edx, %edi
	mov	8(%esi), %eax
	mull	8(%esp)
	lea	-12(%esi), %esi
	jmp	L(m0)

L(b0):	mov	(%esi), %eax
	mull	4(%esp)
	mov	-4(%esi), %ebp
	add	%eax, %ebp
	adc	%edx, %edi
	mov	4(%esi), %eax
	mull	8(%esp)
	add	%eax, %ebp
	adc	%edx, %edi
	mov	8(%esi), %eax
	mull	12(%esp)
	lea	-16(%esi), %esi
	jmp	L(m0)

L(b1):	mov	8(%esi), %ebp
	lea	-4(%esi), %esi
	jmp	L(m1)

L(b2):	mov	8(%esi), %eax
	mull	4(%esp)
	mov	4(%esi), %ebp
	lea	-8(%esi), %esi
	jmp	L(m0)

	ALIGN(16)
L(top):	mov	(%esi), %eax
	mull	4(%esp)
	mov	-4(%esi), %ebx
	xor	%ecx, %ecx
	add	%eax, %ebx
	adc	%edx, %ecx
	mov	4(%esi), %eax
	mull	8(%esp)
	add	%eax, %ebx
	adc	%edx, %ecx
	mov	8(%esi), %eax
	mull	12(%esp)
	add	%eax, %ebx
	adc	%edx, %ecx
	lea	-16(%esi), %esi
	mov	16(%esp), %eax
	mul	%ebp
	add	%eax, %ebx
	adc	%edx, %ecx
	mov	20(%esp), %eax
	mul	%edi
	mov	%ebx, %ebp
	mov	%ecx, %edi
L(m0):	add	%eax, %ebp
	adc	%edx, %edi
L(m1):	sub	$4, 52(%esp)
	ja	L(top)

L(end):	mov	4(%esp), %eax
	mul	%edi
	mov	60(%esp), %edi
	add	%eax, %ebp
	adc	$0, %edx
	mov	4(%edi), %ecx
	mov	%edx, %esi
	mov	%ebp, %eax
	sal	%cl, %esi
	mov	%ecx, %ebx
	neg	%ecx
	shr	%cl, %eax
	or	%esi, %eax
	lea	1(%eax), %esi
	mull	(%edi)
	mov	%ebx, %ecx
	mov	%eax, %ebx
	mov	%ebp, %eax
	sal	%cl, %eax
	add	%eax, %ebx
	adc	%esi, %edx
	imul	56(%esp), %edx
	mov	56(%esp), %esi
	sub	%edx, %eax
	lea	(%eax,%esi), %edx
	cmp	%eax, %ebx
	cmovb(	%edx, %eax)
	mov	%eax, %edx
	sub	%esi, %eax
	cmovb(	%edx, %eax)
	add	$28, %esp
	pop	%ebx
	pop	%esi
	pop	%edi
	pop	%ebp
	shr	%cl, %eax
	ret
EPILOGUE()

	ALIGN(16)
PROLOGUE(mpn_mod_1s_4p_cps)
	sub	$56, %esp
	mov	%esi, 44(%esp)
	mov	64(%esp), %esi
	mov	%edi, 48(%esp)
	mov	%ebx, 40(%esp)
	mov	$-1, %ebx
	mov	%ebp, 52(%esp)
	bsr	%esi, %eax
	xor	$31, %eax
	mov	%eax, %ecx
	mov	%eax, 24(%esp)
	mov	%ebx, %eax
	sal	%cl, %esi
	mov	%esi, %ecx
	mov	%esi, %edi
	mov	%esi, %ebp
	neg	%ecx
	not	%edi
	mov	%ecx, 20(%esp)
	mov	$32, %ecx
	sub	24(%esp), %ecx
	mov	%edi, %edx
	mov	%edi, 16(%esp)
	mov	20(%esp), %edi
	div	%esi
	mov	%eax, %ebx
	shr	%cl, %eax
	movzbl	24(%esp), %ecx
	mov	%eax, 12(%esp)
	mov	$1, %eax
	sal	%cl, %eax
	or	%eax, 12(%esp)
	imul	12(%esp), %edi
	mov	%edi, %eax
	mov	%edi, 20(%esp)
	mul	%ebx
	mov	%eax, %ecx
	lea	1(%edx,%edi), %eax
	neg	%eax
	imul	%eax, %ebp
	lea	(%ebp,%esi), %eax
	cmp	%ebp, %ecx
	cmovb(	%eax, %ebp)
	mov	%ebp, %eax
	mul	%ebx
	lea	1(%ebp,%edx), %edi
	mov	%eax, %ecx
	neg	%edi
	mov	%edi, 8(%esp)
	imul	%esi, %edi
	mov	%edi, %eax
	add	%esi, %eax
	cmp	%edi, %ecx
	cmovae(	%edi, %eax)
	mov	%eax, 32(%esp)
	mov	32(%esp), %edi
	mul	%ebx
	mov	%eax, 36(%esp)
	lea	1(%edi,%edx), %eax
	negl	%eax
	imul	%esi, %eax
	mov	%eax, %ecx
	add	%esi, %ecx
	cmp	%eax, 36(%esp)
	cmovae(	%eax, %ecx)
	mov	%ecx, (%esp)
	mov	%ecx, %eax
	mul	%ebx
	mov	%eax, %edi
	mov	(%esp), %eax
	lea	1(%eax,%edx), %ecx
	mov	60(%esp), %edx
	neg	%ecx
	imul	%esi, %ecx
	mov	%ebx, (%edx)
	add	%ecx, %esi
	cmp	%ecx, %edi
	cmovae(	%ecx, %esi)
	mov	24(%esp), %ecx
	shrl	%cl, 20(%esp)
	mov	20(%esp), %edi
	mov	%esi, 4(%esp)
	mov	%ecx, 4(%edx)
	movzbl	24(%esp), %ecx
	mov	%edi, 8(%edx)
	shr	%cl, %ebp
	shr	%cl, %eax
	mov	%ebp, 12(%edx)
	shrl	%cl, 32(%esp)
	mov	32(%esp), %edi
	shrl	%cl, 4(%esp)
	mov	%eax, 20(%edx)
	mov	%edi, 16(%edx)
	mov	4(%esp), %edi
	mov	%edi, 24(%edx)
	mov	40(%esp), %ebx
	mov	44(%esp), %esi
	mov	48(%esp), %edi
	mov	52(%esp), %ebp
	add	$56, %esp
	ret
EPILOGUE()
