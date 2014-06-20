dnl  mpn_mod_1_4 for Pentium 4 and P6 models with SSE2 (i.e., 9,D,E,F).

dnl  Contributed to the GNU project by Torbjorn Granlund.

dnl  Copyright 2009 Free Software Foundation, Inc.
dnl
dnl  This file is part of the GNU MP Library.
dnl
dnl  The GNU MP Library is free software; you can redistribute it and/or modify
dnl  it under the terms of the GNU Lesser General Public License as published
dnl  by the Free Software Foundation; either version 3 of the License, or (at
dnl  your option) any later version.
dnl
dnl  The GNU MP Library is distributed in the hope that it will be useful, but
dnl  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
dnl  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
dnl  License for more details.
dnl
dnl  You should have received a copy of the GNU Lesser General Public License
dnl  along with the GNU MP Library.  If not, see http://www.gnu.org/licenses/.

include(`../config.m4')

C TODO:
C  * Optimize.  The present code was written quite straightforwardly.
C  * Optimize post-loop reduction code.

C                           cycles/limb
C P6 model 0-8,10-12)           -
C P6 model 9   (Banias)         ?
C P6 model 13  (Dothan)         3.4
C P4 model 0-1 (Willamette):    ?
C P4 model 2   (Northwood):     4
C P4 model 3-4 (Prescott):      ?

C INPUT PARAMETERS
C ap		sp + 4
C n		sp + 8
C b		sp + 12
C cps		sp + 16

define(`B1modb', `%mm1')
define(`B2modb', `%mm2')
define(`B3modb', `%mm3')
define(`B4modb', `%mm4')
define(`B5modb', `%mm5')
define(`ap', `%edx')
define(`n', `%eax')

	TEXT
	ALIGN(16)
PROLOGUE(mpn_mod_1s_4p)
	push	%ebx
	mov	8(%esp), ap
	mov	12(%esp), n
	mov	20(%esp), %ecx

	movd	8(%ecx), B1modb
	movd	12(%ecx), B2modb
	movd	16(%ecx), B3modb
	movd	20(%ecx), B4modb
	movd	24(%ecx), B5modb

	mov	n, %ebx
	lea	-4(ap,n,4), ap
	and	$3, %ebx
	je	L(b0)
	cmp	$2, %ebx
	jc	L(b1)
	je	L(b2)

L(b3):	movd	-4(ap), %mm7
	pmuludq	B1modb, %mm7
	movd	-8(ap), %mm6
	paddq	%mm6, %mm7
	movd	(ap), %mm6
	pmuludq	B2modb, %mm6
	paddq	%mm6, %mm7
	lea	-24(ap), ap
	add	$-3, n
	jz	L(end)
	jmp	L(top)

L(b0):	movd	-8(ap), %mm7
	pmuludq	B1modb, %mm7
	movd	-12(ap), %mm6
	paddq	%mm6, %mm7
	movd	-4(ap), %mm6
	pmuludq	B2modb, %mm6
	paddq	%mm6, %mm7
	movd	(ap), %mm6
	pmuludq	B3modb, %mm6
	paddq	%mm6, %mm7
	lea	-28(ap), ap
	add	$-4, n
	jz	L(end)
	jmp	L(top)

L(b1):	movd	(ap), %mm7
	lea	-16(ap), ap
	dec	n
	jz	L(x)
	jmp	L(top)

L(b2):	movd	(ap), %mm7
	pmuludq	B1modb, %mm7
	movd	-4(ap), %mm6
	paddq	%mm6, %mm7
	lea	-20(ap), ap
	add	$-2, n
	jz	L(end)

	ALIGN(8)
L(top):	movd	4(ap), %mm0
	pmuludq	B1modb, %mm0
	movd	0(ap), %mm6
	paddq	%mm6, %mm0

	movd	8(ap), %mm6
	pmuludq	B2modb, %mm6
	paddq	%mm6, %mm0

	movd	12(ap), %mm6
	pmuludq	B3modb, %mm6
	paddq	%mm6, %mm0

	movq	%mm7, %mm6
	psrlq	$32, %mm7		C rh
	pmuludq	B5modb, %mm7
	pmuludq	B4modb, %mm6

	paddq	%mm0, %mm7
	paddq	%mm6, %mm7

	add	$-16, ap
	add	$-4, n
	jnz	L(top)
L(end):

	pcmpeqd	%mm4, %mm4
	psrlq	$32, %mm4		C 0x00000000FFFFFFFF
	pand	%mm7, %mm4		C rl
	psrlq	$32, %mm7		C rh
	pmuludq	B1modb, %mm7		C rh,cl
	paddq	%mm4, %mm7		C rh,rl

L(x):	movd	4(%ecx), %mm4		C cnt
	psllq	%mm4, %mm7		C rh,rl normalized
	movq	%mm7, %mm2		C rl in low half
	psrlq	$32, %mm7		C rh
	movd	(%ecx), %mm1		C bi
	pmuludq	%mm7, %mm1		C qh,ql
	paddq	%mm2, %mm1		C qh-1,ql
	movd	%mm1, %ecx		C ql
	psrlq	$32, %mm1		C qh-1
	movd	16(%esp), %mm3		C b
	pmuludq	%mm1, %mm3		C (qh-1) * b
	psubq	%mm3, %mm2		C r in low half (could use psubd)
	movd	%mm2, %eax		C r
	mov	16(%esp), %ebx
	sub	%ebx, %eax		C r
	cmp	%eax, %ecx
	lea	(%eax,%ebx), %edx
	cmovc(	%edx, %eax)
	movd	%mm4, %ecx		C cnt
	cmp	%ebx, %eax
	jae	L(fix)
	emms
	pop	%ebx
	shr	%cl, %eax
	ret

L(fix):	sub	%ebx, %eax
	emms
	pop	%ebx
	shr	%cl, %eax
	ret
EPILOGUE()

PROLOGUE(mpn_mod_1s_4p_cps)
	push	%ebp
	push	%edi
	push	%esi
	push	%ebx
	sub	$12, %esp
	mov	36(%esp), %ebx
	bsr	%ebx, %ecx
	xor	$31, %ecx
	mov	%ecx, 4(%esp)
	sal	%cl, %ebx
	mov	%ebx, %edx
	not	%edx
	mov	$-1, %eax
	div	%ebx
	mov	%eax, %esi
	mov	$1, %ebp
	sal	%cl, %ebp
	neg	%ecx
	shr	%cl, %eax
	or	%eax, %ebp
	mov	%ebx, %eax
	neg	%eax
	imul	%ebp, %eax
	mov	%esi, %ecx
	mov	%eax, 8(%esp)
	mul	%ecx
	mov	%edx, %esi
	not	%esi
	sub	8(%esp), %esi
	imul	%ebx, %esi
	lea	(%esi,%ebx), %edx
	cmp	%esi, %eax
	cmovb(	%edx, %esi)
	mov	%esi, %eax
	mul	%ecx
	lea	(%esi,%edx), %edi
	not	%edi
	imul	%ebx, %edi
	lea	(%edi,%ebx), %edx
	cmp	%edi, %eax
	cmovb(	%edx, %edi)
	mov	%edi, %eax
	mul	%ecx
	lea	(%edi,%edx), %ebp
	not	%ebp
	imul	%ebx, %ebp
	lea	(%ebp,%ebx), %edx
	cmp	%ebp, %eax
	cmovb(	%edx, %ebp)
	mov	%ebp, %eax
	mul	%ecx
	add	%ebp, %edx
	not	%edx
	imul	%ebx, %edx
	add	%edx, %ebx
	cmp	%edx, %eax
	cmovb(	%ebx, %edx)
	mov	32(%esp), %eax
	mov	%ecx, (%eax)
	mov	4(%esp), %ecx
	mov	%ecx, 4(%eax)
	mov	8(%esp), %ebx
	shr	%cl, %ebx
	mov	%ebx, 8(%eax)
	shr	%cl, %esi
	mov	%esi, 12(%eax)
	shr	%cl, %edi
	mov	%edi, 16(%eax)
	shr	%cl, %ebp
	mov	%ebp, 20(%eax)
	shr	%cl, %edx
	mov	%edx, 24(%eax)
	add	$12, %esp
	pop	%ebx
	pop	%esi
	pop	%edi
	pop	%ebp
	ret
EPILOGUE()
