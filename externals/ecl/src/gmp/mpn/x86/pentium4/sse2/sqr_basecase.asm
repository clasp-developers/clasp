dnl  Intel Pentium-4 mpn_sqr_basecase -- square an mpn number.

dnl  Copyright 2001, 2002 Free Software Foundation, Inc.
dnl
dnl  This file is part of the GNU MP Library.
dnl
dnl  The GNU MP Library is free software; you can redistribute it and/or
dnl  modify it under the terms of the GNU Lesser General Public License as
dnl  published by the Free Software Foundation; either version 2.1 of the
dnl  License, or (at your option) any later version.
dnl
dnl  The GNU MP Library is distributed in the hope that it will be useful,
dnl  but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl  Lesser General Public License for more details.
dnl
dnl  You should have received a copy of the GNU Lesser General Public
dnl  License along with the GNU MP Library; see the file COPYING.LIB.  If
dnl  not, write to the Free Software Foundation, Inc., 51 Franklin Street,
dnl  Fifth Floor, Boston, MA 02110-1301, USA.

include(`../config.m4')


C P4: approx 3.5 cycles per crossproduct, or 7 cycles per triangular
C     product, at around 30x30 limbs.


C void mpn_sqr_basecase (mp_ptr dst, mp_srcptr src, mp_size_t size);
C
C The algorithm is basically the same as mpn/generic/sqr_basecase.c, but a
C lot of function call overheads are avoided, especially when the size is
C small.
C
C On small sizes there's only a small speedup over mpn_mul_basecase,
C presumably branch mispredictions are a bigger fraction of the work done.
C It's not clear how to help this.

defframe(PARAM_SIZE,12)
defframe(PARAM_SRC, 8)
defframe(PARAM_DST, 4)

	TEXT
	ALIGN(8)
PROLOGUE(mpn_sqr_basecase)
deflit(`FRAME',0)

	movl	PARAM_SIZE, %edx
	movl	PARAM_SRC, %eax
	movl	PARAM_DST, %ecx

	cmpl	$2, %edx

	je	L(two_limbs)
	ja	L(three_or_more)

C -----------------------------------------------------------------------------
C one limb only
	C eax	src
	C ebx
	C ecx	dst
	C edx

	movl	(%eax), %eax
	mull	%eax

	movl	%eax, (%ecx)
	movl	%edx, 4(%ecx)

	ret

C -----------------------------------------------------------------------------
L(two_limbs):
	C eax	src
	C ebx
	C ecx	dst
	C edx	size

	movd	(%eax), %mm1
	movd	4(%eax), %mm0
	pmuludq	%mm1, %mm0		C src[0]*src[1]

	pmuludq	%mm1, %mm1		C src[0]^2

	movd	4(%eax), %mm2
	pmuludq	%mm2, %mm2		C src[1]^2

	movd	%mm1, (%ecx)		C dst[0]
	psrlq	$32, %mm1

	pcmpeqd	%mm3, %mm3
	psrlq	$32, %mm3		C 0x00000000FFFFFFFF
	pand	%mm0, %mm3		C low(src[0]*src[1])
	psrlq	$32, %mm0		C high(src[0]*src[1])

	psllq	$1, %mm3		C 2*low(src[0]*src[1])
	paddq	%mm3, %mm1		C high(src[0]^2)
	movd	%mm1, 4(%ecx)		C dst[1]

	pcmpeqd	%mm4, %mm4
	psrlq	$32, %mm4		C 0x00000000FFFFFFFF
	pand	%mm2, %mm4		C low(src[1]^2)
	psrlq	$32, %mm2		C high(src[1]^2)

	psllq	$1, %mm0		C 2*high(src[0]*src[1])
	psrlq	$32, %mm1		C carry
	paddq	%mm1, %mm0
	paddq	%mm4, %mm0		C low(src[1]^2)
	movd	%mm0, 8(%ecx)		C dst[2]

	psrlq	$32, %mm0		C carry
	paddq	%mm2, %mm0		C high(src[1]^2)
	movd	%mm0, 12(%ecx)		C dst[3]

	ASSERT(z,`
	psrlq	$32, %mm0
	movd	%mm0, %eax
	orl	%eax, %eax')

	emms
	ret


C -----------------------------------------------------------------------------
L(three_or_more):

	C eax	src
	C ebx
	C ecx	dst
	C edx	size
	C esi
	C edi
	C ebp
	C
	C First multiply src[0]*src[1..size-1] and store at dst[1..size].

defframe(SAVE_ESI,  -4)
defframe(SAVE_EDI,  -8)
defframe(SAVE_EBP, -12)
deflit(STACK_SPACE, 12)

	subl	$STACK_SPACE, %esp	FRAME_subl_esp(STACK_SPACE)
	pxor	%mm0, %mm0		C initial carry
	movd	(%eax), %mm7		C multiplier

	movl	%esi, SAVE_ESI
	movl	%edi, SAVE_EDI
	movl	%ebp, SAVE_EBP


	movl	%eax, %esi
	movl	%ecx, %edi
	subl	$1, %edx

	C First multiply src[0]*src[1..size-1] and store at dst[1..size].
L(mul1):
	C eax	src, incrementing
	C ebx
	C ecx	dst, incrementing
	C edx	counter, size-1 iterations
	C esi	src
	C edi	dst
	C ebp
	C
	C mm0	carry limb
	C mm7	multiplier

	movd	4(%eax), %mm1
	addl	$4, %eax
	pmuludq	%mm7, %mm1
	paddq	%mm1, %mm0
	movd	%mm0, 4(%ecx)
	addl	$4, %ecx
	psrlq	$32, %mm0
	subl	$1, %edx
	jnz	L(mul1)


	movl	PARAM_SIZE, %ebp
	subl	$3, %ebp
	jz	L(corner)


	C Add products src[n]*src[n+1..size-1] at dst[2*n-1...], for
	C n=1..size-2.  The last two products, which are the end corner of
	C the product triangle, are handled separately to save looping
	C overhead.

L(outer):
	C eax
	C ebx
	C ecx
	C edx
	C esi	src, incrementing
	C edi	dst, incrementing
	C ebp	size, decrementing
	C
	C mm0	prev carry

	movd	4(%esi), %mm7		C multiplier
	movd	%mm0, 4(%ecx)		C prev carry

	leal	8(%esi), %eax		C next src
	addl	$4, %esi

	leal	8(%edi), %ecx		C next dst
	addl	$8, %edi

	leal	1(%ebp), %edx		C counter

	pxor	%mm0, %mm0		C initial carry limb, clear carry flag

L(inner):
	C eax	src, incrementing
	C edx
	C ecx	dst, incrementing
	C edx	counter
	C esi	outer src
	C edi	outer dst
	C ebp	outer size
	C
	C mm0	carry

	movd	(%eax), %mm1
	leal	4(%eax), %eax
	movd	4(%ecx),%mm2
	pmuludq	%mm7, %mm1
	paddq	%mm2, %mm1
	paddq	%mm1, %mm0
	subl	$1, %edx
	movd	%mm0, 4(%ecx)
	psrlq	$32, %mm0
	leal	4(%ecx), %ecx
	jnz	L(inner)

	subl	$1, %ebp
	jnz	L(outer)


L(corner):
	C esi	&src[size-3]
	C edi	&dst[2*size-6]
	C mm0	carry
	C
	C       +-----+-----+--
	C       | mm0 | dst so far
	C       +-----+-----+--
	C +-----+-----+
	C |     |     |  src[size-2]*src[size-1]
	C +-----+-----+

	movd	4(%esi), %mm1
	movd	8(%esi), %mm2
	pmuludq	%mm2, %mm1		C src[size-1]*src[size-2]

	movl	PARAM_SRC, %eax
	movd	(%eax), %mm2
	pmuludq	%mm2, %mm2		C src[0]^2

	pcmpeqd	%mm7, %mm7
	psrlq	$32, %mm7

	movl	PARAM_DST, %edx
	movd	4(%edx), %mm3		C dst[1]

	paddq	%mm1, %mm0
	movd	%mm0, 12(%edi)		C dst[2*size-3]

	psrlq	$32, %mm0
	movd	%mm0, 16(%edi)		C dst[2*size-2]

	movd	%mm2, (%edx)		C dst[0]
	psrlq	$32, %mm2

	psllq	$1, %mm3		C 2*dst[1]
	paddq	%mm3, %mm2
	movd	%mm2, 4(%edx)
	psrlq	$32, %mm2

	movl	PARAM_SIZE, %ecx
	subl	$2, %ecx

	C Now form squares on the diagonal src[0]^2,...,src[size-1]^2, and
	C add to the triangular parts dst[1..2*size-2] with those left
	C shifted by 1 bit.

L(diag):
	C eax	src, incrementing
	C ebx
	C ecx	counter, size-2 iterations
	C edx	dst, incrementing
	C esi
	C edi
	C ebp
	C
	C mm2	carry
	C mm7	0x00000000FFFFFFFF

	movd	4(%eax), %mm0	C src limb
	addl	$4, %eax
	pmuludq	%mm0, %mm0
	movq	%mm7, %mm1
	pand	%mm0, %mm1	C diagonal low
	psrlq	$32, %mm0	C diagonal high

	movd	8(%edx), %mm3
	psllq	$1, %mm3	C 2*dst[i]
	paddq	%mm3, %mm1
	paddq	%mm1, %mm2
	movd	%mm2, 8(%edx)
	psrlq	$32, %mm2

	movd	12(%edx), %mm3
	psllq	$1, %mm3	C 2*dst[i+1]
	paddq	%mm3, %mm0
	paddq	%mm0, %mm2
	movd	%mm2, 12(%edx)
	addl	$8, %edx
	psrlq	$32, %mm2

	subl	$1, %ecx
	jnz	L(diag)


	movd	4(%eax), %mm0	C src[size-1]
	pmuludq	%mm0, %mm0
	pand	%mm0, %mm7	C diagonal low
	psrlq	$32, %mm0	C diagonal high

	movd	8(%edx), %mm3	C dst[2*size-2]
	psllq	$1, %mm3
	paddq	%mm3, %mm7
	paddq	%mm7, %mm2
	movd	%mm2, 8(%edx)
	psrlq	$32, %mm2

	paddq	%mm0, %mm2
	movd	%mm2, 12(%edx)	C dst[2*size-1]

	ASSERT(z,`	C no further carry
	psrlq	$32, %mm2
	movd	%mm2, %eax
	orl	%eax, %eax')


	movl	SAVE_ESI, %esi
	movl	SAVE_EDI, %edi
	movl	SAVE_EBP, %ebp
	addl	$STACK_SPACE, %esp
	emms
	ret

EPILOGUE()
