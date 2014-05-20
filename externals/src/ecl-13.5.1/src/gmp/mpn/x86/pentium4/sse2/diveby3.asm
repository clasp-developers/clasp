dnl  Intel Pentium-4 mpn_divexact_by3 -- mpn exact division by 3.

dnl  Copyright 2001, 2002, 2003 Free Software Foundation, Inc.
dnl
dnl  This file is part of the GNU MP Library.
dnl
dnl  The GNU MP Library is free software; you can redistribute it and/or modify
dnl  it under the terms of the GNU Lesser General Public License as published
dnl  by the Free Software Foundation; either version 2 of the License, or (at
dnl  your option) any later version.
dnl
dnl  The GNU MP Library is distributed in the hope that it will be useful, but
dnl  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
dnl  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
dnl  License for more details.
dnl
dnl  You should have received a copy of the GNU Lesser General Public License
dnl  along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
dnl  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
dnl  Boston, MA 02110-1301, USA.

include(`../config.m4')


C P4: 18.0 cycles/limb


C mp_limb_t mpn_divexact_by3c (mp_ptr dst, mp_srcptr src, mp_size_t size,
C                              mp_limb_t carry);
C
C The dependent chain in the loop is as follows, and this is what the code
C measures.
C
C	psubq     (src-cbit) - climb	2
C	pmuludq   s*inverse		8
C	pand      mask q		2
C	psllq     2*q			2
C	paddq     q+2*q			2
C	psrlq     high(3*q)		2
C				       --
C				       18
C
C Perhaps the s*inverse can be taken off the dependent chain as described in
C mpn/generic/diveby3.c, with a modified 3*q calculation that can give
C high(3*q)*inv too.

defframe(PARAM_CARRY,16)
defframe(PARAM_SIZE, 12)
defframe(PARAM_SRC,   8)
defframe(PARAM_DST,   4)

	TEXT
	ALIGN(16)

PROLOGUE(mpn_divexact_by3c)
deflit(`FRAME',0)

	movl	PARAM_SRC, %eax
	pxor	%mm0, %mm0

	movd	PARAM_CARRY, %mm1
	movl	$0xAAAAAAAB, %ecx

	movl	PARAM_DST, %edx
	pcmpeqd	%mm6, %mm6

	movd	%ecx, %mm7
	movl	PARAM_SIZE, %ecx

	psrlq	$32, %mm6		C 0x00000000FFFFFFFF


L(top):
	C eax	src, incrementing
	C ebx
	C ecx	counter, limbs, decrementing
	C edx	dst, incrementing
	C
	C mm0	carry bit
	C mm1	carry limb
	C mm6	0x00000000FFFFFFFF
	C mm7	0xAAAAAAAB, inverse of 3

	movd	(%eax), %mm2
	addl	$4, %eax

	psubq	%mm0, %mm2		C src - cbit

	psubq	%mm1, %mm2		C src - cbit - climb
	movq	%mm2, %mm0
	psrlq	$63, %mm0		C new cbit

	pmuludq	%mm7, %mm2		C s*inverse
	movd	%mm2, (%edx)		C q
	addl	$4, %edx

	movq	%mm6, %mm1

	pand	%mm2, %mm1

	pand	%mm6, %mm2

	psllq	$1, %mm1

	C

	paddq	%mm2, %mm1

	C

	psrlq	$32, %mm1

	subl	$1, %ecx
	jnz	L(top)


	paddd	%mm1, %mm0
	movd	%mm0, %eax
	emms
	ret

EPILOGUE()
