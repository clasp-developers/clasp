dnl  Alpha ev6 nails mpn_addmul_2.

dnl  Copyright 2002, 2005, 2006 Free Software Foundation, Inc.
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


dnl  Runs at 4.0 cycles/limb.  Speed limited by mulq latency.  With unrolling,
dnl  the ulimb load and the 3 bookkeeping increments and the `bis' that copies
dnl  from r21 to r5 could be removed and the instruction count reduced from 21
dnl  to to 16.  We could thereby reach about 2.3 cycles/limb.

include(`../config.m4')

dnl  INPUT PARAMETERS
define(`rp',`r16')
define(`up',`r17')
define(`n',`r18')
define(`vp',`r19')

dnl  Useful register aliases
define(`numb_mask',`r24')
define(`ulimb',`r25')
define(`rlimb',`r27')

define(`m0a',`r0')
define(`m0b',`r1')
define(`m1a',`r2')
define(`m1b',`r3')

define(`acc0',`r4')
define(`acc1',`r5')

define(`v0',`r6')
define(`v1',`r7')

dnl Used for temps: r8 r19 r28

define(`NAIL_BITS',`GMP_NAIL_BITS')
define(`NUMB_BITS',`GMP_NUMB_BITS')

dnl  This declaration is munged by configure
NAILS_SUPPORT(3-63)

ASM_START()
PROLOGUE(mpn_addmul_2)
	lda	numb_mask,-1(r31)
	srl	numb_mask,NAIL_BITS,numb_mask

	ldq	v0,	0(vp)
	ldq	v1,	8(vp)

	bis	r31,	r31,	acc0		C	zero acc0
	sll	v0,NAIL_BITS,	v0
	bis	r31,	r31,	acc1		C	zero acc1
	sll	v1,NAIL_BITS,	v1
	bis	r31,	r31,	r19

	ldq	ulimb,	0(up)
	lda	up,	8(up)
	mulq	v0,	ulimb,	m0a		C U1
	umulh	v0,	ulimb,	m0b		C U1
	mulq	v1,	ulimb,	m1a		C U1
	umulh	v1,	ulimb,	m1b		C U1
	lda	n,	-1(n)
	beq	n,	L(end)			C U0
	ALIGN(16)
L(top):	bis	r31,	r31,	r31		C U1	nop
	addq	r19,	acc0,	acc0		C U0	propagate nail
	ldq	rlimb,	0(rp)			C L0
	ldq	ulimb,	0(up)			C L1

	lda	rp,	8(rp)			C L1
	srl	m0a,NAIL_BITS,	r8		C U0
	lda	up,	8(up)			C L0
	mulq	v0,	ulimb,	m0a		C U1

	addq	r8,	acc0,	r19		C U0
	addq	m0b,	acc1,	acc0		C L1
	umulh	v0,	ulimb,	m0b		C U1
	bis	r31,	r31,	r31		C L0	nop

	addq	rlimb,	r19,	r19		C L1	FINAL PROD-SUM
	srl	m1a,NAIL_BITS,	r8		C U0
	bis	r31,	r31,	r31		C L0	nop
	mulq	v1,	ulimb,	m1a		C U1

	addq	r8,	acc0,	acc0		C U0
	bis	r31,	m1b,	acc1		C L1
	umulh	v1,	ulimb,	m1b		C U1
	and	r19,numb_mask,	r28		C L0	extract numb part

	lda	n,	-1(n)			C L0
	srl	r19,NUMB_BITS,	r19		C U1	extract nail part
	stq	r28,	-8(rp)			C L1
	bne	n,	L(top)			C U0

L(end):	ldq	rlimb,	0(rp)
	addq	r19,	acc0,	acc0		C	propagate nail
	lda	rp,	8(rp)
	srl	m0a,NAIL_BITS,	r8		C U0
	addq	r8,	acc0,	r19
	addq	m0b,	acc1,	acc0
	addq	rlimb,	r19,	r19
	srl	m1a,NAIL_BITS,	r8		C U0
	addq	r8,	acc0,	acc0
	bis	r31,	m1b,	acc1
	and	r19,numb_mask,	r28		C extract limb

	srl	r19,NUMB_BITS,	r19		C extract nail
	stq	r28,	-8(rp)

	addq	r19,	acc0,	acc0		C propagate nail
	and	acc0,numb_mask,	r28
	stq	r28,	0(rp)
	srl	acc0,NUMB_BITS,	r19
	addq	r19,	acc1,	r0

	ret	r31,	(r26),	1
EPILOGUE()
ASM_END()
