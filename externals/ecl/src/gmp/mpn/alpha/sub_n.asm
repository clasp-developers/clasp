dnl  Alpha mpn_sub_n -- Subtract two limb vectors of the same length > 0 and
dnl  store difference in a third limb vector.

dnl  Copyright 1995, 2000, 2002, 2005 Free Software Foundation, Inc.

dnl  This file is part of the GNU MP Library.

dnl  The GNU MP Library is free software; you can redistribute it and/or modify
dnl  it under the terms of the GNU Lesser General Public License as published
dnl  by the Free Software Foundation; either version 2.1 of the License, or (at
dnl  your option) any later version.

dnl  The GNU MP Library is distributed in the hope that it will be useful, but
dnl  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
dnl  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
dnl  License for more details.

dnl  You should have received a copy of the GNU Lesser General Public License
dnl  along with the GNU MP Library; see the file COPYING.LIB.  If not, write
dnl  to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
dnl  Boston, MA 02110-1301, USA.

include(`../config.m4')

C      cycles/limb
C EV4:     7.75
C EV5:     5.75
C EV6:     4

C  INPUT PARAMETERS
C  rp	r16
C  up	r17
C  vp	r18
C  n	r19

ASM_START()
PROLOGUE(mpn_sub_n)
	ldq	r3,0(r17)
	ldq	r4,0(r18)

	subq	r19,1,r19
	and	r19,4-1,r2	C number of limbs in first loop
	bis	r31,r31,r0
	beq	r2,$L0		C if multiple of 4 limbs, skip first loop

	subq	r19,r2,r19

$Loop0:	subq	r2,1,r2
	ldq	r5,8(r17)
	addq	r4,r0,r4
	ldq	r6,8(r18)
	cmpult	r4,r0,r1
	subq	r3,r4,r4
	cmpult	r3,r4,r0
	stq	r4,0(r16)
	bis	r0,r1,r0

	addq	r17,8,r17
	addq	r18,8,r18
	bis	r5,r5,r3
	bis	r6,r6,r4
	addq	r16,8,r16
	bne	r2,$Loop0

$L0:	beq	r19,$Lend

	ALIGN(8)
$Loop:	subq	r19,4,r19

	ldq	r5,8(r17)
	addq	r4,r0,r4
	ldq	r6,8(r18)
	cmpult	r4,r0,r1
	subq	r3,r4,r4
	cmpult	r3,r4,r0
	stq	r4,0(r16)
	bis	r0,r1,r0

	ldq	r3,16(r17)
	addq	r6,r0,r6
	ldq	r4,16(r18)
	cmpult	r6,r0,r1
	subq	r5,r6,r6
	cmpult	r5,r6,r0
	stq	r6,8(r16)
	bis	r0,r1,r0

	ldq	r5,24(r17)
	addq	r4,r0,r4
	ldq	r6,24(r18)
	cmpult	r4,r0,r1
	subq	r3,r4,r4
	cmpult	r3,r4,r0
	stq	r4,16(r16)
	bis	r0,r1,r0

	ldq	r3,32(r17)
	addq	r6,r0,r6
	ldq	r4,32(r18)
	cmpult	r6,r0,r1
	subq	r5,r6,r6
	cmpult	r5,r6,r0
	stq	r6,24(r16)
	bis	r0,r1,r0

	addq	r17,32,r17
	addq	r18,32,r18
	addq	r16,32,r16
	bne	r19,$Loop

$Lend:	addq	r4,r0,r4
	cmpult	r4,r0,r1
	subq	r3,r4,r4
	cmpult	r3,r4,r0
	stq	r4,0(r16)
	bis	r0,r1,r0
	ret	r31,(r26),1
EPILOGUE(mpn_sub_n)
ASM_END()
