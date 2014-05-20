dnl  Alpha mpn_rshift -- Shift a number right.

dnl  Copyright 1994, 1995, 2000, 2002 Free Software Foundation, Inc.

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
C EV4:     4.75
C EV5:     3.75
C EV6:     2

C  INPUT PARAMETERS
C  rp	r16
C  up	r17
C  n	r18
C  cnt	r19


ASM_START()
PROLOGUE(mpn_rshift)
	ldq	r4,0(r17)	C load first limb
	addq	r17,8,r17
	subq	r31,r19,r7
	subq	r18,1,r18
	and	r18,4-1,r20	C number of limbs in first loop
	sll	r4,r7,r0	C compute function result

	beq	r20,$L0
	subq	r18,r20,r18

	ALIGN(8)
$Loop0:	ldq	r3,0(r17)
	addq	r16,8,r16
	addq	r17,8,r17
	subq	r20,1,r20
	srl	r4,r19,r5
	sll	r3,r7,r6
	bis	r3,r3,r4
	bis	r5,r6,r8
	stq	r8,-8(r16)
	bne	r20,$Loop0

$L0:	beq	r18,$Lend

	ALIGN(8)
$Loop:	ldq	r3,0(r17)
	addq	r16,32,r16
	subq	r18,4,r18
	srl	r4,r19,r5
	sll	r3,r7,r6

	ldq	r4,8(r17)
	srl	r3,r19,r1
	bis	r5,r6,r8
	stq	r8,-32(r16)
	sll	r4,r7,r2

	ldq	r3,16(r17)
	srl	r4,r19,r5
	bis	r1,r2,r8
	stq	r8,-24(r16)
	sll	r3,r7,r6

	ldq	r4,24(r17)
	srl	r3,r19,r1
	bis	r5,r6,r8
	stq	r8,-16(r16)
	sll	r4,r7,r2

	addq	r17,32,r17
	bis	r1,r2,r8
	stq	r8,-8(r16)

	bgt	r18,$Loop

$Lend:	srl	r4,r19,r8
	stq	r8,0(r16)
	ret	r31,(r26),1
EPILOGUE(mpn_rshift)
ASM_END()
