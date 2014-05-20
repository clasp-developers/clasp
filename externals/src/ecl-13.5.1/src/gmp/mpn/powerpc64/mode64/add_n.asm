dnl  PowerPC-64 mpn_add_n -- Add two limb vectors of the same length > 0 and
dnl  store sum in a third limb vector.

dnl  Copyright 1999, 2000, 2001 Free Software Foundation, Inc.

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

C INPUT PARAMETERS
C res_ptr	r3
C s1_ptr	r4
C s2_ptr	r5
C size		r6

C This code runs at 1.5 cycles/limb on the PPC630.

ASM_START()
PROLOGUE(mpn_add_n)
	mtctr	r6		C copy size into CTR
	addic	r0,r0,0		C clear cy
	ld	r8,0(r4)	C load least significant s1 limb
	ld	r0,0(r5)	C load least significant s2 limb
	addi	r3,r3,-8	C offset res_ptr, it's updated before it's used
	bdz	L(eno)		C If done, skip loop
L(top):	ld	r9,8(r4)	C load s1 limb
	ld	r10,8(r5)	C load s2 limb
	adde	r7,r0,r8	C add limbs with cy, set cy
	std	r7,8(r3)	C store result limb
	bdz	L(ene)		C decrement CTR and exit if done
	ldu	r8,16(r4)	C load s1 limb and update s1_ptr
	ldu	r0,16(r5)	C load s2 limb and update s2_ptr
	adde	r7,r10,r9	C add limbs with cy, set cy
	stdu	r7,16(r3)	C store result limb and update res_ptr
	bdnz	L(top)		C decrement CTR and loop back

L(eno):	adde	r7,r0,r8
	std	r7,8(r3)	C store ultimate result limb
	li	r3,0		C load cy into ...
	addze	r3,r3		C ... return value register
	blr
L(ene):	adde	r7,r10,r9
	std	r7,16(r3)
	li	r3,0		C load cy into ...
	addze	r3,r3		C ... return value register
	blr
EPILOGUE(mpn_add_n)
