dnl  PowerPC-64 mpn_sub_n -- Subtract two limb vectors of the same length > 0
dnl  and store difference in a third limb vector.

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
PROLOGUE(mpn_sub_n)
	mtctr	r6		C copy size into CTR
	addic	r0,r6,-1	C set cy
	ld	r8,0(r4)	C load least significant s1 limb
	ld	r0,0(r5)	C load least significant s2 limb
	addi	r3,r3,-8	C offset res_ptr, it's updated before it's used
	bdz	L(eno)		C If done, skip loop
L(top):	ld	r9,8(r4)	C load s1 limb
	ld	r10,8(r5)	C load s2 limb
	subfe	r7,r0,r8	C subtract limbs with cy, set cy
	std	r7,8(r3)	C store result limb
	bdz	L(ene)		C decrement CTR and exit if done
	ldu	r8,16(r4)	C load s1 limb and update s1_ptr
	ldu	r0,16(r5)	C load s2 limb and update s2_ptr
	subfe	r7,r10,r9	C subtract limbs with cy, set cy
	stdu	r7,16(r3)	C store result limb and update res_ptr
	bdnz	L(top)		C decrement CTR and loop back

L(eno):	subfe	r7,r0,r8
	std	r7,8(r3)	C store ultimate result limb
	subfe	r3,r0,r0	C load !cy into ...
	subfic	r3,r3,0		C ... return value register
	blr
L(ene):	subfe	r7,r10,r9
	std	r7,16(r3)
	subfe	r3,r0,r0	C load !cy into ...
	subfic	r3,r3,0		C ... return value register
	blr
EPILOGUE(mpn_sub_n)
