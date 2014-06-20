dnl  PowerPC-64 mpn_submul_1 -- Multiply a limb vector with a limb and subtract
dnl  the result from a second limb vector.

dnl  Copyright 1999, 2000, 2001, 2003, 2004, 2005, 2006 Free Software
dnl  Foundation, Inc.

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

C		cycles/limb
C POWER3/PPC630:    6-18
C POWER4/PPC970:    10
C POWER5:           10.5

C INPUT PARAMETERS
define(`rp', `r3')
define(`up', `r4')
define(`n', `r5')
define(`vl', `r6')
define(`cy', `r7')

ASM_START()
PROLOGUE(mpn_submul_1)
	li	cy, 0			C cy_limb = 0

PROLOGUE(mpn_submul_1c)
	mtctr	n
	addic	r0, r0, 0
	addi	rp, rp, -8
	ALIGN(16)
L(top):
	ld	r0, 0(up)
	ld	r10, 8(rp)
	mulld	r9, r0, vl
	mulhdu	r5, r0, vl
	adde	r9, r9, cy
	addi	up, up, 8
	addze	cy, r5
	subf	r12, r9, r10
	not	r0, r10
	addc	r11, r9, r0		C inverted carry from subf
	stdu	r12, 8(rp)
	bdnz	L(top)

	addze	r3, cy
	blr
EPILOGUE(mpn_submul_1)
EPILOGUE(mpn_submul_1c)
