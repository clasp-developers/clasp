dnl  PowerPC-64 mpn_sqr_diagonal.

dnl  Copyright 2001, 2002, 2003, 2005, 2006 Free Software Foundation, Inc.

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

C		cycles/limb
C POWER3/PPC630:    18
C POWER4/PPC970:     8

C INPUT PARAMETERS
C rp	r3
C up	r4
C n	r5

ASM_START()
PROLOGUE(mpn_sqr_diagonal)
ifdef(`HAVE_ABI_mode32',
`	rldicl	r5, r5, 0, 32')		C zero extend n
	mtctr	r5
	ld	r0, 0(r4)
	bdz	L(end)
	ALIGN(16)

L(top):	mulld	r5, r0, r0
	mulhdu	r6, r0, r0
	ld	r0, 8(r4)
	addi	r4, r4, 8
	std	r5, 0(r3)
	std	r6, 8(r3)
	addi	r3, r3, 16
	bdnz	L(top)

L(end):	mulld	r5, r0, r0
	mulhdu	r6, r0, r0
	std	r5, 0(r3)
	std	r6, 8(r3)

	blr
EPILOGUE()
