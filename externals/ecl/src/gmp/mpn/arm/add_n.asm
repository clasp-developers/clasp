dnl  ARM mpn_add_n -- Add two limb vectors of the same length > 0 and store sum
dnl  in a third limb vector.
dnl  Contributed by Robert Harley.

dnl  Copyright 1997, 2000, 2001 Free Software Foundation, Inc.

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

C This code runs at 5 cycles/limb.

define(`rp',`r0')
define(`up',`r1')
define(`vp',`r2')
define(`n',`r3')


ASM_START()
PROLOGUE(mpn_add_n)
	stmfd	sp!, { r8, r9, lr }
	movs	n, n, lsr #1
	bcc	L(skip1)
	ldr	r12, [up], #4
	ldr	lr, [vp], #4
	adds	r12, r12, lr
	str	r12, [rp], #4
L(skip1):
	tst	n, #1
	beq	L(skip2)
	ldmia	up!, { r8, r9 }
	ldmia	vp!, { r12, lr }
	adcs	r8, r8, r12
	adcs	r9, r9, lr
	stmia	rp!, { r8, r9 }
L(skip2):
	bics	n, n, #1
	beq	L(return)
	stmfd	sp!, { r4, r5, r6, r7 }
L(add_n_loop):
	ldmia	up!, { r4, r5, r6, r7 }
	ldmia	vp!, { r8, r9, r12, lr }
	adcs	r4, r4, r8
	ldr	r8, [rp, #12]			C cache allocate
	adcs	r5, r5, r9
	adcs	r6, r6, r12
	adcs	r7, r7, lr
	stmia	rp!, { r4, r5, r6, r7 }
	sub	n, n, #2
	teq	n, #0
	bne	L(add_n_loop)
	ldmfd	sp!, { r4, r5, r6, r7 }
L(return):
	adc	r0, n, #0
	ldmfd	sp!, { r8, r9, pc }
EPILOGUE(mpn_add_n)
