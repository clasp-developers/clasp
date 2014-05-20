dnl  ARM mpn_addmul_1 -- Multiply a limb vector with a limb and add the result
dnl  to a second limb vector.

dnl  Copyright 1998, 2000, 2001, 2003 Free Software Foundation, Inc.

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

C            cycles/limb
C StrongARM:  7.75-9.75  (dependent on vl value)
C XScale:        8-9     (dependent on vl value, estimated)

define(`rp',`r0')
define(`up',`r1')
define(`n',`r2')
define(`vl',`r3')
define(`rl',`r12')
define(`ul',`r6')
define(`r',`lr')


ASM_START()
PROLOGUE(mpn_addmul_1)
	stmfd	sp!, { r4-r6, lr }
	mov	r4, #0			C clear r4
	adds	r0, r0, #0		C clear cy
	tst	n, #1
	beq	L(skip1)
	ldr	ul, [up], #4
	ldr	rl, [rp, #0]
	umull	r5, r4, ul, vl
	adds	r, rl, r5
	str	r, [rp], #4
L(skip1):
	tst	n, #2
	beq	L(skip2)
	ldr	ul, [up], #4
	ldr	rl, [rp, #0]
	mov	r5, #0
	umlal	r4, r5, ul, vl
	ldr	ul, [up], #4
	adcs	r, rl, r4
	ldr	rl, [rp, #4]
	mov	r4, #0
	umlal	r5, r4, ul, vl
	str	r, [rp], #4
	adcs	r, rl, r5
	str	r, [rp], #4
L(skip2):
	bics	r, n, #3
	beq	L(return)

	ldr	ul, [up], #4
	ldr	rl, [rp, #0]
	mov	r5, #0
	umlal	r4, r5, ul, vl
	b	L(in)

L(loop):
	ldr	ul, [up], #4
	adcs	r, rl, r5
	ldr	rl, [rp, #4]
	mov	r5, #0
	umlal	r4, r5, ul, vl
	str	r, [rp], #4
L(in):	ldr	ul, [up], #4
	adcs	r, rl, r4
	ldr	rl, [rp, #4]
	mov	r4, #0
	umlal	r5, r4, ul, vl
	str	r, [rp], #4
	ldr	ul, [up], #4
	adcs	r, rl, r5
	ldr	rl, [rp, #4]
	mov	r5, #0
	umlal	r4, r5, ul, vl
	str	r, [rp], #4
	ldr	ul, [up], #4
	adcs	r, rl, r4
	ldr	rl, [rp, #4]
	mov	r4, #0
	umlal	r5, r4, ul, vl
	str	r, [rp], #4
	sub	n, n, #4
	bics	r, n, #3
	bne	L(loop)

	adcs	r, rl, r5
	str	r, [rp], #4
L(return):
	adc	r0, r4, #0
	ldmfd	sp!, { r4-r6, pc }
EPILOGUE(mpn_addmul_1)
