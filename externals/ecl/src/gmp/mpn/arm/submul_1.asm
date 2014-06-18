dnl  ARM mpn_submul_1 -- Multiply a limb vector with a limb and subtract the
dnl  result from a second limb vector.

dnl  Copyright 1998, 2000, 2001, 2003 Free Software Foundation, Inc.

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
PROLOGUE(mpn_submul_1)
	stmfd	sp!, { r4-r6, lr }
	subs	r4, r0, r0		C clear r4, set cy
	tst	n, #1
	beq	L(skip1)
	ldr	ul, [up], #4
	ldr	rl, [rp, #0]
	umull	r5, r4, ul, vl
	subs	r, rl, r5
	str	r, [rp], #4
L(skip1):
	tst	n, #2
	beq	L(skip2)
	ldr	ul, [up], #4
	ldr	rl, [rp, #0]
	mov	r5, #0
	umlal	r4, r5, ul, vl
	ldr	ul, [up], #4
	sbcs	r, rl, r4
	ldr	rl, [rp, #4]
	mov	r4, #0
	umlal	r5, r4, ul, vl
	str	r, [rp], #4
	sbcs	r, rl, r5
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
	sbcs	r, rl, r5
	ldr	rl, [rp, #4]
	mov	r5, #0
	umlal	r4, r5, ul, vl
	str	r, [rp], #4
L(in):	ldr	ul, [up], #4
	sbcs	r, rl, r4
	ldr	rl, [rp, #4]
	mov	r4, #0
	umlal	r5, r4, ul, vl
	str	r, [rp], #4
	ldr	ul, [up], #4
	sbcs	r, rl, r5
	ldr	rl, [rp, #4]
	mov	r5, #0
	umlal	r4, r5, ul, vl
	str	r, [rp], #4
	ldr	ul, [up], #4
	sbcs	r, rl, r4
	ldr	rl, [rp, #4]
	mov	r4, #0
	umlal	r5, r4, ul, vl
	str	r, [rp], #4
	sub	n, n, #4
	bics	r, n, #3
	bne	L(loop)

	sbcs	r, rl, r5
	str	r, [rp], #4
L(return):
	sbc	r0, r0, r0
	sub	r0, r4, r0
	ldmfd	sp!, { r4-r6, pc }
EPILOGUE(mpn_submul_1)
