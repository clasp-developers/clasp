dnl  ARM mpn_copyd.

dnl  Copyright 2003 Free Software Foundation, Inc.

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

C This runs at 3 cycles/limb in the StrongARM.

define(`rp',`r0')
define(`up',`r1')
define(`n',`r2')


ASM_START()
PROLOGUE(mpn_copyd)
	mov	r12, n, lsl #2
	sub	r12, r12, #4
	add	rp, rp, r12			C make rp point at last limb
	add	up, up, r12			C make up point at last limb

	tst	n, #1
	beq	L(skip1)
	ldr	r3, [up], #-4
	str	r3, [rp], #-4
L(skip1):
	tst	n, #2
	beq	L(skip2)
	ldmda	up!, { r3, r12 }		C load 2 limbs
	stmda	rp!, { r3, r12 }		C store 2 limbs
L(skip2):
	bics	n, n, #3
	beq	L(return)
	stmfd	sp!, { r7, r8, r9 }		C save regs on stack
L(loop):
	ldmda	up!, { r3, r8, r9, r12 }	C load 4 limbs
	ldr	r7, [rp, #-12]			C cache allocate
	subs	n, n, #4
	stmda	rp!, { r3, r8, r9, r12 }	C store 4 limbs
	bne	L(loop)
	ldmfd	sp!, { r7, r8, r9 }		C restore regs from stack
L(return):
	mov	pc, lr
EPILOGUE(mpn_copyd)
