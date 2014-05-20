dnl  ARM mpn_copyi.

dnl  Copyright 2003 Free Software Foundation, Inc.

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

C This runs at 3 cycles/limb in the StrongARM.

define(`rp',`r0')
define(`up',`r1')
define(`n',`r2')


ASM_START()
PROLOGUE(mpn_copyi)
	tst	n, #1
	beq	L(skip1)
	ldr	r3, [up], #4
	str	r3, [rp], #4
L(skip1):
	tst	n, #2
	beq	L(skip2)
	ldmia	up!, { r3, r12 }		C load 2 limbs
	stmia	rp!, { r3, r12 }		C store 2 limbs
L(skip2):
	bics	n, n, #3
	beq	L(return)
	stmfd	sp!, { r7, r8, r9 }		C save regs on stack
L(loop):
	ldmia	up!, { r3, r8, r9, r12 }	C load 4 limbs
	ldr	r7, [rp, #12]			C cache allocate
	subs	n, n, #4
	stmia	rp!, { r3, r8, r9, r12 }	C store 4 limbs
	bne	L(loop)
	ldmfd	sp!, { r7, r8, r9 }		C restore regs from stack
L(return):
	mov	pc, lr
EPILOGUE(mpn_copyi)
