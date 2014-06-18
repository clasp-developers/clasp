dnl  PowerPC-32 mpn_add_n -- add limb vectors.

dnl  Copyright 2002, 2005 Free Software Foundation, Inc.

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

C                cycles/limb
C 603e:            ?
C 604e:            3.25
C 75x (G3):        3.5
C 7400,7410 (G4):  3.5
C 744x,745x (G4+): 4.25
C power4/ppc970:   2.0
C power5:          2.5

C INPUT PARAMETERS
C rp	r3
C s1p	r4
C s2p	r5
C n	r6
C cy	r7

ASM_START()
PROLOGUE(mpn_add_nc)
	addic	r0,r7,-1	C set hw cy from cy argument
	cmpwi	cr0,r6,15	C more than 15 limbs?
	ble	L(com)		C branch if <= 15 limbs
	b	L(BIG)
EPILOGUE(mpn_add_nc)
PROLOGUE(mpn_add_n)
	addic	r0,r0,0		C clear hw cy
	cmpwi	cr0,r6,15	C more than 15 limbs?
	bgt	L(BIG)		C branch if > 15 limbs

L(com):	mtctr	r6		C copy size into CTR
	addi	r3,r3,-4	C offset rp, it's updated before it's used
	lwz	r0,0(r4)	C load s1 limb
	lwz	r7,0(r5)	C load s2 limb
	adde	r10,r7,r0
	bdz	L(endS)
L(loopS):
	lwzu	r0,4(r4)	C load s1 limb
	lwzu	r7,4(r5)	C load s2 limb
	stwu	r10,4(r3)	C store result limb
	adde	r10,r7,r0
	bdnz	L(loopS)
L(endS):
	stwu	r10,4(r3)	C store result limb
	li	r3,0
	addze	r3,r3
	blr

L(BIG):
	stmw	r30,-8(r1)	C should avoid this for small sizes!
	andi.	r12,r6,3
	mtctr	r12		C copy size into CTR
	addi	r4,r4,-4
	addi	r5,r5,-4
	addi	r3,r3,-4
	beq	L(multiple_of_4)
	lwzu	r0,4(r4)	C load s1 limb
	lwzu	r7,4(r5)	C load s2 limb
	adde	r10,r7,r0
	bdz	L(end0)
L(loop0):
	lwzu	r0,4(r4)	C load s1 limb
	lwzu	r7,4(r5)	C load s2 limb
	stwu	r10,4(r3)	C store result limb
	adde	r10,r7,r0
	bdnz	L(loop0)
L(end0):
	stwu	r10,4(r3)	C store result limb
L(multiple_of_4):
	srwi	r6,r6,2
	mtctr	r6		C copy size into CTR

	lwz	r0,4(r4)	C load s1 limb
	lwz	r7,4(r5)	C load s2 limb
	lwz	r8,8(r4)	C load s1 limb
	lwz	r9,8(r5)	C load s2 limb
	lwz	r10,12(r4)	C load s1 limb
	lwz	r11,12(r5)	C load s2 limb
	lwzu	r12,16(r4)	C load s1 limb
	adde	r31,r7,r0	C add limbs with cy, set cy
	lwzu	r6,16(r5)	C load s2 limb
	bdz	L(enda)

L(loop):
	lwz	r0,4(r4)	C load s1 limb
	adde	r30,r9,r8	C add limbs with cy, set cy
	lwz	r7,4(r5)	C load s2 limb
	stw	r31,4(r3)	C store result limb
	lwz	r8,8(r4)	C load s1 limb
	adde	r31,r11,r10	C add limbs with cy, set cy
	lwz	r9,8(r5)	C load s2 limb
	stw	r30,8(r3)	C store result limb
	lwz	r10,12(r4)	C load s1 limb
	adde	r30,r6,r12	C add limbs with cy, set cy
	lwz	r11,12(r5)	C load s2 limb
	stw	r31,12(r3)	C store result limb
	lwzu	r12,16(r4)	C load s1 limb
	adde	r31,r7,r0	C add limbs with cy, set cy
	stwu	r30,16(r3)	C store result limb
	lwzu	r6,16(r5)	C load s2 limb
	bdnz	L(loop)		C decrement CTR and loop back
L(enda):
	adde	r30,r9,r8	C add limbs with cy, set cy
	stw	r31,4(r3)	C store result limb
	adde	r31,r11,r10	C add limbs with cy, set cy
	stw	r30,8(r3)	C store result limb
	adde	r30,r6,r12	C add limbs with cy, set cy
	stw	r31,12(r3)	C store result limb
	stw	r30,16(r3)	C store result limb
L(end):
	li	r3,0
	addze	r3,r3
	lmw	r30,-8(r1)
	blr
EPILOGUE(mpn_add_n)
