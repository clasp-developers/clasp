dnl  PowerPC-64 mpn_addmul_1 -- Multiply a limb vector with a limb and add
dnl  the result to a second limb vector.

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
C POWER4/PPC970:     8
C POWER5:            8

C TODO
C  * Reduce the number of registers used.  Some mul destination registers could
C    be coalesced.
C  * Delay std for preserving registers, and suppress them for n=1.
C  * Write faster feed-in code.  If nothing else, avoid one or two up updates.

C INPUT PARAMETERS
define(`rp', `r3')
define(`up', `r4')
define(`n', `r5')
define(`vl', `r6')

ASM_START()
PROLOGUE(mpn_addmul_1)
	std	r31, -8(r1)
	std	r30, -16(r1)
	std	r29, -24(r1)
	std	r28, -32(r1)
	std	r27, -40(r1)
	std	r26, -48(r1)

	rldicl.	r0, n, 0,62	C r0 = n & 3, set cr0
	cmpdi	cr6, r0, 2
	addi	n, n, 3		C compute count...
	srdi	n, n, 2		C ...for ctr
	mtctr	n		C copy count into ctr
	beq	cr0, L(b00)
	blt	cr6, L(b01)
	beq	cr6, L(b10)

L(b11):	ld	r26, 0(up)
	ld	r28, 0(rp)
	addi	up, up, 8
	nop
	mulld	r0, r26, r6
	mulhdu	r12, r26, r6
	addc	r0, r0, r28
	std	r0, 0(rp)
	addi	rp, rp, 8
	b	L(fic)

L(b00):	ld	r26, 0(up)
	ld	r27, 8(up)
	ld	r28, 0(rp)
	ld	r29, 8(rp)
	addi	up, up, 16
	nop
	mulld	r0, r26, r6
	mulhdu	r5, r26, r6
	mulld	r7, r27, r6
	mulhdu	r8, r27, r6
	addc	r7, r7, r5
	addze	r12, r8
	addc	r0, r0, r28
	std	r0, 0(rp)
	adde	r7, r7, r29
	std	r7, 8(rp)
	addi	rp, rp, 16
	b	L(fic)

L(b01):	bdnz	L(gt1)
	ld	r26, 0(up)
	ld	r28, 0(rp)
	mulld	r0, r26, r6
	mulhdu	r8, r26, r6
	addc	r0, r0, r28
	std	r0, 0(rp)
	b	L(ret)
L(gt1):	ld	r26, 0(up)
	ld	r27, 8(up)
	mulld	r0, r26, r6
	mulhdu	r5, r26, r6
	ld	r26, 16(up)
	ld	r28, 0(rp)
	mulld	r7, r27, r6
	mulhdu	r8, r27, r6
	ld	r29, 8(rp)
	ld	r30, 16(rp)
	mulld	r9, r26, r6
	mulhdu	r10, r26, r6
	addc	r7, r7, r5
	adde	r9, r9, r8
	addze	r12, r10
	addc	r0, r0, r28
	std	r0, 0(rp)
	adde	r7, r7, r29
	std	r7, 8(rp)
	adde	r9, r9, r30
	std	r9, 16(rp)
	addi	up, up, 24
	addi	rp, rp, 24
	b	L(fic)

L(b10):	addic	r0, r0, 0
	li	r12, 0		C cy_limb = 0
L(fic):	ld	r26, 0(up)
	ld	r27, 8(up)
	addi	up, up, 16
	bdz	L(end)
				C registers dying
L(top):	mulld	r0, r26, r6	C
	mulhdu	r5, r26, r6	C 26
	ld	r26, 0(up)	C
	ld	r28, 0(rp)	C
	mulld	r7, r27, r6	C
	mulhdu	r8, r27, r6	C 27
	ld	r27, 8(up)	C
	ld	r29, 8(rp)	C
	adde	r0, r0, r12	C 0 12
	adde	r7, r7, r5	C 5 7
	mulld	r9, r26, r6	C
	mulhdu	r10, r26, r6	C 26
	ld	r26, 16(up)	C
	ld	r30, 16(rp)	C
	mulld	r11, r27, r6	C
	mulhdu	r12, r27, r6	C 27
	ld	r27, 24(up)	C
	ld	r31, 24(rp)	C
	adde	r9, r9, r8	C 8 9
	adde	r11, r11, r10	C 10 11
	addze	r12, r12	C 12
	addc	r0, r0, r28	C 0 28
	std	r0, 0(rp)	C 0
	adde	r7, r7, r29	C 7 29
	std	r7, 8(rp)	C 7
	adde	r9, r9, r30	C 9 30
	std	r9, 16(rp)	C 9
	adde	r11, r11, r31	C 11 31
	std	r11, 24(rp)	C 11
	addi	up, up, 32	C
	addi	rp, rp, 32	C
	bdnz	L(top)		C

L(end):	mulld	r0, r26, r6
	mulhdu	r5, r26, r6
	ld	r28, 0(rp)
	nop
	mulld	r7, r27, r6
	mulhdu	r8, r27, r6
	ld	r29, 8(rp)
	nop
	adde	r0, r0, r12
	adde	r7, r7, r5
	addze	r8, r8
	addc	r0, r0, r28
	std	r0, 0(rp)
	adde	r7, r7, r29
	std	r7, 8(rp)
L(ret):	addze	r3, r8
	ld	r31, -8(r1)
	ld	r30, -16(r1)
	ld	r29, -24(r1)
	ld	r28, -32(r1)
	ld	r27, -40(r1)
	ld	r26, -48(r1)
	blr
EPILOGUE()
