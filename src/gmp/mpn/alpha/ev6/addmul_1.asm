dnl  Alpha ev6 mpn_addmul_1 -- Multiply a limb vector with a limb and add the
dnl  result to a second limb vector.

dnl  Copyright 2000, 2003, 2004, 2005 Free Software Foundation, Inc.

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

C      cycles/limb
C EV4:    42
C EV5:    18
C EV6:     3.5

C  INPUT PARAMETERS
C  rp	  r16
C  up	  r17
C  n	  r18
C  vlimb  r19

dnl  This code was written in cooperation with ev6 pipeline expert Steve Root.

dnl  The stores can issue a cycle late so we have paired no-op's to 'catch'
dnl  them, so that further disturbance to the schedule is damped.

dnl  We couldn't pair the loads, because the entangled schedule of the carry's
dnl  has to happen on one side {0} of the machine.

dnl  This is a great schedule for the d_cache, a poor schedule for the b_cache.
dnl  The lockup on U0 means that any stall can't be recovered from.  Consider a
dnl  ldq in L1, say that load gets stalled because it collides with a fill from
dnl  the b_cache.  On the next cycle, this load gets priority.  If first looks
dnl  at L0, and goes there.  The instruction we intended for L0 gets to look at
dnl  L1, which is NOT where we want it.  It either stalls 1, because it can't
dnl  go in L0, or goes there, and causes a further instruction to stall.

dnl  So for b_cache, we're likely going to want to put one or more cycles back
dnl  into the code! And, of course, put in lds prefetch for the rp[] operand.
dnl  At a place where we have an mt followed by a bookkeeping, put the
dnl  bookkeeping in upper, and the prefetch into lower.

dnl  Note, the ldq's and stq's are at the end of the quadpacks.  Note, we'd
dnl  like not to have an ldq or an stq to preceded a conditional branch in a
dnl  quadpack.  The conditional branch moves the retire pointer one cycle
dnl  later.


ASM_START()
PROLOGUE(mpn_addmul_1)
	ldq	r3,	0(r17)		C
	and	r18,	7,	r20	C
	lda	r18,	-9(r18)		C
	cmpeq	r20,	1,	r21	C
	beq	r21,	$L1		C

$1mod8:	ldq	r5,	0(r16)		C
	mulq	r19,	r3,	r7	C
	umulh	r19,	r3,	r8	C
	addq	r5,	r7,	r23	C
	cmpult	r23,	r7,	r20	C
	addq	r8,	r20,	r0	C
	stq	r23,	0(r16)		C
	bge	r18,	$ent1		C
	ret	r31,	(r26),	1	C

$L1:	lda	r8,	0(r31)		C zero carry reg
	lda	r24,	0(r31)		C zero carry reg
	cmpeq	r20,	2,	r21	C
	bne	r21,	$2mod8		C
	cmpeq	r20,	3,	r21	C
	bne	r21,	$3mod8		C
	cmpeq	r20,	4,	r21	C
	bne	r21,	$4mod8		C
	cmpeq	r20,	5,	r21	C
	bne	r21,	$5mod8		C
	cmpeq	r20,	6,	r21	C
	bne	r21,	$6mod8		C
	cmpeq	r20,	7,	r21	C
	beq	r21,	$0mod8		C

$7mod8:	ldq	r5,	0(r16)		C
	lda	r17,	8(r17)		C
	mulq	r19,	r3,	r7	C
	umulh	r19,	r3,	r24	C
	addq	r5,	r7,	r23	C
	cmpult	r23,	r7,	r20	C
	addq	r24,	r20,	r24	C
	stq	r23,	0(r16)		C
	lda	r16,	8(r16)		C
	ldq	r3,	0(r17)		C
$6mod8:	ldq	r1,	8(r17)		C
	mulq	r19,	r3,	r25	C
	umulh	r19,	r3,	r3	C
	mulq	r19,	r1,	r28	C
	ldq	r0,	16(r17)		C
	ldq	r4,	0(r16)		C
	umulh	r19,	r1,	r8	C
	ldq	r1,	24(r17)		C
	lda	r17,	48(r17)		C L1 bookkeeping
	mulq	r19,	r0,	r2	C
	ldq	r5,	8(r16)		C
	lda	r16,	-32(r16)	C L1 bookkeeping
	umulh	r19,	r0,	r6	C
	addq	r4,	r25,	r4	C lo + acc
	mulq	r19,	r1,	r7	C
	br	r31,	$ent6		C

$ent1:	lda	r17,	8(r17)		C
	lda	r16,	8(r16)		C
	lda	r8,	0(r0)		C
	ldq	r3,	0(r17)		C
$0mod8:	ldq	r1,	8(r17)		C
	mulq	r19,	r3,	r2	C
	umulh	r19,	r3,	r6	C
	mulq	r19,	r1,	r7	C
	ldq	r0,	16(r17)		C
	ldq	r4,	0(r16)		C
	umulh	r19,	r1,	r24	C
	ldq	r1,	24(r17)		C
	mulq	r19,	r0,	r25	C
	ldq	r5,	8(r16)		C
	umulh	r19,	r0,	r3	C
	addq	r4,	r2,	r4	C lo + acc
	mulq	r19,	r1,	r28	C
	lda	r16,	-16(r16)	C
	br	r31,	$ent0		C

$3mod8:	ldq	r5,	0(r16)		C
	lda	r17,	8(r17)		C
	mulq	r19,	r3,	r7	C
	umulh	r19,	r3,	r8	C
	addq	r5,	r7,	r23	C
	cmpult	r23,	r7,	r20	C
	addq	r8,	r20,	r24	C
	stq	r23,	0(r16)		C
	lda	r16,	8(r16)		C
	ldq	r3,	0(r17)		C
$2mod8:	ldq	r1,	8(r17)		C
	mulq	r19,	r3,	r25	C
	umulh	r19,	r3,	r3	C
	mulq	r19,	r1,	r28	C
	ble	r18,	$n23		C
	ldq	r0,	16(r17)		C
	ldq	r4,	0(r16)		C
	umulh	r19,	r1,	r8	C
	ldq	r1,	24(r17)		C
	lda	r17,	16(r17)		C L1 bookkeeping
	mulq	r19,	r0,	r2	C
	ldq	r5,	8(r16)		C
	lda	r16,	0(r16)		C L1 bookkeeping
	umulh	r19,	r0,	r6	C
	addq	r4,	r25,	r4	C lo + acc
	mulq	r19,	r1,	r7	C
	br	r31,	$ent2		C

$5mod8:	ldq	r5,	0(r16)		C
	lda	r17,	8(r17)		C
	mulq	r19,	r3,	r7	C
	umulh	r19,	r3,	r24	C
	addq	r5,	r7,	r23	C
	cmpult	r23,	r7,	r20	C
	addq	r24,	r20,	r8	C
	stq	r23,	0(r16)		C
	lda	r16,	8(r16)		C
	ldq	r3,	0(r17)		C
$4mod8:	ldq	r1,	8(r17)		C
	mulq	r19,	r3,	r2	C
	umulh	r19,	r3,	r6	C
	mulq	r19,	r1,	r7	C
	ldq	r0,	16(r17)		C
	ldq	r4,	0(r16)		C
	umulh	r19,	r1,	r24	C
	ldq	r1,	24(r17)		C
	lda	r17,	32(r17)		C L1 bookkeeping
	mulq	r19,	r0,	r25	C
	ldq	r5,	8(r16)		C
	lda	r16,	16(r16)		C L1 bookkeeping
	umulh	r19,	r0,	r3	C
	addq	r4,	r2,	r4	C lo + acc
	mulq	r19,	r1,	r28	C
	cmpult	r4,	r2,	r20	C L0 lo add => carry
	addq	r4,	r8,	r22	C U0 hi add => answer
	ble	r18,	$Lend		C
ALIGN(16)
$Loop:
	bis	r31,	r31,	r31	C U1 mt
	cmpult	r22,	r8,	r21	C L0 hi add => carry
	addq	r6,	r20,	r6	C U0 hi mul + carry
	ldq	r0,	0(r17)		C

	bis	r31,	r31,	r31	C U1 mt
	addq	r5,	r7,	r23	C L0 lo + acc
	addq	r6,	r21,	r6	C U0 hi mul + carry
	ldq	r4,	0(r16)		C L1

	umulh	r19,	r1,	r8	C U1
	cmpult	r23,	r7,	r20	C L0 lo add => carry
	addq	r23,	r6,	r23	C U0 hi add => answer
	ldq	r1,	8(r17)		C L1

	mulq	r19,	r0,	r2	C U1
	cmpult	r23,	r6,	r21	C L0 hi add => carry
	addq	r24,	r20,	r24	C U0 hi mul + carry
	ldq	r5,	8(r16)		C L1

	umulh	r19,	r0,	r6	C U1
	addq	r4,	r25,	r4	C U0 lo + acc
	stq	r22,	-16(r16)	C L0
	stq	r23,	-8(r16)		C L1

	bis	r31,	r31,	r31	C L0 st slosh
	mulq	r19,	r1,	r7	C U1
	bis	r31,	r31,	r31	C L1 st slosh
	addq	r24,	r21,	r24	C U0 hi mul + carry
$ent2:
	cmpult	r4,	r25,	r20	C L0 lo add => carry
	bis	r31,	r31,	r31	C U1 mt
	lda	r18,	-8(r18)		C L1 bookkeeping
	addq	r4,	r24,	r22	C U0 hi add => answer

	bis	r31,	r31,	r31	C U1 mt
	cmpult	r22,	r24,	r21	C L0 hi add => carry
	addq	r3,	r20,	r3	C U0 hi mul + carry
	ldq	r0,	16(r17)		C L1

	bis	r31,	r31,	r31	C U1 mt
	addq	r5,	r28,	r23	C L0 lo + acc
	addq	r3,	r21,	r3	C U0 hi mul + carry
	ldq	r4,	16(r16)		C L1

	umulh	r19,	r1,	r24	C U1
	cmpult	r23,	r28,	r20	C L0 lo add => carry
	addq	r23,	r3,	r23	C U0 hi add => answer
	ldq	r1,	24(r17)		C L1

	mulq	r19,	r0,	r25	C U1
	cmpult	r23,	r3,	r21	C L0 hi add => carry
	addq	r8,	r20,	r8	C U0 hi mul + carry
	ldq	r5,	24(r16)		C L1

	umulh	r19,	r0,	r3	C U1
	addq	r4,	r2,	r4	C U0 lo + acc
	stq	r22,	0(r16)		C L0
	stq	r23,	8(r16)		C L1

	bis	r31,	r31,	r31	C L0 st slosh
	mulq	r19,	r1,	r28	C U1
	bis	r31,	r31,	r31	C L1 st slosh
	addq	r8,	r21,	r8	C L0 hi mul + carry
$ent0:
	cmpult	r4,	r2,	r20	C L0 lo add => carry
	bis	r31,	r31,	r31	C U1 mt
	lda	r17,	64(r17)		C L1 bookkeeping
	addq	r4,	r8,	r22	C U0 hi add => answer

	bis	r31,	r31,	r31	C U1 mt
	cmpult	r22,	r8,	r21	C L0 hi add => carry
	addq	r6,	r20,	r6	C U0 hi mul + carry
	ldq	r0,	-32(r17)	C L1

	bis	r31,	r31,	r31	C U1 mt
	addq	r5,	r7,	r23	C L0 lo + acc
	addq	r6,	r21,	r6	C U0 hi mul + carry
	ldq	r4,	32(r16)		C L1

	umulh	r19,	r1,	r8	C U1
	cmpult	r23,	r7,	r20	C L0 lo add => carry
	addq	r23,	r6,	r23	C U0 hi add => answer
	ldq	r1,	-24(r17)	C L1

	mulq	r19,	r0,	r2	C U1
	cmpult	r23,	r6,	r21	C L0 hi add => carry
	addq	r24,	r20,	r24	C U0 hi mul + carry
	ldq	r5,	40(r16)		C L1

	umulh	r19,	r0,	r6	C U1
	addq	r4,	r25,	r4	C U0 lo + acc
	stq	r22,	16(r16)		C L0
	stq	r23,	24(r16)		C L1

	bis	r31,	r31,	r31	C L0 st slosh
	mulq	r19,	r1,	r7	C U1
	bis	r31,	r31,	r31	C L1 st slosh
	addq	r24,	r21,	r24	C U0 hi mul + carry
$ent6:
	cmpult	r4,	r25,	r20	C L0 lo add => carry
	bis	r31,	r31,	r31	C U1 mt
	lda	r16,	64(r16)		C L1 bookkeeping
	addq	r4,	r24,	r22	C U0 hi add => answer

	bis	r31,	r31,	r31	C U1 mt
	cmpult	r22,	r24,	r21	C L0 hi add => carry
	addq	r3,	r20,	r3	C U0 hi mul + carry
	ldq	r0,	-16(r17)	C L1

	bis	r31,	r31,	r31	C U1 mt
	addq	r5,	r28,	r23	C L0 lo + acc
	addq	r3,	r21,	r3	C U0 hi mul + carry
	ldq	r4,	-16(r16)	C L1

	umulh	r19,	r1,	r24	C U1
	cmpult	r23,	r28,	r20	C L0 lo add => carry
	addq	r23,	r3,	r23	C U0 hi add => answer
	ldq	r1,	-8(r17)		C L1

	mulq	r19,	r0,	r25	C U1
	cmpult	r23,	r3,	r21	C L0 hi add => carry
	addq	r8,	r20,	r8	C U0 hi mul + carry
	ldq	r5,	-8(r16)		C L1

	umulh	r19,	r0,	r3	C U1
	addq	r4,	r2,	r4	C L0 lo + acc
	stq	r22,	-32(r16)	C L0
	stq	r23,	-24(r16)	C L1

	bis	r31,	r31,	r31	C L0 st slosh
	mulq	r19,	r1,	r28	C U1
	bis	r31,	r31,	r31	C L1 st slosh
	addq	r8,	r21,	r8	C U0 hi mul + carry

	cmpult	r4,	r2,	r20	C L0 lo add => carry
	addq	r4,	r8,	r22	C U0 hi add => answer
	ldl	r31,	256(r17)	C prefetch up[]
	bgt	r18,	$Loop		C U1 bookkeeping

$Lend:	cmpult	r22,	r8,	r21	C
	addq	r6,	r20,	r6	C
	addq	r5,	r7,	r23	C
	addq	r6,	r21,	r6	C
	ldq	r4,	0(r16)		C
	umulh	r19,	r1,	r8	C
	cmpult	r23,	r7,	r20	C
	addq	r23,	r6,	r23	C
	cmpult	r23,	r6,	r21	C
	addq	r24,	r20,	r24	C
	ldq	r5,	8(r16)		C
	addq	r4,	r25,	r4	C
	stq	r22,	-16(r16)	C
	stq	r23,	-8(r16)		C
	addq	r24,	r21,	r24	C
	cmpult	r4,	r25,	r20	C
	addq	r4,	r24,	r22	C
	cmpult	r22,	r24,	r21	C
	addq	r3,	r20,	r3	C
	addq	r5,	r28,	r23	C
	addq	r3,	r21,	r3	C
	cmpult	r23,	r28,	r20	C
	addq	r23,	r3,	r23	C
	cmpult	r23,	r3,	r21	C
	addq	r8,	r20,	r8	C
	stq	r22,	0(r16)		C
	stq	r23,	8(r16)		C
	addq	r8,	r21,	r0	C
	ret	r31,	(r26),	1	C

$n23:	ldq	r4,	0(r16)		C
	ldq	r5,	8(r16)		C
	umulh	r19,	r1,	r8	C
	addq	r4,	r25,	r4	C
	cmpult	r4,	r25,	r20	C
	addq	r4,	r24,	r22	C
	cmpult	r22,	r24,	r21	C
	addq	r3,	r20,	r3	C
	addq	r5,	r28,	r23	C
	addq	r3,	r21,	r3	C
	cmpult	r23,	r28,	r20	C
	addq	r23,	r3,	r23	C
	cmpult	r23,	r3,	r21	C
	addq	r8,	r20,	r8	C
	stq	r22,	0(r16)		C
	stq	r23,	8(r16)		C
	addq	r8,	r21,	r0	C
	ret	r31,	(r26),	1	C
EPILOGUE()
ASM_END()
