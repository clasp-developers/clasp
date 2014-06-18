dnl  PowerPC-32/VMX and PowerPC-64/VMX mpn_popcount.

dnl  Copyright 2006 Free Software Foundation, Inc.

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

C                   cycles/limb
C 7400,7410 (G4):       2.75
C 744x,745x (G4+):      2.25
C 970 (G5):             5.3

C STATUS
C  * Works for all sizes and alignments.

C TODO
C  * Tune the awkward huge n outer loop code.
C  * Two lvx, two vperm, and two vxor could make us a similar hamdist.
C  * For the 970, a combined VMX+intop approach might be best.
C  * Compress cnsts table in 64-bit mode, only half the values are needed.

define(`GMP_LIMB_BYTES', eval(GMP_LIMB_BITS/8))
define(`LIMBS_PER_VR',  eval(16/GMP_LIMB_BYTES))
define(`LIMBS_PER_2VR', eval(32/GMP_LIMB_BYTES))

define(`OPERATION_popcount')

ifdef(`OPERATION_popcount',`
  define(`func',`mpn_popcount')
  define(`up',		`r3')
  define(`n',		`r4')
  define(`HAM',		`dnl')
')
ifdef(`OPERATION_hamdist',`
  define(`func',`mpn_hamdist')
  define(`up',		`r3')
  define(`vp',		`r4')
  define(`n',		`r5')
  define(`HAM',		`$1')
')

define(`x01010101',`v2')
define(`x00110011',`v7')
define(`x00001111',`v10')
define(`cnt1',`v11')
define(`cnt2',`v12')
define(`cnt4',`v13')

ifelse(GMP_LIMB_BITS,32,`
	define(`LIMB32',`	$1')
	define(`LIMB64',`')
',`
	define(`LIMB32',`')
	define(`LIMB64',`	$1')
')

C The inner loop handles up to 2^34 bits, i.e., 2^31 64-limbs, due to overflow
C in vsum4ubs.  For large operands, we work in chunks, of size LIMBS_PER_CHUNK.
define(`LIMBS_PER_CHUNK', 0x1000)
define(`LIMBS_CHUNK_THRES', 0x1001)

ASM_START()
PROLOGUE(mpn_popcount)
	mfspr	r10, 256
	oris	r0, r10, 0xfffc		C Set VRSAVE bit 0-13
	mtspr	256, r0

ifdef(`HAVE_ABI_mode32',
`	rldicl	n, n, 0, 32')		C zero extend n

C Load various constants into vector registers
	LEA(	r11, cnsts)
	li	r12, 16
	vspltisb cnt1, 1		C 0x0101...01 used as shift count
	vspltisb cnt2, 2		C 0x0202...02 used as shift count
	vspltisb cnt4, 4		C 0x0404...04 used as shift count
	lvx	x01010101, 0, r11	C 0x3333...33
	lvx	x00110011, r12, r11	C 0x5555...55
	vspltisb x00001111, 15		C 0x0f0f...0f

LIMB64(`lis	r0, LIMBS_CHUNK_THRES	')
LIMB64(`cmpd	cr7, n, r0		')

	lvx	v0, 0, up
	addi	r7, r11, 96
	rlwinm	r6, up, 2,26,29
	lvx	v8, r7, r6
	vand	v0, v0, v8

LIMB32(`rlwinm	r8, up, 30,30,31	')
LIMB64(`rlwinm	r8, up, 29,31,31	')
	add	n, n, r8		C compensate n for rounded down `up'

	vxor	v1, v1, v1
	li	r8, 0			C grand total count

	vxor	v3, v3, v3		C zero total count

	addic.	n, n, -LIMBS_PER_VR
	ble	L(sum)

	addic.	n, n, -LIMBS_PER_VR
	ble	L(lsum)

C For 64-bit machines, handle huge n that would overflow vsum4ubs
LIMB64(`ble	cr7, L(small)		')
LIMB64(`addis	r9, n, -LIMBS_PER_CHUNK	') C remaining n
LIMB64(`lis	n, LIMBS_PER_CHUNK	')
L(small):


LIMB32(`srwi	r7, n, 3	')	C loop count corresponding to n
LIMB64(`srdi	r7, n, 2	')	C loop count corresponding to n
	addi	r7, r7, 1
	mtctr	r7			C copy n to count register
	b	L(ent)

	ALIGN(8)
L(top):	lvx	v0, 0, up
	li	r7, 128			C prefetch distance
L(ent):	lvx	v1, r12, up
	addi	up, up, 32
	vsr	v4, v0, cnt1
	vsr	v5, v1, cnt1
	dcbt	up, r7			C prefetch
	vand	v8, v4, x01010101
	vand	v9, v5, x01010101
	vsububm	v0, v0, v8		C 64 2-bit accumulators (0..2)
	vsububm	v1, v1, v9		C 64 2-bit accumulators (0..2)
	vsr	v4, v0, cnt2
	vsr	v5, v1, cnt2
	vand	v8, v0, x00110011
	vand	v9, v1, x00110011
	vand	v4, v4, x00110011
	vand	v5, v5, x00110011
	vaddubm	v0, v4, v8		C 32 4-bit accumulators (0..4)
	vaddubm	v1, v5, v9		C 32 4-bit accumulators (0..4)
	vaddubm	v8, v0, v1		C 32 4-bit accumulators (0..8)
	vsr	v9, v8, cnt4
	vand	v6, v8, x00001111
	vand	v9, v9, x00001111
	vaddubm	v6, v9, v6		C 16 8-bit accumulators (0..16)
	vsum4ubs v3, v6, v3		C sum 4 x 4 bytes into 4 32-bit fields
	bdnz	L(top)

	andi.	n, n, eval(LIMBS_PER_2VR-1)
	beq	L(rt)

	lvx	v0, 0, up
	vxor	v1, v1, v1
	cmpwi	n, LIMBS_PER_VR
	ble	L(sum)
L(lsum):
	vor	v1, v0, v0
	lvx	v0, r12, up
L(sum):
LIMB32(`rlwinm	r6, n, 4,26,27	')
LIMB64(`rlwinm	r6, n, 5,26,26	')
	addi	r7, r11, 32
	lvx	v8, r7, r6
	vand	v0, v0, v8

	vsr	v4, v0, cnt1
	vsr	v5, v1, cnt1
	vand	v8, v4, x01010101
	vand	v9, v5, x01010101
	vsububm	v0, v0, v8		C 64 2-bit accumulators (0..2)
	vsububm	v1, v1, v9		C 64 2-bit accumulators (0..2)
	vsr	v4, v0, cnt2
	vsr	v5, v1, cnt2
	vand	v8, v0, x00110011
	vand	v9, v1, x00110011
	vand	v4, v4, x00110011
	vand	v5, v5, x00110011
	vaddubm	v0, v4, v8		C 32 4-bit accumulators (0..4)
	vaddubm	v1, v5, v9		C 32 4-bit accumulators (0..4)
	vaddubm	v8, v0, v1		C 32 4-bit accumulators (0..8)
	vsr	v9, v8, cnt4
	vand	v6, v8, x00001111
	vand	v9, v9, x00001111
	vaddubm	v6, v9, v6		C 16 8-bit accumulators (0..16)
	vsum4ubs v3, v6, v3		C sum 4 x 4 bytes into 4 32-bit fields

L(rt):
	li	r7, -16			C FIXME: does all ppc32 and ppc64 ABIs
	stvx	v3, r7, r1		C FIXME: ...support storing below sp?

	lwz	r7, -16(r1)
	add	r8, r8, r7
	lwz	r7, -12(r1)
	add	r8, r8, r7
	lwz	r7, -8(r1)
	add	r8, r8, r7
	lwz	r7, -4(r1)
	add	r8, r8, r7

C Handle outer loop for huge n.  We inherit cr7 and r0 from above.
LIMB64(`ble	cr7, L(ret)
	vxor	v3, v3, v3		C zero total count
	mr	n, r9
	cmpd	cr7, n, r0
	ble	cr7, L(2)
	addis	r9, n, -LIMBS_PER_CHUNK	C remaining n
	lis	n, LIMBS_PER_CHUNK
L(2):	srdi	r7, n, 2		C loop count corresponding to n
	mtctr	r7			C copy n to count register
	b	L(top)
')

L(ret):	mr	r3, r8
	mtspr	256, r10
	blr
EPILOGUE()

DEF_OBJECT(cnsts,16)
	.byte	0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55
	.byte	0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55

	.byte	0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33
	.byte	0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33
C Masks for high end of number
	.byte	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
	.byte	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff

	.byte	0xff,0xff,0xff,0xff,0x00,0x00,0x00,0x00
	.byte	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00

	.byte	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
	.byte	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00

	.byte	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
	.byte	0xff,0xff,0xff,0xff,0x00,0x00,0x00,0x00
C Masks for low end of number
	.byte	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
	.byte	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff

	.byte	0x00,0x00,0x00,0x00,0xff,0xff,0xff,0xff
	.byte	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff

	.byte	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
	.byte	0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff

	.byte	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
	.byte	0x00,0x00,0x00,0x00,0xff,0xff,0xff,0xff
END_OBJECT(cnsts)
ASM_END()
