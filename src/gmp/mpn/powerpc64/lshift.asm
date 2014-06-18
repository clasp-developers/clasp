dnl  PowerPC-64 mpn_lshift -- rp[] = up[] << cnt

dnl  Copyright 2003, 2005 Free Software Foundation, Inc.

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
C POWER3/PPC630:     1.5
C POWER4/PPC970:     3.0

C INPUT PARAMETERS
define(`rp',`r3')
define(`up',`r4')
define(`n',`r5')
define(`cnt',`r6')

define(`tnc',`r5')
define(`v0',`r0')
define(`v1',`r7')
define(`u0',`r8')
define(`u1',`r9')
define(`h0',`r10')
define(`h1',`r11')


ASM_START()
PROLOGUE(mpn_lshift)
ifdef(`HAVE_ABI_mode32',
`	rldicl	r7, r5, 0, 32	C zero extend n
	mtctr	r7',		C copy n to count register
`	mtctr	n')		C copy n to count register

ifdef(`HAVE_ABI_mode32',
`	rldic	r0, n, 3, 32',	C byte count corresponding to n
`	rldicr	r0, n, 3, 60')	C byte count corresponding to n

	add	rp, rp, r0	C rp = rp + n
	add	up, up, r0	C up = up + n
	addi	rp, rp, 8	C rp now points 16 beyond end
	addi	up, up, -8	C up now points to last limb
	subfic	tnc, cnt, 64	C reverse shift count

	ld	u0, 0(up)
	sld	h0, u0, cnt
	srd	r12, u0, tnc	C return value
	bdz	L(1)		C jump for n = 1

	ld	u1, -8(up)
	bdz	L(2)		C jump for n = 2

	ldu	u0, -16(up)
	bdz	L(end)		C jump for n = 3

L(oop):	srd	v1, u1, tnc
	sld	h1, u1, cnt
	ld	u1, -8(up)
	or	h0, v1, h0
	stdu	h0, -16(rp)

	bdz	L(exit)

	srd	v0, u0, tnc
	sld	h0, u0, cnt
	ldu	u0, -16(up)
	or	h1, v0, h1
	std	h1, -8(rp)

	bdnz	L(oop)

L(end):	srd	v1, u1, tnc
	sld	h1, u1, cnt
	or	h0, v1, h0
	stdu	h0, -16(rp)
	srd	v0, u0, tnc
	sld	h0, u0, cnt
	or	h1, v0, h1
	std	h1, -8(rp)
L(1):	std	h0, -16(rp)
ifdef(`HAVE_ABI_mode32',
`	srdi	r3, r12, 32
	mr	r4, r12
',`	mr	r3, r12
')
	blr

L(exit):	srd	v0, u0, tnc
	sld	h0, u0, cnt
	or	h1, v0, h1
	std	h1, -8(rp)
L(2):	srd	v1, u1, tnc
	sld	h1, u1, cnt
	or	h0, v1, h0
	stdu	h0, -16(rp)
	std	h1, -8(rp)
ifdef(`HAVE_ABI_mode32',
`	srdi	r3, r12, 32
	mr	r4, r12
',`	mr	r3, r12
')
	blr
EPILOGUE()
