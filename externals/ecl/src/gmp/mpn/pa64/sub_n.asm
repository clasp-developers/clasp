dnl  HP-PA 2.0 mpn_sub_n -- Subtract two limb vectors of the same length > 0
dnl  and store difference in a third limb vector.

dnl  Copyright 1997, 2000, 2002, 2003 Free Software Foundation, Inc.

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


dnl  This runs at 2 cycles/limb on PA8000 and 1.6875 cycles/limb on PA8500.  It
dnl  should be possible to reach the cache bandwith 1.5 cycles/limb at least
dnl  with PA8500.  The problem now is stalling of the first SUB,DB after LDO,
dnl  where the processor gets confused about where carry comes from.

include(`../config.m4')

dnl INPUT PARAMETERS
define(`rp',`%r26')
define(`up',`%r25')
define(`vp',`%r24')
define(`n',`%r23')

ifdef(`HAVE_ABI_2_0w',
`       .level  2.0w
',`     .level  2.0
')
PROLOGUE(mpn_sub_n)
	sub		%r0, n, %r22
	depw,z		%r22, 30, 3, %r28	C r28 = 2 * (-n & 7)
	depw,z		%r22, 28, 3, %r22	C r22 = 8 * (-n & 7)
	sub		up, %r22, up		C offset up
	sub		vp, %r22, vp		C offset vp
	blr		%r28, %r0		C branch into loop
	sub		rp, %r22, rp		C offset rp and set carry

LDEF(loop)
	ldd		0(up), %r20
	ldd		0(vp), %r31
	sub,db		%r20, %r31, %r20
	std		%r20, 0(rp)
LDEF(7)	ldd		8(up), %r21
	ldd		8(vp), %r19
	sub,db		%r21, %r19, %r21
	std		%r21, 8(rp)
LDEF(6)	ldd		16(up), %r20
	ldd		16(vp), %r31
	sub,db		%r20, %r31, %r20
	std		%r20, 16(rp)
LDEF(5)	ldd		24(up), %r21
	ldd		24(vp), %r19
	sub,db		%r21, %r19, %r21
	std		%r21, 24(rp)
LDEF(4)	ldd		32(up), %r20
	ldd		32(vp), %r31
	sub,db		%r20, %r31, %r20
	std		%r20, 32(rp)
LDEF(3)	ldd		40(up), %r21
	ldd		40(vp), %r19
	sub,db		%r21, %r19, %r21
	std		%r21, 40(rp)
LDEF(2)	ldd		48(up), %r20
	ldd		48(vp), %r31
	sub,db		%r20, %r31, %r20
	std		%r20, 48(rp)
LDEF(1)	ldd		56(up), %r21
	ldd		56(vp),%r19
	sub,db		%r21, %r19, %r21
	ldo		64(up), up
	std		%r21, 56(rp)
	ldo		64(vp), vp
	addib,>		-8, n, L(loop)
	ldo		64(rp), rp

	add,dc		%r0, %r0, %r29
	subi		1, %r29, %r29
	bve		(%r2)
ifdef(`HAVE_ABI_2_0w',
`	copy		%r29, %r28
',`	ldi		0, %r28
')
EPILOGUE(mpn_sub_n)
