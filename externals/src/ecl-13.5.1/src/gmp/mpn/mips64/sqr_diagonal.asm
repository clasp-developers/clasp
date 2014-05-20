dnl  MIPS64 mpn_sqr_diagonal.

dnl  Copyright 2001, 2002 Free Software Foundation, Inc.

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


dnl  INPUT PARAMETERS
dnl  rp		$4
dnl  up		$5
dnl  n		$6

include(`../config.m4')

ASM_START()
PROLOGUE(mpn_sqr_diagonal)
	ld	r8,0(r5)
	daddiu	r6,r6,-2
	dmultu	r8,r8
	bltz	r6,$Lend1
	nop
	ld	r8,8(r5)
	beq	r6,r0,$Lend2
	nop

$Loop:	mflo	r10
	mfhi	r9
	daddiu	r6,r6,-1
	sd	r10,0(r4)
	sd	r9,8(r4)
	dmultu	r8,r8
	ld	r8,16(r5)
	daddiu	r5,r5,8
	bne	r6,r0,$Loop
	daddiu	r4,r4,16

$Lend2: mflo	r10
	mfhi	r9
	sd	r10,0(r4)
	sd	r9,8(r4)
	dmultu	r8,r8
	mflo	r10
	mfhi	r9
	sd	r10,16(r4)
	j	r31
	sd	r9,24(r4)

$Lend1: mflo	r10
	mfhi	r9
	sd	r10,0(r4)
	j	r31
	sd	r9,8(r4)
EPILOGUE(mpn_sqr_diagonal)
