dnl  IA-64 mpn_invert_limb -- Invert a normalized limb.

dnl  Copyright (C) 2000 Free Software Foundation, Inc.

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

C INPUT PARAMETERS
C d = r32

C It should be possible to avoid the xmpy.hu and the following tests by
C explicitly chopping in the last fma.  That would save about 10 cycles.

ASM_START()
	.rodata
	.align 16
ifdef(`HAVE_DOUBLE_IEEE_LITTLE_ENDIAN',`
.LC0:	data4 0x00000000, 0x80000000, 0x0000403f, 0x00000000	C 2^64
.LC1:	data4 0x00000000, 0x80000000, 0x0000407f, 0x00000000	C 2^128

',`ifdef(`HAVE_DOUBLE_IEEE_BIG_ENDIAN',`
.LC0:	data4 0x403f8000, 0x00000000, 0x00000000, 0x00000000	C 2^64
.LC1:	data4 0x407f8000, 0x00000000, 0x00000000, 0x00000000	C 2^128

',`m4_error(`Oops, need to know float endianness
')')')

PROLOGUE(mpn_invert_limb)
	addl		r14 = @ltoff(.LC0),gp
	add		r8 = r32,r32;;			C check for d = 2^63
	ld8		r14 = [r14]
	cmp.eq		p6,p7 = 0,r8;;			C check for d = 2^63
	ldfe		f10 = [r14],16			C 2^64
	setf.sig	f7 = r32
	mov		r8 = -1
   (p6)	br.ret.spnt	b0;;
	ldfe		f8 = [r14]			C 2^128
	fmpy.s1		f11 = f7,f10;;			C scale by 2^64
	fsub.s1		f6 = f8,f11;;
	frcpa.s1	f8,p6 = f6,f7;;
   (p6) fnma.s1		f9 = f7,f8,f1
   (p6) fmpy.s1		f10 = f6,f8;;
   (p6) fmpy.s1		f11 = f9,f9
   (p6) fma.s1		f10 = f9,f10,f10;;
   (p6) fma.s1		f8 = f9,f8,f8
   (p6) fma.s1		f9 = f11,f10,f10;;
   (p6) fma.s1		f8 = f11,f8,f8
   (p6) fnma.s1		f10 = f7,f9,f6;;
   (p6) fma.s1		f8 = f10,f8,f9;;
	fcvt.fxu.trunc.s1 f8 = f8;;
	xmpy.hu		f10 = f8,f7;;			C di * d
	getf.sig	r8 = f8
	getf.sig	r14 = f10;;
	add		r32 = r32,r14;;
	cmp.ltu		p6,p7 = r32,r14;;		C got overflow?
   (p6) add		r8 = -1,r8			C adjust di down
	br.ret.sptk	b0
EPILOGUE(mpn_invert_limb)
ASM_END()
