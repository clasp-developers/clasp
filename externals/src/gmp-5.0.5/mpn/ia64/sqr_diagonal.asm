dnl  IA-64 mpn_sqr_diagonal.  Helper for sqr_basecase.

dnl  Copyright 2001, 2002, 2004 Free Software Foundation, Inc.

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

C         cycles/limb
C Itanium:    4
C Itanium 2:  2

C TODO
C  * Perhaps avoid ctop loop.  Unfortunately, a cloop loop running at 1 c/l
C    would need prohibitive 8-way unrolling.
C  * Instead of messing too much with this, write a nifty mpn_sqr_basecase.

C INPUT PARAMETERS
C rp = r32
C sp = r33
C n = r34

ASM_START()
PROLOGUE(mpn_sqr_diagonal)
	.prologue
	.save	ar.lc, r2
	.save	pr, r15
	.body
ifdef(`HAVE_ABI_32',
`	addp4	r32 = 0, r32
	addp4	r33 = 0, r33
	zxt4	r34 = r34
	;;
')
	ldf8		f32 = [r33], 8		C M	load rp[0] early
	mov		r2 = ar.lc		C I0
	mov		r14 = ar.ec		C I0
	mov		r15 = pr		C I0
	add		r19 = -1, r34		C M I	decr n
	add		r18 = 8, r32		C M I	rp for high limb
	;;
	mov		ar.lc = r19		C I0
	mov		ar.ec = 5		C I0
	mov		pr.rot = 1<<16		C I0
	;;
	br.cexit.spnt	.Ldone			C B
	;;
	ALIGN(32)
.Loop:
  (p16)	ldf8		f32 = [r33], 8		C M
  (p19)	xma.l		f36 = f35, f35, f0	C F
  (p21)	stf8		[r32] = f38, 16		C M2 M3
  (p19)	xma.hu		f40 = f35, f35, f0	C F
  (p21)	stf8		[r18] = f42, 16		C M2 M3
	br.ctop.dptk	.Loop			C B
	;;
.Ldone:
	stf8		[r32] = f38		C M2 M3
	stf8		[r18] = f42		C M2 M3
	mov		ar.ec = r14		C I0
	;;
	mov		pr = r15, 0x1ffff	C I0
	mov		ar.lc = r2		C I0
	br.ret.sptk.many b0			C B
EPILOGUE(mpn_sqr_diagonal)
ASM_END()
