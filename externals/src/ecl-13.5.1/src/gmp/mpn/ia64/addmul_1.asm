dnl  IA-64 mpn_addmul_1 -- Multiply a limb vector with a limb and add the
dnl  result to a second limb vector.

dnl  Copyright 2000, 2001, 2002 Free Software Foundation, Inc.

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
C rp = r32
C up = r33
C n = r34
C v = r35

C         cycles/limb
C Itanium:    4
C Itanium 2:  7

C The full speed is reached C only for really huge operands.  See README for
C possible improvements.


ASM_START()
PROLOGUE(mpn_addmul_1)
	.prologue
	.save	ar.pfs, r21
		alloc		r21 = ar.pfs, 4, 12, 0, 16
	.save	ar.lc, r2
		mov		r2 = ar.lc
		mov		r20 = ar.ec
	.save	pr, r22
		mov		r22 = pr
	.body
ifdef(`HAVE_ABI_32',
`		addp4	r32 = 0, r32
		addp4	r33 = 0, r33
		sxt4	r34 = r34
		;;
')
  { .mfi;	setf.sig	f6 = r35
		nop.f		0
		adds		r19 = -1, r34		C n - 1
} { .mfi;	cmp.ne		p6, p7 = r0, r0
		nop.f		0
		mov		r18 = r32	;;
} { .mfi;	mov		r16 = r32
		nop.f		0
		mov		ar.lc = r19
} { .mfi;	mov		r17 = r33
		nop.f		0
		mov		ar.ec = 7
} { .mii;	cmp.ne		p6, p7 = r0, r0
		mov		pr.rot = 1<<16
		mov		r32 = 0			C clear "carry in"
} { .mib;	mov		r33 = 0			C clear for cmp
		mov		r34 = 0			C clear for cmp
		nop.b		0
} { .mib;	mov		r35 = 0			C clear for cmp
		mov		r36 = 0			C clear for cmp
		nop.b		0		;;
}
		.align	32
.Loop:
	.pred.rel "mutex",p6,p7
  { .mfi; (p16)	ldf8		f32 = [r17], 8		C  *0,3,6,9,12,15,18
	  (p19)	xma.l		f40 = f35, f6, f39	C  0,3,6,*9,12,15,18
	   (p6) add		r14 = r33, r38, 1	C  0,3,6,9,12,15,*18
} { .mfi; (p16)	ldf8		f36 = [r16], 8		C  *0,3,6,9,12,15,18
	  (p19)	xma.hu		f44 = f35, f6, f39	C  0,3,6,*9,12,15,18
	   (p7) add		r14 = r33, r38	;;	C  0,3,6,9,12,15,*18
} { .mii; (p21)	getf.sig	r32 = f42		C  1,4,7,10,13,*16,19
	   (p6) cmp.leu		p8, p9 = r14, r33	C  1,4,7,10,13,16,*19
	   (p7) cmp.ltu		p8, p9 = r14, r33;;	C  1,4,7,10,13,16,*19
}
	.pred.rel "mutex",p8,p9
  { .mib; (p21)	getf.sig	r36 = f46		C  2,5,8,11,14,*17,20
	   (p8) cmp.eq		p6, p7 = r0, r0
		nop.b		0
} { .mib; (p22)	st8		[r18] = r14, 8		C  2,5,8,11,14,17,*20
	   (p9) cmp.ne		p6, p7 = r0, r0
		br.ctop.sptk	.Loop		;;
}
	.pred.rel "mutex",p6,p7
	   (p6)	add		r8 = 1, r38
	   (p7)	mov		r8 = r38
		mov		pr = r22,0x1fffe
		mov		ar.lc = r2
		mov		ar.ec = r20
		mov		ar.pfs = r21;;
		br.ret.sptk.many b0
EPILOGUE(mpn_addmul_1)
ASM_END()
