dnl  IA-64 mpn_addlsh1_n/mpn_sublsh1_n -- rp[] = up[] +- (vp[] << 1).

dnl  Copyright 2003, 2004, 2005 Free Software Foundation, Inc.

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

C           cycles/limb
C Itanium:      3.0
C Itanium 2:    1.5

C TODO
C  * Use shladd in feed-in code (for mpn_addlsh1_n).

C INPUT PARAMETERS
define(`rp',`r32')
define(`up',`r33')
define(`vp',`r34')
define(`n',`r35')

ifdef(`OPERATION_addlsh1_n',`
  define(ADDSUB,       add)
  define(PRED,	       ltu)
  define(INCR,	       1)
  define(LIM,	       -1)
  define(func, mpn_addlsh1_n)
')
ifdef(`OPERATION_sublsh1_n',`
  define(ADDSUB,       sub)
  define(PRED,	       gtu)
  define(INCR,	       -1)
  define(LIM,	       0)
  define(func, mpn_sublsh1_n)
')

C Some useful aliases for registers we use
define(`u0',`r14') define(`u1',`r15') define(`u2',`r16') define(`u3',`r17')
define(`v0',`r18') define(`v1',`r19') define(`v2',`r20') define(`v3',`r21')
define(`w0',`r22') define(`w1',`r23') define(`w2',`r24') define(`w3',`r25')
define(`s0',`r26') define(`s1',`r27') define(`s2',`r28') define(`s3',`r29')
define(`x0',`r30') define(`x1',`r31') define(`x2',`r30') define(`x3',`r31')

MULFUNC_PROLOGUE(mpn_addlsh1_n mpn_sublsh1_n)

ASM_START()
PROLOGUE(func)
	.prologue
	.save	ar.lc, r2
	.body
ifdef(`HAVE_ABI_32',`
	addp4		rp = 0, rp		C			M I
	addp4		up = 0, up		C			M I
	addp4		vp = 0, vp		C			M I
	zxt4		n = n			C			I
	;;
')
 {.mmi;	ld8		r11 = [vp], 8		C			M01
	ld8		r10 = [up], 8		C			M01
	mov.i		r2 = ar.lc		C			I0
}{.mmi;	and		r14 = 3, n		C			M I
	cmp.lt		p15, p0 = 4, n		C			M I
	add		n = -4, n		C			M I
	;;
}{.mmi;	cmp.eq		p6, p0 = 1, r14		C			M I
	cmp.eq		p7, p0 = 2, r14		C			M I
	cmp.eq		p8, p0 = 3, r14		C			M I
}{.bbb
  (p6)	br.dptk		.Lb01			C			B
  (p7)	br.dptk		.Lb10			C			B
  (p8)	br.dptk		.Lb11			C			B
}

.Lb00:	ld8		v0 = [vp], 8		C			M01
	ld8		u0 = [up], 8		C			M01
	shr.u		n = n, 2		C			I0
	;;
	ld8		v1 = [vp], 8		C			M01
	ld8		u1 = [up], 8		C			M01
	add		x3 = r11, r11		C			M I
	;;
	ld8		v2 = [vp], 8		C			M01
	ld8		u2 = [up], 8		C			M01
	ADDSUB		w3 = r10, x3		C			M I
  (p15)	br.dpnt		.grt4			C			B
	;;
	shrp		x0 = v0, r11, 63	C			I0
	cmp.PRED	p8, p0 = w3, r10	C			M I
	;;
	shrp		x1 = v1, v0, 63		C			I0
	ADDSUB		w0 = u0, x0		C			M I
	;;
	cmp.PRED	p6, p0 = w0, u0		C			M I
	ADDSUB		w1 = u1, x1		C			M I
	br		.Lcj4			C			B

.grt4:	ld8		v3 = [vp], 8		C			M01
	shrp		x0 = v0, r11, 63	C			I0
	cmp.PRED	p8, p0 = w3, r10	C			M I
	add		n = -1, n
	;;
	ld8		u3 = [up], 8		C			M01
	mov.i		ar.lc = n		C			I0
	shrp		x1 = v1, v0, 63		C			I0
	ld8		v0 = [vp], 8		C			M01
	ADDSUB		w0 = u0, x0		C			M I
	;;
	cmp.PRED	p6, p0 = w0, u0		C			M I
	ld8		u0 = [up], 8		C			M01
	ADDSUB		w1 = u1, x1		C			M I
	br		.LL00			C			B

.Lb01:	add		x2 = r11, r11		C			M I
	shr.u		n = n, 2		C			I0
  (p15)	br.dpnt		.grt1			C			B
	;;
	ADDSUB		w2 = r10, x2		C			M I
	shr.u		r8 = r11, 63		C retval		I0
	;;
	cmp.PRED	p6, p0 = w2, r10	C			M I
	;;
	st8		[rp] = w2, 8		C			M23
   (p6)	add		r8 = 1, r8		C			M I
	br.ret.sptk.many b0			C			B

.grt1:	ld8		v3 = [vp], 8		C			M01
	ld8		u3 = [up], 8		C			M01
	mov.i		ar.lc = n		C FIXME swap with next	I0
	;;
	ld8		v0 = [vp], 8		C			M01
	ld8		u0 = [up], 8		C			M01
	ADDSUB		w2 = r10, x2
	;;
	ld8		v1 = [vp], 8		C			M01
	ld8		u1 = [up], 8		C			M01
	shrp		x3 = v3, r11, 63	C			I0
	;;
	ld8		v2 = [vp], 8		C			M01
	ld8		u2 = [up], 8		C			M01
	cmp.PRED	p6, p0 = w2, r10	C			M I
	ADDSUB		w3 = u3, x3		C			M I
	br.cloop.dptk	.grt5			C			B
	;;
	shrp		x0 = v0, v3, 63		C			I0
	cmp.PRED	p8, p0 = w3, u3		C			M I
	br		.Lcj5			C			B

.grt5:	shrp		x0 = v0, v3, 63		C			I0
	ld8		v3 = [vp], 8		C			M01
	cmp.PRED	p8, p0 = w3, u3		C			M I
	br		.LL01			C			B

.Lb10:	ld8		v2 = [vp], 8		C			M01
	ld8		u2 = [up], 8		C			M01
	shr.u		n = n, 2		C			I0
	add		x1 = r11, r11		C			M I
  (p15)	br.dpnt		.grt2			C			B
	;;
	ADDSUB		w1 = r10, x1		C			M I
	shrp		x2 = v2, r11, 63	C			I0
	;;
	cmp.PRED	p8, p0 = w1, r10	C			M I
	ADDSUB		w2 = u2, x2		C			M I
	shr.u		r8 = v2, 63		C retval		I0
	;;
	cmp.PRED	p6, p0 = w2, u2		C			M I
	br		.Lcj2			C			B

.grt2:	ld8		v3 = [vp], 8		C			M01
	ld8		u3 = [up], 8		C			M01
	mov.i		ar.lc = n		C			I0
	;;
	ld8		v0 = [vp], 8		C			M01
	ld8		u0 = [up], 8		C			M01
	ADDSUB		w1 = r10, x1		C			M I
	;;
	ld8		v1 = [vp], 8		C			M01
	shrp		x2 = v2, r11, 63	C			I0
	cmp.PRED	p8, p0 = w1, r10	C			M I
	;;
	ld8		u1 = [up], 8		C			M01
	shrp		x3 = v3, v2, 63		C			I0
	ld8		v2 = [vp], 8		C			M01
	ADDSUB		w2 = u2, x2		C			M I
	;;
	cmp.PRED	p6, p0 = w2, u2		C			M I
	ld8		u2 = [up], 8		C			M01
	ADDSUB		w3 = u3, x3		C			M I
	br.cloop.dpnt	.Loop			C			B
	br		.Lskip			C			B

.Lb11:	ld8		v1 = [vp], 8		C			M01
	ld8		u1 = [up], 8		C			M01
	shr.u		n = n, 2		C			I0
	add		x0 = r11, r11		C			M I
	;;
	ld8		v2 = [vp], 8		C			M01
	ld8		u2 = [up], 8		C			M01
  (p15)	br.dpnt		.grt3			C			B
	;;

	shrp		x1 = v1, r11, 63	C			I0
	ADDSUB		w0 = r10, x0		C			M I
	;;
	cmp.PRED	p6, p0 = w0, r10	C			M I
	ADDSUB		w1 = u1, x1		C			M I
	;;
	shrp		x2 = v2, v1, 63		C			I0
	cmp.PRED	p8, p0 = w1, u1		C			M I
	br		.Lcj3			C			B

.grt3:	ld8		v3 = [vp], 8		C			M01
	ld8		u3 = [up], 8		C			M01
	mov.i		ar.lc = n		C			I0
	shrp		x1 = v1, r11, 63	C			I0
	ADDSUB		w0 = r10, x0		C			M I
	;;
	ld8		v0 = [vp], 8		C			M01
	cmp.PRED	p6, p0 = w0, r10	C			M I
	ld8		u0 = [up], 8		C			M01
	ADDSUB		w1 = u1, x1		C			M I
	;;
	shrp		x2 = v2, v1, 63		C			I0
	ld8		v1 = [vp], 8		C			M01
	cmp.PRED	p8, p0 = w1, u1		C			M I
	br		.LL11			C			B


C *** MAIN LOOP START ***
	ALIGN(32)
.Loop:	st8		[rp] = w1, 8		C			M23
	shrp		x0 = v0, v3, 63		C			I0
   (p8)	cmp.eq.or	p6, p0 = LIM, w2	C			M I
   (p8)	add		w2 = INCR, w2		C			M I
	ld8		v3 = [vp], 8		C			M01
	cmp.PRED	p8, p0 = w3, u3		C			M I
	;;
.LL01:	ld8		u3 = [up], 8		C			M01
	shrp		x1 = v1, v0, 63		C			I0
   (p6)	cmp.eq.or	p8, p0 = LIM, w3	C			M I
   (p6)	add		w3 = INCR, w3		C			M I
	ld8		v0 = [vp], 8		C			M01
	ADDSUB		w0 = u0, x0		C			M I
	;;
	st8		[rp] = w2, 8		C			M23
	cmp.PRED	p6, p0 = w0, u0		C			M I
	ld8		u0 = [up], 8		C			M01
	ADDSUB		w1 = u1, x1		C			M I
	;;
.LL00:	st8		[rp] = w3, 8		C			M23
	shrp		x2 = v2, v1, 63		C			I0
   (p8)	cmp.eq.or	p6, p0 = LIM, w0	C			M I
   (p8)	add		w0 = INCR, w0		C			M I
	ld8		v1 = [vp], 8		C			M01
	cmp.PRED	p8, p0 = w1, u1		C			M I
	;;
.LL11:	ld8		u1 = [up], 8		C			M01
	shrp		x3 = v3, v2, 63		C			I0
   (p6)	cmp.eq.or	p8, p0 = LIM, w1	C			M I
   (p6)	add		w1 = INCR, w1		C			M I
	ld8		v2 = [vp], 8		C			M01
	ADDSUB		w2 = u2, x2		C			M I
	;;
	st8		[rp] = w0, 8		C			M23
	cmp.PRED	p6, p0 = w2, u2		C			M I
	ld8		u2 = [up], 8		C			M01
	ADDSUB		w3 = u3, x3		C			M I
	br.cloop.dptk	.Loop			C			B
	;;
C *** MAIN LOOP END ***

.Lskip:	st8		[rp] = w1, 8		C			M23
	shrp		x0 = v0, v3, 63		C			I0
   (p8)	cmp.eq.or	p6, p0 = LIM, w2	C			M I
   (p8)	add		w2 = INCR, w2		C			M I
	cmp.PRED	p8, p0 = w3, u3		C			M I
	;;
.Lcj5:	shrp		x1 = v1, v0, 63		C			I0
   (p6)	cmp.eq.or	p8, p0 = LIM, w3	C			M I
   (p6)	add		w3 = INCR, w3		C			M I
	ADDSUB		w0 = u0, x0		C			M I
	;;
	st8		[rp] = w2, 8		C			M23
	cmp.PRED	p6, p0 = w0, u0		C			M I
	ADDSUB		w1 = u1, x1		C			M I
	;;
.Lcj4:	st8		[rp] = w3, 8		C			M23
	shrp		x2 = v2, v1, 63		C			I0
   (p8)	cmp.eq.or	p6, p0 = LIM, w0	C			M I
   (p8)	add		w0 = INCR, w0		C			M I
	cmp.PRED	p8, p0 = w1, u1		C			M I
	;;
.Lcj3:	shr.u		r8 = v2, 63		C			I0
   (p6)	cmp.eq.or	p8, p0 = LIM, w1	C			M I
   (p6)	add		w1 = INCR, w1		C			M I
	ADDSUB		w2 = u2, x2		C			M I
	;;
	st8		[rp] = w0, 8		C			M23
	cmp.PRED	p6, p0 = w2, u2		C			M I
	;;
.Lcj2:	st8		[rp] = w1, 8		C			M23
   (p8)	cmp.eq.or	p6, p0 = LIM, w2	C			M I
   (p8)	add		w2 = INCR, w2		C			M I
	;;
.Lcj1:	st8		[rp] = w2, 8		C			M23
	mov.i		ar.lc = r2		C			I0
   (p6)	add		r8 = 1, r8		C			M I
	br.ret.sptk.many b0			C			B
EPILOGUE()
ASM_END()
