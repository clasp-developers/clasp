dnl  IA-64 mpn_add_n/mpn_sub_n -- mpn addition and subtraction.

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
C Itanium:      2.67
C Itanium 2:    1.25

C TODO
C  * Consider using special code for small n, using something like
C    "switch (8 * (n >= 8) + (n mod 8))" to enter it and feed-in code.

C INPUT PARAMETERS
define(`rp',`r32')
define(`up',`r33')
define(`vp',`r34')
define(`n',`r35')

ifdef(`OPERATION_add_n',`
  define(ADDSUB,	add)
  define(PRED,		ltu)
  define(INCR,		1)
  define(LIM,		-1)
  define(func, mpn_add_n)
')
ifdef(`OPERATION_sub_n',`
  define(ADDSUB,	sub)
  define(PRED,		gtu)
  define(INCR,		-1)
  define(LIM,		0)
  define(func, mpn_sub_n)
')

C Some useful aliases for registers we use
define(`u0',`r14') define(`u1',`r15') define(`u2',`r16') define(`u3',`r17')
define(`u4',`r18') define(`u5',`r19') define(`u6',`r20') define(`u7',`r21')
define(`v0',`r24') define(`v1',`r25') define(`v2',`r26') define(`v3',`r27')
define(`v4',`r28') define(`v5',`r29') define(`v6',`r30') define(`v7',`r31')
define(`w0',`r22') define(`w1',`r9') define(`w2',`r8') define(`w3',`r23')
define(`w4',`r22') define(`w5',`r9') define(`w6',`r8') define(`w7',`r23')
define(`rpx',`r3')

MULFUNC_PROLOGUE(mpn_add_n mpn_sub_n)

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
{.mmi		C 00
	ld8		r11 = [vp], 8		C			M01
	ld8		r10 = [up], 8		C			M01
	mov.i		r2 = ar.lc		C			I0
}
{.mmi
	and		r14 = 7, n		C			M I
	cmp.lt		p15, p14 = 8, n		C			M I
	add		n = -8, n		C			M I
	;;
}
{.mmi		C 01
	cmp.eq		p6, p0 = 1, r14		C			M I
	cmp.eq		p7, p0 = 2, r14		C			M I
	cmp.eq		p8, p0 = 3, r14		C			M I
}
{.bbb
   (p6)	br.dptk		.Lb001			C			B
   (p7)	br.dptk		.Lb010			C			B
   (p8)	br.dptk		.Lb011			C			B
	;;
}
{.mmi		C 02
	cmp.eq		p9, p0 = 4, r14		C			M I
	cmp.eq		p10, p0 = 5, r14	C			M I
	cmp.eq		p11, p0 = 6, r14	C			M I
}
{.bbb
   (p9)	br.dptk		.Lb100			C			B
  (p10)	br.dptk		.Lb101			C			B
  (p11)	br.dptk		.Lb110			C			B
	;;
}		C 03
{.mmb
	cmp.eq		p12, p0 = 7, r14	C			M I
	add		n = -1, n		C loop count		M I
  (p12)	br.dptk		.Lb111			C			B
}


.Lb000:	ld8		v2 = [vp], 8		C			M01
	ld8		u2 = [up], 8		C			M01
	add		rpx = 8, rp		C			M I
	;;
	ld8		v3 = [vp], 8		C			M01
	ld8		u3 = [up], 8		C			M01
	ADDSUB		w1 = r10, r11		C			M I
	;;
	ld8		v4 = [vp], 8		C			M01
	ld8		u4 = [up], 8		C			M01
	cmp.PRED	p7, p0 = w1, r10	C			M I
	;;
	ld8		v5 = [vp], 8		C			M01
	ld8		u5 = [up], 8		C			M01
	ADDSUB		w2 = u2, v2		C			M I
	;;
	ld8		v6 = [vp], 8		C			M01
	ld8		u6 = [up], 8		C			M01
	cmp.PRED	p8, p0 = w2, u2		C			M I
	;;
	ld8		v7 = [vp], 8		C			M01
	ld8		u7 = [up], 8		C			M01
	ADDSUB		w3 = u3, v3		C			M I
	;;
	ld8		v0 = [vp], 8		C			M01
	ld8		u0 = [up], 8		C			M01
	cmp.PRED	p9, p0 = w3, u3		C			M I
   (p7)	cmp.eq.or	p8, p0 = LIM, w2	C			M I
   (p7)	add		w2 = INCR, w2		C			M I
  (p14)	br.cond.dptk	.Lcj8			C			B
	;;

.grt8:	ld8		v1 = [vp], 8		C			M01
	ld8		u1 = [up], 8		C			M01
	shr.u		n = n, 3		C			I0
	;;
	add		r11 = 512, vp
	ld8		v2 = [vp], 8		C			M01
	add		r10 = 512, up
	ld8		u2 = [up], 8		C			M01
	nop.i		0
	nop.b		0
	;;
	ld8		v3 = [vp], 8		C			M01
	ld8		u3 = [up], 8		C			M01
	mov.i		ar.lc = n		C			I0
	br		.LL000			C			B

.Lb001:	add		rpx = 16, rp		C			M I
	ADDSUB		w0 = r10, r11		C			M I
  (p15)	br.cond.dpnt	.grt1			C			B
	;;
	cmp.PRED	p6, p0 = w0, r10	C			M I
	mov		r8 = 0			C			M I
	br		.Lcj1			C			B

.grt1:	ld8		v1 = [vp], 8		C			M01
	ld8		u1 = [up], 8		C			M01
	shr.u		n = n, 3		C			I0
	;;
	ld8		v2 = [vp], 8		C			M01
	ld8		u2 = [up], 8		C			M01
	cmp.ne		p9, p0 = r0, r0		C read near Loop
	;;
	ld8		v3 = [vp], 8		C			M01
	ld8		u3 = [up], 8		C			M01
	mov.i		ar.lc = n		C			I0
	;;
	ld8		v4 = [vp], 8		C			M01
	ld8		u4 = [up], 8		C			M01
	cmp.PRED	p6, p0 = w0, r10	C			M I
	;;
	ld8		v5 = [vp], 8		C			M01
	ld8		u5 = [up], 8		C			M01
	ADDSUB		w1 = u1, v1		C			M I
	;;
	ld8		v6 = [vp], 8		C			M01
	ld8		u6 = [up], 8		C			M01
	cmp.PRED	p7, p0 = w1, u1		C			M I
	;;
	ld8		v7 = [vp], 8		C			M01
	ld8		u7 = [up], 8		C			M01
	ADDSUB		w2 = u2, v2		C			M I
	;;
	add		r11 = 512, vp
	ld8		v0 = [vp], 8		C			M01
	add		r10 = 512, up
	ld8		u0 = [up], 8		C			M01
	br.cloop.dptk	.Loop			C			B
	br		.Lcj9			C			B

.Lb010:	ld8		v0 = [vp], 8		C			M01
	ld8		u0 = [up], 8		C			M01
	add		rpx = 24, rp		C			M I
	ADDSUB		w7 = r10, r11		C			M I
  (p15)	br.cond.dpnt	.grt2			C			B
	;;
	cmp.PRED	p9, p0 = w7, r10	C			M I
	ADDSUB		w0 = u0, v0		C			M I
	br		.Lcj2			C			B

.grt2:	ld8		v1 = [vp], 8		C			M01
	ld8		u1 = [up], 8		C			M01
	shr.u		n = n, 3		C			I0
	;;
	ld8		v2 = [vp], 8		C			M01
	ld8		u2 = [up], 8		C			M01
	;;
	ld8		v3 = [vp], 8		C			M01
	ld8		u3 = [up], 8		C			M01
	mov.i		ar.lc = n		C			I0
	;;
	ld8		v4 = [vp], 8		C			M01
	ld8		u4 = [up], 8		C			M01
	;;
	ld8		v5 = [vp], 8		C			M01
	ld8		u5 = [up], 8		C			M01
	cmp.PRED	p9, p0 = w7, r10	C			M I
	;;
	ld8		v6 = [vp], 8		C			M01
	ld8		u6 = [up], 8		C			M01
	ADDSUB		w0 = u0, v0		C			M I
	;;
	add		r11 = 512, vp
	ld8		v7 = [vp], 8		C			M01
	add		r10 = 512, up
	ld8		u7 = [up], 8		C			M01
	br		.LL01x			C			B

.Lb011:	ld8		v7 = [vp], 8		C			M01
	ld8		u7 = [up], 8		C			M01
	ADDSUB		w6 = r10, r11		C			M I
	;;
	ld8		v0 = [vp], 8		C			M01
	ld8		u0 = [up], 8		C			M01
  (p15)	br.cond.dpnt	.grt3			C			B
	;;
	cmp.PRED	p8, p0 = w6, r10	C			M I
	ADDSUB		w7 = u7, v7		C			M I
	;;
	st8		[rp] = w6, 8		C			M23
	cmp.PRED	p9, p0 = w7, u7		C			M I
	br		.Lcj3			C			B

.grt3:	ld8		v1 = [vp], 8		C			M01
	ld8		u1 = [up], 8		C			M01
	add		rpx = 32, rp		C			M I
	;;
	ld8		v2 = [vp], 8		C			M01
	ld8		u2 = [up], 8		C			M01
	shr.u		n = n, 3		C			I0
	;;
	ld8		v3 = [vp], 8		C			M01
	ld8		u3 = [up], 8		C			M01
	cmp.PRED	p8, p0 = w6, r10	C			M I
	;;
	ld8		v4 = [vp], 8		C			M01
	ld8		u4 = [up], 8		C			M01
	mov.i		ar.lc = n		C			I0
	ADDSUB		w7 = u7, v7		C			M I
	nop.i		0
	nop.b		0
	;;
	ld8		v5 = [vp], 8		C			M01
	ld8		u5 = [up], 8		C			M01
	cmp.PRED	p9, p0 = w7, u7		C			M I
	;;
	add		r11 = 512, vp
	ld8		v6 = [vp], 8		C			M01
	add		r10 = 512, up
	ld8		u6 = [up], 8		C			M01
   (p8)	cmp.eq.or	p9, p0 = LIM, w7	C			M I
	;;
	ld8		v7 = [vp], 8		C			M01
	ld8		u7 = [up], 8		C			M01
   (p8)	add		w7 = INCR, w7		C			M I
	st8		[rp] = w6, 8		C			M23
	ADDSUB		w0 = u0, v0		C			M I
	br		.LL01x			C			B

.Lb100:	ld8		v6 = [vp], 8		C			M01
	ld8		u6 = [up], 8		C			M01
	add		rpx = 8, rp		C			M I
	;;
	ld8		v7 = [vp], 8		C			M01
	ld8		u7 = [up], 8		C			M01
	ADDSUB		w5 = r10, r11		C			M I
	;;
	ld8		v0 = [vp], 8		C			M01
	ld8		u0 = [up], 8		C			M01
  (p15)	br.cond.dpnt	.grt4			C			B
	;;
	cmp.PRED	p7, p0 = w5, r10	C			M I
	ADDSUB		w6 = u6, v6		C			M I
	;;
	cmp.PRED	p8, p0 = w6, u6		C			M I
	ADDSUB		w7 = u7, v7		C			M I
	br		.Lcj4			C			B

.grt4:	ld8		v1 = [vp], 8		C			M01
	ld8		u1 = [up], 8		C			M01
	shr.u		n = n, 3		C			I0
	cmp.PRED	p7, p0 = w5, r10	C			M I
	;;
	ld8		v2 = [vp], 8		C			M01
	ld8		u2 = [up], 8		C			M01
	ADDSUB		w6 = u6, v6		C			M I
	;;
	ld8		v3 = [vp], 8		C			M01
	ld8		u3 = [up], 8		C			M01
	cmp.PRED	p8, p0 = w6, u6		C			M I
	;;
	ld8		v4 = [vp], 8		C			M01
	ld8		u4 = [up], 8		C			M01
	mov.i		ar.lc = n		C			I0
	;;
	ld8		v5 = [vp], 8		C			M01
	ld8		u5 = [up], 8		C			M01
	ADDSUB		w7 = u7, v7		C			M I
	;;
	add		r11 = 512, vp
	ld8		v6 = [vp], 8		C			M01
	add		r10 = 512, up
	ld8		u6 = [up], 8		C			M01
	cmp.PRED	p9, p0 = w7, u7		C			M I
	;;
	ld8		v7 = [vp], 8		C			M01
	ld8		u7 = [up], 8		C			M01
   (p7)	cmp.eq.or	p8, p0 = LIM, w6	C			M I
   (p7)	add		w6 = INCR, w6		C			M I
	br		.LL100			C			B

.Lb101:	ld8		v5 = [vp], 8		C			M01
	ld8		u5 = [up], 8		C			M01
	add		rpx = 16, rp		C			M I
	;;
	ld8		v6 = [vp], 8		C			M01
	ld8		u6 = [up], 8		C			M01
	ADDSUB		w4 = r10, r11		C			M I
	;;
	ld8		v7 = [vp], 8		C			M01
	ld8		u7 = [up], 8		C			M01
	cmp.PRED	p6, p0 = w4, r10	C			M I
	;;
	ld8		v0 = [vp], 8		C			M01
	ld8		u0 = [up], 8		C			M01
	ADDSUB		w5 = u5, v5		C			M I
	shr.u		n = n, 3		C			I0
  (p15)	br.cond.dpnt	.grt5			C			B
	;;
	cmp.PRED	p7, p0 = w5, u5		C			M I
	ADDSUB		w6 = u6, v6		C			M I
	br		.Lcj5			C			B

.grt5:	ld8		v1 = [vp], 8		C			M01
	ld8		u1 = [up], 8		C			M01
	;;
	ld8		v2 = [vp], 8		C			M01
	ld8		u2 = [up], 8		C			M01
	mov.i		ar.lc = n		C			I0
	;;
	ld8		v3 = [vp], 8		C			M01
	ld8		u3 = [up], 8		C			M01
	cmp.PRED	p7, p0 = w5, u5		C			M I
	;;
	ld8		v4 = [vp], 8		C			M01
	ld8		u4 = [up], 8		C			M01
	ADDSUB		w6 = u6, v6		C			M I
	;;
	add		r11 = 512, vp
	ld8		v5 = [vp], 8		C			M01
	add		r10 = 512, up
	ld8		u5 = [up], 8		C			M01
	br		.LL101			C			B

.Lb110:	ld8		v4 = [vp], 8		C			M01
	ld8		u4 = [up], 8		C			M01
	add		rpx = 24, rp		C			M I
	;;
	ld8		v5 = [vp], 8		C			M01
	ld8		u5 = [up], 8		C			M01
	ADDSUB		w3 = r10, r11		C			M I
	;;
	ld8		v6 = [vp], 8		C			M01
	ld8		u6 = [up], 8		C			M01
	shr.u		n = n, 3		C			I0
	;;
	ld8		v7 = [vp], 8		C			M01
	ld8		u7 = [up], 8		C			M01
	cmp.PRED	p9, p0 = w3, r10	C			M I
	;;
	ld8		v0 = [vp], 8		C			M01
	ld8		u0 = [up], 8		C			M01
	ADDSUB		w4 = u4, v4		C			M I
  (p14)	br.cond.dptk	.Lcj67			C			B
	;;

.grt6:	ld8		v1 = [vp], 8		C			M01
	ld8		u1 = [up], 8		C			M01
	mov.i		ar.lc = n		C			I0
	cmp.PRED	p9, p0 = w3, r10	C			M I
	nop.i		0
	nop.b		0
	;;
	ld8		v2 = [vp], 8		C			M01
	ld8		u2 = [up], 8		C			M01
	ADDSUB		w4 = u4, v4		C			M I
	;;
	add		r11 = 512, vp
	ld8		v3 = [vp], 8		C			M01
	add		r10 = 512, up
	ld8		u3 = [up], 8		C			M01
	br		.LL11x			C			B

.Lb111:	ld8		v3 = [vp], 8		C			M01
	ld8		u3 = [up], 8		C			M01
	add		rpx = 32, rp		C			M I
	;;
	ld8		v4 = [vp], 8		C			M01
	ld8		u4 = [up], 8		C			M01
	ADDSUB		w2 = r10, r11		C			M I
	;;
	ld8		v5 = [vp], 8		C			M01
	ld8		u5 = [up], 8		C			M01
	cmp.PRED	p8, p0 = w2, r10	C			M I
	;;
	ld8		v6 = [vp], 8		C			M01
	ld8		u6 = [up], 8		C			M01
	ADDSUB		w3 = u3, v3		C			M I
	;;
	ld8		v7 = [vp], 8		C			M01
	ld8		u7 = [up], 8		C			M01
	cmp.PRED	p9, p0 = w3, u3		C			M I
	;;
	ld8		v0 = [vp], 8		C			M01
	ld8		u0 = [up], 8		C			M01
  (p15)	br.cond.dpnt	.grt7			C			B
	;;
	st8		[rp] = w2, 8		C			M23
   (p8)	cmp.eq.or	p9, p0 = LIM, w3	C			M I
   (p8)	add		w3 = INCR, w3		C			M I
	ADDSUB		w4 = u4, v4		C			M I
	br		.Lcj67			C			B

.grt7:	ld8		v1 = [vp], 8		C			M01
	ld8		u1 = [up], 8		C			M01
	shr.u		n = n, 3		C			I0
   (p8)	cmp.eq.or	p9, p0 = LIM, w3	C			M I
	nop.i		0
	nop.b		0
	;;
	add		r11 = 512, vp
	ld8		v2 = [vp], 8		C			M01
	add		r10 = 512, up
	ld8		u2 = [up], 8		C			M01
   (p8)	add		w3 = INCR, w3		C			M I
	nop.b		0
	;;
	ld8		v3 = [vp], 8		C			M01
	ld8		u3 = [up], 8		C			M01
	mov.i		ar.lc = n		C			I0
	st8		[rp] = w2, 8		C			M23
	ADDSUB		w4 = u4, v4		C			M I
	br		.LL11x			C			B

C *** MAIN LOOP START ***
	ALIGN(32)
.Loop:	ld8		v1 = [vp], 8		C			M01
	cmp.PRED	p7, p0 = w1, u1		C			M I
   (p9)	cmp.eq.or	p6, p0 = LIM, w0	C			M I
	ld8		u1 = [up], 8		C			M01
   (p9)	add		w0 = INCR, w0		C			M I
	ADDSUB		w2 = u2, v2		C			M I
	;;
	ld8		v2 = [vp], 8		C			M01
	cmp.PRED	p8, p0 = w2, u2		C			M I
   (p6)	cmp.eq.or	p7, p0 = LIM, w1	C			M I
	ld8		u2 = [up], 8		C			M01
   (p6)	add		w1 = INCR, w1		C			M I
	ADDSUB		w3 = u3, v3		C			M I
	;;
	st8		[rp] = w0, 8		C			M23
	ld8		v3 = [vp], 8		C			M01
	cmp.PRED	p9, p0 = w3, u3		C			M I
   (p7)	cmp.eq.or	p8, p0 = LIM, w2	C			M I
	ld8		u3 = [up], 8		C			M01
   (p7)	add		w2 = INCR, w2		C			M I
	;;
.LL000:	st8		[rp] = w1, 16		C			M23
	st8		[rpx] = w2, 32		C			M23
   (p8)	cmp.eq.or	p9, p0 = LIM, w3	C			M I
	lfetch		[r10], 64
   (p8)	add		w3 = INCR, w3		C			M I
	ADDSUB		w4 = u4, v4		C			M I
	;;
.LL11x:	st8		[rp] = w3, 8		C			M23
	ld8		v4 = [vp], 8		C			M01
	cmp.PRED	p6, p0 = w4, u4		C			M I
	ld8		u4 = [up], 8		C			M01
	ADDSUB		w5 = u5, v5		C			M I
	;;
	ld8		v5 = [vp], 8		C			M01
	cmp.PRED	p7, p0 = w5, u5		C			M I
   (p9)	cmp.eq.or	p6, p0 = LIM, w4	C			M I
	ld8		u5 = [up], 8		C			M01
   (p9)	add		w4 = INCR, w4		C			M I
	ADDSUB		w6 = u6, v6		C			M I
	;;
.LL101:	ld8		v6 = [vp], 8		C			M01
	cmp.PRED	p8, p0 = w6, u6		C			M I
   (p6)	cmp.eq.or	p7, p0 = LIM, w5	C			M I
	ld8		u6 = [up], 8		C			M01
   (p6)	add		w5 = INCR, w5		C			M I
	ADDSUB		w7 = u7, v7		C			M I
	;;
	st8		[rp] = w4, 8		C			M23
	ld8		v7 = [vp], 8		C			M01
	cmp.PRED	p9, p0 = w7, u7		C			M I
   (p7)	cmp.eq.or	p8, p0 = LIM, w6	C			M I
	ld8		u7 = [up], 8		C			M01
   (p7)	add		w6 = INCR, w6		C			M I
	;;
.LL100:	st8		[rp] = w5, 16		C			M23
	st8		[rpx] = w6, 32		C			M23
   (p8)	cmp.eq.or	p9, p0 = LIM, w7	C			M I
	lfetch		[r11], 64
   (p8)	add		w7 = INCR, w7		C			M I
	ADDSUB		w0 = u0, v0		C			M I
	;;
.LL01x:	st8		[rp] = w7, 8		C			M23
	ld8		v0 = [vp], 8		C			M01
	cmp.PRED	p6, p0 = w0, u0		C			M I
	ld8		u0 = [up], 8		C			M01
	ADDSUB		w1 = u1, v1		C			M I
	br.cloop.dptk	.Loop			C			B
	;;
C *** MAIN LOOP END ***

	cmp.PRED	p7, p0 = w1, u1		C			M I
   (p9)	cmp.eq.or	p6, p0 = LIM, w0	C			M I
   (p9)	add		w0 = INCR, w0		C			M I
	ADDSUB		w2 = u2, v2		C			M I
	;;
.Lcj9:	cmp.PRED	p8, p0 = w2, u2		C			M I
   (p6)	cmp.eq.or	p7, p0 = LIM, w1	C			M I
	st8		[rp] = w0, 8		C			M23
   (p6)	add		w1 = INCR, w1		C			M I
	ADDSUB		w3 = u3, v3		C			M I
	;;
	cmp.PRED	p9, p0 = w3, u3		C			M I
   (p7)	cmp.eq.or	p8, p0 = LIM, w2	C			M I
   (p7)	add		w2 = INCR, w2		C			M I
	;;
.Lcj8:	st8		[rp] = w1, 16		C			M23
	st8		[rpx] = w2, 32		C			M23
   (p8)	cmp.eq.or	p9, p0 = LIM, w3	C			M I
   (p8)	add		w3 = INCR, w3		C			M I
	ADDSUB		w4 = u4, v4		C			M I
	;;
.Lcj67:	st8		[rp] = w3, 8		C			M23
	cmp.PRED	p6, p0 = w4, u4		C			M I
	ADDSUB		w5 = u5, v5		C			M I
	;;
	cmp.PRED	p7, p0 = w5, u5		C			M I
   (p9)	cmp.eq.or	p6, p0 = LIM, w4	C			M I
   (p9)	add		w4 = INCR, w4		C			M I
	ADDSUB		w6 = u6, v6		C			M I
	;;
.Lcj5:	cmp.PRED	p8, p0 = w6, u6		C			M I
   (p6)	cmp.eq.or	p7, p0 = LIM, w5	C			M I
	st8		[rp] = w4, 8		C			M23
   (p6)	add		w5 = INCR, w5		C			M I
	ADDSUB		w7 = u7, v7		C			M I
	;;
.Lcj4:	cmp.PRED	p9, p0 = w7, u7		C			M I
   (p7)	cmp.eq.or	p8, p0 = LIM, w6	C			M I
   (p7)	add		w6 = INCR, w6		C			M I
	;;
	st8		[rp] = w5, 16		C			M23
	st8		[rpx] = w6, 32		C			M23
.Lcj3:
   (p8)	cmp.eq.or	p9, p0 = LIM, w7	C			M I
   (p8)	add		w7 = INCR, w7		C			M I
	ADDSUB		w0 = u0, v0		C			M I
	;;
.Lcj2:	st8		[rp] = w7, 8		C			M23
	cmp.PRED	p6, p0 = w0, u0		C			M I
	;;
   (p9)	cmp.eq.or	p6, p0 = LIM, w0	C			M I
   (p9)	add		w0 = INCR, w0		C			M I
	mov		r8 = 0			C			M I
	;;
.Lcj1:	st8		[rp] = w0, 8		C			M23
	mov.i		ar.lc = r2		C			I0
   (p6)	mov		r8 = 1			C			M I
	br.ret.sptk.many b0			C			B
EPILOGUE()
ASM_END()
