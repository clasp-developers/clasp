dnl  IA-64 mpn_mul_2 -- Multiply a n-limb number with a 2-limb number and store
dnl  store the result to a (n+1)-limb number.

dnl  Copyright 2004 Free Software Foundation, Inc.

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
C Itanium:    3.15
C Itanium 2:  1.625

C Note that this is very similar to addmul_2.asm.  If you change this file,
C please change that file too.

C TODO
C  * Clean up variable names, and try to decrease the number of distinct
C    registers used.
C  * Cleanup feed-in code to not require zeroing several registers.
C  * Make sure we don't depend on uninitialized predicate registers.
C  * We currently cross-jump very aggressively, at the expense of a few cycles
C    per operation.  Consider changing that.
C  * Could perhaps save a few cycles by using 1 c/l carry propagation in
C    wind-down code.
C  * Ultimately rewrite.  The problem with this code is that it first uses a
C    loaded u value in one xma pair, then leaves it live over several unrelated
C    xma pairs, before it uses it again.  It should actually be quite possible
C    to just swap some aligned xma pairs around.  But we should then schedule
C    u loads further from the first use.

C INPUT PARAMETERS
define(`rp',`r32')
define(`up',`r33')
define(`n',`r34')
define(`vp',`r35')

define(`srp',`r3')

define(`v0',`f6')
define(`v1',`f7')

define(`s0',`r14')
define(`acc0',`r15')

define(`pr0_0',`r16') define(`pr0_1',`r17')
define(`pr0_2',`r18') define(`pr0_3',`r19')

define(`pr1_0',`r20') define(`pr1_1',`r21')
define(`pr1_2',`r22') define(`pr1_3',`r23')

define(`acc1_0',`r24') define(`acc1_1',`r25')
define(`acc1_2',`r26') define(`acc1_3',`r27')

dnl define(`',`r28')
dnl define(`',`r29')
dnl define(`',`r30')
dnl define(`',`r31')

define(`fp0b_0',`f8') define(`fp0b_1',`f9')
define(`fp0b_2',`f10') define(`fp0b_3',`f11')

define(`fp1a_0',`f12') define(`fp1a_1',`f13')
define(`fp1a_2',`f14') define(`fp1a_3',`f15')

define(`fp1b_0',`f32') define(`fp1b_1',`f33')
define(`fp1b_2',`f34') define(`fp1b_3',`f35')

define(`fp2a_0',`f36') define(`fp2a_1',`f37')
define(`fp2a_2',`f38') define(`fp2a_3',`f39')

define(`u_0',`f44') define(`u_1',`f45')
define(`u_2',`f46') define(`u_3',`f47')

define(`ux',`f49')
define(`uy',`f51')

ASM_START()
PROLOGUE(mpn_mul_2)
	.prologue
	.save	ar.lc, r2
	.body

ifdef(`HAVE_ABI_32',
`	addp4		rp = 0, rp		C			M I
	addp4		up = 0, up		C			M I
	addp4		vp = 0, vp		C			M I
	zxt4		n = n			C			I
	;;')

{.mmi		C 00
	ldf8		ux = [up], 8		C			M
	ldf8		v0 = [vp], 8		C			M
	mov.i		r2 = ar.lc		C			I0
}{.mmi
	nop		0			C			M
	and		r14 = 3, n		C			M I
	add		n = -2, n		C			M I
	;;
}{.mmi		C 01
	ldf8		uy = [up], 8		C			M
	ldf8		v1 = [vp]		C			M
	shr.u		n = n, 2		C			I
}{.mmi
	nop		0			C			M
	cmp.eq		p10, p0 = 1, r14	C			M I
	cmp.eq		p11, p0 = 2, r14	C			M I
	;;
}{.mmi		C 02
	nop		0			C			M
	cmp.eq		p12, p0 = 3, r14	C			M I
	mov.i		ar.lc = n		C			I0
}{.bbb
  (p10) br.dptk		.Lb01			C			B
  (p11) br.dptk		.Lb10			C			B
  (p12) br.dptk		.Lb11			C			B
	;;
}

	ALIGN(32)
.Lb00:	ldf8		u_1 = [up], 8
	mov		acc1_2 = 0
	mov		pr1_2 = 0
	mov		pr0_3 = 0
	cmp.ne		p8, p9 = r0, r0
	;;
	xma.l		fp0b_3 = ux, v0, f0
	cmp.ne		p12, p13 = r0, r0
	ldf8		u_2 = [up], 8
	xma.hu		fp1a_3 = ux, v0, f0
	br.cloop.dptk	.grt4

	xma.l		fp0b_0 = uy, v0, f0
	xma.hu		fp1a_0 = uy, v0, f0
	;;
	getf.sig	acc0 = fp0b_3
	xma.l		fp1b_3 = ux, v1, fp1a_3
	xma.hu		fp2a_3 = ux, v1, fp1a_3
	;;
	xma.l		fp0b_1 = u_1, v0, f0
	xma.hu		fp1a_1 = u_1, v0, f0
	;;
	getf.sig	pr0_0 = fp0b_0
	xma.l		fp1b_0 = uy, v1, fp1a_0
	xma.hu		fp2a_0 = uy, v1, fp1a_0
	;;
	getf.sig	pr1_3 = fp1b_3
	getf.sig	acc1_3 = fp2a_3
	xma.l		fp0b_2 = u_2, v0, f0
	xma.hu		fp1a_2 = u_2, v0, f0
	br		.Lcj4

.grt4:	xma.l		fp0b_0 = uy, v0, f0
	xma.hu		fp1a_0 = uy, v0, f0
	;;
	getf.sig	acc0 = fp0b_3
	xma.l		fp1b_3 = ux, v1, fp1a_3
	ldf8		u_3 = [up], 8
	xma.hu		fp2a_3 = ux, v1, fp1a_3
	;;
	xma.l		fp0b_1 = u_1, v0, f0
	xma.hu		fp1a_1 = u_1, v0, f0
	;;
	getf.sig	pr0_0 = fp0b_0
	xma.l		fp1b_0 = uy, v1, fp1a_0
	xma.hu		fp2a_0 = uy, v1, fp1a_0
	;;
	ldf8		u_0 = [up], 8
	getf.sig	pr1_3 = fp1b_3
	;;
	getf.sig	acc1_3 = fp2a_3
	xma.l		fp0b_2 = u_2, v0, f0
	xma.hu		fp1a_2 = u_2, v0, f0
	br		.LL00


	ALIGN(32)
.Lb01:	ldf8		u_0 = [up], 8		C M
	mov		acc1_1 = 0		C M I
	mov		pr1_1 = 0		C M I
	mov		pr0_2 = 0		C M I
	cmp.ne		p6, p7 = r0, r0		C M I
	;;
	xma.l		fp0b_2 = ux, v0, f0	C F
	cmp.ne		p10, p11 = r0, r0	C M I
	ldf8		u_1 = [up], 8		C M
	xma.hu		fp1a_2 = ux, v0, f0	C F
	;;
	xma.l		fp0b_3 = uy, v0, f0	C F
	xma.hu		fp1a_3 = uy, v0, f0	C F
	;;
	getf.sig	acc0 = fp0b_2		C M
	xma.l		fp1b_2 = ux, v1,fp1a_2	C F
	xma.hu		fp2a_2 = ux, v1,fp1a_2	C F
	ldf8		u_2 = [up], 8		C M
	br.cloop.dptk	.grt5

	xma.l		fp0b_0 = u_0, v0, f0	C F
	xma.hu		fp1a_0 = u_0, v0, f0	C F
	;;
	getf.sig	pr0_3 = fp0b_3		C M
	xma.l		fp1b_3 = uy, v1,fp1a_3	C F
	xma.hu		fp2a_3 = uy, v1,fp1a_3	C F
	;;
	getf.sig	pr1_2 = fp1b_2		C M
	getf.sig	acc1_2 = fp2a_2		C M
	xma.l		fp0b_1 = u_1, v0, f0	C F
	xma.hu		fp1a_1 = u_1, v0, f0	C F
	br		.Lcj5

.grt5:	xma.l		fp0b_0 = u_0, v0, f0
	xma.hu		fp1a_0 = u_0, v0, f0
	;;
	getf.sig	pr0_3 = fp0b_3
	xma.l		fp1b_3 = uy, v1, fp1a_3
	xma.hu		fp2a_3 = uy, v1, fp1a_3
	;;
	ldf8		u_3 = [up], 8
	getf.sig	pr1_2 = fp1b_2
	;;
	getf.sig	acc1_2 = fp2a_2
	xma.l		fp0b_1 = u_1, v0, f0
	xma.hu		fp1a_1 = u_1, v0, f0
	br		.LL01


C We have two variants for n = 2.  They turn out to run at exactly the same
C speed.  But the first, odd variant might allow one cycle to be trimmed.
	ALIGN(32)
ifdef(`',`
.Lb10:		C 03
	br.cloop.dptk	.grt2
		C 04
		C 05
		C 06
	xma.l		fp0b_1 = ux, v0, f0	C 0
	xma.hu		fp1a_1 = ux, v0, f0	C 1
	;;	C 07
	xma.l		fp0b_2 = uy, v0, f0	C 1
	xma.l		fp1b_1 = ux, v1, f0	C 1
	;;	C 08
	xma.hu		fp1a_2 = uy, v0, f0	C 2
	xma.hu		fp2a_1 = ux, v1, f0	C 2
	;;	C 09
	xma.l		fp1b_2 = uy, v1, f0	C 2
	xma.hu		fp2a_2 = uy, v1, f0	C 3
	;;	C 10
	getf.sig	r16 = fp1a_1
	stf8		[rp] = fp0b_1, 8
	;;	C 11
	getf.sig	r17 = fp0b_2
		C 12
	getf.sig	r18 = fp1b_1
		C 13
	getf.sig	r19 = fp1a_2
		C 14
	getf.sig	r20 = fp2a_1
		C 15
	getf.sig	r21 = fp1b_2
	;;	C 16
	getf.sig	r8 = fp2a_2
	add		r24 = r16, r17
	;;	C 17
	cmp.ltu		p6, p7 = r24, r16
	add		r26 = r24, r18
	;;	C 18
	cmp.ltu		p8, p9 = r26, r24
	;;	C 19
	st8		[rp] = r26, 8
  (p6)	add		r25 = r19, r20, 1
  (p7)	add		r25 = r19, r20
	;;	C 20
  (p8)	add		r27 = r25, r21, 1
  (p9)	add		r27 = r25, r21
  (p6)	cmp.leu		p10, p0 = r25, r19
  (p7)	cmp.ltu		p10, p0 = r25, r19
	;;	C 21
  (p10)	add		r8 = 1, r8
  (p8)	cmp.leu		p12, p0 = r27, r25
  (p9)	cmp.ltu		p12, p0 = r27, r25
	;;	C 22
	st8		[rp] = r27, 8
	mov.i		ar.lc = r2
  (p12)	add		r8 = 1, r8
	br.ret.sptk.many b0
')

.Lb10:		C 03
	br.cloop.dptk	.grt2
		C 04
		C 05
		C 06
	xma.l		fp0b_1 = ux, v0, f0
	xma.hu		fp1a_1 = ux, v0, f0
	;;	C 07
	xma.l		fp0b_2 = uy, v0, f0
	xma.hu		fp1a_2 = uy, v0, f0
	;;	C 08
		C 09
		C 10
	stf8		[rp] = fp0b_1, 8
	xma.l		fp1b_1 = ux, v1, fp1a_1
	xma.hu		fp2a_1 = ux, v1, fp1a_1
	;;	C 11
	getf.sig	acc0 = fp0b_2
	xma.l		fp1b_2 = uy, v1, fp1a_2
	xma.hu		fp2a_2 = uy, v1, fp1a_2
	;;	C 12
		C 13
		C 14
	getf.sig	pr1_1 = fp1b_1
		C 15
	getf.sig	acc1_1 = fp2a_1
		C 16
	getf.sig	pr1_2 = fp1b_2
		C 17
	getf.sig	r8 = fp2a_2
	;;	C 18
		C 19
	add		s0 = pr1_1, acc0
	;;	C 20
	st8		[rp] = s0, 8
	cmp.ltu		p8, p9 = s0, pr1_1
	sub		r31 = -1, acc1_1
	;;	C 21
	.pred.rel "mutex", p8, p9
  (p8)	add		acc0 = pr1_2, acc1_1, 1
  (p9)	add		acc0 = pr1_2, acc1_1
  (p8)	cmp.leu		p10, p0 = r31, pr1_2
  (p9)	cmp.ltu		p10, p0 = r31, pr1_2
	;;	C 22
	st8		[rp] = acc0, 8
	mov.i		ar.lc = r2
  (p10)	add		r8 = 1, r8
	br.ret.sptk.many b0


.grt2:	ldf8		u_3 = [up], 8
	mov		acc1_0 = 0
	mov		pr1_0 = 0
	;;
	mov		pr0_1 = 0
	xma.l		fp0b_1 = ux, v0, f0
	ldf8		u_0 = [up], 8
	xma.hu		fp1a_1 = ux, v0, f0
	;;
	xma.l		fp0b_2 = uy, v0, f0
	xma.hu		fp1a_2 = uy, v0, f0
	;;
	getf.sig	acc0 = fp0b_1
	xma.l		fp1b_1 = ux, v1, fp1a_1
	xma.hu		fp2a_1 = ux, v1, fp1a_1
	;;
	ldf8		u_1 = [up], 8
	xma.l		fp0b_3 = u_3, v0, f0
	xma.hu		fp1a_3 = u_3, v0, f0
	;;
	getf.sig	pr0_2 = fp0b_2
	xma.l		fp1b_2 = uy, v1, fp1a_2
	xma.hu		fp2a_2 = uy, v1, fp1a_2
	;;
	ldf8		u_2 = [up], 8
	getf.sig	pr1_1 = fp1b_1
	;;
	getf.sig	acc1_1 = fp2a_1
	xma.l		fp0b_0 = u_0, v0, f0
	cmp.ne		p8, p9 = r0, r0
	cmp.ne		p12, p13 = r0, r0
	xma.hu		fp1a_0 = u_0, v0, f0
	br		.LL10


	ALIGN(32)
.Lb11:	mov		acc1_3 = 0
	mov		pr1_3 = 0
	mov		pr0_0 = 0
	cmp.ne		p6, p7 = r0, r0
	;;
	ldf8		u_2 = [up], 8
	br.cloop.dptk	.grt3
	;;
	xma.l		fp0b_0 = ux, v0, f0
	xma.hu		fp1a_0 = ux, v0, f0
	;;
	cmp.ne		p10, p11 = r0, r0
	xma.l		fp0b_1 = uy, v0, f0
	xma.hu		fp1a_1 = uy, v0, f0
	;;
	getf.sig	acc0 = fp0b_0
	xma.l		fp1b_0 = ux, v1, fp1a_0
	xma.hu		fp2a_0 = ux, v1, fp1a_0
	;;
	xma.l		fp0b_2 = u_2, v0, f0
	xma.hu		fp1a_2 = u_2, v0, f0
	;;
	getf.sig	pr0_1 = fp0b_1
	xma.l		fp1b_1 = uy, v1, fp1a_1
	xma.hu		fp2a_1 = uy, v1, fp1a_1
	;;
	getf.sig	pr1_0 = fp1b_0
	getf.sig	acc1_0 = fp2a_0
	br		.Lcj3

.grt3:	xma.l		fp0b_0 = ux, v0, f0
	cmp.ne		p10, p11 = r0, r0
	ldf8		u_3 = [up], 8
	xma.hu		fp1a_0 = ux, v0, f0
	;;
	xma.l		fp0b_1 = uy, v0, f0
	xma.hu		fp1a_1 = uy, v0, f0
	;;
	getf.sig	acc0 = fp0b_0
	xma.l		fp1b_0 = ux, v1, fp1a_0
	ldf8		u_0 = [up], 8
	xma.hu		fp2a_0 = ux, v1, fp1a_0
	;;
	xma.l		fp0b_2 = u_2, v0, f0
	xma.hu		fp1a_2 = u_2, v0, f0
	;;
	getf.sig	pr0_1 = fp0b_1
	xma.l		fp1b_1 = uy, v1, fp1a_1
	xma.hu		fp2a_1 = uy, v1, fp1a_1
	;;
	ldf8		u_1 = [up], 8
	getf.sig	pr1_0 = fp1b_0
	;;
	getf.sig	acc1_0 = fp2a_0
	xma.l		fp0b_3 = u_3, v0, f0
	xma.hu		fp1a_3 = u_3, v0, f0
	br		.LL11


C *** MAIN LOOP START ***
	ALIGN(32)
.Loop:						C 00
	.pred.rel "mutex", p12, p13
	getf.sig	pr0_3 = fp0b_3
	xma.l		fp1b_3 = u_3, v1, fp1a_3
  (p12)	add		s0 = pr1_0, acc0, 1
  (p13)	add		s0 = pr1_0, acc0
	xma.hu		fp2a_3 = u_3, v1, fp1a_3
	;;					C 01
	.pred.rel "mutex", p8, p9
	.pred.rel "mutex", p12, p13
	ldf8		u_3 = [up], 8
	getf.sig	pr1_2 = fp1b_2
  (p8)	cmp.leu		p6, p7 = acc0, pr0_1
  (p9)	cmp.ltu		p6, p7 = acc0, pr0_1
  (p12)	cmp.leu		p10, p11 = s0, pr1_0
  (p13)	cmp.ltu		p10, p11 = s0, pr1_0
	;;					C 02
	.pred.rel "mutex", p6, p7
	getf.sig	acc1_2 = fp2a_2
	st8		[rp] = s0, 8
	xma.l		fp0b_1 = u_1, v0, f0
  (p6)	add		acc0 = pr0_2, acc1_0, 1
  (p7)	add		acc0 = pr0_2, acc1_0
	xma.hu		fp1a_1 = u_1, v0, f0
	;;					C 03
.LL01:
	.pred.rel "mutex", p10, p11
	getf.sig	pr0_0 = fp0b_0
	xma.l		fp1b_0 = u_0, v1, fp1a_0
  (p10)	add		s0 = pr1_1, acc0, 1
  (p11)	add		s0 = pr1_1, acc0
	xma.hu		fp2a_0 = u_0, v1, fp1a_0
	;;					C 04
	.pred.rel "mutex", p6, p7
	.pred.rel "mutex", p10, p11
	ldf8		u_0 = [up], 8
	getf.sig	pr1_3 = fp1b_3
  (p6)	cmp.leu		p8, p9 = acc0, pr0_2
  (p7)	cmp.ltu		p8, p9 = acc0, pr0_2
  (p10)	cmp.leu		p12, p13 = s0, pr1_1
  (p11)	cmp.ltu		p12, p13 = s0, pr1_1
	;;					C 05
	.pred.rel "mutex", p8, p9
	getf.sig	acc1_3 = fp2a_3
	st8		[rp] = s0, 8
	xma.l		fp0b_2 = u_2, v0, f0
  (p8)	add		acc0 = pr0_3, acc1_1, 1
  (p9)	add		acc0 = pr0_3, acc1_1
	xma.hu		fp1a_2 = u_2, v0, f0
	;;					C 06
.LL00:
	.pred.rel "mutex", p12, p13
	getf.sig	pr0_1 = fp0b_1
	xma.l		fp1b_1 = u_1, v1, fp1a_1
  (p12)	add		s0 = pr1_2, acc0, 1
  (p13)	add		s0 = pr1_2, acc0
	xma.hu		fp2a_1 = u_1, v1, fp1a_1
	;;					C 07
	.pred.rel "mutex", p8, p9
	.pred.rel "mutex", p12, p13
	ldf8		u_1 = [up], 8
	getf.sig	pr1_0 = fp1b_0
  (p8)	cmp.leu		p6, p7 = acc0, pr0_3
  (p9)	cmp.ltu		p6, p7 = acc0, pr0_3
  (p12)	cmp.leu		p10, p11 = s0, pr1_2
  (p13)	cmp.ltu		p10, p11 = s0, pr1_2
	;;					C 08
	.pred.rel "mutex", p6, p7
	getf.sig	acc1_0 = fp2a_0
	st8		[rp] = s0, 8
	xma.l		fp0b_3 = u_3, v0, f0
  (p6)	add		acc0 = pr0_0, acc1_2, 1
  (p7)	add		acc0 = pr0_0, acc1_2
	xma.hu		fp1a_3 = u_3, v0, f0
	;;					C 09
.LL11:
	.pred.rel "mutex", p10, p11
	getf.sig	pr0_2 = fp0b_2
	xma.l		fp1b_2 = u_2, v1, fp1a_2
  (p10)	add		s0 = pr1_3, acc0, 1
  (p11)	add		s0 = pr1_3, acc0
	xma.hu		fp2a_2 = u_2, v1, fp1a_2
	;;					C 10
	.pred.rel "mutex", p6, p7
	.pred.rel "mutex", p10, p11
	ldf8		u_2 = [up], 8
	getf.sig	pr1_1 = fp1b_1
  (p6)	cmp.leu		p8, p9 = acc0, pr0_0
  (p7)	cmp.ltu		p8, p9 = acc0, pr0_0
  (p10)	cmp.leu		p12, p13 = s0, pr1_3
  (p11)	cmp.ltu		p12, p13 = s0, pr1_3
	;;					C 11
	.pred.rel "mutex", p8, p9
	getf.sig	acc1_1 = fp2a_1
	st8		[rp] = s0, 8
	xma.l		fp0b_0 = u_0, v0, f0
  (p8)	add		acc0 = pr0_1, acc1_3, 1
  (p9)	add		acc0 = pr0_1, acc1_3
	xma.hu		fp1a_0 = u_0, v0, f0
.LL10:	br.cloop.dptk	.Loop			C 12
	;;
C *** MAIN LOOP END ***

.Lcj6:
	.pred.rel "mutex", p12, p13
	getf.sig	pr0_3 = fp0b_3
	xma.l		fp1b_3 = u_3, v1, fp1a_3
  (p12)	add		s0 = pr1_0, acc0, 1
  (p13)	add		s0 = pr1_0, acc0
	xma.hu		fp2a_3 = u_3, v1, fp1a_3
	;;
	.pred.rel "mutex", p8, p9
	.pred.rel "mutex", p12, p13
	getf.sig	pr1_2 = fp1b_2
  (p8)	cmp.leu		p6, p7 = acc0, pr0_1
  (p9)	cmp.ltu		p6, p7 = acc0, pr0_1
  (p12)	cmp.leu		p10, p11 = s0, pr1_0
  (p13)	cmp.ltu		p10, p11 = s0, pr1_0
	;;
	.pred.rel "mutex", p6, p7
	getf.sig	acc1_2 = fp2a_2
	st8		[rp] = s0, 8
	xma.l		fp0b_1 = u_1, v0, f0
  (p6)	add		acc0 = pr0_2, acc1_0, 1
  (p7)	add		acc0 = pr0_2, acc1_0
	xma.hu		fp1a_1 = u_1, v0, f0
	;;
.Lcj5:
	.pred.rel "mutex", p10, p11
	getf.sig	pr0_0 = fp0b_0
	xma.l		fp1b_0 = u_0, v1, fp1a_0
  (p10)	add		s0 = pr1_1, acc0, 1
  (p11)	add		s0 = pr1_1, acc0
	xma.hu		fp2a_0 = u_0, v1, fp1a_0
	;;
	.pred.rel "mutex", p6, p7
	.pred.rel "mutex", p10, p11
	getf.sig	pr1_3 = fp1b_3
  (p6)	cmp.leu		p8, p9 = acc0, pr0_2
  (p7)	cmp.ltu		p8, p9 = acc0, pr0_2
  (p10)	cmp.leu		p12, p13 = s0, pr1_1
  (p11)	cmp.ltu		p12, p13 = s0, pr1_1
	;;
	.pred.rel "mutex", p8, p9
	getf.sig	acc1_3 = fp2a_3
	st8		[rp] = s0, 8
	xma.l		fp0b_2 = u_2, v0, f0
  (p8)	add		acc0 = pr0_3, acc1_1, 1
  (p9)	add		acc0 = pr0_3, acc1_1
	xma.hu		fp1a_2 = u_2, v0, f0
	;;
.Lcj4:
	.pred.rel "mutex", p12, p13
	getf.sig	pr0_1 = fp0b_1
	xma.l		fp1b_1 = u_1, v1, fp1a_1
  (p12)	add		s0 = pr1_2, acc0, 1
  (p13)	add		s0 = pr1_2, acc0
	xma.hu		fp2a_1 = u_1, v1, fp1a_1
	;;
	.pred.rel "mutex", p8, p9
	.pred.rel "mutex", p12, p13
	getf.sig	pr1_0 = fp1b_0
  (p8)	cmp.leu		p6, p7 = acc0, pr0_3
  (p9)	cmp.ltu		p6, p7 = acc0, pr0_3
  (p12)	cmp.leu		p10, p11 = s0, pr1_2
  (p13)	cmp.ltu		p10, p11 = s0, pr1_2
	;;
	.pred.rel "mutex", p6, p7
	getf.sig	acc1_0 = fp2a_0
	st8		[rp] = s0, 8
  (p6)	add		acc0 = pr0_0, acc1_2, 1
  (p7)	add		acc0 = pr0_0, acc1_2
	;;
.Lcj3:
	.pred.rel "mutex", p10, p11
	getf.sig	pr0_2 = fp0b_2
	xma.l		fp1b_2 = u_2, v1, fp1a_2
  (p10)	add		s0 = pr1_3, acc0, 1
  (p11)	add		s0 = pr1_3, acc0
	xma.hu		fp2a_2 = u_2, v1, fp1a_2
	;;
	.pred.rel "mutex", p6, p7
	.pred.rel "mutex", p10, p11
	getf.sig	pr1_1 = fp1b_1
  (p6)	cmp.leu		p8, p9 = acc0, pr0_0
  (p7)	cmp.ltu		p8, p9 = acc0, pr0_0
  (p10)	cmp.leu		p12, p13 = s0, pr1_3
  (p11)	cmp.ltu		p12, p13 = s0, pr1_3
	;;
	.pred.rel "mutex", p8, p9
	getf.sig	acc1_1 = fp2a_1
	st8		[rp] = s0, 8
  (p8)	add		acc0 = pr0_1, acc1_3, 1
  (p9)	add		acc0 = pr0_1, acc1_3
	;;
	.pred.rel "mutex", p12, p13
  (p12)	add		s0 = pr1_0, acc0, 1
  (p13)	add		s0 = pr1_0, acc0
	;;
	.pred.rel "mutex", p8, p9
	.pred.rel "mutex", p12, p13
	getf.sig	pr1_2 = fp1b_2
  (p8)	cmp.leu		p6, p7 = acc0, pr0_1
  (p9)	cmp.ltu		p6, p7 = acc0, pr0_1
  (p12)	cmp.leu		p10, p11 = s0, pr1_0
  (p13)	cmp.ltu		p10, p11 = s0, pr1_0
	;;
	.pred.rel "mutex", p6, p7
	getf.sig	acc1_2 = fp2a_2
	st8		[rp] = s0, 8
  (p6)	add		acc0 = pr0_2, acc1_0, 1
  (p7)	add		acc0 = pr0_2, acc1_0
	;;
	.pred.rel "mutex", p10, p11
  (p10)	add		s0 = pr1_1, acc0, 1
  (p11)	add		s0 = pr1_1, acc0
	;;
	.pred.rel "mutex", p6, p7
	.pred.rel "mutex", p10, p11
  (p6)	cmp.leu		p8, p9 = acc0, pr0_2
  (p7)	cmp.ltu		p8, p9 = acc0, pr0_2
  (p10)	cmp.leu		p12, p13 = s0, pr1_1
  (p11)	cmp.ltu		p12, p13 = s0, pr1_1
	;;
	.pred.rel "mutex", p8, p9
	st8		[rp] = s0, 8
  (p8)	add		acc0 = pr1_2, acc1_1, 1
  (p9)	add		acc0 = pr1_2, acc1_1
	;;
	.pred.rel "mutex", p8, p9
  (p8)	cmp.leu		p10, p11 = acc0, pr1_2
  (p9)	cmp.ltu		p10, p11 = acc0, pr1_2
  (p12)	add		acc0 = 1, acc0
	;;
	st8		[rp] = acc0, 8
  (p12)	cmp.eq.or	p10, p0 = 0, acc0
	mov		r8 = acc1_2
	;;
	.pred.rel "mutex", p10, p11
  (p10)	add		r8 = 1, r8
	mov.i		ar.lc = r2
	br.ret.sptk.many b0
EPILOGUE()
ASM_END()
