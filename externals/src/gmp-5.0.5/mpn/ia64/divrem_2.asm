dnl  IA-64 mpn_divrem_2 -- Divide an n-limb number by a 2-limb number.

dnl  Copyright 2004, 2005 Free Software Foundation, Inc.

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
C Itanium:    63
C Itanium 2:  46


C TODO
C  * Further optimize the loop.  We could probably do some more trickery with
C    arithmetic in the FPU, or perhaps use a non-zero addend of xma in more
C    places.
C  * Software pipeline for perhaps 5 saved cycles, around the end and start of
C    the loop.
C  * Schedule code outside of loop better.
C  * Update the comments.  They are now using the same name for the same
C    logical quantity.
C  * Handle conditional zeroing of r31 in loop more cleanly.
C  * Inline mpn_invert_limb and schedule its insns across the entire init code.
C  * Ultimately, use 2-limb, or perhaps 3-limb or 4-limb inverse.

define(`qp',`r32')
define(`qxn',`r33')
define(`np',`r34')
define(`nn',`r35')
define(`dp',`r36')

define(`fnh',`f11')
define(`fminus1',`f10')
define(`fd0',`f13')
define(`fd1',`f14')
define(`d0',`r39')
define(`d1',`r36')
define(`fnl',`f32')
define(`fdinv',`f12')

define(`R1',`r38') define(`R0',`r37')
define(`P1',`r28') define(`P0',`r27')

ASM_START()

C HP's assembler requires these declarations for importing mpn_invert_limb
	.global	mpn_invert_limb
	.type	mpn_invert_limb,@function

PROLOGUE(mpn_divrem_2)
	.prologue
	.save ar.pfs, r42
	.save ar.lc, r44
	.save rp, r41
ifdef(`HAVE_ABI_32',
`	addp4		qp = 0, qp		C M I
	addp4		np = 0, np		C M I
	addp4		dp = 0, dp		C M I
	zxt4		nn = nn			C I
	zxt4		qxn = qxn		C I
	;;
')

	alloc		r42 = ar.pfs, 5,8,1,0	C M2
	ld8		d0 = [dp], 8		C M0M1	d0
	mov		r44 = ar.lc		C I0
	shladd		np = nn, 3, np		C M I
	;;
	ld8		d1 = [dp]		C M0M1	d1
	mov		r41 = b0		C I0
	add		r15 = -8, np		C M I
	add		np = -16, np		C M I
	mov		r40 = r0		C M I
	;;
	ld8		R1 = [r15]		C M0M1	n1
	ld8		R0 = [r34], -8		C M0M1	n0
	;;
	cmp.ltu		p6, p0 = d1, R1		C M I
	cmp.eq		p8, p0 = d1, R1		C M I
	;;
  (p8)	cmp.leu		p6, p0 = d0, R0
	cmp.ltu		p8, p9 = R0, d0
  (p6)	br.cond.dpnt	.L_high_limb_1		C FIXME: inline!
.L8:

	mov		r45 = d1
	br.call.sptk.many b0 = mpn_invert_limb	C FIXME: inline+schedule
	;;
	setf.sig	fd1 = d1		C d1
	setf.sig	fd0 = d0		C d0
	add		r14 = r33, r35		C nn + qxn
	;;
	setf.sig	fdinv = r8		C dinv
	mov		r9 = -1
	add		r35 = -3, r14
	;;
	setf.sig	fminus1 = r9
	cmp.gt		p6, p0 = r0, r35
	shladd		qp = r35, 3, qp
	mov		ar.lc = r35
	mov		r31 = 0			C n0
  (p6)	br.cond.dpnt	.Ldone
	;;
	ALIGN(16)
C *** MAIN LOOP START ***
.Loop:		C 00
	mov		r15 = R0		C nadj = n10
	cmp.le		p14, p15 = 0, R0	C check high bit of R0
	cmp.le		p8, p0 = r33, r35	C dividend limbs remaining?
	;;	C 01
	.pred.rel "mutex", p14, p15
  (p8)	ld8		r31 = [r34], -8		C n0
  (p15)	add		r15 = d1, R0		C nadj = n10 + d1
  (p15)	add		r14 = 1, R1		C nh + (nl:63)
  (p14)	mov		r14 = R1		C nh
	cmp.eq		p6, p0 = d1, R1		C nh == d1
  (p6)	br.cond.spnt	.L_R1_eq_d1
	;;	C 02
	setf.sig	f8 = r14		C n2 + (nl:63)
	setf.sig	f15 = r15		C nadj
	sub		r23 = -1, R1		C r23 = ~nh
	;;	C 03
	setf.sig	fnh = r23
	setf.sig	fnl = R0
	;;	C 08
	xma.hu		f7 = fdinv, f8, f15	C xh = HI(dinv*(nh-nmask)+nadj)
	;;	C 12
	xma.l		f7 = f7, fminus1, fnh	C nh + xh
	;;	C 16
	getf.sig	r14 = f7
	xma.hu		f9 = f7, fd1, fnl	C xh = HI(q1*d1+nl)
	xma.l		f33 = f7, fd1, fnl	C xh = LO(q1*d1+nl)
	;;	C 20
	getf.sig	r16 = f9
	sub		r24 = d1, R1
		C 21
	getf.sig	r17 = f33
	;;	C 25
	cmp.eq		p6, p7 = r16, r24
	;;	C 26
	.pred.rel "mutex", p6, p7
  (p6)	xma.l		f8 = f7, fminus1, f0	C f8 = -f7
  (p7)	xma.l		f8 = f7,fminus1,fminus1	C f8 = -f7-1
	;;	C 27
	.pred.rel "mutex", p6, p7
  (p6)	sub		r18 = 0, r14		C q = -q1
  (p7)	sub		r18 = -1, r14		C q = -q1-1
  (p6)	add		r14 = 0, r17		C n1 = xl
  (p7)	add		r14 = d1, r17		C n1 = xl + d1
	;;	C 30
	xma.hu		f9 = fd0, f8, f0	C d0*(-f7-1) = -d0*f7-d0
	xma.l		f35 = fd0, f8, f0
	;;	C 34
	getf.sig	P1 = f9		C P1
		C 35
	getf.sig	P0 = f35		C P0
	;;
.L_adj:		C 40
	cmp.ltu		p8, p0 = r31, P0	C p8 = cy from low limb
	cmp.ltu		p6, p0 = r14, P1	C p6 = prel cy from high limb
	sub		R0 = r31, P0
	sub		R1 = r14, P1
	;;	C 41
  (p8)	cmp.eq.or	p6, p0 = 0, R1		C p6 = final cy from high limb
  (p8)	add		R1 = -1, R1
	cmp.ne		p10, p0 = r0, r0	C clear p10 FIXME: use unc below!
	cmp.ne		p13, p0 = r0, r0	C clear p13 FIXME: use unc below!
	;;	C 42
  (p6)	add		R0 = R0, d0
  (p6)	add		R1 = R1, d1
  (p6)	add		r18 = -1, r18		C q--
	;;	C 43
  (p6)	cmp.ltu		p10, p0 = R0, d0
  (p6)	cmp.ltu		p0, p13 = R1, d1
	;;	C 44
  (p10)	cmp.ne.and	p0, p13 = -1, R1	C p13 = !cy
  (p10)	add		R1 = 1, R1
  (p13)	br.cond.spnt	.L_two_too_big		C jump if not cy
	;;	C 45
	st8		[qp] = r18, -8
	add		r35 = -1, r35
	mov		r31 = 0			C n0, next iteration
	br.cloop.sptk	.Loop
C *** MAIN LOOP END ***
	;;
.Ldone:
	mov		r8 = r40
	mov		b0 = r41
	add		r21 = 8, r34
	add		r22 = 16, r34
	;;
	st8		[r21] = R0
	st8		[r22] = R1
	mov		ar.pfs = r42
	mov		ar.lc = r44
	br.ret.sptk.many b0

.L_high_limb_1:
	.pred.rel "mutex", p8, p9
	sub		R0 = R0, d0
  (p8)	sub		R1 = R1, d1, 1
  (p9)	sub		R1 = R1, d1
	mov		r40 = 1
	br.sptk		.L8
	;;

.L_two_too_big:
	add		R0 = R0, d0
	add		R1 = R1, d1
	;;
	add		r18 = -1, r18		C q--
	cmp.ltu		p10, p0 = R0, d0
	;;
  (p10)	add		R1 = 1, R1
	st8		[qp] = r18, -8
	add		r35 = -1, r35
	mov		r31 = 0			C n0, next iteration
	br.cloop.sptk	.Loop
	br.sptk		.Ldone

.L_R1_eq_d1:
	add		r14 = R0, d1		C r = R0 + d1
	mov		r18 = -1		C q = -1
	;;
	cmp.leu		p6, p0 = R0, r14
 (p6)	br.cond.spnt	.L20			C jump unless cy
	;;
	sub		P1 = r14, d0
	add		R0 = r31, d0
	;;
	cmp.ltu		p8, p9 = R0, r31
	;;
	.pred.rel "mutex", p8, p9
	st8		[qp] = r18, -8
  (p8)	add		R1 = r0, P1, 1		C R1 = n1 - P1 - cy
  (p9)	add		R1 = r0, P1		C R1 = n1 - P1
	add		r35 = -1, r35
	mov		r31 = 0			C n0, next iteration
	br.cloop.sptk	.Loop
	br.sptk		.Ldone
	;;
.L20:	cmp.ne		p6, p7 = 0, d0
	;;
	.pred.rel "mutex", p6, p7
  (p6)	add		P1 = -1, d0
  (p7)	mov		P1 = d0
	sub		P0 = r0, d0
	br.sptk		.L_adj
EPILOGUE()
ASM_END()
