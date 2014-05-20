dnl  Itanium-2 mpn_gcd_1 -- mpn by 1 gcd.

dnl  Copyright 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

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


C           cycles/bitpair (1x1 gcd)
C Itanium:      14 (approx)
C Itanium 2:     6.3


C mpn_gcd_1 (mp_srcptr xp, mp_size_t xsize, mp_limb_t y);
C
C The entry sequence is designed to expect xsize>1 and hence a modexact
C call.  This ought to be more common than a 1x1 operation.  Our critical
C path is thus stripping factors of 2 from y, calling modexact, then
C stripping factors of 2 from the x remainder returned.
C
C The common factors of 2 between x and y must be determined using the
C original x, not the remainder from the modexact.  This is done with
C x_orig which is xp[0].  There's plenty of time to do this while the rest
C of the modexact etc is happening.
C
C It's possible xp[0] is zero.  In this case the trailing zeros calculation
C popc((x-1)&~x) gives 63, and that's clearly no less than what y will
C have, making min(x_twos,y_twos) == y_twos.
C
C The main loop consists of transforming x,y to abs(x-y),min(x,y), and then
C stripping factors of 2 from abs(x-y).  Those factors of two are
C determined from just y-x, without the abs(), since there's the same
C number of trailing zeros on n or -n in twos complement.  That makes the
C dependent chain
C
C	cycles
C	  1    sub     x-y and x-y-1
C	  3    andcm   (x-y-1)&~(x-y)
C	  2    popcnt  trailing zeros
C	  3    shr.u   strip abs(x-y)
C	 ---
C	  9
C
C The selection of x-y versus y-x for abs(x-y), and the selection of the
C minimum of x and y, is done in parallel with the above.
C
C The algorithm takes about 0.68 iterations per bit (two N bit operands) on
C average, hence the final 6.3 cycles/bitpair.
C
C The loop is not as fast as one might hope, since there's extra latency
C from andcm going across to the `multimedia' popcnt, and vice versa from
C multimedia shr.u back to the integer sub.
C
C The loop branch is .sptk.clr since we usually expect a good number of
C iterations, and the iterations are data dependent so it's unlikely past
C results will predict anything much about the future.
C
C Not done:
C
C An alternate algorithm which didn't strip all twos, but instead applied
C tbit and predicated extr on x, and then y, was attempted.  The loop was 6
C cycles, but the algorithm is an average 1.25 iterations per bitpair for a
C total 7.25 c/bp, which is slower than the current approach.
C
C Alternatives:
C
C Perhaps we could do something tricky by extracting a few high bits and a
C few low bits from the operands, and looking up a table which would give a
C set of predicates to control some shifts or subtracts or whatever.  That
C could knock off multiple bits per iteration.
C
C The right shifts are a bit of a bottleneck (shr at 2 or 3 cycles, or extr
C only going down I0), perhaps it'd be possible to shift left instead,
C using add.  That would mean keeping track of the lowest not-yet-zeroed
C bit, using some sort of mask.
C
C Itanium-1:
C
C This code is not designed for itanium-1 and in fact doesn't run well on
C that chip.  The loop seems to be about 21 cycles, probably because we end
C up with a 10 cycle replay for not forcibly scheduling the shr.u latency.
C Lack of branch hints might introduce a couple of bubbles too.
C

ASM_START()
	.explicit				C What does this mean?

C HP's assembler requires these declarations for importing mpn_modexact_1c_odd
	.global	mpn_modexact_1c_odd
	.type	mpn_modexact_1c_odd,@function

PROLOGUE(mpn_gcd_1)

		C r32	xp
		C r33	xsize
		C r34	y

define(x,           r8)
define(xp_orig,     r32)
define(xsize,       r33)
define(y,           r34)  define(inputs, 3)
define(save_rp,     r35)
define(save_pfs,    r36)
define(x_orig,      r37)
define(x_orig_one,  r38)
define(y_twos,      r39)  define(locals, 5)
define(out_xp,      r40)
define(out_xsize,   r41)
define(out_divisor, r42)
define(out_carry,   r43)  define(outputs, 4)

	.prologue
{ .mmi;
ifdef(`HAVE_ABI_32',
`		addp4	r9 = 0, xp_orig   define(xp,r9)',	C M0
`					  define(xp,xp_orig)')
	.save ar.pfs, save_pfs
		alloc	save_pfs = ar.pfs, inputs, locals, outputs, 0 C M2
	.save rp, save_rp
		mov	save_rp = b0		C I0
}{	.body
		add	r10 = -1, y		C M3  y-1
}		;;

{ .mmi;		ld8	x = [xp]		C M0  x = xp[0] if no modexact
		ld8	x_orig = [xp]		C M1  orig x for common twos
		cmp.ne	p6,p0 = 1, xsize	C I0
}{ .mmi;	andcm	y_twos = r10, y		C M2  (y-1)&~y
		mov	out_xp = xp_orig	C M3
		mov	out_xsize = xsize	C I1
}		;;

		mov	out_carry = 0

		C

		popcnt	y_twos = y_twos		C I0  y twos
		;;

		C

{ .mmi;		add	x_orig_one = -1, x_orig	C M0  orig x-1
		shr.u	out_divisor = y, y_twos	C I0  y without twos
}{		shr.u	y = y, y_twos		C I1  y without twos
	(p6)	br.call.sptk.many b0 = mpn_modexact_1c_odd  C if xsize>1
}		;;

		C modexact can leave x==0
{ .mmi;		cmp.eq	p6,p0 = 0, x		C M0  if {xp,xsize} % y == 0
		andcm	x_orig = x_orig_one, x_orig	C M1  orig (x-1)&~x
		add	r9 = -1, x		C I0  x-1
}		;;

{ .mmi;		andcm	r9 = r9, x		C M0  (x-1)&~x
		mov	b0 = save_rp		C I0
}		;;

		C

		popcnt	x_orig = x_orig		C I0  orig x twos

		popcnt	r9 = r9			C I0  x twos
		;;

		C

{		cmp.lt	p7,p0 = x_orig, y_twos	C M0  orig x_twos < y_twos
		shr.u	x = x, r9		C I0  x odd
}		;;

{	(p7)	mov	y_twos = x_orig		C M0  common twos
		add	r10 = -1, y		C I0  y-1
	(p6)	br.dpnt.few .Ldone_y		C B0  x%y==0 then result y
}		;;

		C


		C No noticable difference in speed for the loop aligned to
		C 32 or just 16.
.Ltop:
		C r8	x
		C r10  y-1
		C r34	y
		C r38	common twos, for use at end

{  .mmi;	cmp.gtu	p8,p9 = x, y	C M0  x>y
		cmp.ne	p10,p0 = x, y	C M1  x==y
		sub	r9 = y, x	C I0  d = y - x
}{ .mmi;	sub	r10 = r10, x	C M2  d-1 = y - x - 1
}		;;

{ .mmi;	.pred.rel "mutex", p8, p9
	(p8)	sub	x = x, y	C M0  x>y  use x=x-y, y unchanged
	(p9)	mov	y = x		C M1  y>=x use y=x
	(p9)	mov	x = r9		C I0  y>=x use x=y-x
}{ .mmi;	andcm	r9 = r10, r9	C M2  (d-1)&~d
		;;

		add	r10 = -1, y	C M0  new y-1
		popcnt	r9 = r9		C I0  twos on x-y
}		;;

{		shr.u	x = x, r9	C I0   new x without twos
	(p10)	br.sptk.few.clr .Ltop
}		;;



		C result is y
.Ldone_y:
		shl	r8 = y, y_twos		C I   common factors of 2
		;;
		mov	ar.pfs = save_pfs	C I0
		br.ret.sptk.many b0

EPILOGUE()
