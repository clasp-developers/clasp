dnl  IA-64 mpn_Xshift.

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

C This code runs at 2 cycles/limb for large operands on the Itanium.  It needs
C a very deep software pipeline, since shl/shr.u have a 4 cycle latency.  The
C main loop here is not great; it is oversheduled with respect to the shr.u
C instructions, and this actually turns out to give considerably more complex
C wind down code.  The code runs slowly for operands with <= 8 limbs, since we
C have a non-scheduled loop for that case.  We also have a primitive loop for
C the unrolling edge, and as a consequence of the main loop stupidity it is
C executed 1-4 steps instead of 0-3 steps.

C By having 63 separate loops using the shrp instruction, we could easily reach
C 1 cycle/limb.  Such loops would require a less deep software pipeline, since
C shrp unlike shl/shr.u have a plain one cycle latency.

C INPUT PARAMETERS
C rp = r32
C sp = r33
C n = r34
C cnt = r35

ifdef(`OPERATION_lshift',`
	define(`FSH',`shl')
	define(`BSH',`shr.u')
	define(`UPD',`-8')
	define(`func',`mpn_lshift')
')
ifdef(`OPERATION_rshift',`
	define(`FSH',`shr.u')
	define(`BSH',`shl')
	define(`UPD',`8')
	define(`func',`mpn_rshift')
')

ASM_START()
PROLOGUE(func)
	.prologue
ifdef(`HAVE_ABI_32',
`	addp4	r32 = 0, r32
	addp4	r33 = 0, r33
	sxt4	r34 = r34
	zxt4	r35 = r35
	;;
')
	add	r34 = -1, r34
	sub	r31 = 64, r35
	.save	ar.lc, r2
	mov	r2 = ar.lc;;
	.body
	cmp.leu	p6, p7 = 8,r34
ifdef(`OPERATION_lshift',`
	shladd	r33 = r34, 3, r33
	shladd	r32 = r34, 3, r32;;
')
	ld8	r19 = [r33], UPD	;;
	BSH	r8 = r19, r31		C function return value
   (p6) br.dptk	.Lbig

C
C Code for small operands.  Not an optimization for the Itanium, it is here
C just to simplify the general case.
C
	mov	ar.lc = r34;;
	br.cloop.dptk .Loops
	FSH	r26 = r19, r35	;;
	st8	[r32] = r26
	mov	ar.lc = r2
	br.ret.sptk.many b0
.Loops:
	ld8	r16 = [r33], UPD
	FSH	r26 = r19, r35	;;
	BSH	r27 = r16, r31	;;
	{ .mib;	nop.b 0;; }			C delay to save 6 cycles...
	{ .mib;	nop.b 0;; }			C delay to save 6 cycles...
	{ .mib;	nop.b 0;; }			C delay to save 6 cycles...
	or	r27 = r27, r26
	mov	r19 = r16	;;
	st8	[r32] = r27, UPD
	br.cloop.dptk .Loops
	FSH	r26 = r19, r35	;;
	st8	[r32] = r26
	mov	ar.lc = r2
	br.ret.sptk.many b0

C
C Code for operands with >8 limbs.  An edge loop and a very deep software
C pipeline.
C
.Lbig:	and	r15 = 3, r34
	shr.u	r14 = r34, 2	;;
	mov	ar.lc = r15
.Loop0:
	ld8	r16 = [r33], UPD
	FSH	r26 = r19, r35	;;
	BSH	r27 = r16, r31	;;
	{ .mib;	nop.b 0;; }			C delay to save 6 cycles...
	{ .mib;	nop.b 0;; }			C delay to save 6 cycles...
	{ .mib;	nop.b 0;; }			C delay to save 6 cycles...
	or	r27 = r27, r26
	mov	r19 = r16	;;
	st8	[r32] = r27, UPD
	br.cloop.dptk .Loop0

.Lunroll:
	add	r14 = -2, r14	;;
	mov	ar.lc = r14

.Lphase1:
  { .mmi
	ld8	r16 = [r33], UPD	;;
} { .mmi
	ld8	r17 = [r33], UPD	;;
} { .mmi
	ld8	r18 = [r33], UPD
	FSH	r26 = r19, r35	;;
} { .mmi
	ld8	r19 = [r33], UPD
	BSH	r27 = r16, r31	;;
} { .mib
	FSH	r20 = r16, r35
}

.Lphase2:
  { .mmi
	ld8	r16 = [r33], UPD
	BSH	r21 = r17, r31
} { .mib
	FSH	r22 = r17, r35	;;
} { .mmi
	ld8	r17 = [r33], UPD
	BSH	r23 = r18, r31
} { .mib
	or	r27 = r27, r26
	FSH	r24 = r18, r35
	br.cloop.dptk .Loop
}
	br.sptk	.Lend2
.Loop:
  { .mmi
	st8	[r32] = r27, UPD
	ld8	r18 = [r33], UPD
	BSH	r25 = r19, r31
} { .mib
	or	r21 = r21, r20
	FSH	r26 = r19, r35	;;
} { .mmi
	st8	[r32] = r21, UPD
	ld8	r19 = [r33], UPD
	BSH	r27 = r16, r31
} { .mib
	or	r23 = r23, r22
	FSH	r20 = r16, r35	;;
} { .mmi
	st8	[r32] = r23, UPD
	ld8	r16 = [r33], UPD
	BSH	r21 = r17, r31
} { .mib
	or	r25 = r25, r24
	FSH	r22 = r17, r35	;;
} { .mmi
	st8	[r32] = r25, UPD
	ld8	r17 = [r33], UPD
	BSH	r23 = r18, r31
} { .mib
	or	r27 = r27, r26
	FSH	r24 = r18, r35
	br.cloop.sptk .Loop;;
}
.Lend2:
  { .mmi
	st8	[r32] = r27, UPD
	ld8	r18 = [r33], UPD
	BSH	r25 = r19, r31
} { .mib
	or	r21 = r21, r20
	FSH	r26 = r19, r35	;;
} { .mmi
	st8	[r32] = r21, UPD
	BSH	r27 = r16, r31
} { .mib
	or	r23 = r23, r22
	FSH	r20 = r16, r35	;;
} { .mmi
	st8	[r32] = r23, UPD
	BSH	r21 = r17, r31
} { .mib
	or	r25 = r25, r24
	FSH	r22 = r17, r35	;;
} { .mmi
	st8	[r32] = r25, UPD
	BSH	r23 = r18, r31
} { .mib
	or	r27 = r27, r26
	FSH	r24 = r18, r35	;;
}

  { .mmi
	st8	[r32] = r27, UPD
} { .mib
	or	r21 = r21, r20	;;
} { .mmi
	st8	[r32] = r21, UPD
} { .mib
	or	r23 = r23, r22	;;
} { .mmi
	st8	[r32] = r23, UPD;;
} { .mmi
	st8	[r32] = r24
}
	mov	ar.lc = r2
	br.ret.sptk.many b0
EPILOGUE(func)
ASM_END()
