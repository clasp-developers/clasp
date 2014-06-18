dnl  x86 mpn_mod_1 -- mpn by limb remainder.

dnl  Copyright 1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.
dnl
dnl  This file is part of the GNU MP Library.
dnl
dnl  The GNU MP Library is free software; you can redistribute it and/or
dnl  modify it under the terms of the GNU Lesser General Public License as
dnl  published by the Free Software Foundation; either version 2.1 of the
dnl  License, or (at your option) any later version.
dnl
dnl  The GNU MP Library is distributed in the hope that it will be useful,
dnl  but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl  Lesser General Public License for more details.
dnl
dnl  You should have received a copy of the GNU Lesser General Public
dnl  License along with the GNU MP Library; see the file COPYING.LIB.  If
dnl  not, write to the Free Software Foundation, Inc., 51 Franklin Street,
dnl  Fifth Floor, Boston, MA 02110-1301, USA.

include(`../config.m4')


C      cycles/limb
C 486     42 approx, maybe
C P5      44
C P6      39
C K6      20
C K7      41
C P4      58


C mp_limb_t mpn_mod_1 (mp_srcptr src, mp_size_t size, mp_limb_t divisor);
C mp_limb_t mpn_mod_1c (mp_srcptr src, mp_size_t size, mp_limb_t divisor,
C                       mp_limb_t carry);
C
C Essentially this code is the same as the division based part of
C mpn/generic/mod_1.c, but has the advantage that we get the desired divl
C instruction even when gcc is not being used (where longlong.h only has the
C rather slow generic C udiv_qrnnd().
C
C A test is done to see if the high limb is less the the divisor, and if so
C one less div is done.  A div is between 20 and 40 cycles on the various
C x86s, so assuming high<divisor about half the time, then this test saves
C half that amount.  The branch misprediction penalty on each chip is less
C than half a div.
C
C
C Notes for K6:
C
C Back-to-back div instructions take 20 cycles, the same as the loop here,
C so it seems there's nothing to gain by rearranging.  Pairing the mov and
C loop instructions was found to gain nothing.  Normally we use a loop
C instruction rather than decl/jnz, but it gains nothing here.
C
C A multiply-by-inverse is used in mpn/x86/k6/pre_mod_1.asm, but it saves
C only 2 c/l so currently we haven't bothered with the same for mpn_mod_1.
C If an inverse takes about 40 cycles for normalized or perhaps 60 for
C unnormalized (due to bsfl being slow on k6) then the threshold would be at
C least 20 or 30 limbs.
C

defframe(PARAM_CARRY,  16)
defframe(PARAM_DIVISOR,12)
defframe(PARAM_SIZE,   8)
defframe(PARAM_SRC,    4)

	TEXT

	ALIGN(16)
PROLOGUE(mpn_mod_1)
deflit(`FRAME',0)

	movl	PARAM_SIZE, %ecx
	pushl	%ebx		FRAME_pushl()

	movl	PARAM_SRC, %ebx
	pushl	%esi		FRAME_pushl()

	orl	%ecx, %ecx
	jz	L(done_zero)

	movl	PARAM_DIVISOR, %esi
	movl	-4(%ebx,%ecx,4), %eax	C src high limb

	cmpl	%esi, %eax

	sbbl	%edx, %edx		C -1 if high<divisor

	addl	%edx, %ecx		C skip one division if high<divisor
	jz	L(done_eax)

	andl	%eax, %edx		C carry if high<divisor


L(top):
	C eax	scratch (quotient)
	C ebx	src
	C ecx	counter
	C edx	carry (remainder)
	C esi	divisor
	C edi
	C ebp

	movl	-4(%ebx,%ecx,4), %eax

	divl	%esi

	decl	%ecx
	jnz	L(top)


	movl	%edx, %eax
L(done_eax):
	popl	%esi

	popl	%ebx

	ret

EPILOGUE()


	C This code located after mpn_mod_1, so the jump to L(top) here is
	C back and hence will be predicted as taken.  (size==0 is considered
	C unlikely.)

	ALIGN(16)
PROLOGUE(mpn_mod_1c)
deflit(`FRAME',0)

	movl	PARAM_SIZE, %ecx
	pushl	%ebx		FRAME_pushl()

	movl	PARAM_SRC, %ebx
	pushl	%esi		FRAME_pushl()

	movl	PARAM_DIVISOR, %esi
	orl	%ecx, %ecx

	movl	PARAM_CARRY, %edx
	jnz	L(top)

	popl	%esi
	movl	%edx, %eax

	popl	%ebx

	ret


	C This code is for mpn_mod_1, but is positioned here to save some
	C space in the alignment padding.
	C
L(done_zero):
	popl	%esi
	xorl	%eax, %eax

	popl	%ebx

	ret

EPILOGUE()
