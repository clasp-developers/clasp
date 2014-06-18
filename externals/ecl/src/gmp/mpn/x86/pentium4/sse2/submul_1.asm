dnl  Intel Pentium-4 mpn_submul_1 -- Multiply a limb vector with a limb and
dnl  subtract the result from a second limb vector.

dnl  Copyright 2001, 2002 Free Software Foundation, Inc.
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


C P4: 7 cycles/limb, unstable timing, at least on early Pentium4 silicon
C     (stepping 10).


C mp_limb_t mpn_submul_1 (mp_ptr dst, mp_srcptr src, mp_size_t size,
C                         mp_limb_t mult);
C mp_limb_t mpn_submul_1c (mp_ptr dst, mp_srcptr src, mp_size_t size,
C                          mp_limb_t mult, mp_limb_t carry);
C
C This code is not particularly good at 7 c/l.  The dependent chain is only
C 4 c/l and there's only 4 MMX unit instructions, so it's not clear why that
C speed isn't achieved.
C
C The arrangements made here to get a two instruction dependent chain are
C slightly subtle.  In the loop the carry (or borrow rather) is a negative
C so that a paddq can be used to give a low limb ready to store, and a high
C limb ready to become the new carry after a psrlq.
C
C If the carry was a simple twos complement negative then the psrlq shift
C would need to bring in 0 bits or 1 bits according to whether the high was
C zero or non-zero, since a non-zero value would represent a negative
C needing sign extension.  That wouldn't be particularly easy to arrange and
C certainly would add an instruction to the dependent chain, so instead an
C offset is applied so that the high limb will be 0xFFFFFFFF+c.  With c in
C the range -0xFFFFFFFF to 0, the value 0xFFFFFFFF+c is in the range 0 to
C 0xFFFFFFFF and is therefore always positive and can always have 0 bits
C shifted in, which is what psrlq does.
C
C The extra 0xFFFFFFFF must be subtracted before c is used, but that can be
C done off the dependent chain.  The total adjustment then is to add
C 0xFFFFFFFF00000000 to offset the new carry, and subtract
C 0x00000000FFFFFFFF to remove the offset from the current carry, for a net
C add of 0xFFFFFFFE00000001.  In the code this is applied to the destination
C limb when fetched.
C
C It's also possible to view the 0xFFFFFFFF adjustment as a ones-complement
C negative, which is how it's undone for the return value, but that doesn't
C seem as clear.

defframe(PARAM_CARRY,     20)
defframe(PARAM_MULTIPLIER,16)
defframe(PARAM_SIZE,      12)
defframe(PARAM_SRC,       8)
defframe(PARAM_DST,       4)

	TEXT
	ALIGN(16)

PROLOGUE(mpn_submul_1c)
deflit(`FRAME',0)
	movd	PARAM_CARRY, %mm1
	jmp	L(start_1c)
EPILOGUE()

PROLOGUE(mpn_submul_1)
deflit(`FRAME',0)
	pxor	%mm1, %mm1		C initial borrow

L(start_1c):
	movl	PARAM_SRC, %eax
	pcmpeqd	%mm0, %mm0

	movd	PARAM_MULTIPLIER, %mm7
	pcmpeqd	%mm6, %mm6

	movl	PARAM_DST, %edx
	psrlq	$32, %mm0		C 0x00000000FFFFFFFF

	movl	PARAM_SIZE, %ecx
	psllq	$32, %mm6		C 0xFFFFFFFF00000000

	psubq	%mm0, %mm6		C 0xFFFFFFFE00000001

	psubq	%mm1, %mm0		C 0xFFFFFFFF - borrow


	C eax	src, incrementing
	C ebx
	C ecx	loop counter, decrementing
	C edx	dst, incrementing
	C
	C mm0	0xFFFFFFFF - borrow
	C mm6	0xFFFFFFFE00000001
	C mm7	multiplier

L(loop):
	movd	(%eax), %mm1		C src
	leal	4(%eax), %eax
	movd	(%edx), %mm2		C dst
	paddq	%mm6, %mm2		C add 0xFFFFFFFE00000001
	pmuludq	%mm7, %mm1
	psubq	%mm1, %mm2		C prod
	paddq	%mm2, %mm0		C borrow
	subl	$1, %ecx
	movd	%mm0, (%edx)		C result
	psrlq	$32, %mm0
	leal	4(%edx), %edx
	jnz	L(loop)

	movd	%mm0, %eax
	notl	%eax
	emms
	ret

EPILOGUE()
