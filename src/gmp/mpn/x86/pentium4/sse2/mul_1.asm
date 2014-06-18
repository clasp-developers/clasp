dnl  Intel Pentium-4 mpn_mul_1 -- Multiply a limb vector with a limb and store
dnl  the result in a second limb vector.

dnl  Copyright 2001, 2002, 2003, 2005 Free Software Foundation, Inc.
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

C                           src != dst      src == dst
C P6 model 9  (Banias)          ?.?
C P6 model 13 (Dothan)          4.75            4.75
C P4 model 0  (Willamette)      4.0             6.0
C P4 model 1  (?)               4.0             6.0
C P4 model 2  (Northwood)       4.0             6.0
C P4 model 3  (Prescott)        ?.?             ?.?
C P4 model 4  (Nocona)          ?.?             ?.?

C mp_limb_t mpn_mul_1 (mp_ptr dst, mp_srcptr src, mp_size_t size,
C                      mp_limb_t multiplier);
C mp_limb_t mpn_mul_1c (mp_ptr dst, mp_srcptr src, mp_size_t size,
C                       mp_limb_t multiplier, mp_limb_t carry);
C
C Only the carry limb propagation is on the dependent chain, hence 4 c/l.
C Unfortunately when src==dst the write-combining described in
C mpn/x86/pentium4/README takes us up to 6 c/l.

defframe(PARAM_CARRY,     20)
defframe(PARAM_MULTIPLIER,16)
defframe(PARAM_SIZE,      12)
defframe(PARAM_SRC,       8)
defframe(PARAM_DST,       4)

	TEXT
	ALIGN(16)
PROLOGUE(mpn_mul_1c)
deflit(`FRAME',0)
	movd	PARAM_CARRY, %mm0
	jmp	L(start_1c)
EPILOGUE()

PROLOGUE(mpn_mul_1)
deflit(`FRAME',0)
	pxor	%mm0, %mm0
L(start_1c):
	movl	PARAM_SRC, %eax
	movd	PARAM_MULTIPLIER, %mm7
	movl	PARAM_DST, %edx
	movl	PARAM_SIZE, %ecx

L(top):
	C eax	src, incrementing
	C ebx
	C ecx	counter, size iterations
	C edx	dst, incrementing
	C
	C mm0	carry limb
	C mm7	multiplier

	movd	(%eax), %mm1
	addl	$4, %eax
	pmuludq	%mm7, %mm1

	paddq	%mm1, %mm0
	movd	%mm0, (%edx)
	addl	$4, %edx

	psrlq	$32, %mm0

	subl	$1, %ecx
	jnz	L(top)


	movd	%mm0, %eax
	emms
	ret

EPILOGUE()
