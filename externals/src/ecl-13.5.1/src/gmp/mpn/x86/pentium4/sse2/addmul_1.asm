dnl  Intel Pentium-4 mpn_addmul_1 -- Multiply a limb vector with a limb and add
dnl  the result to a second limb vector.

dnl  Copyright 2001, 2002, 2004, 2005 Free Software Foundation, Inc.
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


C P3 model 9  (Banias)          ?.?
C P3 model 13 (Dothan)          5.8
C P4 model 0  (Willamette)      5.5
C P4 model 1  (?)               5.5
C P4 model 2  (Northwood)       5.5
C P4 model 3  (Prescott)        6.0
C P4 model 4  (Nocona)

C mp_limb_t mpn_addmul_1 (mp_ptr dst, mp_srcptr src, mp_size_t size,
C                         mp_limb_t multiplier);
C mp_limb_t mpn_addmul_1c (mp_ptr dst, mp_srcptr src, mp_size_t size,
C                          mp_limb_t multiplier, mp_limb_t carry);
C
C Only the carry limb propagation is on the dependent chain, but some other
C Pentium4 pipeline magic brings down performance to 6 cycles/l from the
C ideal 4 cycles/l.

defframe(PARAM_CARRY,     20)
defframe(PARAM_MULTIPLIER,16)
defframe(PARAM_SIZE,      12)
defframe(PARAM_SRC,       8)
defframe(PARAM_DST,       4)

	TEXT
	ALIGN(16)
PROLOGUE(mpn_addmul_1c)
deflit(`FRAME',0)
	movd	PARAM_CARRY, %mm4
	jmp	L(start_1c)
EPILOGUE()

PROLOGUE(mpn_addmul_1)
deflit(`FRAME',0)
	pxor	%mm4, %mm4
L(start_1c):
	movl	PARAM_SRC, %eax
	movl	PARAM_SIZE, %ecx
	movl	PARAM_DST, %edx
	movd	PARAM_MULTIPLIER, %mm7

	C eax	src, incrementing
	C ecx	loop counter, decrementing
	C edx	dst, incrementing
	C
	C mm4	carry, low 32-bits
	C mm7	multiplier

	movd		(%eax), %mm2	C ul = up[i]
	pmuludq		%mm7, %mm2

	shrl	$1, %ecx
	jnc	L(even)

	leal		4(%eax), %eax
	movd		(%edx), %mm1
	paddq		%mm2, %mm1
	paddq		%mm1, %mm4
	movd		%mm4, (%edx)
	psrlq		$32, %mm4

	testl	%ecx, %ecx
	jz	L(rtn)
	leal	4(%edx), %edx

	movd		(%eax), %mm2	C ul = up[i]
	pmuludq		%mm7, %mm2
L(even):
	movd		4(%eax), %mm0	C ul = up[i]
	movd		(%edx), %mm1	C rl = rp[0]
	pmuludq		%mm7, %mm0

	subl	$1, %ecx
	jz	L(end)
L(loop):
	paddq		%mm2, %mm1	C rl += prod
	movd		8(%eax), %mm2	C ul = up[i]
	paddq		%mm1, %mm4	C mm4 = prod + cy
	movd		4(%edx), %mm3	C rl = rp[0]
	pmuludq		%mm7, %mm2
	movd		%mm4, (%edx)
	psrlq		$32, %mm4

	paddq		%mm0, %mm3	C rl += prod
	movd		12(%eax), %mm0	C ul = up[i]
	paddq		%mm3, %mm4	C mm4 = prod + cy
	movd		8(%edx), %mm1	C rl = rp[0]
	pmuludq		%mm7, %mm0
	movd		%mm4, 4(%edx)
	psrlq		$32, %mm4

	leal	8(%eax), %eax
	leal	8(%edx), %edx
	subl	$1, %ecx
	jnz	L(loop)
L(end):
	paddq		%mm2, %mm1	C rl += prod
	paddq		%mm1, %mm4	C mm4 = prod + cy
	movd		4(%edx), %mm3	C rl = rp[0]
	movd		%mm4, (%edx)
	psrlq		$32, %mm4
	paddq		%mm0, %mm3	C rl += prod
	paddq		%mm3, %mm4	C mm4 = prod + cy
	movd		%mm4, 4(%edx)
	psrlq		$32, %mm4
L(rtn):
	movd	%mm4, %eax
	emms
	ret

EPILOGUE()
