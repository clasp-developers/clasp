dnl  Intel Pentium-4 mpn_mul_basecase -- mpn by mpn multiplication.

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


C P4: 6.0 cycles/crossproduct (approx)


C void mpn_mul_basecase (mp_ptr wp,
C                        mp_srcptr xp, mp_size_t xsize,
C                        mp_srcptr yp, mp_size_t ysize);
C
C Nothing special here, basically just mpn/generic/mul_basecase.c done with
C mpn_mul_1 and mpn_addmul_1 inline.  As per mpn_addmul_1, the dependent
C chain in the inner loop is 4 c/l, but measures about 6.
C
C Enhancements:
C
C Perhaps some sort of vertical method would suit, though there'd be branch
C mispredictions on the end sections.  But it's not clear how to get less
C than 4 instructions per crossproduct, and unless that can be done then a
C basic addmul_1 style may as well be used (assuming it can be brought up to
C its proper 4 c/l).

defframe(PARAM_YSIZE, 20)
defframe(PARAM_YP,    16)
defframe(PARAM_XSIZE, 12)
defframe(PARAM_XP,    8)
defframe(PARAM_WP,    4)

define(SAVE_EBX,`PARAM_XP')
define(SAVE_ESI,`PARAM_YP')
define(SAVE_EDI,`PARAM_YSIZE')
define(SAVE_EBP,`PARAM_WP')

	TEXT
	ALIGN(8)
PROLOGUE(mpn_mul_basecase)
deflit(`FRAME',0)

	movl	PARAM_XP, %eax
	movl	%ebx, SAVE_EBX
	pxor	%mm0, %mm0		C initial carry

	movl	PARAM_YP, %edx
	movl	%esi, SAVE_ESI

	movl	PARAM_WP, %ebx
	movl	%ebp, SAVE_EBP
	movl	%eax, %esi		C xp

	movd	(%edx), %mm7		C yp[0]

	movl	PARAM_XSIZE, %ecx

	movl	PARAM_YSIZE, %ebp
	movl	%edi, SAVE_EDI
	movl	%ebx, %edi		C wp

L(mul1):
	C eax	xp, incrementing
	C ebx	wp, incrementing
	C ecx	xsize, decrementing
	C edx	yp
	C esi	xp
	C edi	wp
	C ebp	ysize
	C
	C mm0	carry limb
	C mm7	multiplier

	movd	(%eax), %mm1
	addl	$4, %eax
	pmuludq	%mm7, %mm1
	paddq	%mm1, %mm0
	movd	%mm0, (%ebx)
	addl	$4, %ebx
	psrlq	$32, %mm0
	subl	$1, %ecx
	jnz	L(mul1)

	movd	%mm0, (%ebx)

	subl	$1, %ebp
	jz	L(done)


L(outer):
	C eax
	C ebx
	C ecx
	C edx	yp, incrementing
	C esi	xp
	C edi	wp, incrementing
	C ebp	ysize, decrementing

	movl	%esi, %eax		C xp

	leal	4(%edi), %ebx		C next wp
	addl	$4, %edi

	movd	4(%edx), %mm7		C next yp limb
	addl	$4, %edx

	pxor	%mm0, %mm0		C initial carry

	movl	PARAM_XSIZE, %ecx


L(inner):
	C eax	xp, incrementing
	C ebx	wp, incrementing
	C ecx	xsize, decrementing
	C edx	outer yp
	C esi	outer xp
	C edi	outer wp
	C ebp	outer ysize

	movd	(%eax), %mm1
	leal	4(%eax), %eax
	movd	(%ebx),%mm2
	pmuludq	%mm7, %mm1
	paddq	%mm2, %mm1
	paddq	%mm1, %mm0
	subl	$1, %ecx
	movd	%mm0, (%ebx)
	psrlq	$32, %mm0
	leal	4(%ebx), %ebx
	jnz	L(inner)

	movd	%mm0, (%ebx)

	subl	$1, %ebp
	jnz	L(outer)


L(done):
	movl	SAVE_EBX, %ebx
	movl	SAVE_ESI, %esi
	movl	SAVE_EDI, %edi
	movl	SAVE_EBP, %ebp
	emms
	ret

EPILOGUE()
