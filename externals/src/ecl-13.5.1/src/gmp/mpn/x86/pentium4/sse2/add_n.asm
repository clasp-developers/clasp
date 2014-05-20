dnl  Intel Pentium-4 mpn_add_n -- mpn addition.

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


C P4 Willamette, Northwood: 4.0 cycles/limb if dst!=src1 and dst!=src2
C			    6.0 cycles/limb if dst==src1 or dst==src2
C P4 Prescott:		    >= 5 cycles/limb

C mp_limb_t mpn_add_n (mp_ptr dst, mp_srcptr src1, mp_srcptr src2,
C                      mp_size_t size);
C mp_limb_t mpn_add_nc (mp_ptr dst, mp_srcptr src1, mp_srcptr src2,
C                       mp_size_t size, mp_limb_t carry);
C
C The 4 c/l achieved here isn't particularly good, but is better than 9 c/l
C for a basic adc loop.

defframe(PARAM_CARRY,20)
defframe(PARAM_SIZE, 16)
defframe(PARAM_SRC2, 12)
defframe(PARAM_SRC1, 8)
defframe(PARAM_DST,  4)

dnl  re-use parameter space
define(SAVE_EBX,`PARAM_SRC1')

	TEXT
	ALIGN(8)

PROLOGUE(mpn_add_nc)
deflit(`FRAME',0)

	movd	PARAM_CARRY, %mm0
	jmp	L(start_nc)

EPILOGUE()

	ALIGN(8)
PROLOGUE(mpn_add_n)
deflit(`FRAME',0)

	pxor	%mm0, %mm0

L(start_nc):
	movl	PARAM_SRC1, %eax
	movl	%ebx, SAVE_EBX
	movl	PARAM_SRC2, %ebx
	movl	PARAM_DST, %edx
	movl	PARAM_SIZE, %ecx

	leal	(%eax,%ecx,4), %eax	C src1 end
	leal	(%ebx,%ecx,4), %ebx	C src2 end
	leal	(%edx,%ecx,4), %edx	C dst end
	negl	%ecx			C -size

L(top):
	C eax	src1 end
	C ebx	src2 end
	C ecx	counter, limbs, negative
	C edx	dst end
	C mm0	carry bit

	movd	(%eax,%ecx,4), %mm1
	movd	(%ebx,%ecx,4), %mm2
	paddq	%mm2, %mm1

	paddq	%mm1, %mm0
	movd	%mm0, (%edx,%ecx,4)

	psrlq	$32, %mm0

	addl	$1, %ecx
	jnz	L(top)


	movd	%mm0, %eax
	movl	SAVE_EBX, %ebx
	emms
	ret

EPILOGUE()
