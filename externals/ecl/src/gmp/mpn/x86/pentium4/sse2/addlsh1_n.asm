dnl  Intel Pentium-4 mpn_addlsh1_n -- mpn x+2*y.

dnl  Copyright 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
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


C        cycles/limb (approx)
C      dst!=src1,2  dst==src1  dst==src2
C P4:      4.5         7.25       6.75


C mp_limb_t mpn_addlsh1_n (mp_ptr dst, mp_srcptr src1, mp_srcptr src2,
C                          mp_size_t size);
C
C The slightly strange combination of indexing and pointer incrementing
C that's used seems to work best.  Not sure why, but %ecx,4 with src1 and/or
C src2 is a slowdown.
C
C The dependent chain is simply the paddq of x+2*y to the previous carry,
C then psrlq to get the new carry.  That makes 4 c/l the target speed, which
C is almost achieved for separate src/dst but when src==dst the write
C combining anomalies slow it down.

defframe(PARAM_SIZE, 16)
defframe(PARAM_SRC2, 12)
defframe(PARAM_SRC1, 8)
defframe(PARAM_DST,  4)

dnl  re-use parameter space
define(SAVE_EBX,`PARAM_SRC1')

	TEXT
	ALIGN(16)

PROLOGUE(mpn_addlsh1_n)
deflit(`FRAME',0)

	movl	PARAM_SRC1, %eax
	movl	%ebx, SAVE_EBX

	movl	PARAM_SRC2, %ebx
	pxor	%mm0, %mm0		C initial carry

	movl	PARAM_DST, %edx

	movl	PARAM_SIZE, %ecx

	leal	(%edx,%ecx,4), %edx	C dst end
	negl	%ecx			C -size

L(top):
	C eax	src1 end
	C ebx	src2 end
	C ecx	counter, limbs, negative
	C edx	dst end
	C mm0	carry

	movd	(%eax), %mm1
	movd	(%ebx), %mm2
	leal	4(%eax), %eax
	leal	4(%ebx), %ebx
	paddq	%mm2, %mm1
	paddq	%mm2, %mm1

	paddq	%mm1, %mm0

	movd	%mm0, (%edx,%ecx,4)
	psrlq	$32, %mm0
	addl	$1, %ecx
	jnz	L(top)


	movl	SAVE_EBX, %ebx
	movd	%mm0, %eax
	emms
	ret

EPILOGUE()
