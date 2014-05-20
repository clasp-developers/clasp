dnl  Intel Pentium-4 mpn_mod_1 -- mpn by limb remainder.

dnl  Copyright 2001, 2002, 2003 Free Software Foundation, Inc.
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


dnl  P4: 31 cycles/limb.


C mp_limb_t mpn_mod_1 (mp_srcptr src, mp_size_t size, mp_limb_t divisor);
C mp_limb_t mpn_mod_1c (mp_srcptr src, mp_size_t size, mp_limb_t divisor,
C                       mp_limb_t carry);
C mp_limb_t mpn_preinv_mod_1 (mp_srcptr src, mp_size_t size, mp_limb_t divisor,
C                             mp_limb_t inverse);
C
C An idea was tried in the mul-by-inverse to process the last limb by a jump
C back to the top of the loop skipping the -4(%esi) fetch.  But that seemed
C to produce slightly strange timings, like 9 and 10 limb operations about
C the same speed.  The jump would be successively taken and not-taken, which
C in theory should predict ok, but perhaps isn't enjoyed by the chip.
C Duplicating the loop for the last limb seems to be a couple of cycles
C quicker too.
C
C Enhancements:
C
C The loop measures 31 cycles, but the dependent chain would suggest it
C could be done with 30.  Not sure where to start looking for the extra
C cycle.


dnl  MUL_THRESHOLD is the size at which the multiply by inverse method is
dnl  used, rather than plain "divl"s.  Minimum value 2.
dnl
dnl  The inverse takes about 80-90 cycles to calculate, but after that the
dnl  multiply is 31 c/l versus division at about 58 c/l.

deflit(MUL_THRESHOLD, 5)


defframe(PARAM_INVERSE,16)  dnl mpn_preinv_mod_1
defframe(PARAM_CARRY,  16)  dnl mpn_mod_1c
defframe(PARAM_DIVISOR,12)
defframe(PARAM_SIZE,    8)
defframe(PARAM_SRC,     4)

dnl  re-use parameter space
define(SAVE_ESI,`PARAM_SIZE')
define(SAVE_EBP,`PARAM_SRC')

	TEXT

	ALIGN(16)
PROLOGUE(mpn_preinv_mod_1)
deflit(`FRAME',0)

	movl	PARAM_SIZE, %ecx
	movl	%esi, SAVE_ESI
	movl	$32, %eax

	movd	%eax, %mm6			C l = 0, so 32-l = 32
	movl	PARAM_SRC, %esi
	movl	%ebp, SAVE_EBP

	movd	PARAM_DIVISOR, %mm5
	pxor	%mm7, %mm7			C l = 0

	movd	-4(%esi,%ecx,4), %mm0		C src high limb
	leal	-8(%esi,%ecx,4), %esi		C &src[size-2]

	movd	PARAM_INVERSE, %mm4
	subl	$2, %ecx			C size-2

	psubq	%mm5, %mm0			C high-divisor
	movq	%mm0, %mm2

	psrlq	$32, %mm0			C -1 if underflow

	pand	%mm5, %mm0			C divisor if underflow

	paddq	%mm2, %mm0			C addback if underflow
	jz	L(inverse_last)			C if size==2
	ja	L(inverse_top)			C if size>2


	C if size==1
	movl	SAVE_ESI, %esi
	movd	%mm0, %eax
	emms
	ret

EPILOGUE()


	ALIGN(16)
PROLOGUE(mpn_mod_1c)
deflit(`FRAME',0)
	movl	PARAM_SIZE, %ecx
	movl	%esi, SAVE_ESI

	movl	PARAM_SRC, %esi
	movl	%ebp, SAVE_EBP

	movl	PARAM_CARRY, %edx
	orl	%ecx, %ecx
	jz	L(divide_done)		C result==carry if size==0

	movl	PARAM_DIVISOR, %ebp
	jmp	L(start_1c)

EPILOGUE()


	ALIGN(16)
PROLOGUE(mpn_mod_1)
deflit(`FRAME',0)

	movl	PARAM_SIZE, %ecx
	movl	%esi, SAVE_ESI

	movl	PARAM_SRC, %esi
	movl	%ebp, SAVE_EBP

	movl	PARAM_DIVISOR, %ebp
	xorl	%edx, %edx		C result 0 if size==0

	orl	%ecx, %ecx
	jz	L(divide_done)
	movl	-4(%esi,%ecx,4), %eax	C src high limb

	leal	-1(%ecx), %edx
	cmpl	%ebp, %eax		C c if high<divisor

	cmovc(	%edx, %ecx)		C size-1 if high<divisor

	movl	$0, %edx		C initial carry
	cmovc(	%eax, %edx)		C src high limb if high<divisor

	orl	%ecx, %ecx
	jz	L(divide_done)		C if size==1 and skip div


L(start_1c):
	C eax
	C ebx
	C ecx	size
	C edx	carry
	C esi	src
	C edi
	C ebp	divisor

	leal	-4(%esi,%ecx,4), %esi	C &src[size-1]
	cmpl	$MUL_THRESHOLD, %ecx
	jae	L(mul_by_inverse)


L(divide_top):
	C eax
	C ebx
	C ecx	counter, limbs, decrementing
	C edx	remainder
	C esi	src, decrementing
	C edi
	C ebp	divisor

	movl	(%esi), %eax
	subl	$4, %esi

	divl	%ebp

	subl	$1, %ecx
	jnz	L(divide_top)


L(divide_done):
	movl	SAVE_ESI, %esi
	movl	SAVE_EBP, %ebp
	movl	%edx, %eax
	ret


C -----------------------------------------------------------------------------

L(mul_by_inverse):
	C eax
	C ebx
	C ecx	size
	C edx	carry
	C esi	src
	C edi
	C ebp	divisor

	bsrl	%ebp, %eax		C 31-l

	movd	%edx, %mm1		C carry
	movl	%ecx, %edx		C size
	movl	$31, %ecx

	C

	xorl	%eax, %ecx		C l = leading zeros on d
	addl	$1, %eax		C 32-l

	shll	%cl, %ebp		C normalize d
	movd	%ecx, %mm7		C l
	leal	-1(%edx), %ecx		C size-1

	movd	%eax, %mm6		C 32-l
	movl	$-1, %edx
	movl	$-1, %eax

	C

	subl	%ebp, %edx		C (b-d)-1 so edx:eax = b*(b-d)-1

	divl	%ebp			C floor (b*(b-d)-1 / d)

	movd	%ebp, %mm5		C d
	movd	(%esi), %mm0		C src high limb
	punpckldq %mm1, %mm0
	psrlq	%mm6, %mm0		C n2 = high (carry:srchigh << l)

	C

	movd	%eax, %mm4		C m


C The dependent chain here consists of
C
C	2   paddd    n1+n2
C	8   pmuludq  m*(n1+n2)
C	2   paddq    n2:nadj + m*(n1+n2)
C	2   psrlq    q1
C	8   pmuludq  d*q1
C	2   psubq    (n-d)-q1*d
C	2   psrlq    high mask
C	2   pand     d masked
C	2   paddd    n2+d addback
C	--
C	30
C
C But it seems to run at 31 cycles, so presumably there's something else
C going on.


	ALIGN(16)
L(inverse_top):
	C eax
	C ebx
	C ecx	counter, size-1 to 1
	C edx
	C esi	src, decrementing
	C edi
	C ebp
	C
	C mm0	n2
	C mm4	m
	C mm5	d
	C mm6	32-l
	C mm7	l

	ASSERT(b,`C n2<d
	 movd	%mm0, %eax
	 movd	%mm5, %edx
	 cmpl	%edx, %eax')

	movd	-4(%esi), %mm1		C next src limbs
	movd	(%esi), %mm2
	leal	-4(%esi), %esi

	punpckldq %mm2, %mm1
	psrlq	%mm6, %mm1		C n10

	movq	%mm1, %mm2		C n10
	movq	%mm1, %mm3		C n10
	psrad	$31, %mm1		C -n1
	pand	%mm5, %mm1		C -n1 & d
	paddd	%mm2, %mm1		C nadj = n10+(-n1&d), ignore overflow

	psrld	$31, %mm2		C n1
	paddd	%mm0, %mm2		C n2+n1
	punpckldq %mm0, %mm1		C n2:nadj

	pmuludq	%mm4, %mm2		C m*(n2+n1)

	paddq	%mm2, %mm1		C n2:nadj + m*(n2+n1)

	psrlq	$32, %mm1		C q1 = high(n2:nadj + m*(n2+n1))

	pmuludq	%mm5, %mm1		C q1*d
	punpckldq %mm0, %mm3		C n
	psubq	%mm5, %mm3		C n - d
	pxor	%mm0, %mm0

	psubq	%mm1, %mm3		C n - (q1+1)*d

	por	%mm3, %mm0		C remainder -> n2
	psrlq	$32, %mm3		C high n - (q1+1)*d, 0 or -1

	ASSERT(be,`C 0 or -1
	 movd	%mm3, %eax
	 addl	$1, %eax
	 cmpl	$1, %eax')

	pand	%mm5, %mm3		C mask & d

	paddd	%mm3, %mm0		C addback if necessary

	subl	$1, %ecx
	jnz	L(inverse_top)


	C Least significant limb.
	C Same code as the loop, but there's no -4(%esi) limb to fetch.

L(inverse_last):
	C eax
	C ebx
	C ecx
	C edx
	C esi	&src[0]
	C
	C mm0	n2
	C mm4	m
	C mm5	d
	C mm6	32-l
	C mm7	l

	movd	(%esi), %mm1		C src[0]
	psllq	%mm7, %mm1		C n10

	movq	%mm1, %mm2		C n10
	movq	%mm1, %mm3		C n10
	psrad	$31, %mm1		C -n1
	pand	%mm5, %mm1		C -n1 & d
	paddd	%mm2, %mm1		C nadj = n10+(-n1&d), ignore overflow

	psrld	$31, %mm2		C n1
	paddd	%mm0, %mm2		C n2+n1
	punpckldq %mm0, %mm1		C n2:nadj

	pmuludq	%mm4, %mm2		C m*(n2+n1)

	paddq	%mm2, %mm1		C n2:nadj + m*(n2+n1)

	psrlq	$32, %mm1		C q1 = high(n2:nadj + m*(n2+n1))

	pmuludq	%mm5, %mm1		C q1*d
	punpckldq %mm0, %mm3		C n
	psubq	%mm5, %mm3		C n - d
	pxor	%mm0, %mm0

	psubq	%mm1, %mm3		C n - (q1+1)*d

	por	%mm3, %mm0		C remainder -> n2
	psrlq	$32, %mm3		C high n - (q1+1)*d, 0 or -1

	ASSERT(be,`C 0 or -1
	 movd	%mm3, %eax
	 addl	$1, %eax
	 cmpl	$1, %eax')

	movl	SAVE_EBP, %ebp
	pand	%mm5, %mm3		C mask & d

	movl	SAVE_ESI, %esi
	paddd	%mm3, %mm0		C addback if necessary

	psrld	%mm7, %mm0

	movd	%mm0, %eax

	emms
	ret

EPILOGUE()
