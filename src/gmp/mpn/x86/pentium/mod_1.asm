dnl  Intel P5 mpn_mod_1 -- mpn by limb remainder.

dnl  Copyright 1999, 2000, 2002 Free Software Foundation, Inc.
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


C P5: 28.0 cycles/limb


C mp_limb_t mpn_mod_1 (mp_srcptr src, mp_size_t size, mp_limb_t divisor);
C mp_limb_t mpn_mod_1c (mp_srcptr src, mp_size_t size, mp_limb_t divisor,
C                       mp_limb_t carry);
C mp_limb_t mpn_preinv_mod_1 (mp_srcptr src, mp_size_t size, mp_limb_t divisor,
C                             mp_limb_t inverse);
C
C This code is not unlike mpn/x86/p6/mod_1.asm, it does the same sort of
C multiply by inverse without on-the-fly shifts.  See that code for some
C general comments.
C
C Alternatives:
C
C P5 shldl is 4 cycles, so shifting on the fly would be at least 5 cycles
C slower, probably more depending what it did to register usage.  Using MMX
C on P55 would be better, but still at least 4 or 5 instructions and so 2 or
C 3 cycles.


dnl  These thresholds are the sizes where the multiply by inverse method is
dnl  used, rather than plain "divl"s.  Minimum value 2.
dnl
dnl  MUL_NORM_THRESHOLD is for an already normalized divisor (high bit set),
dnl  MUL_UNNORM_THRESHOLD for an unnormalized divisor.
dnl
dnl  With the divl loop at 44 c/l and the inverse at 28 c/l with about 70
dnl  cycles to setup, the threshold should be about ceil(70/16)==5, which is
dnl  what happens in practice.
dnl
dnl  An unnormalized divisor gets an extra 40 cycles at the end for the
dnl  final (r*2^n)%(d*2^n) and shift.  This increases the threshold by about
dnl  40/16=3.
dnl
dnl  PIC adds between 4 and 7 cycles (not sure why it varies), but this
dnl  doesn't change the thresholds.
dnl
dnl  The entry sequence code that chooses between MUL_NORM_THRESHOLD and
dnl  MUL_UNNORM_THRESHOLD is a bit horrible, but it adds only 2 cycles
dnl  (branch free) and ensures the choice between div or mul is optimal.

deflit(MUL_NORM_THRESHOLD,   ifdef(`PIC',5,5))
deflit(MUL_UNNORM_THRESHOLD, ifdef(`PIC',8,8))

deflit(MUL_NORM_DELTA, eval(MUL_NORM_THRESHOLD - MUL_UNNORM_THRESHOLD))


defframe(PARAM_INVERSE, 16)   dnl mpn_preinv_mod_1
defframe(PARAM_CARRY,   16)   dnl mpn_mod_1c
defframe(PARAM_DIVISOR, 12)
defframe(PARAM_SIZE,     8)
defframe(PARAM_SRC,      4)

dnl  re-using parameter space
define(VAR_NORM,    `PARAM_DIVISOR')
define(VAR_INVERSE, `PARAM_SIZE')

	TEXT

	ALIGN(8)
PROLOGUE(mpn_preinv_mod_1)
deflit(`FRAME',0)

	pushl	%ebp	FRAME_pushl()
	pushl	%esi	FRAME_pushl()

	movl	PARAM_SRC, %esi
	movl	PARAM_SIZE, %edx

	pushl	%edi	FRAME_pushl()
	pushl	%ebx	FRAME_pushl()

	movl	PARAM_DIVISOR, %ebp
	movl	PARAM_INVERSE, %eax

	movl	-4(%esi,%edx,4), %edi	C src high limb
	leal	-8(%esi,%edx,4), %esi	C &src[size-2]

	movl	$0, VAR_NORM
	decl	%edx

	jnz	L(start_preinv)

	subl	%ebp, %edi		C src-divisor
	popl	%ebx

	sbbl	%ecx, %ecx		C -1 if underflow
	movl	%edi, %eax		C src-divisor

	andl	%ebp, %ecx		C d if underflow
	popl	%edi

	addl	%ecx, %eax		C remainder, with possible addback
	popl	%esi

	popl	%ebp

	ret

EPILOGUE()


	ALIGN(8)
PROLOGUE(mpn_mod_1c)
deflit(`FRAME',0)

	movl	PARAM_DIVISOR, %eax
	movl	PARAM_SIZE, %ecx

	sarl	$31, %eax			C d highbit
	movl	PARAM_CARRY, %edx

	orl	%ecx, %ecx
	jz	L(done_edx)			C result==carry if size==0

	andl	$MUL_NORM_DELTA, %eax
	pushl	%ebp		FRAME_pushl()

	addl	$MUL_UNNORM_THRESHOLD, %eax	C norm or unnorm thresh
	pushl	%esi		FRAME_pushl()

	movl	PARAM_SRC, %esi
	movl	PARAM_DIVISOR, %ebp

	cmpl	%eax, %ecx
	jb	L(divide_top)

	movl	%edx, %eax		C carry as pretend src high limb
	leal	1(%ecx), %edx		C size+1

	cmpl	$0x1000000, %ebp
	jmp	L(mul_by_inverse_1c)

EPILOGUE()


	ALIGN(8)
PROLOGUE(mpn_mod_1)
deflit(`FRAME',0)

	movl	PARAM_SIZE, %ecx
	pushl	%ebp		FRAME_pushl()

	orl	%ecx, %ecx
	jz	L(done_zero)

	movl	PARAM_SRC, %eax
	movl	PARAM_DIVISOR, %ebp

	sarl	$31, %ebp		C -1 if divisor normalized
	movl	-4(%eax,%ecx,4), %eax	C src high limb

	movl	PARAM_DIVISOR, %edx
	pushl	%esi		FRAME_pushl()

	andl	$MUL_NORM_DELTA, %ebp
	cmpl	%edx, %eax		C carry flag if high<divisor

	sbbl	%edx, %edx		C -1 if high<divisor
	addl	$MUL_UNNORM_THRESHOLD, %ebp C norm or unnorm thresh

	addl	%edx, %ecx		C size-1 if high<divisor
	jz	L(done_eax)

	cmpl	%ebp, %ecx
	movl	PARAM_DIVISOR, %ebp

	movl	PARAM_SRC, %esi
	jae	L(mul_by_inverse)

	andl	%eax, %edx		C high as initial carry if high<divisor


L(divide_top):
	C eax	scratch (quotient)
	C ebx
	C ecx	counter, limbs, decrementing
	C edx	scratch (remainder)
	C esi	src
	C edi
	C ebp	divisor

	movl	-4(%esi,%ecx,4), %eax

	divl	%ebp

	decl	%ecx
	jnz	L(divide_top)


	popl	%esi
	popl	%ebp

L(done_edx):
	movl	%edx, %eax

	ret


L(done_zero):
	xorl	%eax, %eax
	popl	%ebp

	ret


C -----------------------------------------------------------------------------
C
C The divisor is normalized using the same code as the pentium
C count_leading_zeros in longlong.h.  Going through the GOT for PIC costs a
C couple of cycles, but is more or less unavoidable.


	ALIGN(8)
L(mul_by_inverse):
	C eax	src high limb
	C ebx
	C ecx	size or size-1
	C edx
	C esi	src
	C edi
	C ebp	divisor

	movl	PARAM_SIZE, %edx
	cmpl	$0x1000000, %ebp

L(mul_by_inverse_1c):
	sbbl	%ecx, %ecx
	cmpl	$0x10000, %ebp

	sbbl	$0, %ecx
	cmpl	$0x100, %ebp

	sbbl	$0, %ecx
	pushl	%edi		FRAME_pushl()

	pushl	%ebx		FRAME_pushl()
	movl	%ebp, %ebx		C d

ifdef(`PIC',`
	call	L(here)
L(here):
	popl	%edi
	leal	25(,%ecx,8), %ecx	C 0,-1,-2,-3 -> 25,17,9,1

	shrl	%cl, %ebx
	addl	$_GLOBAL_OFFSET_TABLE_+[.-L(here)], %edi

	C AGI
	movl	__clz_tab@GOT(%edi), %edi
	addl	$-34, %ecx

	C AGI
	movb	(%ebx,%edi), %bl

',`
	leal	25(,%ecx,8), %ecx	C 0,-1,-2,-3 -> 25,17,9,1

	shrl	%cl, %ebx
	addl	$-34, %ecx

	C AGI
	movb	__clz_tab(%ebx), %bl
')
	movl	%eax, %edi		C carry -> n1

	addl	%ebx, %ecx		C -34 + c + __clz_tab[d>>c] = -clz-1
	leal	-8(%esi,%edx,4), %esi	C &src[size-2]

	xorl	$-1, %ecx		C clz
	movl	$-1, %edx

	ASSERT(e,`pushl	%eax		C clz calculation same as bsrl
		bsrl	%ebp, %eax
		xorl	$31, %eax
		cmpl	%eax, %ecx
		popl	%eax')

	shll	%cl, %ebp		C d normalized
	movl	%ecx, VAR_NORM

	subl	%ebp, %edx		C (b-d)-1, so edx:eax = b*(b-d)-1
	movl	$-1, %eax

	divl	%ebp			C floor (b*(b-d)-1) / d

L(start_preinv):
	movl	%eax, VAR_INVERSE
	movl	%ebp, %eax		C d

	movl	%ecx, %edx		C fake high, will cancel


C For mpn_mod_1 and mpn_preinv_mod_1, the initial carry in %edi is the src
C high limb, and this may be greater than the divisor and may need one copy
C of the divisor subtracted (only one, because the divisor is normalized).
C This is accomplished by having the initial ecx:edi act as a fake previous
C n2:n10.  The initial edx:eax is d, acting as a fake (q1+1)*d which is
C subtracted from ecx:edi, with the usual addback if it produces an
C underflow.


L(inverse_top):
	C eax	scratch (n10, n1, q1, etc)
	C ebx	scratch (nadj, src limit)
	C ecx	old n2
	C edx	scratch
	C esi	src pointer, &src[size-2] to &src[0]
	C edi	old n10
	C ebp	d

	subl	%eax, %edi	   C low  n - (q1+1)*d
	movl	(%esi), %eax	   C new n10

	sbbl	%edx, %ecx	   C high n - (q1+1)*d, 0 or -1
	movl	%ebp, %ebx	   C d

	sarl	$31, %eax	   C -n1
	andl	%ebp, %ecx	   C d if underflow

	addl	%edi, %ecx	   C remainder -> n2, and possible addback
	ASSERT(b,`cmpl %ebp, %ecx')
	andl	%eax, %ebx	   C -n1 & d

	movl	(%esi), %edi	   C n10
	andl	$1, %eax	   C n1

	addl	%ecx, %eax         C n2+n1
	addl	%edi, %ebx         C nadj = n10 + (-n1 & d), ignoring overflow

	mull	VAR_INVERSE        C m*(n2+n1)

	addl	%eax, %ebx         C low(m*(n2+n1) + nadj), giving carry flag
	leal	1(%ecx), %eax      C 1+n2

	adcl	%edx, %eax         C 1 + high[n2<<32 + m*(n2+n1) + nadj] = q1+1
	movl	PARAM_SRC, %ebx

	sbbl	$0, %eax	   C use q1 if q1+1 overflows
	subl	$4, %esi	   C step src ptr

	mull	%ebp		   C (q1+1)*d

	cmpl	%ebx, %esi
	jae	L(inverse_top)



	C %edi (after subtract and addback) is the remainder modulo d*2^n
	C and must be reduced to 0<=r<d by calculating r*2^n mod d*2^n and
	C right shifting by n.
	C
	C If d was already normalized on entry so that n==0 then nothing is
	C needed here.  This is always the case for preinv_mod_1.  For mod_1
	C or mod_1c the chance of n==0 is low, but about 40 cycles can be
	C saved.

	subl	%eax, %edi	   C low  n - (q1+1)*d
	movl	%ecx, %ebx	   C n2

	sbbl	%edx, %ebx	   C high n - (q1+1)*d, 0 or -1
	xorl	%esi, %esi	   C next n2

	andl	%ebp, %ebx	   C d if underflow
	movl	VAR_NORM, %ecx

	addl	%ebx, %edi	   C remainder, with possible addback
	orl	%ecx, %ecx

	jz	L(done_mul_edi)


	C Here using %esi=n2 and %edi=n10, unlike the above

	shldl(	%cl, %edi, %esi)   C n2

	shll	%cl, %edi	   C n10

	movl	%edi, %eax	   C n10
	movl	%edi, %ebx	   C n10

	sarl	$31, %ebx          C -n1

	shrl	$31, %eax          C n1
	andl	%ebp, %ebx         C -n1 & d

	addl	%esi, %eax	   C n2+n1
	addl	%edi, %ebx         C nadj = n10 + (-n1 & d), ignoring overflow

	mull	VAR_INVERSE        C m*(n2+n1)

	addl	%eax, %ebx         C m*(n2+n1) + nadj, low giving carry flag
	leal	1(%esi), %eax      C 1+n2

	adcl	%edx, %eax         C 1 + high(n2<<32 + m*(n2+n1) + nadj) = q1+1

	sbbl	$0, %eax	   C use q1 if q1+1 overflows

	mull	%ebp		   C (q1+1)*d

	subl	%eax, %edi	   C low  n - (q1+1)*d
	popl	%ebx

	sbbl	%edx, %esi	   C high n - (q1+1)*d, 0 or -1
	movl	%edi, %eax

	andl	%ebp, %esi	   C d if underflow
	popl	%edi

	addl	%esi, %eax	   C addback if underflow
	popl	%esi

	shrl	%cl, %eax	   C denorm remainder
	popl	%ebp

	ret


L(done_mul_edi):
	movl	%edi, %eax
	popl	%ebx

	popl	%edi
L(done_eax):
	popl	%esi

	popl	%ebp

	ret

EPILOGUE()
