dnl  Intel P6 mpn_mod_1 -- mpn by limb remainder.

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


C P6: 21.5 cycles/limb


C mp_limb_t mpn_mod_1 (mp_srcptr src, mp_size_t size, mp_limb_t divisor);
C mp_limb_t mpn_mod_1c (mp_srcptr src, mp_size_t size, mp_limb_t divisor,
C                       mp_limb_t carry);
C mp_limb_t mpn_preinv_mod_1 (mp_srcptr src, mp_size_t size, mp_limb_t divisor,
C                             mp_limb_t inverse);
C
C The code here is in two parts, a simple divl loop and a mul-by-inverse.
C The divl is used by mod_1 and mod_1c for small sizes, until the savings in
C the mul-by-inverse can overcome the time to calculate an inverse.
C preinv_mod_1 goes straight to the mul-by-inverse.
C
C The mul-by-inverse normalizes the divisor (or for preinv_mod_1 it's
C already normalized).  The calculation done is r=a%(d*2^n) followed by a
C final (r*2^n)%(d*2^n), where a is the dividend, d the divisor, and n is
C the number of leading zero bits on d.  This means there's no bit shifts in
C the main loop, at the cost of an extra divide step at the end.
C
C The simple divl for mod_1 is able to skip one divide step if high<divisor.
C For mod_1c the carry parameter is the high of the first divide step, and
C no attempt is make to skip that step since carry==0 will be very rare.
C
C The mul-by-inverse always skips one divide step, but then needs an extra
C step at the end, unless the divisor was already normalized (n==0).  This
C leads to different mul-by-inverse thresholds for normalized and
C unnormalized divisors, in mod_1 and mod_1c.
C
C Alternatives:
C
C If n is small then the extra divide step could be done by a few shift and
C trial subtract steps instead of a full divide.  That would probably be 3
C or 4 cycles/bit, so say up to n=8 might benefit from that over a 21 cycle
C divide.  However it's considered that small divisors, meaning biggish n,
C are more likely than small n, and that it's not worth the branch
C mispredicts of a loop.
C
C Past:
C
C There used to be some MMX based code for P-II and P-III, roughly following
C the K7 form, but it was slower (about 24.0 c/l) than the code here.  That
C code did have an advantage that mod_1 was able to do one less divide step
C when high<divisor and the divisor unnormalized, but the speed advantage of
C the current code soon overcomes that.
C
C Future:
C
C It's not clear whether what's here is optimal.  A rough count of micro-ops
C on the dependent chain would suggest a couple of cycles could be shaved,
C perhaps.


dnl  The following thresholds are the sizes where the multiply by inverse
dnl  method is used instead of plain divl's.  Minimum value 2 each.
dnl
dnl  MUL_NORM_THRESHOLD is for normalized divisors (high bit set),
dnl  MUL_UNNORM_THRESHOLD for unnormalized divisors.
dnl
dnl  With the divl loop at 39 c/l, and the inverse loop at 21.5 c/l but
dnl  setups for the inverse of about 50, the threshold should be around
dnl  50/(39-21.5)==2.85.  An unnormalized divisor gets an extra divide step
dnl  at the end, so if that's about 25 cycles then that threshold might be
dnl  around (50+25)/(39-21.5) == 4.3.

deflit(MUL_NORM_THRESHOLD,   4)
deflit(MUL_UNNORM_THRESHOLD, 5)

deflit(MUL_NORM_DELTA, eval(MUL_NORM_THRESHOLD - MUL_UNNORM_THRESHOLD))


defframe(PARAM_INVERSE, 16)  dnl  mpn_preinv_mod_1
defframe(PARAM_CARRY,   16)  dnl  mpn_mod_1c
defframe(PARAM_DIVISOR, 12)
defframe(PARAM_SIZE,     8)
defframe(PARAM_SRC,      4)

defframe(SAVE_EBX,    -4)
defframe(SAVE_ESI,    -8)
defframe(SAVE_EDI,    -12)
defframe(SAVE_EBP,    -16)

defframe(VAR_NORM,    -20)
defframe(VAR_INVERSE, -24)

deflit(STACK_SPACE, 24)

	TEXT

	ALIGN(16)
PROLOGUE(mpn_preinv_mod_1)
deflit(`FRAME',0)

	movl	PARAM_SRC, %edx
	subl	$STACK_SPACE, %esp	FRAME_subl_esp(STACK_SPACE)

	movl	%ebx, SAVE_EBX
	movl	PARAM_SIZE, %ebx

	movl	%ebp, SAVE_EBP
	movl	PARAM_DIVISOR, %ebp

	movl	%esi, SAVE_ESI
	movl	PARAM_INVERSE, %eax

	movl	%edi, SAVE_EDI
	movl	-4(%edx,%ebx,4), %edi	C src high limb

	movl	$0, VAR_NORM
	leal	-8(%edx,%ebx,4), %ecx	C &src[size-2]

	C

	movl	%edi, %esi
	subl	%ebp, %edi		C high-divisor

	cmovc(	%esi, %edi)		C restore if underflow
	decl	%ebx
	jnz	L(preinv_entry)

	jmp	L(done_edi)

EPILOGUE()


	ALIGN(16)
PROLOGUE(mpn_mod_1c)
deflit(`FRAME',0)

	movl	PARAM_SIZE, %ecx
	subl	$STACK_SPACE, %esp	FRAME_subl_esp(STACK_SPACE)

	movl	%ebp, SAVE_EBP
	movl	PARAM_DIVISOR, %eax

	movl	%esi, SAVE_ESI
	movl	PARAM_CARRY, %edx

	movl	PARAM_SRC, %esi
	orl	%ecx, %ecx
	jz	L(done_edx)		C result==carry if size==0

	sarl	$31, %eax
	movl	PARAM_DIVISOR, %ebp

	andl	$MUL_NORM_DELTA, %eax

	addl	$MUL_UNNORM_THRESHOLD, %eax

	cmpl	%eax, %ecx
	jb	L(divide_top)


	C The carry parameter pretends to be the src high limb.

	movl	%ebx, SAVE_EBX
	leal	1(%ecx), %ebx		C size+1

	movl	%edx, %eax		C carry
	jmp	L(mul_by_inverse_1c)

EPILOGUE()


	ALIGN(16)
PROLOGUE(mpn_mod_1)
deflit(`FRAME',0)

	movl	PARAM_SIZE, %ecx
	subl	$STACK_SPACE, %esp	FRAME_subl_esp(STACK_SPACE)
	movl	$0, %edx		C initial carry (if can't skip a div)

	movl	%esi, SAVE_ESI
	movl	PARAM_SRC, %eax

	movl	%ebp, SAVE_EBP
	movl	PARAM_DIVISOR, %ebp

	movl	PARAM_DIVISOR, %esi
	orl	%ecx, %ecx
	jz	L(done_edx)

	movl	-4(%eax,%ecx,4), %eax	C src high limb

	sarl	$31, %ebp

	andl	$MUL_NORM_DELTA, %ebp

	addl	$MUL_UNNORM_THRESHOLD, %ebp
	cmpl	%esi, %eax		C carry flag if high<divisor

	cmovc(	%eax, %edx)		C src high limb as initial carry
	movl	PARAM_SRC, %esi

	sbbl	$0, %ecx		C size-1 to skip one div
	jz	L(done_eax)		C done if had size==1

	cmpl	%ebp, %ecx
	movl	PARAM_DIVISOR, %ebp
	jae	L(mul_by_inverse)


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


L(done_edx):
	movl	%edx, %eax
L(done_eax):
	movl	SAVE_ESI, %esi

	movl	SAVE_EBP, %ebp
	addl	$STACK_SPACE, %esp

	ret


C -----------------------------------------------------------------------------

L(mul_by_inverse):
	C eax	src high limb
	C ebx
	C ecx
	C edx
	C esi	src
	C edi
	C ebp	divisor

	movl	%ebx, SAVE_EBX
	movl	PARAM_SIZE, %ebx

L(mul_by_inverse_1c):
	bsrl	%ebp, %ecx		C 31-l

	movl	%edi, SAVE_EDI
	xorl	$31, %ecx		C l

	movl	%ecx, VAR_NORM
	shll	%cl, %ebp		C d normalized

	movl	%eax, %edi		C src high -> n2
	subl	%ebp, %eax

	cmovnc(	%eax, %edi)		C n2-divisor if no underflow

	movl	$-1, %eax
	movl	$-1, %edx

	subl	%ebp, %edx		C (b-d)-1 so  edx:eax = b*(b-d)-1
	leal	-8(%esi,%ebx,4), %ecx	C &src[size-2]

	divl	%ebp			C floor (b*(b-d)-1) / d

L(preinv_entry):
	movl	%eax, VAR_INVERSE



C No special scheduling of loads is necessary in this loop, out of order
C execution hides the latencies already.
C
C The way q1+1 is generated in %ebx and d is moved to %eax for the multiply
C seems fastest.  The obvious change to generate q1+1 in %eax and then just
C multiply by %ebp (as per mpn/x86/pentium/mod_1.asm in fact) runs 1 cycle
C slower, for no obvious reason.


	ALIGN(16)
L(inverse_top):
	C eax	n10 (then scratch)
	C ebx	scratch (nadj, q1)
	C ecx	src pointer, decrementing
	C edx	scratch
	C esi	n10
	C edi	n2
	C ebp	divisor

	movl	(%ecx), %eax	   C next src limb
	movl	%eax, %esi

	sarl	$31, %eax	   C -n1
	movl	%ebp, %ebx

	andl	%eax, %ebx	   C -n1 & d
	negl	%eax		   C n1

	addl	%edi, %eax         C n2+n1

	mull	VAR_INVERSE        C m*(n2+n1)

	addl	%esi, %ebx         C nadj = n10 + (-n1 & d), ignoring overflow
	subl	$4, %ecx

	C

	addl	%ebx, %eax         C m*(n2+n1) + nadj, low giving carry flag
	leal	1(%edi), %ebx      C n2+1
	movl	%ebp, %eax	   C d

	adcl	%edx, %ebx         C 1 + high(n2<<32 + m*(n2+n1) + nadj) = q1+1
	jz	L(q1_ff)

	mull	%ebx		   C (q1+1)*d

	C

	subl	%eax, %esi	   C low n - (q1+1)*d

	sbbl	%edx, %edi	   C high n - (q1+1)*d, 0 or -1

	andl	%ebp, %edi	   C d if underflow

	addl	%esi, %edi	   C remainder with addback if necessary

	cmpl	PARAM_SRC, %ecx
	jae	L(inverse_top)


C -----------------------------------------------------------------------------
L(inverse_loop_done):

	C %edi is the remainder modulo d*2^n and now must be reduced to
	C 0<=r<d by calculating r*2^n mod d*2^n and then right shifting by
	C n.  If d was already normalized on entry so that n==0 then nothing
	C is needed here.  The chance of n==0 is low, but it's true of say
	C PP from gmp-impl.h.
	C
	C eax
	C ebx
	C ecx
	C edx
	C esi
	C edi	remainder
	C ebp	divisor (normalized)

	movl	VAR_NORM, %ecx
	movl	$0, %esi

	orl	%ecx, %ecx
	jz	L(done_edi)


	C Here use %edi=n10 and %esi=n2, opposite to the loop above.
	C
	C The q1=0xFFFFFFFF case is handled with an sbbl to adjust q1+1
	C back, rather than q1_ff special case code.  This is simpler and
	C costs only 2 uops.

	shldl(	%cl, %edi, %esi)

	shll	%cl, %edi

	movl	%edi, %eax	   C n10
	movl	%ebp, %ebx	   C d

	sarl	$31, %eax          C -n1

	andl	%eax, %ebx         C -n1 & d
	negl	%eax		   C n1

	addl	%edi, %ebx         C nadj = n10 + (-n1 & d), ignoring overflow
	addl	%esi, %eax	   C n2+n1

	mull	VAR_INVERSE        C m*(n2+n1)

	C

	addl	%ebx, %eax         C m*(n2+n1) + nadj, low giving carry flag
	leal	1(%esi), %ebx      C n2+1

	adcl	%edx, %ebx         C 1 + high(n2<<32 + m*(n2+n1) + nadj) = q1+1

	sbbl	$0, %ebx
	movl	%ebp, %eax	   C d

	mull	%ebx		   C (q1+1)*d

	movl	SAVE_EBX, %ebx

	C

	subl	%eax, %edi	   C low  n - (q1+1)*d is remainder

	sbbl	%edx, %esi	   C high n - (q1+1)*d, 0 or -1

	andl	%ebp, %esi
	movl	SAVE_EBP, %ebp

	leal	(%esi,%edi), %eax  C remainder
	movl	SAVE_ESI, %esi

	shrl	%cl, %eax	   C denorm remainder
	movl	SAVE_EDI, %edi
	addl	$STACK_SPACE, %esp

	ret


L(done_edi):
	movl	SAVE_EBX, %ebx
	movl	%edi, %eax

	movl	SAVE_ESI, %esi

	movl	SAVE_EDI, %edi

	movl	SAVE_EBP, %ebp
	addl	$STACK_SPACE, %esp

	ret


C -----------------------------------------------------------------------------
C
C Special case for q1=0xFFFFFFFF, giving q=0xFFFFFFFF meaning the low dword
C of q*d is simply -d and the remainder n-q*d = n10+d.
C
C This is reached only very rarely.

L(q1_ff):
	C eax	(divisor)
	C ebx	(q1+1 == 0)
	C ecx	src pointer
	C edx
	C esi	n10
	C edi	(n2)
	C ebp	divisor

	leal	(%ebp,%esi), %edi	C n-q*d remainder -> next n2

	cmpl	PARAM_SRC, %ecx
	jae	L(inverse_top)

	jmp	L(inverse_loop_done)


EPILOGUE()
