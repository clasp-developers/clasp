dnl  AMD K6 mpn_gcd_finda.

dnl  Copyright 2000, 2002, 2004 Free Software Foundation, Inc.
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


C K6: 680 cycles (approx) on average


dnl  How many trial subtractions to attempt before launching into a full
dnl  division.

deflit(TRIAL_SUBS, 8)


C mp_limb_t mpn_gcd_finda (const mp_limb_t cp[2]);
C
C This code is probably not optimal, but it's already a good improvement
C over the generic C.
C

defframe(PARAM_CP, 4)

defframe(SAVE_EBX,      -4)
defframe(SAVE_ESI,      -8)
defframe(SAVE_EDI,     -12)
defframe(SAVE_EBP,     -16)

defframe(VAR_N2H,      -20)
defframe(VAR_N2L,      -24)
defframe(VAR_Q,        -28)
defframe(VAR_N2L_NORM, -32)

deflit(STACK_SPACE, 32)

	TEXT
	ALIGN(32)

PROLOGUE(mpn_gcd_finda)
deflit(`FRAME',0)

	movl	PARAM_CP, %eax
	subl	$STACK_SPACE, %esp
deflit(`FRAME',STACK_SPACE)

	movl	%ebx, SAVE_EBX

	movl	%esi, SAVE_ESI
	movl	(%eax), %ecx

	movl	%edi, SAVE_EDI
	movl	4(%eax), %edx

	movl	%ebp, SAVE_EBP

	ASSERT(nz,`orl %ecx, %ecx')
	ASSERT(nz,`orl %edx, %edx')

	movl	%ecx, %eax
	movl	%edx, %ebx

	negl	%eax
	notl	%ebx

	cmpl	%ecx, %eax
	movl	%ebx, %esi

	sbbl	%edx, %esi

	jb	L(top)

	movl	%ecx, %eax
	movl	%edx, %ebx

	negl	%ecx
	notl	%edx

	jmp	L(top)


	ALIGN(8)
L(restore):
	C eax	n2 l
	C ebx	n2 h
	C ecx	n1-n2 l
	C edx	n1-n2 h
	C esi	old n1 h
	C edi
	C ebp

	movl	%ebx, %edx
	movl	%esi, %ebx

	movl	%eax, %esi
	addl	%ecx, %eax

	movl	%esi, %ecx


L(top):
	C n1 >= n2
	C
	C eax	n2 l
	C ebx	n2 h
	C ecx	n1 l
	C edx	n1 h
	C esi
	C edi
	C ebp

	orl	%ebx, %ebx
	jz	L(done)

L(entry):
	subl	%eax, %ecx
	sbbl	%ebx, %edx
	ASSERT(nc)

forloop(i,1,TRIAL_SUBS,`
	movl	%edx, %esi
	subl	%eax, %ecx

	sbbl	%ebx, %edx
	jc	L(restore)
')


	C n1 >= n2
	C
	C eax	n2 l
	C ebx	n2 h
	C ecx	n1 l
	C edx	n1 h
	C esi
	C edi
	C ebp

	movl	%eax, VAR_N2L
	movl	%ecx, %esi		C n1l

	bsrl	%ebx, %ecx

	movl	%ebx, VAR_N2H
	notl	%ecx			C n2h leading zeros (low 5 bits)

	shldl(	%cl, %eax, %ebx)	C n2h normalized

	shll	%cl, %eax		C n2l normalized
	movl	%edx, %edi		C n1h

	movl	%eax, VAR_N2L_NORM
	xorl	%ebp, %ebp

	shldl(	%cl, %edi, %ebp)	C n1h shifted
	shldl(	%cl, %esi, %edi)	C n1m shifted

	shll	%cl, %esi		C n1l shifted
	movl	%ebp, %edx

	movl	%edi, %eax

	divl	%ebx			C n1h:n1m / n2h

	movl	%edx, %edi		C n1h:n1m:n1l - q*n2h
	movl	VAR_N2L_NORM, %edx

	mull	%edx			C q*n2l

	subl	%eax, %esi
	movl	VAR_N2L_NORM, %ebp

	sbbl	%edx, %edi		C n1h:n1m:n1l - q*(n2h:n2l)

	jnc	L(div_done)
	addl	%ebp, %esi

	adcl	%ebx, %edi		C addback n2h:n2l

	jc	L(div_done)
	addl	%ebp, %esi

	adcl	%ebx, %edi		C further addback n2h:n2l
	ASSERT(c)

L(div_done):
	shrdl(	%cl, %edi, %esi)

	shrl	%cl, %edi		C unshift n1m:n1l remainder
	movl	%esi, %eax

	movl	VAR_N2L, %ecx
	movl	%edi, %ebx

	movl	VAR_N2H, %edx
	orl	%ebx, %ebx

	jnz	L(entry)


L(done):
	movl	SAVE_EBX, %ebx
	movl	SAVE_ESI, %esi
	movl	SAVE_EDI, %edi
	movl	SAVE_EBP, %ebp
	addl	$STACK_SPACE, %esp
	ret

EPILOGUE()
