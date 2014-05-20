dnl  AMD64 mpn_addlsh2_n and mpn_rsblsh2_n.  R = 2*V +- U.
dnl  ("rsb" means reversed subtract, name mandated by mpn_sublsh2_n which
dnl  subtacts the shifted operand from the unshifted operand.)

dnl  Copyright 2009 Free Software Foundation, Inc.

dnl  This file is part of the GNU MP Library.

dnl  The GNU MP Library is free software; you can redistribute it and/or modify
dnl  it under the terms of the GNU Lesser General Public License as published
dnl  by the Free Software Foundation; either version 3 of the License, or (at
dnl  your option) any later version.

dnl  The GNU MP Library is distributed in the hope that it will be useful, but
dnl  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
dnl  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
dnl  License for more details.

dnl  You should have received a copy of the GNU Lesser General Public License
dnl  along with the GNU MP Library.  If not, see http://www.gnu.org/licenses/.

include(`../config.m4')


C	     cycles/limb
C K8,K9:	 2
C K10:		 2
C P4:		 ?
C P6 core2: 	 3
C P6 corei7:	 2.75
C P6 atom:	 ?

C INPUT PARAMETERS
define(`rp',	`%rdi')
define(`up',	`%rsi')
define(`vp',	`%rdx')
define(`n',	`%rcx')

ifdef(`OPERATION_addlsh2_n',`
  define(ADDSUB,        `add')
  define(ADCSBB,       `adc')
  define(func, mpn_addlsh2_n)')
ifdef(`OPERATION_rsblsh2_n',`
  define(ADDSUB,        `sub')
  define(ADCSBB,       `sbb')
  define(func, mpn_rsblsh2_n)')

MULFUNC_PROLOGUE(mpn_addlsh2_n mpn_rsblsh2_n)

ASM_START()
	TEXT
	ALIGN(16)
PROLOGUE(func)
	push	%r12
	push	%r13
	push	%r14
	push	%r15

	mov	(vp), %r8
	lea	(,%r8,4), %r12
	shr	$62, %r8

	mov	R32(n), R32(%rax)
	lea	(rp,n,8), rp
	lea	(up,n,8), up
	lea	(vp,n,8), vp
	neg	n
	and	$3, R8(%rax)
	je	L(b00)
	cmp	$2, R8(%rax)
	jc	L(b01)
	je	L(b10)

L(b11):	mov	8(vp,n,8), %r10
	lea	(%r8,%r10,4), %r14
	shr	$62, %r10
	mov	16(vp,n,8), %r11
	lea	(%r10,%r11,4), %r15
	shr	$62, %r11
	ADDSUB	(up,n,8), %r12
	ADCSBB	8(up,n,8), %r14
	ADCSBB	16(up,n,8), %r15
	sbb	R32(%rax), R32(%rax)		  C save carry for next
	mov	%r12, (rp,n,8)
	mov	%r14, 8(rp,n,8)
	mov	%r15, 16(rp,n,8)
	add	$3, n
	js	L(top)
	jmp	L(end)

L(b01):	mov	%r8, %r11
	ADDSUB	(up,n,8), %r12
	sbb	R32(%rax), R32(%rax)		  C save carry for next
	mov	%r12, (rp,n,8)
	add	$1, n
	js	L(top)
	jmp	L(end)

L(b10):	mov	8(vp,n,8), %r11
	lea	(%r8,%r11,4), %r15
	shr	$62, %r11
	ADDSUB	(up,n,8), %r12
	ADCSBB	8(up,n,8), %r15
	sbb	R32(%rax), R32(%rax)		  C save carry for next
	mov	%r12, (rp,n,8)
	mov	%r15, 8(rp,n,8)
	add	$2, n
	js	L(top)
	jmp	L(end)

L(b00):	mov	8(vp,n,8), %r9
	mov	16(vp,n,8), %r10
	jmp	L(e00)

	ALIGN(16)
L(top):	mov	16(vp,n,8), %r10
	mov	(vp,n,8), %r8
	mov	8(vp,n,8), %r9
	lea	(%r11,%r8,4), %r12
	shr	$62, %r8
L(e00):	lea	(%r8,%r9,4), %r13
	shr	$62, %r9
	mov	24(vp,n,8), %r11
	lea	(%r9,%r10,4), %r14
	shr	$62, %r10
	lea	(%r10,%r11,4), %r15
	shr	$62, %r11
	add	R32(%rax), R32(%rax)		  C restore carry
	ADCSBB	(up,n,8), %r12
	ADCSBB	8(up,n,8), %r13
	ADCSBB	16(up,n,8), %r14
	ADCSBB	24(up,n,8), %r15
	mov	%r12, (rp,n,8)
	mov	%r13, 8(rp,n,8)
	mov	%r14, 16(rp,n,8)
	sbb	R32(%rax), R32(%rax)		  C save carry for next
	mov	%r15, 24(rp,n,8)
	add	$4, n
	js	L(top)
L(end):

ifdef(`OPERATION_addlsh2_n',`
	sub	R32(%r11), R32(%rax)
	neg	R32(%rax)')
ifdef(`OPERATION_rsblsh2_n',`
	add	R32(%r11), R32(%rax)
	movslq	R32(%rax), %rax')

	pop	%r15
	pop	%r14
	pop	%r13
	pop	%r12
	ret
EPILOGUE()
