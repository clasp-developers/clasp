dnl  AMD64 mpn_addlsh_n and mpn_rsblsh_n.  R = V2^k +- U.
dnl  ("rsb" means reversed subtract, name mandated by mpn_sublsh1_n which
dnl  subtacts the shifted operand from the unshifted operand.)

dnl  Copyright 2006 Free Software Foundation, Inc.

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
C K8,K9:	 3.25	(mpn_lshift + mpn_add_n costs about 4.1 c/l)
C K10:		 3.25	(mpn_lshift + mpn_add_n costs about 4.1 c/l)
C P4:		14
C P6-15:	 4

C This was written quickly and not optimized at all.  Surely one could get
C closer to 3 c/l or perhaps even under 3 c/l.  Ideas:
C   1) Use indexing to save the 3 LEA
C   2) Write reasonable feed-in code
C   3) Be more clever about register usage
C   4) Unroll more, handling CL negation, carry save/restore cost much now
C   5) Reschedule

C INPUT PARAMETERS
define(`rp',	`%rdi')
define(`up',	`%rsi')
define(`vp',	`%rdx')
define(`n',	`%rcx')
define(`cnt',	`%r8')

ifdef(`OPERATION_addlsh_n',`
  define(ADDSUBC,       `adc')
  define(func, mpn_addlsh_n)
')
ifdef(`OPERATION_rsblsh_n',`
  define(ADDSUBC,       `sbb')
  define(func, mpn_rsblsh_n)
')

MULFUNC_PROLOGUE(mpn_addlsh_n mpn_rsblsh_n)

ASM_START()
	TEXT
	ALIGN(16)
PROLOGUE(func)

	push	%r12
	push	%r13
	push	%r14
	push	%r15
	push	%rbx

	mov	n, %rax
	xor	%ebx, %ebx		C clear carry save register
	mov	%r8d, %ecx		C shift count
	xor	%r15d, %r15d		C limb carry

	mov	%eax, %r11d
	and	$3, %r11d
	je	L(4)
	sub	$1, %r11d

L(oopette):
	mov	0(vp), %r8
	mov	%r8, %r12
	shl	%cl, %r8
	or	%r15, %r8
	neg	%cl
	mov	%r12, %r15
	shr	%cl, %r15
	neg	%cl
	add	%ebx, %ebx
	ADDSUBC	0(up), %r8
	mov	%r8, 0(rp)
	sbb	%ebx, %ebx
	lea	8(up), up
	lea	8(vp), vp
	lea	8(rp), rp
	sub	$1, %r11d
	jnc	L(oopette)

L(4):
	sub	$4, %rax
	jc	L(end)

L(oop):
	mov	0(vp), %r8
	mov	%r8, %r12
	mov	8(vp), %r9
	mov	%r9, %r13
	mov	16(vp), %r10
	mov	%r10, %r14
	mov	24(vp), %r11

	shl	%cl, %r8
	shl	%cl, %r9
	shl	%cl, %r10
	or	%r15, %r8
	mov	%r11, %r15
	shl	%cl, %r11

	neg	%cl

	shr	%cl, %r12
	shr	%cl, %r13
	shr	%cl, %r14
	shr	%cl, %r15		C used next loop

	or	%r12, %r9
	or	%r13, %r10
	or	%r14, %r11

	neg	%cl

	add	%ebx, %ebx		C restore carry flag

	ADDSUBC	0(up), %r8
	ADDSUBC	8(up), %r9
	ADDSUBC	16(up), %r10
	ADDSUBC	24(up), %r11

	mov	%r8, 0(rp)
	mov	%r9, 8(rp)
	mov	%r10, 16(rp)
	mov	%r11, 24(rp)

	sbb	%ebx, %ebx		C save carry flag

	lea	32(up), up
	lea	32(vp), vp
	lea	32(rp), rp

	sub	$4, %rax
	jnc	L(oop)
L(end):
	add	%ebx, %ebx
	ADDSUBC	$0, %r15
	mov	%r15, %rax
	pop	%rbx
	pop	%r15
	pop	%r14
	pop	%r13
	pop	%r12

	ret
EPILOGUE()
