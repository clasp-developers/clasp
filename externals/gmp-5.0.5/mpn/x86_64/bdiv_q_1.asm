dnl  AMD64 mpn_bdiv_q_1, mpn_pi1_bdiv_q_1 -- schoolbook Hensel division by
dnl  1-limb divisor, returning quotient only.

dnl  Copyright 2001, 2002, 2004, 2005, 2006, 2009 Free Software Foundation,
dnl  Inc.

dnl  This file is part of the GNU MP Library.

dnl  The GNU MP Library is free software; you can redistribute it and/or modify
dnl  it under the terms of the GNU Lesser General Public License as published
dnl  by the Free Software Foundation; either version 3 of the License, or (at
dnl  your option) any later version.

dnl The GNU MP Library is distributed in the hope that it will be useful, but
dnl  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
dnl  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
dnl  License for more details.

dnl  You should have received a copy of the GNU Lesser General Public License
dnl  along with the GNU MP Library.  If not, see http://www.gnu.org/licenses/.

include(`../config.m4')


C	     cycles/limb
C K8,K9:	10
C K10:		10
C P4:		33
C P6 core2:	13.25
C P6 corei7:	14
C P6 atom:	42


C INPUT PARAMETERS
C rp		rdi
C up		rsi
C n		rdx
C d		rcx
C di		r8	just mpn_pi1_bdiv_q_1
C shift		r9	just mpn_pi1_bdiv_q_1


ASM_START()
	TEXT
	ALIGN(16)
PROLOGUE(mpn_bdiv_q_1)
	push	%rbx

	mov	%rcx, %rax
	xor	R32(%rcx), R32(%rcx)	C shift count
	mov	%rdx, %r10

	bt	$0, R32(%rax)
	jnc	L(evn)			C skip bsfq unless divisor is even

L(odd):	mov	%rax, %rbx
	shr	R32(%rax)
	and	$127, R32(%rax)		C d/2, 7 bits

ifdef(`PIC',`
	mov	binvert_limb_table@GOTPCREL(%rip), %rdx
',`
	movabs	$binvert_limb_table, %rdx
')

	movzbl	(%rdx,%rax), R32(%rax)	C inv 8 bits

	mov	%rbx, %r11		C d without twos

	lea	(%rax,%rax), R32(%rdx)	C 2*inv
	imul	R32(%rax), R32(%rax)	C inv*inv
	imul	R32(%rbx), R32(%rax)	C inv*inv*d
	sub	R32(%rax), R32(%rdx)	C inv = 2*inv - inv*inv*d, 16 bits

	lea	(%rdx,%rdx), R32(%rax)	C 2*inv
	imul	R32(%rdx), R32(%rdx)	C inv*inv
	imul	R32(%rbx), R32(%rdx)	C inv*inv*d
	sub	R32(%rdx), R32(%rax)	C inv = 2*inv - inv*inv*d, 32 bits

	lea	(%rax,%rax), %r8	C 2*inv
	imul	%rax, %rax		C inv*inv
	imul	%rbx, %rax		C inv*inv*d
	sub	%rax, %r8		C inv = 2*inv - inv*inv*d, 64 bits

	jmp	L(com)

L(evn):	bsf	%rax, %rcx
	shr	R8(%rcx), %rax
	jmp	L(odd)
EPILOGUE()

PROLOGUE(mpn_pi1_bdiv_q_1)
	push	%rbx

	mov	%rcx, %r11		C d
	mov	%rdx, %r10		C n
	mov	%r9, %rcx		C shift
L(com):
	mov	(%rsi), %rax		C up[0]

	dec	%r10
	jz	L(one)

	mov	8(%rsi), %rdx		C up[1]
	lea	(%rsi,%r10,8), %rsi	C up end
	lea	(%rdi,%r10,8), %rdi	C rp end
	neg	%r10			C -n

	shrd	R8(%rcx), %rdx, %rax

	xor	R32(%rbx), R32(%rbx)
	jmp	L(ent)

	ALIGN(8)
L(top):
	C rax	q
	C rbx	carry bit, 0 or 1
	C rcx	shift
	C rdx
	C rsi	up end
	C rdi	rp end
	C r10	counter, limbs, negative

	mul	%r11			C carry limb in rdx
	mov	(%rsi,%r10,8), %rax
	mov	8(%rsi,%r10,8), %r9
	shrd	R8(%rcx), %r9, %rax
	nop
	sub	%rbx, %rax		C apply carry bit
	setc	R8(%rbx)
	sub	%rdx, %rax		C apply carry limb
	adc	$0, %rbx
L(ent):	imul	%r8, %rax
	mov	%rax, (%rdi,%r10,8)
	inc	%r10
	jnz	L(top)

	mul	%r11			C carry limb in rdx
	mov	(%rsi), %rax		C up high limb
	shr	R8(%rcx), %rax
	sub	%rbx, %rax		C apply carry bit
	sub	%rdx, %rax		C apply carry limb
	imul	%r8, %rax
	mov	%rax, (%rdi)
	pop	%rbx
	ret

L(one):	shr	R8(%rcx), %rax
	imul	%r8, %rax
	mov	%rax, (%rdi)
	pop	%rbx
	ret
EPILOGUE()
