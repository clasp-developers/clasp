dnl  AMD64 mpn_redc_1 -- Montgomery reduction with a one-limb modular inverse.

dnl  Copyright 2004, 2008 Free Software Foundation, Inc.
dnl
dnl  This file is part of the GNU MP Library.
dnl
dnl  The GNU MP Library is free software; you can redistribute it and/or
dnl  modify it under the terms of the GNU Lesser General Public License as
dnl  published by the Free Software Foundation; either version 3 of the
dnl  License, or (at your option) any later version.
dnl
dnl  The GNU MP Library is distributed in the hope that it will be useful,
dnl  but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl  Lesser General Public License for more details.
dnl
dnl  You should have received a copy of the GNU Lesser General Public License
dnl  along with the GNU MP Library.  If not, see http://www.gnu.org/licenses/.

include(`../config.m4')


C	     cycles/limb
C	     cycles/limb
C K8,K9:	 2.5
C K10:		 2.5
C P4:		 ?
C P6-15 (Core2): 5.3
C P6-28 (Atom):	 ?

C TODO
C  * Handle certain sizes, e.g., 1, 2, 3, 4, 8, with single-loop code.
C    The code for 1, 2, 3, 4 should perhaps be completely register based.
C  * Perhaps align outer loops.
C  * The sub_n at the end leaks side-channel data.  How do we fix that?
C  * Write mpn_add_n_sub_n computing R = A + B - C.  It should run at 2 c/l.
C  * We could software pipeline the IMUL stuff, by putting it before the
C    outer loops and before the end of the outer loops.  The last outer
C    loop iteration would then compute an unneeded product, but it is at
C    least not a stray read from up[], since it is at up[n].
C  * Can we combine both the add_n and sub_n into the loops, somehow?

C INPUT PARAMETERS
define(`rp',	  `%rdi')
define(`up',	  `%rsi')
define(`param_mp',`%rdx')
define(`n',	  `%rcx')
define(`invm',	  `%r8')

define(`mp',	  `%r13')
define(`i',	  `%r11')
define(`nneg',	  `%r12')

ASM_START()
	TEXT
	ALIGN(32)
PROLOGUE(mpn_redc_1)
	push	%rbp
	push	%rbx
	push	%r12
	push	%r13
	push	%r14
	push	n
	sub	$8, %rsp		C maintain ABI required rsp alignment

	lea	(param_mp,n,8), mp	C mp += n
	lea	(up,n,8), up		C up += n

	mov	n, nneg
	neg	nneg

	mov	R32(n), R32(%rax)
	and	$3, R32(%rax)
	jz	L(b0)
	cmp	$2, R32(%rax)
	jz	L(b2)
	jg	L(b3)

L(b1):	C lea	(mp), mp
	lea	-16(up), up
L(o1):	mov	nneg, i
	mov	16(up,nneg,8), %rbp	C up[0]
	imul	invm, %rbp

	mov	(mp,i,8), %rax
	xor	%ebx, %ebx
	mul	%rbp
	add	$1, i
	jnz	1f
	add	%rax, 8(up,i,8)
	adc	$0, %rdx
	mov	%rdx, %r14
	jmp	L(n1)

1:	mov	%rax, %r9
	mov	(mp,i,8), %rax
	mov	%rdx, %r14
	jmp	L(mi1)

	ALIGN(16)
L(lo1):	add	%r10, (up,i,8)
	adc	%rax, %r9
	mov	(mp,i,8), %rax
	adc	%rdx, %r14
L(mi1):	xor	%r10d, %r10d
	mul	%rbp
	add	%r9, 8(up,i,8)
	adc	%rax, %r14
	adc	%rdx, %rbx
	mov	8(mp,i,8), %rax
	mul	%rbp
	add	%r14, 16(up,i,8)
	adc	%rax, %rbx
	adc	%rdx, %r10
	mov	16(mp,i,8), %rax
	mul	%rbp
	xor	%r9d, %r9d
	xor	%r14d, %r14d
	add	%rbx, 24(up,i,8)
	adc	%rax, %r10
	mov	24(mp,i,8), %rax
	adc	%rdx, %r9
	xor	%ebx, %ebx
	mul	%rbp
	add	$4, i
	js	L(lo1)
L(ed1):	add	%r10, (up)
	adc	%rax, %r9
	adc	%rdx, %r14
	xor	%r10d, %r10d
	add	%r9, 8(up)
	adc	$0, %r14
L(n1):	mov	%r14, 16(up,nneg,8)	C up[0]
	add	$8, up
	dec	n
	jnz	L(o1)
C	lea	(mp), mp
	lea	16(up), up
	jmp	L(common)

L(b0):	C lea	(mp), mp
	lea	-16(up), up
L(o0):	mov	nneg, i
	mov	16(up,nneg,8), %rbp	C up[0]
	imul	invm, %rbp

	mov	(mp,i,8), %rax
	xor	%r10d, %r10d
	mul	%rbp
	mov	%rax, %r14
	mov	%rdx, %rbx
	jmp	L(mi0)

	ALIGN(16)
L(lo0):	add	%r10, (up,i,8)
	adc	%rax, %r9
	mov	(mp,i,8), %rax
	adc	%rdx, %r14
	xor	%r10d, %r10d
	mul	%rbp
	add	%r9, 8(up,i,8)
	adc	%rax, %r14
	adc	%rdx, %rbx
L(mi0):	mov	8(mp,i,8), %rax
	mul	%rbp
	add	%r14, 16(up,i,8)
	adc	%rax, %rbx
	adc	%rdx, %r10
	mov	16(mp,i,8), %rax
	mul	%rbp
	xor	%r9d, %r9d
	xor	%r14d, %r14d
	add	%rbx, 24(up,i,8)
	adc	%rax, %r10
	mov	24(mp,i,8), %rax
	adc	%rdx, %r9
	xor	%ebx, %ebx
	mul	%rbp
	add	$4, i
	js	L(lo0)
L(ed0):	add	%r10, (up)
	adc	%rax, %r9
	adc	%rdx, %r14
	xor	%r10d, %r10d
	add	%r9, 8(up)
	adc	$0, %r14
	mov	%r14, 16(up,nneg,8)	C up[0]
	add	$8, up
	dec	n
	jnz	L(o0)
C	lea	(mp), mp
	lea	16(up), up
	jmp	L(common)


L(b3):	lea	-8(mp), mp
	lea	-24(up), up
L(o3):	mov	nneg, i
	mov	24(up,nneg,8), %rbp	C up[0]
	imul	invm, %rbp

	mov	8(mp,i,8), %rax
	mul	%rbp
	mov	%rax, %rbx
	mov	%rdx, %r10
	jmp	L(mi3)

	ALIGN(16)
L(lo3):	add	%r10, (up,i,8)
	adc	%rax, %r9
	mov	(mp,i,8), %rax
	adc	%rdx, %r14
	xor	%r10d, %r10d
	mul	%rbp
	add	%r9, 8(up,i,8)
	adc	%rax, %r14
	adc	%rdx, %rbx
	mov	8(mp,i,8), %rax
	mul	%rbp
	add	%r14, 16(up,i,8)
	adc	%rax, %rbx
	adc	%rdx, %r10
L(mi3):	mov	16(mp,i,8), %rax
	mul	%rbp
	xor	%r9d, %r9d
	xor	%r14d, %r14d
	add	%rbx, 24(up,i,8)
	adc	%rax, %r10
	mov	24(mp,i,8), %rax
	adc	%rdx, %r9
	xor	%ebx, %ebx
	mul	%rbp
	add	$4, i
	js	L(lo3)
L(ed3):	add	%r10, 8(up)
	adc	%rax, %r9
	adc	%rdx, %r14
	xor	%r10d, %r10d
	add	%r9, 16(up)
	adc	$0, %r14
	mov	%r14, 24(up,nneg,8)	C up[0]
	add	$8, up
	dec	n
	jnz	L(o3)
	lea	8(mp), mp
	lea	24(up), up
	jmp	L(common)

L(b2):	lea	-16(mp), mp
	lea	-32(up), up
L(o2):	mov	nneg, i
	mov	32(up,nneg,8), %rbp	C up[0]
	imul	invm, %rbp

	mov	16(mp,i,8), %rax
	mul	%rbp
	xor	%r14d, %r14d
	mov	%rax, %r10
	mov	24(mp,i,8), %rax
	mov	%rdx, %r9
	jmp	L(mi2)

	ALIGN(16)
L(lo2):	add	%r10, (up,i,8)
	adc	%rax, %r9
	mov	(mp,i,8), %rax
	adc	%rdx, %r14
	xor	%r10d, %r10d
	mul	%rbp
	add	%r9, 8(up,i,8)
	adc	%rax, %r14
	adc	%rdx, %rbx
	mov	8(mp,i,8), %rax
	mul	%rbp
	add	%r14, 16(up,i,8)
	adc	%rax, %rbx
	adc	%rdx, %r10
	mov	16(mp,i,8), %rax
	mul	%rbp
	xor	%r9d, %r9d
	xor	%r14d, %r14d
	add	%rbx, 24(up,i,8)
	adc	%rax, %r10
	mov	24(mp,i,8), %rax
	adc	%rdx, %r9
L(mi2):	xor	%ebx, %ebx
	mul	%rbp
	add	$4, i
	js	L(lo2)
L(ed2):	add	%r10, 16(up)
	adc	%rax, %r9
	adc	%rdx, %r14
	xor	%r10d, %r10d
	add	%r9, 24(up)
	adc	$0, %r14
	mov	%r14, 32(up,nneg,8)	C up[0]
	add	$8, up
	dec	n
	jnz	L(o2)
	lea	16(mp), mp
	lea	32(up), up


L(common):
	lea	(mp,nneg,8), mp		C restore entry mp

C   cy = mpn_add_n (rp, up, up - n, n);
C		    rdi rsi  rdx    rcx
	lea	(up,nneg,8), up		C up -= n
	lea	(up,nneg,8), %rdx	C rdx = up - n [up entry value]
	mov	rp, nneg		C preserve rp over first call
	mov	8(%rsp), %rcx		C pass entry n
C	mov	rp, %rdi
	CALL(	mpn_add_n)
	test	R32(%rax), R32(%rax)
	jz	L(ret)

C     mpn_sub_n (rp, rp, mp, n);
C		 rdi rsi rdx rcx
	mov	nneg, %rdi
	mov	nneg, %rsi
	mov	mp, %rdx
	mov	8(%rsp), %rcx		C pass entry n
	CALL(	mpn_sub_n)

L(ret):
	add	$8, %rsp
	pop	n			C just increment rsp
	pop	%r14
	pop	%r13
	pop	%r12
	pop	%rbx
	pop	%rbp
	ret
EPILOGUE()
