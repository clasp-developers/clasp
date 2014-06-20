dnl  AMD64 mpn_addmul_2 -- Multiply an n-limb vector with a 2-limb vector and
dnl  add the result to a third limb vector.

dnl  Copyright 2008 Free Software Foundation, Inc.

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
C K8,K9:	 2.375
C K10:		 2.375
C P4:		 ?
C P6 core2:	 4.45
C P6 corei7:	 4.35

C This code is the result of running a code generation and optimization tool
C suite written by David Harvey and Torbjorn Granlund.

C TODO
C  * Work on feed-in and wind-down code.
C  * Convert "mov $0" to "xor".
C  * Adjust initial lea to save some bytes.
C  * Perhaps adjust n from n_param&3 value?

C INPUT PARAMETERS
define(`rp',     `%rdi')
define(`up',     `%rsi')
define(`n_param',`%rdx')
define(`vp',     `%rcx')

define(`v0', `%r8')
define(`v1', `%r9')
define(`w0', `%rbx')
define(`w1', `%rcx')
define(`w2', `%rbp')
define(`w3', `%r10')
define(`n',  `%r11')

ASM_START()
	TEXT
	ALIGN(16)
PROLOGUE(mpn_addmul_2)
	push	%rbx
	push	%rbp

	mov	(vp), v0
	mov	8(vp), v1

	mov	n_param, n
	neg	n
	lea	-32(up,n_param,8), up
	lea	-32(rp,n_param,8), rp

	and	$3, R32(n_param)
	jz	L(am2p0)
	cmp	$2, R32(n_param)
	jc	L(am2p1)
	jz	L(am2p2)
L(am2p3):
	mov	32(up,n,8), %rax
	mul	v0
	mov	%rax, w1
	mov	32(up,n,8), %rax
	mov	%rdx, w2
	xor	R32(w3), R32(w3)
	add	$2, n
	jmp	L(am3)
L(am2p0):
	mov	32(up,n,8), %rax
	mul	v0
	mov	%rax, w0
	mov	32(up,n,8), %rax
	mov	%rdx, w1
	xor	R32(w2), R32(w2)
	add	$3, n
	jmp	L(am0)
L(am2p1):
	mov	32(up,n,8), %rax
	mul	v0
	mov	%rax, w3
	mov	32(up,n,8), %rax
	mov	%rdx, w0
	xor	R32(w1), R32(w1)
	jmp	L(am1)
L(am2p2):
	mov	32(up,n,8), %rax
	mul	v0
	mov	%rax, w2
	mov	32(up,n,8), %rax
	mov	%rdx, w3
	xor	R32(w0), R32(w0)
	xor	R32(w1), R32(w1)
	add	$1, n
	jmp	L(am2)

	ALIGN(32)
L(top):
	add	w3, (rp,n,8)		C 0 21
	adc	%rax, w0		C 1 24
	mov	8(up,n,8), %rax
	adc	%rdx, w1		C 3 26
	mov	$0, R32(w2)
	mul	v0
	add	%rax, w0		C 2 26
	mov	8(up,n,8), %rax
	adc	%rdx, w1		C 4 28
	adc	$0, R32(w2)		C 6 30
L(am0):	mul	v1
	add	w0, 8(rp,n,8)		C 3 27
	adc	%rax, w1		C 6 30
	adc	%rdx, w2		C 8 32
	mov	16(up,n,8), %rax
	mov	$0, R32(w3)
	mul	v0
	add	%rax, w1		C 8
	mov	16(up,n,8), %rax
	adc	%rdx, w2		C 10
	adc	$0, R32(w3)		C 12
L(am3):	mul	v1
	add	w1, 16(rp,n,8)		C 9
	adc	%rax, w2		C 12
	mov	24(up,n,8), %rax
	adc	%rdx, w3		C 14
	mul	v0
	mov	$0, R32(w0)
	add	%rax, w2		C 14
	adc	%rdx, w3		C 16
	mov	$0, R32(w1)
	mov	24(up,n,8), %rax
	adc	$0, R32(w0)		C 18
L(am2):	mul	v1
	add	w2, 24(rp,n,8)		C 15
	adc	%rax, w3		C 18
	adc	%rdx, w0		C 20
	mov	32(up,n,8), %rax
	mul	v0
	add	%rax, w3		C 20
	mov	32(up,n,8), %rax
	adc	%rdx, w0		C 22
	adc	$0, R32(w1)		C 24
L(am1):	mul	v1
	add	$4, n
	js	L(top)

	add	w3, (rp,n,8)
	adc	%rax, w0
	adc	%rdx, w1
	mov	w0, 8(rp,n,8)
	mov	w1, %rax

	pop	%rbp
	pop	%rbx
	ret
EPILOGUE()
