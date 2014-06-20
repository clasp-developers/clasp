dnl  AMD64 mpn_gcd_1 -- mpn by 1 gcd.

dnl  Based on the K7 gcd_1.asm, by Kevin Ryde.  Rehacked for AMD64 by Torbjorn
dnl  Granlund.

dnl  Copyright 2000, 2001, 2002, 2005, 2009 Free Software Foundation, Inc.

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


C K8: 6.75 cycles/bit (approx)  1x1 gcd
C     10.0 cycles/limb          Nx1 reduction (modexact_1_odd)


dnl  Reduce using x%y if x is more than DIV_THRESHOLD bits bigger than y,
dnl  where x is the larger of the two.  See tune/README for more.
dnl
dnl  div at 80 cycles compared to the gcd at about 7 cycles/bitpair
dnl  suggests 80/7*2=23

deflit(DIV_THRESHOLD, 23)


C ctz_table[n] is the number of trailing zeros on n, or MAXSHIFT if n==0.


deflit(MAXSHIFT, 6)
deflit(MASK, eval((m4_lshift(1,MAXSHIFT))-1))

DEF_OBJECT(ctz_table,64)
	.byte	MAXSHIFT
forloop(i,1,MASK,
`	.byte	m4_count_trailing_zeros(i)
')
END_OBJECT(ctz_table)

C mp_limb_t mpn_gcd_1 (mp_srcptr up, mp_size_t n, mp_limb_t vlimb);


C INPUT PARAMETERS
define(`up',    `%rdi')
define(`n',     `%rsi')
define(`vlimb', `%rdx')

	TEXT
	ALIGN(16)

PROLOGUE(mpn_gcd_1)
	mov	(%rdi), %r8		C src low limb
	or	%rdx, %r8		C x | y
	mov	$-1, R32(%rcx)

L(twos):
	inc	R32(%rcx)
	shr	%r8
	jnc	L(twos)

	shr	R8(%rcx), %rdx
	mov	R32(%rcx), R32(%r8)	C common twos

L(divide_strip_y):
	shr	%rdx
	jnc	L(divide_strip_y)
	adc	%rdx, %rdx

	push	%r8
	push	%rdx
	sub	$8, %rsp		C maintain ABI required rsp alignment

	CALL(	mpn_modexact_1_odd)

	add	$8, %rsp
	pop	%rdx
	pop	%r8

	test	%rax, %rax

	mov	%rax, %rcx
	jnz	L(strip_x)

	mov	%rdx, %rax
	jmp	L(done)

L(strip_x):
	LEA(	ctz_table, %r9)
	jmp	L(strip_x_top)

	ALIGN(16)
L(top):
	cmovc	%r10, %rcx		C if x-y gave carry, use x,y-x	0
	cmovc	%rax, %rdx		C				0

L(strip_x_top):
	mov	%rcx, %rax		C				1
	and	$MASK, R32(%rcx)	C				1

	mov	(%r9,%rcx), R8(%rcx)	C				1

	shr	R8(%rcx), %rax		C				4
	cmp	$MAXSHIFT, R8(%rcx)	C				4

	mov	%rax, %rcx		C				5
	mov	%rdx, %r10		C				5
	je	L(strip_x_top)		C				5

	sub	%rax, %r10		C				6
	sub	%rdx, %rcx		C				6
	jnz	L(top)			C				6

L(done):
	mov	%r8, %rcx
	shl	R8(%rcx), %rax
	ret

EPILOGUE()
