dnl  AMD64 mpn_popcount, mpn_hamdist -- population count and hamming distance.

dnl  Copyright 2004, 2005, 2007 Free Software Foundation, Inc.

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


C		     popcount	      hamdist
C		    cycles/limb	    cycles/limb
C K8,K9:		 6		 7
C K10:			 6		 7
C P4:			12		14.3
C P6-15:		 7		 8

C TODO
C  * Tune.  It should be possible to reach 5 c/l for popcount and 6 c/l for
C    hamdist for K8/K9.


ifdef(`OPERATION_popcount',`
  define(`func',`mpn_popcount')
  define(`up',		`%rdi')
  define(`n',		`%rsi')
  define(`h55555555',	`%r10')
  define(`h33333333',	`%r11')
  define(`h0f0f0f0f',	`%rcx')
  define(`h01010101',	`%rdx')
  define(`HAM',		`dnl')
')
ifdef(`OPERATION_hamdist',`
  define(`func',`mpn_hamdist')
  define(`up',		`%rdi')
  define(`vp',		`%rsi')
  define(`n',		`%rdx')
  define(`h55555555',	`%r10')
  define(`h33333333',	`%r11')
  define(`h0f0f0f0f',	`%rcx')
  define(`h01010101',	`%r14')
  define(`HAM',		`$1')
')


MULFUNC_PROLOGUE(mpn_popcount mpn_hamdist)

ASM_START()
	TEXT
	ALIGN(32)
PROLOGUE(func)

	pushq	%r12
	pushq	%r13
 HAM(`	pushq	%r14		')

	movq	$0x5555555555555555, h55555555
	movq	$0x3333333333333333, h33333333
	movq	$0x0f0f0f0f0f0f0f0f, h0f0f0f0f
	movq	$0x0101010101010101, h01010101

	leaq	(up,n,8), up
 HAM(`	leaq	(vp,n,8), vp	')
	negq	n

	xorl	%eax, %eax

	btq	$0, n
	jnc	L(oop)

	movq	(up,n,8), %r8
 HAM(`	xorq	(vp,n,8), %r8	')

	movq	%r8, %r9
	shrq	%r8
	andq	h55555555, %r8
	subq	%r8, %r9

	movq	%r9, %r8
	shrq	$2, %r9
	andq	h33333333, %r8
	andq	h33333333, %r9
	addq	%r8, %r9		C 16 4-bit fields (0..4)

	movq	%r9, %r8
	shrq	$4, %r9
	andq	h0f0f0f0f, %r8
	andq	h0f0f0f0f, %r9
	addq	%r8, %r9		C 8 8-bit fields (0..16)

	imulq	h01010101, %r9		C sum the 8 fields in high 8 bits
	shrq	$56, %r9

	addq	%r9, %rax		C add to total
	addq	$1, n
	jz	L(done)

	ALIGN(16)
L(oop):	movq	(up,n,8), %r8
	movq	8(up,n,8), %r12
 HAM(`	xorq	(vp,n,8), %r8	')
 HAM(`	xorq	8(vp,n,8), %r12	')

	movq	%r8, %r9
	movq	%r12, %r13
	shrq	%r8
	shrq	%r12
	andq	h55555555, %r8
	andq	h55555555, %r12
	subq	%r8, %r9
	subq	%r12, %r13

	movq	%r9, %r8
	movq	%r13, %r12
	shrq	$2, %r9
	shrq	$2, %r13
	andq	h33333333, %r8
	andq	h33333333, %r9
	andq	h33333333, %r12
	andq	h33333333, %r13
	addq	%r8, %r9		C 16 4-bit fields (0..4)
	addq	%r12, %r13		C 16 4-bit fields (0..4)

	addq	%r13, %r9		C 16 4-bit fields (0..8)
	movq	%r9, %r8
	shrq	$4, %r9
	andq	h0f0f0f0f, %r8
	andq	h0f0f0f0f, %r9
	addq	%r8, %r9		C 8 8-bit fields (0..16)

	imulq	h01010101, %r9		C sum the 8 fields in high 8 bits
	shrq	$56, %r9

	addq	%r9, %rax		C add to total
	addq	$2, n
	jnc	L(oop)

L(done):
 HAM(`	popq	%r14		')
	popq	%r13
	popq	%r12
	ret

EPILOGUE()
