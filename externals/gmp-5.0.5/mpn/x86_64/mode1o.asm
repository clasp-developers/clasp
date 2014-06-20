dnl  AMD64 mpn_modexact_1_odd -- exact division style remainder.

dnl  Copyright 2000, 2001, 2002, 2003, 2004, 2005, 2006 Free Software
dnl  Foundation, Inc.
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
C K8,K9:	10
C K10:		10
C P4:		33
C P6 core2:	13
C P6 corei7:	14.5
C P6 Atom:	35


C mp_limb_t mpn_modexact_1_odd (mp_srcptr src, mp_size_t size,
C                               mp_limb_t divisor);
C mp_limb_t mpn_modexact_1c_odd (mp_srcptr src, mp_size_t size,
C                                mp_limb_t divisor, mp_limb_t carry);
C
C
C The dependent chain in the main loop is
C
C                            cycles
C	subq	%rdx, %rax	1
C	imulq	%r9, %rax	4
C	mulq	%r8		5
C			      ----
C       total		       10
C
C The movq load from src seems to need to be scheduled back before the jz to
C achieve this speed, out-of-order execution apparently can't completely
C hide the latency otherwise.
C
C The l=src[i]-cbit step is rotated back too, since that allows us to avoid
C it for the first iteration (where there's no cbit).
C
C The code alignment used (32-byte) for the loop also seems necessary.
C Without that the non-PIC case has adcq crossing the 0x60 offset,
C apparently making it run at 11 cycles instead of 10.
C
C Not done:
C
C divq for size==1 was measured at about 79 cycles, compared to the inverse
C at about 25 cycles (both including function call overheads), so that's not
C used.
C
C Enhancements:
C
C For PIC, we shouldn't really need the GOT fetch for binvert_limb_table,
C it'll be in rodata or text in libgmp.so and can be accessed directly %rip
C relative.  This would be for small model only (something we don't
C presently detect, but which is all that gcc 3.3.3 supports), since 8-byte
C PC-relative relocations are apparently not available.  Some rough
C experiments with binutils 2.13 looked worrylingly like it might come out
C with an unwanted text segment relocation though, even with ".protected".


ASM_START()
	TEXT
	ALIGN(32)
PROLOGUE(mpn_modexact_1_odd)

	movl	$0, %ecx

PROLOGUE(mpn_modexact_1c_odd)

	C rdi	src
	C rsi	size
	C rdx	divisor
	C rcx	carry

	movq	%rdx, %r8		C d
	shrl	%edx			C d/2
ifdef(`PIC',`
	movq	binvert_limb_table@GOTPCREL(%rip), %r9
',`
	movabsq	$binvert_limb_table, %r9
')

	andl	$127, %edx
	movq	%rcx, %r10		C initial carry

	movzbl	(%r9,%rdx), %edx	C inv 8 bits

	movq	(%rdi), %rax		C src[0]
	leaq	(%rdi,%rsi,8), %r11	C src end
	movq	%r8, %rdi		C d, made available to imull

	leal	(%rdx,%rdx), %ecx	C 2*inv
	imull	%edx, %edx		C inv*inv

	negq	%rsi			C -size

	imull	%edi, %edx		C inv*inv*d

	subl	%edx, %ecx		C inv = 2*inv - inv*inv*d, 16 bits

	leal	(%rcx,%rcx), %edx	C 2*inv
	imull	%ecx, %ecx		C inv*inv

	imull	%edi, %ecx		C inv*inv*d

	subl	%ecx, %edx		C inv = 2*inv - inv*inv*d, 32 bits
	xorl	%ecx, %ecx		C initial cbit

	leaq	(%rdx,%rdx), %r9	C 2*inv
	imulq	%rdx, %rdx		C inv*inv

	imulq	%r8, %rdx		C inv*inv*d

	subq	%rdx, %r9		C inv = 2*inv - inv*inv*d, 64 bits
	movq	%r10, %rdx		C initial climb

	ASSERT(e,`	C d*inv == 1 mod 2^64
	movq	%r8, %r10
	imulq	%r9, %r10
	cmpq	$1, %r10')

	incq	%rsi
	jz	L(one)


	ALIGN(16)
L(top):
	C rax	l = src[i]-cbit
	C rcx	new cbit, 0 or 1
	C rdx	climb, high of last product
	C rsi	counter, limbs, negative
	C rdi
	C r8	divisor
	C r9	inverse
	C r11	src end ptr

	subq	%rdx, %rax		C l = src[i]-cbit - climb

	adcq	$0, %rcx		C more cbit
	imulq	%r9, %rax		C q = l * inverse

	mulq	%r8			C climb = high (q * d)

	movq	(%r11,%rsi,8), %rax	C src[i+1]
	subq	%rcx, %rax		C next l = src[i+1] - cbit
	setc	%cl			C new cbit

	incq	%rsi
	jnz	L(top)


L(one):
	subq	%rdx, %rax		C l = src[i]-cbit - climb

	adcq	$0, %rcx		C more cbit
	imulq	%r9, %rax		C q = l * inverse

	mulq	%r8			C climb = high (q * d)

	leaq	(%rcx,%rdx), %rax	C climb+cbit
	ret

EPILOGUE(mpn_modexact_1c_odd)
EPILOGUE(mpn_modexact_1_odd)
