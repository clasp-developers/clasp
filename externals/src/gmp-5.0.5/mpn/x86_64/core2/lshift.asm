dnl  x86-64 mpn_lshift optimized for "Core 2".

dnl  Copyright 2007, 2009 Free Software Foundation, Inc.
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
C K8,K9:	 4.25
C K10:		 4.25
C P4:		14.7
C P6 core2:	 1.27
C P6 corei7:	 1.5


C INPUT PARAMETERS
define(`rp',	`%rdi')
define(`up',	`%rsi')
define(`n',	`%rdx')
define(`cnt',	`%cl')

ASM_START()
	TEXT
	ALIGN(16)
PROLOGUE(mpn_lshift)
	lea	-8(rp,n,8), rp
	lea	-8(up,n,8), up

	mov	%edx, %eax
	and	$3, %eax
	jne	L(nb00)
L(b00):	C n = 4, 8, 12, ...
	mov	(up), %r10
	mov	-8(up), %r11
	xor	%eax, %eax
	shld	%cl, %r10, %rax
	mov	-16(up), %r8
	lea	24(rp), rp
	sub	$4, n
	jmp	L(00)

L(nb00):C n = 1, 5, 9, ...
	cmp	$2, %eax
	jae	L(nb01)
L(b01):	mov	(up), %r9
	xor	%eax, %eax
	shld	%cl, %r9, %rax
	sub	$2, n
	jb	L(le1)
	mov	-8(up), %r10
	mov	-16(up), %r11
	lea	-8(up), up
	lea	16(rp), rp
	jmp	L(01)
L(le1):	shl	%cl, %r9
	mov	%r9, (rp)
	ret

L(nb01):C n = 2, 6, 10, ...
	jne	L(b11)
L(b10):	mov	(up), %r8
	mov	-8(up), %r9
	xor	%eax, %eax
	shld	%cl, %r8, %rax
	sub	$3, n
	jb	L(le2)
	mov	-16(up), %r10
	lea	-16(up), up
	lea	8(rp), rp
	jmp	L(10)
L(le2):	shld	%cl, %r9, %r8
	mov	%r8, (rp)
	shl	%cl, %r9
	mov	%r9, -8(rp)
	ret

	ALIGN(16)			C performance critical!
L(b11):	C n = 3, 7, 11, ...
	mov	(up), %r11
	mov	-8(up), %r8
	xor	%eax, %eax
	shld	%cl, %r11, %rax
	mov	-16(up), %r9
	lea	-24(up), up
	sub	$4, n
	jb	L(end)

	ALIGN(16)
L(top):	shld	%cl, %r8, %r11
	mov	(up), %r10
	mov	%r11, (rp)
L(10):	shld	%cl, %r9, %r8
	mov	-8(up), %r11
	mov	%r8, -8(rp)
L(01):	shld	%cl, %r10, %r9
	mov	-16(up), %r8
	mov	%r9, -16(rp)
L(00):	shld	%cl, %r11, %r10
	mov	-24(up), %r9
	mov	%r10, -24(rp)
	add	$-32, up
	lea	-32(rp), rp
	sub	$4, n
	jnc	L(top)

L(end):	shld	%cl, %r8, %r11
	mov	%r11, (rp)
	shld	%cl, %r9, %r8
	mov	%r8, -8(rp)
	shl	%cl, %r9
	mov	%r9, -16(rp)
	ret
EPILOGUE()
