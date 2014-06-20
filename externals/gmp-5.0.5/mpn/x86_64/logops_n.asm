dnl  AMD64 logops.

dnl  Copyright 2004, 2005, 2006 Free Software Foundation, Inc.

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
C K8,K9:	 1.5
C K10:		 1.75-2 (fluctuating)
C P4:		 2.8/3.35/3.60 (variant1/variant2/variant3)
C P6-15:	 2.0

ifdef(`OPERATION_and_n',`
  define(`func',`mpn_and_n')
  define(`VARIANT_1')
  define(`LOGOP',`andq')')
ifdef(`OPERATION_andn_n',`
  define(`func',`mpn_andn_n')
  define(`VARIANT_2')
  define(`LOGOP',`andq')')
ifdef(`OPERATION_nand_n',`
  define(`func',`mpn_nand_n')
  define(`VARIANT_3')
  define(`LOGOP',`andq')')
ifdef(`OPERATION_ior_n',`
  define(`func',`mpn_ior_n')
  define(`VARIANT_1')
  define(`LOGOP',`orq')')
ifdef(`OPERATION_iorn_n',`
  define(`func',`mpn_iorn_n')
  define(`VARIANT_2')
  define(`LOGOP',`orq')')
ifdef(`OPERATION_nior_n',`
  define(`func',`mpn_nior_n')
  define(`VARIANT_3')
  define(`LOGOP',`orq')')
ifdef(`OPERATION_xor_n',`
  define(`func',`mpn_xor_n')
  define(`VARIANT_1')
  define(`LOGOP',`xorq')')
ifdef(`OPERATION_xnor_n',`
  define(`func',`mpn_xnor_n')
  define(`VARIANT_2')
  define(`LOGOP',`xorq')')


MULFUNC_PROLOGUE(mpn_and_n mpn_andn_n mpn_nand_n mpn_ior_n mpn_iorn_n mpn_nior_n mpn_xor_n mpn_xnor_n)

C INPUT PARAMETERS
define(`rp',`%rdi')
define(`up',`%rsi')
define(`vp',`%rdx')
define(`n',`%rcx')


ASM_START()

ifdef(`VARIANT_1',`
	TEXT
	ALIGN(32)
PROLOGUE(func)
	movq	(vp), %r8
	movl	%ecx, %eax
	leaq	(vp,n,8), vp
	leaq	(up,n,8), up
	leaq	(rp,n,8), rp
	negq	n
	andl	$3, %eax
	je	L(b00)
	cmpl	$2, %eax
	jc	L(b01)
	je	L(b10)

L(b11):	LOGOP	(up,n,8), %r8
	movq	%r8, (rp,n,8)
	decq	n
	jmp	L(e11)
L(b10):	addq	$-2, n
	jmp	L(e10)
L(b01):	LOGOP	(up,n,8), %r8
	movq	%r8, (rp,n,8)
	incq	n
	jz	L(ret)

L(oop):	movq	(vp,n,8), %r8
L(b00):	movq	8(vp,n,8), %r9
	LOGOP	(up,n,8), %r8
	LOGOP	8(up,n,8), %r9
	nop
	movq	%r8, (rp,n,8)
	movq	%r9, 8(rp,n,8)
L(e11):	movq	16(vp,n,8), %r8
L(e10):	movq	24(vp,n,8), %r9
	LOGOP	16(up,n,8), %r8
	LOGOP	24(up,n,8), %r9
	movq	%r8, 16(rp,n,8)
	movq	%r9, 24(rp,n,8)
	addq	$4, n
	jnc	L(oop)
L(ret):	ret
EPILOGUE()
')

ifdef(`VARIANT_2',`
	TEXT
	ALIGN(32)
PROLOGUE(func)
	movq	(vp), %r8
	notq	%r8
	movl	%ecx, %eax
	leaq	(vp,n,8), vp
	leaq	(up,n,8), up
	leaq	(rp,n,8), rp
	negq	n
	andl	$3, %eax
	je	L(b00)
	cmpl	$2, %eax
	jc	L(b01)
	je	L(b10)

L(b11):	LOGOP	(up,n,8), %r8
	movq	%r8, (rp,n,8)
	decq	n
	jmp	L(e11)
L(b10):	addq	$-2, n
	jmp	L(e10)
	.byte	0x90,0x90,0x90,0x90,0x90,0x90,0x90,0x90,0x90,0x90,0x90,0x90,0x90
L(b01):	LOGOP	(up,n,8), %r8
	movq	%r8, (rp,n,8)
	incq	n
	jz	L(ret)

L(oop):	movq	(vp,n,8), %r8
	notq	%r8
L(b00):	movq	8(vp,n,8), %r9
	notq	%r9
	LOGOP	(up,n,8), %r8
	LOGOP	8(up,n,8), %r9
	movq	%r8, (rp,n,8)
	movq	%r9, 8(rp,n,8)
L(e11):	movq	16(vp,n,8), %r8
	notq	%r8
L(e10):	movq	24(vp,n,8), %r9
	notq	%r9
	LOGOP	16(up,n,8), %r8
	LOGOP	24(up,n,8), %r9
	movq	%r8, 16(rp,n,8)
	movq	%r9, 24(rp,n,8)
	addq	$4, n
	jnc	L(oop)
L(ret):	ret
EPILOGUE()
')

ifdef(`VARIANT_3',`
	TEXT
	ALIGN(32)
PROLOGUE(func)
	movq	(vp), %r8
	movl	%ecx, %eax
	leaq	(vp,n,8), vp
	leaq	(up,n,8), up
	leaq	(rp,n,8), rp
	negq	n
	andl	$3, %eax
	je	L(b00)
	cmpl	$2, %eax
	jc	L(b01)
	je	L(b10)

L(b11):	LOGOP	(up,n,8), %r8
	notq	%r8
	movq	%r8, (rp,n,8)
	decq	n
	jmp	L(e11)
L(b10):	addq	$-2, n
	jmp	L(e10)
	.byte	0x90,0x90,0x90,0x90,0x90,0x90,0x90,0x90,0x90,0x90
L(b01):	LOGOP	(up,n,8), %r8
	notq	%r8
	movq	%r8, (rp,n,8)
	incq	n
	jz	L(ret)

L(oop):	movq	(vp,n,8), %r8
L(b00):	movq	8(vp,n,8), %r9
	LOGOP	(up,n,8), %r8
	notq	%r8
	LOGOP	8(up,n,8), %r9
	notq	%r9
	movq	%r8, (rp,n,8)
	movq	%r9, 8(rp,n,8)
L(e11):	movq	16(vp,n,8), %r8
L(e10):	movq	24(vp,n,8), %r9
	LOGOP	16(up,n,8), %r8
	notq	%r8
	LOGOP	24(up,n,8), %r9
	notq	%r9
	movq	%r8, 16(rp,n,8)
	movq	%r9, 24(rp,n,8)
	addq	$4, n
	jnc	L(oop)
L(ret):	ret
EPILOGUE()
')
