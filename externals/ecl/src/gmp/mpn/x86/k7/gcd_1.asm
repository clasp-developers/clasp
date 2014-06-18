dnl  AMD K7 mpn_gcd_1 -- mpn by 1 gcd.

dnl  Copyright 2000, 2001, 2002 Free Software Foundation, Inc.
dnl
dnl  This file is part of the GNU MP Library.
dnl
dnl  The GNU MP Library is free software; you can redistribute it and/or
dnl  modify it under the terms of the GNU Lesser General Public License as
dnl  published by the Free Software Foundation; either version 2.1 of the
dnl  License, or (at your option) any later version.
dnl
dnl  The GNU MP Library is distributed in the hope that it will be useful,
dnl  but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl  Lesser General Public License for more details.
dnl
dnl  You should have received a copy of the GNU Lesser General Public
dnl  License along with the GNU MP Library; see the file COPYING.LIB.  If
dnl  not, write to the Free Software Foundation, Inc., 51 Franklin Street,
dnl  Fifth Floor, Boston, MA 02110-1301, USA.

include(`../config.m4')


C K7: 6.75 cycles/bit (approx)  1x1 gcd
C     11.0 cycles/limb          Nx1 reduction (modexact_1_odd)


dnl  Reduce using x%y if x is more than DIV_THRESHOLD bits bigger than y,
dnl  where x is the larger of the two.  See tune/README for more.
dnl
dnl  divl at 40 cycles compared to the gcd at about 7 cycles/bitpair
dnl  suggests 40/7*2=11.4 but 7 seems to be about right.

deflit(DIV_THRESHOLD, 7)


C table[n] is the number of trailing zeros on n, or MAXSHIFT if n==0.
C
C This is mixed in with the code, but as per the k7 optimization manual it's
C a full cache line and suitably aligned so it won't get swapped between
C code and data.  Having it in TEXT rather than RODATA saves needing a GOT
C entry when PIC.
C
C Actually, there doesn't seem to be a measurable difference between this in
C it's own cache line or plonked in the middle of the code.  Presumably
C since TEXT is read-only there's no worries about coherency.

deflit(MASK, 63)
deflit(MAXSHIFT, 6)

	TEXT
	ALIGN(64)
L(table):
	.byte	MAXSHIFT
forloop(i,1,MASK,
`	.byte	m4_count_trailing_zeros(i)
')


C mp_limb_t mpn_gcd_1 (mp_srcptr src, mp_size_t size, mp_limb_t limb);
C

defframe(PARAM_LIMB,   12)
defframe(PARAM_SIZE,    8)
defframe(PARAM_SRC,     4)

defframe(SAVE_EBX,     -4)
defframe(SAVE_ESI,     -8)
defframe(SAVE_EDI,    -12)
defframe(SAVE_EBP,    -16)
defframe(CALL_DIVISOR,-20)
defframe(CALL_SIZE,   -24)
defframe(CALL_SRC,    -28)

deflit(STACK_SPACE, 28)

	TEXT
	ALIGN(16)

PROLOGUE(mpn_gcd_1)
deflit(`FRAME',0)

	ASSERT(ne, `cmpl $0, PARAM_LIMB')	C y!=0
	ASSERT(ae, `cmpl $1, PARAM_SIZE')	C size>=1

	movl	PARAM_SRC, %eax
	movl	PARAM_LIMB, %edx
	subl	$STACK_SPACE, %esp	deflit(`FRAME',STACK_SPACE)

	movl	%esi, SAVE_ESI
	movl	%ebx, SAVE_EBX

	movl	(%eax), %esi		C src low limb

ifdef(`PIC',`
	movl	%edi, SAVE_EDI
	call	L(movl_eip_to_edi)
L(here):
	addl	$L(table)-L(here), %edi
')

	movl	%esi, %ebx
	orl	%edx, %esi	C x|y
	movl	$-1, %ecx

L(twos):
	incl	%ecx
	shrl	%esi
	jnc	L(twos)		C 3/4 chance of x or y odd already

	shrl	%cl, %ebx
	shrl	%cl, %edx
	movl	%ecx, %esi	C common twos

	movl	PARAM_SIZE, %ecx
	cmpl	$1, %ecx
	ja	L(divide)


	C eax
	C ebx	x
	C ecx
	C edx	y
	C esi	common twos
	C edi	[PIC] L(table)
	C ebp

	movl	%edx, %eax
	cmpl	%ebx, %edx

	cmovb(	%ebx, %eax)	C swap to make x bigger than y
	cmovb(	%edx, %ebx)


L(strip_y):
	C eax	x
	C ebx	y
	C ecx
	C edx
	C esi	common twos
	C edi	[PIC] L(table)
	C ebp

	ASSERT(nz,`orl %ebx,%ebx')
	shrl	%ebx
	jnc	L(strip_y)
	rcll	%ebx


	C eax	x
	C ebx	y (odd)
	C ecx
	C edx
	C esi	common twos
	C edi	[PIC] L(table)
	C ebp

	movl	%eax, %ecx
	movl	%ebx, %edx
	shrl	$DIV_THRESHOLD, %eax

	cmpl	%eax, %ebx
	movl	%ecx, %eax
	ja	L(strip_x_entry)	C do x%y if x much bigger than y


	xorl	%edx, %edx

	divl	%ebx

	orl	%edx, %edx
	movl	%edx, %eax		C remainder -> x
	movl	%ebx, %edx		C y

	jz	L(done_ebx)
	jmp	L(strip_x)


	C Offset 0x9D here for non-PIC.  About 0.4 cycles/bit is saved by
	C ensuring the end of the jnz at the end of this loop doesn't cross
	C into the next cache line at 0xC0.
	C
	C PIC on the other hand is offset 0xAC here and extends to 0xC9, so
	C it crosses but doesn't suffer any measurable slowdown.

L(top):
	C eax	x
	C ebx	y-x
	C ecx	x-y
	C edx	y
	C esi	twos, for use at end
	C edi	[PIC] L(table)

	cmovc(	%ebx, %ecx)		C if x-y gave carry, use x and y-x
	cmovc(	%eax, %edx)

L(strip_x):
	movl	%ecx, %eax
L(strip_x_entry):
	andl	$MASK, %ecx

	ASSERT(nz, `orl %eax, %eax')

ifdef(`PIC',`
	movb	(%ecx,%edi), %cl
',`
	movb	L(table) (%ecx), %cl
')

	shrl	%cl, %eax
	cmpb	$MAXSHIFT, %cl

	movl	%eax, %ecx
	movl	%edx, %ebx
	je	L(strip_x)

	ASSERT(nz, `testl $1, %eax')	C both odd
	ASSERT(nz, `testl $1, %edx')

	subl	%eax, %ebx
	subl	%edx, %ecx
	jnz	L(top)


L(done):
	movl	%esi, %ecx
	movl	SAVE_ESI, %esi
ifdef(`PIC',`
	movl	SAVE_EDI, %edi
')

	shll	%cl, %eax
	movl	SAVE_EBX, %ebx
	addl	$FRAME, %esp

	ret



C -----------------------------------------------------------------------------
C two or more limbs

dnl  MODEXACT_THRESHOLD is the size at which it's better to call
dnl  mpn_modexact_1_odd than do an inline loop.

deflit(MODEXACT_THRESHOLD, ifdef(`PIC',6,5))

L(divide):
	C eax	src
	C ebx
	C ecx	size
	C edx	y
	C esi	common twos
	C edi	[PIC] L(table)
	C ebp

L(divide_strip_y):
	ASSERT(nz,`orl %edx,%edx')
	shrl	%edx
	jnc	L(divide_strip_y)
	leal	1(%edx,%edx), %ebx		C y now odd

	movl	%ebp, SAVE_EBP
	movl	%eax, %ebp
	movl	-4(%eax,%ecx,4), %eax		C src high limb

	cmp	$MODEXACT_THRESHOLD, %ecx
	jae	L(modexact)

	cmpl	%ebx, %eax			C high cmp divisor
	movl	$0, %edx

	cmovc(	%eax, %edx)			C skip a div if high<divisor
	sbbl	$0, %ecx


L(divide_top):
	C eax	scratch (quotient)
	C ebx	y
	C ecx	counter (size to 1, inclusive)
	C edx	carry (remainder)
	C esi	common twos
	C edi	[PIC] L(table)
	C ebp	src

	movl	-4(%ebp,%ecx,4), %eax

	divl	%ebx

	decl	%ecx
	jnz	L(divide_top)


	C eax
	C ebx	y (odd)
	C ecx
	C edx	x
	C esi	common twos
	C edi	[PIC] L(table)
	C ebp

	orl	%edx, %edx
	movl	SAVE_EBP, %ebp
	movl	%edx, %eax

	movl	%edx, %ecx
	movl	%ebx, %edx
	jnz	L(strip_x_entry)


L(done_ebx):
	movl	%ebx, %eax
	jmp	L(done)



L(modexact):
	C eax
	C ebx	y
	C ecx	size
	C edx
	C esi	common twos
	C edi	[PIC] L(table)
	C ebp	src

ifdef(`PIC',`
	movl	%ebp, CALL_SRC
	movl	%ebx, %ebp		C y
	movl	%edi, %ebx		C L(table)

	addl	$_GLOBAL_OFFSET_TABLE_+[.-L(table)], %ebx
	movl	%ebp, CALL_DIVISOR
	movl	%ecx, CALL_SIZE

	call	GSYM_PREFIX`'mpn_modexact_1_odd@PLT
',`
dnl non-PIC
	movl	%ebx, CALL_DIVISOR
	movl	%ebp, CALL_SRC
	movl	%ecx, CALL_SIZE

	call	GSYM_PREFIX`'mpn_modexact_1_odd
')

	C eax	x
	C ebx	[non-PIC] y
	C ecx
	C edx
	C esi	common twos
	C edi	[PIC] L(table)
	C ebp	[PIC] y

	orl	%eax, %eax
	movl	ifdef(`PIC',`%ebp',`%ebx'), %edx
	movl	SAVE_EBP, %ebp

	movl	%eax, %ecx
	jnz	L(strip_x_entry)

	movl	%edx, %eax
	jmp	L(done)


ifdef(`PIC', `
L(movl_eip_to_edi):
	movl	(%esp), %edi
	ret_internal
')

EPILOGUE()
