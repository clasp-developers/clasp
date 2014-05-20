dnl  AMD K7 mpn_gcd_1 -- mpn by 1 gcd.

dnl  Copyright 2000, 2001, 2002, 2009 Free Software Foundation, Inc.
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

deflit(MAXSHIFT, 6)
deflit(MASK, eval((m4_lshift(1,MAXSHIFT))-1))

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

	mov	PARAM_SRC, %eax
	mov	PARAM_LIMB, %edx
	sub	$STACK_SPACE, %esp	deflit(`FRAME',STACK_SPACE)

	mov	%esi, SAVE_ESI
	mov	%ebx, SAVE_EBX

	mov	(%eax), %esi		C src low limb

ifdef(`PIC',`
	mov	%edi, SAVE_EDI
	call	L(movl_eip_to_edi)
L(here):
	add	$L(table)-L(here), %edi
')

	mov	%esi, %ebx
	or	%edx, %esi	C x|y
	mov	$-1, %ecx

L(twos):
	inc	%ecx
	shr	%esi
	jnc	L(twos)		C 3/4 chance of x or y odd already

	shr	%cl, %ebx
	shr	%cl, %edx
	mov	%ecx, %esi	C common twos

	mov	PARAM_SIZE, %ecx
	cmp	$1, %ecx
	ja	L(divide)


	C eax
	C ebx	x
	C ecx
	C edx	y
	C esi	common twos
	C edi	[PIC] L(table)
	C ebp

	mov	%edx, %eax
	cmp	%ebx, %edx

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
	shr	%ebx
	jnc	L(strip_y)
	rcl	%ebx


	C eax	x
	C ebx	y (odd)
	C ecx
	C edx
	C esi	common twos
	C edi	[PIC] L(table)
	C ebp

	mov	%eax, %ecx
	mov	%ebx, %edx
	shr	$DIV_THRESHOLD, %eax

	cmp	%eax, %ebx
	mov	%ecx, %eax
	ja	L(strip_x_entry)	C do x%y if x much bigger than y


	xor	%edx, %edx

	div	%ebx

	or	%edx, %edx
	mov	%edx, %ecx		C remainder -> x
	mov	%ebx, %edx		C y

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
	mov	%ecx, %eax
L(strip_x_entry):
	and	$MASK, %ecx

	ASSERT(nz, `orl %eax, %eax')

ifdef(`PIC',`
	mov	(%ecx,%edi), %cl
',`
	mov	L(table) (%ecx), %cl
')

	shr	%cl, %eax
	cmp	$MAXSHIFT, %cl

	mov	%eax, %ecx
	mov	%edx, %ebx
	je	L(strip_x)

	ASSERT(nz, `test $1, %eax')	C both odd
	ASSERT(nz, `test $1, %edx')

	sub	%eax, %ebx
	sub	%edx, %ecx
	jnz	L(top)


L(done):
	mov	%esi, %ecx
	mov	SAVE_ESI, %esi
ifdef(`PIC',`
	mov	SAVE_EDI, %edi
')

	shl	%cl, %eax
	mov	SAVE_EBX, %ebx
	add	$FRAME, %esp

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
	ASSERT(nz,`or %edx,%edx')
	shr	%edx
	jnc	L(divide_strip_y)
	lea	1(%edx,%edx), %ebx		C y now odd

	mov	%ebp, SAVE_EBP
	mov	%eax, %ebp
	mov	-4(%eax,%ecx,4), %eax		C src high limb

	cmp	$MODEXACT_THRESHOLD, %ecx
	jae	L(modexact)

	cmp	%ebx, %eax			C high cmp divisor
	mov	$0, %edx

	cmovc(	%eax, %edx)			C skip a div if high<divisor
	sbb	$0, %ecx


L(divide_top):
	C eax	scratch (quotient)
	C ebx	y
	C ecx	counter (size to 1, inclusive)
	C edx	carry (remainder)
	C esi	common twos
	C edi	[PIC] L(table)
	C ebp	src

	mov	-4(%ebp,%ecx,4), %eax

	div	%ebx

	dec	%ecx
	jnz	L(divide_top)


	C eax
	C ebx	y (odd)
	C ecx
	C edx	x
	C esi	common twos
	C edi	[PIC] L(table)
	C ebp

	or	%edx, %edx
	mov	SAVE_EBP, %ebp
	mov	%edx, %eax

	mov	%edx, %ecx
	mov	%ebx, %edx
	jnz	L(strip_x_entry)


L(done_ebx):
	mov	%ebx, %eax
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
	mov	%ebp, CALL_SRC
	mov	%ebx, %ebp		C y
	mov	%edi, %ebx		C L(table)

	add	$_GLOBAL_OFFSET_TABLE_+[.-L(table)], %ebx
	mov	%ebp, CALL_DIVISOR
	mov	%ecx, CALL_SIZE

	call	GSYM_PREFIX`'mpn_modexact_1_odd@PLT
',`
dnl non-PIC
	mov	%ebx, CALL_DIVISOR
	mov	%ebp, CALL_SRC
	mov	%ecx, CALL_SIZE

	call	GSYM_PREFIX`'mpn_modexact_1_odd
')

	C eax	x
	C ebx	[non-PIC] y
	C ecx
	C edx
	C esi	common twos
	C edi	[PIC] L(table)
	C ebp	[PIC] y

	or	%eax, %eax
	mov	ifdef(`PIC',`%ebp',`%ebx'), %edx
	mov	SAVE_EBP, %ebp

	mov	%eax, %ecx
	jnz	L(strip_x_entry)

	mov	%edx, %eax
	jmp	L(done)


ifdef(`PIC', `
L(movl_eip_to_edi):
	mov	(%esp), %edi
	ret_internal
')

EPILOGUE()
