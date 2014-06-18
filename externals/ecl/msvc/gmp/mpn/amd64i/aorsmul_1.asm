;
; AMD64 mpn_add_n/mpn_sub_n -- mpn add or subtract.
;
;  Copyright 1999, 2000, 2001, 2002 Free Software Foundation, Inc.
;   
;  This file is part of the GNU MP Library.
;   
;  The GNU MP Library is free software; you can redistribute it and/or
;  modify it under the terms of the GNU Lesser General Public License as
;  published by the Free Software Foundation; either version 2.1 of the
;  License, or (at your option) any later version.
;   
;  The GNU MP Library is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  Lesser General Public License for more details.
;   
;  You should have received a copy of the GNU Lesser General Public
;  License along with the GNU MP Library; see the file COPYING.LIB.  If
;  not, write to the Free Software Foundation, Inc., 59 Temple Place -
;  Suite 330, Boston, MA 02111-1307, USA.
;
;  Adapted by Brian Gladman AMD64 using the Microsoft VC++ v8 64-bit 
;  compiler and the YASM assembler.
;
;  Calling interface:
;
;  mp_limb_t __gmpn_<op>mul_1(			<op> = add or sub
;		mp_ptr dst,				ecx
;		mp_srcptr src,			edx
;		mp_size_t size,			 r8                 
;		mp_limb_t mult			 r9
;  )
;
;  mp_limb_t __gmpn_<op>mul_1c(
;		mp_ptr dst,				ecx
;		mp_srcptr src,			edx
;		mp_size_t size,			 r8                  
;		mp_limb_t mult,			 r9
;		mp_limb_t carry  [rsp+0x28]
;  )
;
; Calculate src[size] multiplied by mult[1] and add to /subtract from dst[size] and 
; return the carry or borrow from the top of the result

%define dst		rcx
%define len      r8
%define mlt		 r9
%define src		r10
%define cry		r11

%define UNROLL_LOG2			4
%define UNROLL_COUNT		(1 << UNROLL_LOG2)
%define UNROLL_MASK			(UNROLL_COUNT - 1)
%define	UNROLL_BYTES		(8 * UNROLL_COUNT)
%define UNROLL_THRESHOLD	9

%if UNROLL_BYTES >= 256
%error unroll count is too large
%elif UNROLL_BYTES >= 128
%define off	128
%else
%define off	0
%endif


%macro	mac_sub	4

	global	%1%3
	global	%1%4

%ifdef DLL
	export	%1%3
	export	%1%4
%define		PIC
%endif

%1%3:
	movsxd	len,r8d
	mov		src,rdx				; source ptr
	xor		cry,cry				; carry = 0
	dec		len					; test for one limb only
	jnz		%%0					; if more than one
	mov		rax,[src]			; get limb value
	mul		mlt					; rax * mlt -> rdx (hi), rax (lo)
	%2		[dst],rax			; add/sub from destination
	adc		rdx,byte 0			; add any carry into high word
	mov		rax,rdx				; and return the carry value
	ret
%1%4:
	movsxd	len,r8d
	mov		src,rdx				; source pointer
	mov		cry,[rsp+0x28]		; carry value
	dec		len					; test for one limb
	jnz		%%0					; if more than one
	mov		rax,[src]			; get limb value
	mul		mlt					; rax * mlt -> rdx (hi), rax (lo)
	add		rax,cry				; add in input carry
	adc		rdx,byte 0			; propagate it into rdx
	%2		[dst],rax			; add or subtract rax from dest limb
	adc		rdx,byte 0			; propagate carry into high word
	mov		rax,rdx
	ret

%%0:
	cmp		len,byte UNROLL_THRESHOLD
	mov		rax,[src]			; first limb of source
	ja		%%2					; unroll for many limbs
	lea		src,[src+len*8+8]	; next source limb
	lea		dst,[dst+len*8]		; current dst limb
	neg		len
%%1:	
	mul		mlt					; multiply current src limb -> rxx, rax
	add		rax,cry				; add in carry
	adc		rdx,byte 0			; propagate carry into rdx
	%2		[dst+len*8],rax		; add or subtract rax from dest limb
	mov		rax,[src+len*8]		; get next source limb
	adc		rdx,byte 0			; add carry or borrow into high word
	inc		len					; go to next limb
	mov		cry,rdx				; high word -> carry
	jnz		%%1
	mul		mlt					; one more limb to do
	add		rax,cry
	adc		rdx,byte 0
	%2		[dst],rax		
	adc		rdx,byte 0
	mov		rax,rdx				; return carry value as a limb
	ret

%define	jmp_val	rbp				; jump into code sequence
%define rep_cnt	rbx				; repeats for full sequence
%define cry_hi	rsi				; second carry for alternate block

%%2:
	push	rbp
	push	rbx
	push	rsi
	lea		rep_cnt,[len-2]
	dec		len
	shr		rep_cnt,UNROLL_LOG2
	neg		len
	and		len,UNROLL_MASK
	mov		jmp_val,len
	mov		cry_hi,len			; cry_hi and jmp_val are temporary
	shl		jmp_val,2			; values for calculating the jump
	shl		cry_hi,4			; offset into the unrolled code
%ifdef PIC
	lea		cry_hi,[cry_hi+jmp_val]
	lea		jmp_val,[%%3 wrt rip]
	lea		jmp_val,[jmp_val+cry_hi]
%else
	lea	jmp_val,[cry_hi+jmp_val+%%3]
%endif
	neg		len
	mul		mlt
	add		cry,rax				; initial carry, becomes low carry
	adc		rdx,byte 0
	mov		cry_hi,rdx
	test	len,1
	mov		rax,[src+8]			; src second limb
	lea		src,[src+len*8+off+16]
	lea		dst,[dst+len*8+off]
	cmovnz	cry_hi,cry			; high, low carry other way around
	cmovnz	cry,rdx
	xor		len,len
	jmp		jmp_val

%%3:
%define CHUNK_COUNT	2
%assign i 0
%rep	UNROLL_COUNT / CHUNK_COUNT
%assign	disp0	8 * i * CHUNK_COUNT - off

	mul		mlt
	%2		[byte dst+disp0],cry
	mov		cry,len						; len = 0
	adc		cry_hi,rax
	mov		rax,[byte src+disp0]
	adc		cry,rdx	
	mul		mlt
	%2		[byte dst+disp0+8],cry_hi
	mov		cry_hi,len					; len = 0
	adc		cry,rax
	mov		rax,[byte src+disp0+8]
	adc		cry_hi,rdx

%assign i	i + 1
%endrep

	dec		rep_cnt
	lea		src,[src+UNROLL_BYTES]
	lea		dst,[dst+UNROLL_BYTES]
	jns		%%3
	mul		mlt
	%2		[dst-off],cry
	adc		rax,cry_hi
	adc		rdx,len
	%2		[dst-off+8],rax
	adc		rdx,len
	mov		rax,rdx
	pop		rsi
	pop		rbx
	pop		rbp
	ret

%endmacro

	bits 64
	text
	
	mac_sub	__g,add,mpn_addmul_1,mpn_addmul_1c
	mac_sub	__g,sub,mpn_submul_1,mpn_submul_1c

	end
