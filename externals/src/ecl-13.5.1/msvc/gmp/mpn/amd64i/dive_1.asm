;
; AMD64 mpn_divexact_1 -- mpn by limb exact division
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
; void mpn_divexact_1(
;		mp_ptr dst,			rcx
;       mp_srcptr src,		rdx
;       mp_size_t size,	     r8
;       mp_limb_t divisor    r9
; )
;
; since the inverse takes a while to setup,plain division is used for small 
; Multiplying works out faster for size>=3 when the divisor is odd or size>=4 
; when the divisor is even.

	bits 64
	text
	
	extern	__gmp_modlimb_invert_table
	global	__gmpn_divexact_1

%ifdef DLL
	export	__gmpn_divexact_1
%endif

__gmpn_divexact_1:
	movsxd	r8,r8d
	mov		r10,rdx
	mov     rax,r9
	and     rax,byte 1
	add     rax,r8
	cmp     rax,byte 4
	jae     L_mul_by_inverse
	xor     rdx,rdx
L_div_top:
	mov     rax,[r10+r8*8-8]
	div     r9
	mov     [rcx+r8*8-8],rax
	dec     r8
	jnz     L_div_top
	rep		ret			; avoid single byte return

L_mul_by_inverse:
	push	rsi
	push	rdi
	mov		rsi,rdx		; src pointer
	mov		rdi,rcx		; dst pointer
	mov     rax,r9
	stc
	sbb		rcx,rcx		; -1 -> rcx, r11
	mov		r11,rcx
L_strip_twos:
	shr     rax,1
	inc     rcx
	jnc     L_strip_twos
	lea     r9,[rax+rax+1]
	and     rax,byte 127
	lea		rdx,[__gmp_modlimb_invert_table wrt rip]
	movzx   rax,byte [rdx+rax]

; If f(x) = 0, then x[n+1] = x[n] - f(x) / f'(x) is Newton's iteration for a
; root. With f(x) = 1/x - v we obtain x[n + 1] = 2 * x[n] - v * x[n] * x[n]
; as an iteration for x = 1 / v.  This provides quadratic convergence so
; that the number of bits of precision doubles on each iteration.  The
; iteration starts with 8-bit precision.  

	lea     edx, [rax+rax]
	imul    eax, eax
	imul    eax, r9d
	sub     edx, eax				; inv -> rdx (16-bit approx)

	lea     eax, [rdx+rdx]
	imul    edx, edx
	imul    edx, r9d
	sub     eax, edx				; inv -> rcx (32-bit approx)

	lea     rdx, [rax+rax]
	imul    rax, rax
	imul    rax, r9
	sub     rdx, rax				; inv -> rcx (64-bit approx)

	mov     r8,r8
	lea     rsi,[rsi+r8*8]
	lea     rdi,[rdi+r8*8]
	neg     r8
	
	mov     r10,rdx
	xor     r11,r11
	mov     rax,[rsi+r8*8]
	or      rcx,rcx
	mov     rdx,[rsi+r8*8+8]
	jz      L_odd_entry
	shrd	rax,rdx,cl
	inc     r8
	jmp     L_even_entry
	
L_odd_top:
	mul     r9
	mov     rax,[rsi+r8*8]
	sub     rdx,r11
	sub     rax,rdx
	sbb     r11,r11
L_odd_entry:
	imul	rax,r10
	mov     [rdi+r8*8],rax
	inc     r8
	jnz     L_odd_top
	pop		rdi
	pop		rsi
	ret
	
L_even_top:
	mul     r9
	sub     rdx,r11
	mov     rax,[rsi+r8*8-8]
	mov     r11,[rsi+r8*8]
	shrd	rax,r11,cl
	sub     rax,rdx
	sbb     r11,r11
	
L_even_entry:
	imul    rax,r10
	mov     [rdi+r8*8-8],rax
	inc     r8
	jnz     L_even_top
	mul     r9
	mov     rax,[rsi-8]
	sub     rdx,r11
	shr     rax,cl
	sub     rax,rdx
	imul    rax,r10
	mov     [rdi-8],rax
	pop		rdi
	pop		rsi
	ret
	       
	end
