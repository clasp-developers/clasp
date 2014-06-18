;
; AMD64 mpn_mul_basecase -- multiply two mpn numbers.
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
; void __gmpn_mul_basecase(
;		mp_ptr rp,			rcx
;		mp_srcptr xp,		rdx
;		mp_size_t xn,		 r8
;		mp_srcptr yp,		 r9
;		mp_size_t yn [rsp+0x28]		as a *** 32-bit *** word
; )
;
; Multiply up[un] by vp[vn] and write the result to rp[un+vn] with un >= vn on
; entry. 

%include "amd64i.inc"

%define UNROLL_LOG2			4
%define UNROLL_COUNT		(1 << UNROLL_LOG2)
%define UNROLL_MASK			(UNROLL_COUNT - 1)
%define	UNROLL_BYTES		(8 * UNROLL_COUNT)
%define UNROLL_THRESHOLD	5

	bits 64
	text

%define v_par	rsp + 16
%define v_adj	rsp +  8
%define v_xlo	rsp
%define v_len	24

%define	r_ptr	rcx
%define	x_ptr	r11
%define	x_len	 r8
%define	y_ptr	 r9
%define	y_len	r10

%define v_ctr	 r8		; x_len reused
%define v_jmp	r11		; x_ptr reused

%define reg_list	x86_regs,r12

	global	__gmpn_mul_basecase

%ifdef DLL
	export	__gmpn_mul_basecase
%define		PIC
%endif

__gmpn_mul_basecase:
	movsxd	x_len,r8d
	mov     rax,[y_ptr]
	cmp     x_len,2
	ja      mul_m_by_n
	je      mul_2_by_n
	mul		qword [rdx]
	mov     [r_ptr],rax
	mov     [r_ptr+8],rdx
	ret
	
mul_2_by_n:
	movsxd	r10,dword[rsp+0x28]	; load as a 32-bit integer
	mov		x_ptr,rdx
	dec		qword y_len
	jnz     mul_2_by_2
	mov     r8,rax			; y[0] -> r8 (was x_len)	
	mov     rax,[x_ptr]
	mul     r8	
	mov     [r_ptr],rax
	mov     rax,[x_ptr+8]
	mov     r9,rdx			; carry -> r9 (was y_ptr)
	mul     r8
	add		r9,rax
	mov     [r_ptr+8],r9
	adc     rdx,y_len		; note: r10 = 0 (was y_len)
	mov     [r_ptr+16],rdx
	ret

mul_2_by_2:				; r8 (x_len) and r10 (y_len) free
	mov     r10,[x_ptr]		; x[0]
	mul     r10				; y[0] * x[0]
	mov		[r_ptr],rax
	mov     r8,rdx			; cry = { 0, r8 }
	mov     rax,[y_ptr+8]	; y[1]
	mul		r10				; y[1] * x[0]
	add		r8,rax
	adc		rdx,byte 0
	mov		r10,[x_ptr+8]	; x[1] - r11 (x_ptr) now free  
	mov		r11,rdx			; cry = { r11, r8 }
	mov		rax,[y_ptr]		; y[0]
	mul		r10				; y[0] * x[1]
	add		r8,rax
	adc		r11,rdx
	mov		[r_ptr+8],r8
	mov		r8,dword 0
	adc		r8,r8			; cry = { r8, r11 }
	mov		rax,[y_ptr+8]	; y[1]
	mul		r10				; x[1] * y[1]
	add		rax,r11
	adc		rdx,r8
	mov		[r_ptr+16],rax
	mov		[r_ptr+24],rdx
	ret

; do first multiply of y[0] * x[n] as it can simply be stored

mul_m_by_n:
	mov		r10d,dword[rsp+0x28]	; load as a 32-bit integer
	f_push	reg_list
	mov		x_ptr,rdx	
	mov     r12,x_len
	mov     rbp,rax					; y[0] -> rbp
	xor     rbx,rbx					; for carry
	lea     rsi,[x_ptr+r12*8]		; past end of x[]
	lea     rdi,[r_ptr+r12*8]		; past end of r[]
	neg     r12	
.0:	mov		rax,[rsi+r12*8]			; x[n]
	mul     rbp						; x[n] * y[0]
	add     rax,rbx					; add carry from previous round
	mov     [rdi+r12*8],rax			; store r[n]
	mov     rbx,dword 0				; propagate carry
	adc     rbx,rdx
	inc     r12						; next iteration
	jnz     .0
	mov		[rdi],rbx				; store final digit in carry
	mov     rdx,y_len				; done if y_len is 1
	dec     rdx
	jnz     .1						; more to do
	f_pop	reg_list
	ret
	
.1:	cmp     x_len,UNROLL_THRESHOLD	; unroll if many loops
	jae     L_unroll
	lea     y_ptr,[y_ptr+rdx*8+8]	; pointer to end limb of y[]
	neg     x_len					; negative counter for x[n]
	neg     rdx						; negative counter for y[n]
	mov     rax,[rsi+x_len*8]			; x[0] -> rax
	mov     y_len,rdx				; now -(y_len - 1)
	inc     x_len					; negative counter for x[1]
	xor     rbx,rbx					; for carry
	mov     rcx,x_len				; now -(x_len - 1) -> rcx (was r_ptr)
	mov     rbp,[y_ptr+rdx*8]		; y[n] -> rbp
	jmp     .3
.2:	mov     rcx,x_len				; restore x[] counter
	xor     rbx,rbx					; clear carry
	add     rdi,8					; increase end of r[] pointer
	mov     rbp,[y_ptr+y_len*8]		; y[n] -> rbp
	mov     rax,[rsi+rcx*8-8]		; x[m] -> rax
.3:	mul		rbp						; x[m] * y[n]
	add     rbx,rax					; add carry
	adc     rdx,byte 0
	add     [rdi+rcx*8],rbx			; add into r[]
	mov     rax,[rsi+rcx*8]			; next x[m] ->rax
	adc     rdx,byte 0				; add carry to rdx
	inc     rcx						; got to next limb of x[]
	mov     rbx,rdx					; move carry into rbx
	jnz     .3						; got to next limb of x[]
	mul     rbp						; do last limb
	add     rbx,rax					; propagate carry
	adc     rdx,byte  0
	add     [rdi],rbx				; add into r[]
	adc     rdx,byte 0				; add add in any carry
	inc     y_len
	mov     [rdi+8],rdx				; move (not add) carry into r[]
	jnz     .2						; go to next limb of y[]
	f_pop	reg_list
	ret

L_unroll:
	f_add	v_len
	mov		rdi,r_ptr
	mov     rcx,x_len
	mov		rsi,x_ptr
	mov     rbp,[y_ptr+8]
	lea     y_ptr,[y_ptr+rdx*8+8]	
	neg     rdx
	mov     y_len,rdx
	lea     rbx,[UNROLL_COUNT-2+rcx]
	dec     rcx
	mov     rax,[rsi]				; x[0]
	and     rbx,-UNROLL_MASK-1
	neg     rcx
	neg     rbx
	and     rcx,UNROLL_MASK
	mov		[v_par],rcx
	mov     [v_adj],rbx
	mov     rdx,rcx
	shl     rcx,3
%ifdef PIC
	lea		rcx,[rcx+rcx*2]
	lea		v_jmp,[.4 wrt rip]
	lea		v_jmp,[v_jmp+rcx]
%else
	lea		v_jmp,[rcx+rcx*2+.4]
%endif
	neg     rdx
	mov     [v_xlo],rax
	lea     rdi,[rdi+rdx*8+8]
	lea     rsi,[rsi+rdx*8+8]
	jmp     .3
.2:	mov		rbx,[v_adj]
	mov     rax,[v_xlo]
	lea     rdi,[rdi+rbx*8+8]
	lea     rsi,[rsi+rbx*8]
	mov     rbp,[y_ptr+y_len*8]
.3:	mul     rbp
	sar     rbx,UNROLL_LOG2
	mov		rcx,[v_par]
	mov     v_ctr,rbx	
	test    cl,1			; low word of product + carry 
	mov     rbx,dword 0		; is in rcx on even rounds and
	mov     rcx,dword 0		; rbx on odd rounds - we must
	cmovz	rcx,rax			; put low word of first product
	cmovnz	rbx,rax			; in the right register here
	jmp		v_jmp	
.4:

%define CHUNK_COUNT	2
%assign i 0
%rep UNROLL_COUNT / CHUNK_COUNT
%define	disp0	8 * i * CHUNK_COUNT
	
	mov		rax,[byte rsi+disp0]
	adc     rbx,rdx
	mul     rbp
	add		[byte rdi+disp0],rcx
	mov		rcx,dword 0
	adc		rbx,rax
	mov     rax,[byte rsi+disp0+8]
	adc		rcx,rdx
	mul     rbp
	add		[byte rdi+disp0+8],rbx
	mov     rbx,dword 0	
	adc     rcx,rax
	
%assign	i i + 1
%endrep

	inc		v_ctr
	lea     rsi,[UNROLL_BYTES+rsi]
	lea     rdi,[UNROLL_BYTES+rdi]
	jnz     .4

	adc		rdx,byte 0
	add		[rdi],rcx
	adc     rdx,byte 0
	inc     y_len
	mov     [rdi+8],rdx
	jnz     .2
	f_sub	v_len
	f_pop	reg_list
	ret

	end
