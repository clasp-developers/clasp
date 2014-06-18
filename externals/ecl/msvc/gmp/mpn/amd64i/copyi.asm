
; AMD64 mpn_copyi -- incrementing copy limb vector
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
;  Provided by Brian Gladman AMD64 using the Microsoft VC++ v8 64-bit 
;  compiler and the YASM assembler.
;
;  Calling interface:
;
; void mpn_copyi(
;		mp_ptr dst,		rcx 
;		mp_srcptr src,	rdx
;		mp_size_t size	 r8
; )

%define    UNROLL_THRESHOLD 16

%define	d_ptr	rcx
%define	s_ptr	rdx
%define s_len	 r8
	
	bits 64
	text

	global	__gmpn_copyi

%ifdef DLL
	export	__gmpn_copyi
%endif

__gmpn_copyi:
	movsxd	s_len,r8d
	or      s_len,s_len					; none to move?
	jz      .1
	mov     rax,s_ptr					; find relative alignment of
	xor		rax,d_ptr					; source and destination (min
	mov		r9,s_ptr					; 8-byte alignment assumed)
	lea		s_ptr,[s_ptr+s_len*8]
	lea		d_ptr,[d_ptr+s_len*8]
	neg		s_len
	cmp     s_len,byte -UNROLL_THRESHOLD
	jbe     .2							; if many limbs to move
.0:	mov		rax,[s_ptr+s_len*8]			; short move via rax
	mov     [d_ptr+s_len*8],rax
	inc		s_len
	jnz     .0							; avoid single byte ret that
.1:	rep		ret							; interferes with branch prediction

.2:	test	al,8	
	jnz		.7							; not 16 byte aligned
	test	r9,8						; see if src is on 16 byte
	jz		.3							; boundary  
	mov		rax,[s_ptr+s_len*8]			; if not do a one limb copy
	mov		[d_ptr+s_len*8],rax
	inc		s_len
.3:	lea		s_len,[s_len+3]				; now 16 byte aligned
.4:	prefetchnta	[s_ptr+s_len*8-24+3*64]	; should this be +4*64 ??
	movdqa	xmm0,[s_ptr+s_len*8-24]		; move 32 bytes at a time
	movntdq	[d_ptr+s_len*8-24],xmm0
	movdqa	xmm0,[s_ptr+s_len*8-8]
	movntdq	[d_ptr+s_len*8-8],xmm0
	add		s_len,4
	jl		.4
	sfence
	test	s_len,2
	jnz		.5
	movdqa	xmm0,[s_ptr+s_len*8-24]		; move 16 bytes if necessary
	movdqa  [d_ptr+s_len*8-24],xmm0
	add		s_len,2
.5	test	s_len,1
	jnz		.6
	movq	xmm0,[s_ptr+s_len*8-24]		; move 8 bytes if necessary
	movq	[d_ptr+s_len*8-24],xmm0
.6:	ret

.7:	lea		s_len,[s_len+1]				; move 8 bytes at a time 
.8:	movq	xmm0,[s_ptr+s_len*8-8]
	movq	xmm1,[s_ptr+s_len*8]
	movq	[d_ptr+s_len*8-8],xmm0
	movq	[d_ptr+s_len*8],xmm1
	add		s_len,2
	jl		.8
	test	s_len,1
	jnz		.9
	movq	xmm0,[s_ptr-8]
	movq	[d_ptr-8],xmm0
.9:	ret
	
	end
