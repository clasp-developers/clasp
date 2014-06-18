
; AMD64 mpn_copyd -- decrementing copy limb vector
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
; void mpn_copyd(
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

	global	__gmpn_copyd

%ifdef DLL
	export	__gmpn_copyd
%endif

__gmpn_copyd:
	movsxd	s_len,r8d
	cmp     s_len,byte UNROLL_THRESHOLD
	jge     .2							; if many limbs to move
	dec		s_len
	jl      .1
.0:	mov		rax,[s_ptr+s_len*8]			; short move via rax
	mov     [d_ptr+s_len*8],rax
	dec		s_len
	jge     .0							; avoid single byte ret that 
.1:	rep		ret							; interferes with branch prediction

.2:	mov     rax,s_ptr					; find relative alignment of
	xor		rax,d_ptr					; source and destination (min
	test	al,8	
	jnz		.7							; not 16 byte aligned
	lea		rax,[s_ptr+s_len*8]	
	test	al,8						; see if src is on 16 byte
	jz		.3							; boundary 	 
	dec		s_len	
	mov		rax,[rax-8]					; if not do a one limb copy
	mov		[d_ptr+s_len*8],rax
.3:	lea		s_len,[s_len-4]				; now 16 byte aligned
.4:	prefetchnta	[s_ptr+s_len*8+16-3*64]	; should this be -4*64 ??
	movdqa	xmm0,[s_ptr+s_len*8+16]		; move 32 bytes at a time
	movntdq	[d_ptr+s_len*8+16],xmm0
	movdqa	xmm0,[s_ptr+s_len*8]
	movntdq	[d_ptr+s_len*8],xmm0
	sub		s_len,4
	jge		.4
	sfence
	test	s_len,2
	jz		.5
	movdqa	xmm0,[s_ptr+s_len*8+16]		; move 16 bytes if necessary
	movdqa  [d_ptr+s_len*8+16],xmm0
.5	test	s_len,1
	jz		.6
	movq	xmm0,[s_ptr]				; move 8 bytes if necessary
	movq	[d_ptr],xmm0
.6:	ret

.7:	lea		s_len,[s_len-2]				; move 8 bytes at a time 
.8:	movq	xmm0,[s_ptr+s_len*8+8]
	movq	xmm1,[s_ptr+s_len*8]
	movq	[d_ptr+s_len*8+8],xmm0
	movq	[d_ptr+s_len*8],xmm1
	sub		s_len,2
	jge		.8
	test	s_len,1
	jz		.9
	movq	xmm0,[s_ptr]
	movq	[d_ptr],xmm0
.9:	ret
	
	end
