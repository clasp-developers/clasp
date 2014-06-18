
;  Copyright 1992, 1994, 1996, 1999, 2000, 2001, 2002 Free Software
;  Foundation, Inc.
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
; Translation of AT&T syntax code by Brian Gladman 

%include	"x86i.inc"

%define PARAM_SHIFT esp+frame+16
%define PARAM_SIZE  esp+frame+12
%define PARAM_SRC   esp+frame+8
%define PARAM_DST   esp+frame+4
%assign frame		frame+12

	section .text

	global  ___gmpn_rshift
%ifdef	DLL
	export	___gmpn_rshift
%endif

	align   8
___gmpn_rshift: 
    push    edi
    push    esi
    push    ebx
    mov     edi,[PARAM_DST]
    mov     esi,[PARAM_SRC]
    mov     edx,[PARAM_SIZE]
    mov     ecx,[PARAM_SHIFT]
    lea     edi,[-4+edi+edx*4]
    lea     esi,[esi+edx*4]
    neg     edx
    mov     ebx,[esi+edx*4]	; read least significant limb 
    xor     eax,eax
	shrd	eax,ebx,cl
    inc     edx
    jz      Lend
    push    eax             ; push carry limb onto stack 
    test    dl,1
    jnz     L1				; enter Lop in the middle 
    mov     eax,ebx

	align   8
Lop:	
	mov     ebx,[esi+edx*4]	; load next higher limb 
	shrd	eax,ebx,cl
    mov     [edi+edx*4],eax ; store it 
    inc     edx
L1:
	mov     eax,[esi+edx*4]
	shrd	ebx,eax,cl
    mov     [edi+edx*4],ebx
    inc     edx
    jnz     Lop
    shr     eax,cl          ; compute most significant limb 
    mov     [edi],eax       ; store it 
    pop     eax             ; pop carry limb 
    pop     ebx
    pop     esi
    pop     edi
    ret
Lend:
	shr     ebx,cl			; compute most significant limb 
    mov     [edi],ebx       ; store it 
    pop     ebx
    pop     esi
    pop     edi
    ret

	end
