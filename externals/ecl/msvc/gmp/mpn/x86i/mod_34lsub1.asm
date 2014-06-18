
;  Copyright 2000, 2001, 2002 Free Software Foundation, Inc.
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

    section .text

%define PARAM_SIZE    esp+frame+8
%define PARAM_SRC     esp+frame+4

; re-use parameter space 
%define	SAVE_EBX	PARAM_SRC

	global  ___gmpn_mod_34lsub1
%ifdef	DLL
	export	___gmpn_mod_34lsub1
%endif

    align   16
___gmpn_mod_34lsub1:
%assign       frame   0
    mov     ecx,[PARAM_SIZE]
    mov     edx,[PARAM_SRC]
    sub     ecx,2
    ja      Lthree_or_more
    mov     eax,[edx]
    jb      Lone
    mov     ecx,[4+edx]
    mov     edx,eax
    shr     eax,24				; src[0] low 

    and     edx,0xFFFFFF		; src[0] high 
    add     eax,edx
    mov     edx,ecx

    and     ecx,0xFFFF
    shr     edx,16				; src[1] high 
    add     eax,edx

    shl     ecx,8				; src[1] low 
    add     eax,ecx
Lone:
    ret

; eax 
; ebx 
; ecx  size-2 
; edx  src 
; esi 
; edi 
; ebp 

Lthree_or_more:
    mov     [SAVE_EBX],ebx	; and arrange 16-byte loop alignment 
    xor     ebx,ebx
	FR_push esi
    xor     esi,esi
	FR_push edi
    xor     eax,eax         ; and clear carry flag 

; offset 0x40 here 
; eax  acc 0mod3 
; ebx  acc 1mod3 
; ecx  counter,limbs 
; edx  src 
; esi  acc 2mod3 
; edi 
; ebp 

Ltop:
    lea     edx,[12+edx]
    lea     ecx,[-2+ecx]
    adc     eax,[-12+edx]
    adc     ebx,[-8+edx]
    adc     esi,[-4+edx]
    dec     ecx
    jg      Ltop
; ecx is -2,-1 or 0 representing 0,1 or 2 more limbs,respectively 
    mov     edi,0xFFFFFFFF
    inc     ecx
    js      Lcombine
    adc     eax,[edx]
    mov     edi,0xFFFFFF00
    dec     ecx
    js      Lcombine
    adc     ebx,[4+edx]
    mov     edi,0xFFFF0000

; eax  acc 0mod3 
; ebx  acc 1mod3 
; ecx 
; edx 
; esi  acc 2mod3 
; edi  mask 
; ebp 

Lcombine:
    sbb     ecx,ecx			; carry 
    mov     edx,eax         ; 0mod3 
    shr     eax,24          ; 0mod3 high 
    and     ecx,edi         ; carry masked 
    sub     eax,ecx         ; apply carry 
    mov     edi,ebx         ; 1mod3 
    shr     ebx,16          ; 1mod3 high 
    and     edx,0x00FFFFFF  ; 0mod3 low 
    add     eax,edx         ; apply 0mod3 low 
    and     edi,0xFFFF
    shl     edi,8			; 1mod3 low 
    add     eax,ebx         ; apply 1mod3 high 
    add     eax,edi         ; apply 1mod3 low 
    mov     edx,esi         ; 2mod3 
    shr     esi,8			; 2mod3 high 
    and     edx,0xFF        ; 2mod3 low 
    shl     edx,16          ; 2mod3 low 
    add     eax,esi         ; apply 2mod3 high 
    add     eax,edx         ; apply 2mod3 low 
	FR_pop  edi
    mov     ebx,[SAVE_EBX]
	FR_pop  esi
    ret

	end
