
;  Copyright 1996, 1997, 1998, 1999, 2000, 2001, 2002 Free Software
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

%include "x86i.inc"


%define	VAR_STACK_SPACE	8
%define PARAM_YSIZE		esp+frame+20
%define PARAM_YP		esp+frame+16
%define PARAM_XSIZE		esp+frame+12
%define PARAM_XP		esp+frame+8
%define PARAM_WP		esp+frame+4
%define	VAR_MULTIPLIER	esp+frame-4
%define	VAR_COUNTER		esp+frame-8

	section .text

	global  ___gmpn_mul_basecase
%ifdef	DLL
	export	___gmpn_mul_basecase
%endif

	align   8	
___gmpn_mul_basecase: 
    sub     esp,VAR_STACK_SPACE
    push    esi
    push    ebp
    push    edi
%assign	frame			VAR_STACK_SPACE+12
    mov     esi,[PARAM_XP]
    mov     edi,[PARAM_WP]
    mov     ebp,[PARAM_YP]
    mov     eax,[esi]				; load xp[0] 
    mul     dword [ebp]				; multiply by yp[0] 
    mov     [edi],eax				; store to wp[0] 
    mov     ecx,[PARAM_XSIZE]		; xsize 
    dec     ecx						; If xsize = 1,ysize = 1 too 
    jz      Ldone
    FR_push	ebx
    mov     ebx,edx
    lea     esi,[4+esi]
    lea     edi,[4+edi]
LoopM:
    mov     eax,[esi]				; load next limb at xp[j] 
    lea     esi,[4+esi]
    mul     dword [ebp]
    add     eax,ebx
    mov     ebx,edx
    adc     ebx,0
    mov     [edi],eax
    lea     edi,[4+edi]
    dec     ecx
    jnz     LoopM
    mov     [edi],ebx				; most significant limb of product 
    add     edi,4					; increment wp 
    mov     eax,[PARAM_XSIZE]
    shl     eax,2
    sub     edi,eax
    sub     esi,eax
    mov     eax,[PARAM_YSIZE]		; ysize 
    dec     eax
    jz      Lskip
    mov     [VAR_COUNTER],eax		; set index i to ysize 
Louter:
    mov     ebp,[PARAM_YP]			; yp 
    add     ebp,4					; make ebp point to next v limb 
    mov     [PARAM_YP],ebp
    mov     eax,[ebp]				; copy y limb ... 
    mov     [VAR_MULTIPLIER],eax	; ... to stack slot 
    mov     ecx,[PARAM_XSIZE]
    xor     ebx,ebx
    and     ecx,3
    jz      Lend0
Loop0:
    mov     eax,[esi]
    mul     dword [VAR_MULTIPLIER]
    lea     esi,[4+esi]
    add     eax,ebx
    mov     ebx,0
    adc     edx,ebx
    add     [edi],eax
    adc     ebx,edx					; propagate carry into cylimb 
    lea     edi,[4+edi]
    dec     ecx
    jnz     Loop0
Lend0:
    mov     ecx,[PARAM_XSIZE]
    shr     ecx,2
    jz      LendX

	align   8
LoopX:	 
    mov     eax,[esi]
    mul     dword [VAR_MULTIPLIER]
    add     ebx,eax
    mov     ebp,0
    adc     ebp,edx

    mov     eax,[4+esi]
    mul     dword [VAR_MULTIPLIER]
    add     [edi],ebx
    adc     ebp,eax					; new lo + cylimb 
    mov     ebx,0
    adc     ebx,edx

    mov     eax,[8+esi]
    mul     dword [VAR_MULTIPLIER]
    add     [4+edi],ebp
    adc     ebx,eax					; new lo + cylimb 
    mov     ebp,0
    adc     ebp,edx

    mov     eax,[12+esi]
    mul     dword [VAR_MULTIPLIER]
    add     [8+edi],ebx
    adc     ebp,eax					; new lo + cylimb 
    mov     ebx,0
    adc     ebx,edx

    add     [12+edi],ebp
    adc     ebx,0					; propagate carry into cylimb 

    lea     esi,[16+esi]
    lea     edi,[16+edi]
    dec     ecx
    jnz     LoopX
LendX:
    mov     [edi],ebx
    add     edi,4

; we incremented wp and xp in the loop above; compensate 
    mov     eax,[PARAM_XSIZE]
    shl     eax,2
    sub     edi,eax
    sub     esi,eax

    mov     eax,[VAR_COUNTER]
    dec     eax
    mov     [VAR_COUNTER],eax
    jnz     Louter
Lskip:
    pop     ebx
    pop     edi
    pop     ebp
    pop     esi
    add     esp,8
    ret
Ldone:
    mov     [4+edi],edx				; store to wp[1] 
    pop     edi
    pop     ebp
    pop     esi
    add     esp,8
    ret

	end
