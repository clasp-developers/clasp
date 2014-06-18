
;  Copyright 2001, 2002 Free Software Foundation, Inc.
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

%define PARAM_DIVISOR	esp+frame+16
%define PARAM_SIZE		esp+frame+12
%define PARAM_SRC		esp+frame+8
%define PARAM_DST		esp+frame+4
%define	VAR_INVERSE		PARAM_SRC
%assign frame			0

    section .text

	extern	___gmp_modlimb_invert_table
    global  ___gmpn_divexact_1
%ifdef	DLL
	export	___gmpn_divexact_1
%endif

    align   16    
___gmpn_divexact_1: 
    mov     eax,[PARAM_DIVISOR]
	FR_push ebp
    mov     ebp,[PARAM_SIZE]
	FR_push edi
    FR_push ebx
    mov     ecx,-1					; shift count 
    FR_push esi
Lstrip_twos:
    inc     ecx
    shr     eax,1
    jnc     Lstrip_twos
    lea     ebx,[1+eax+eax]			; d without twos 
    and     eax,127					; d/2,7 bits 

%ifdef	PIC
    call    Lmovl_eip_edx
    add     edx,_GLOBAL_OFFSET_TABLE_
    mov     edx,[___gmp_modlimb_invert_table+edx]
    movzx   eax,byte [eax+edx]	; inv 8 bits 
%else
    movzx   eax,byte [___gmp_modlimb_invert_table+eax] ; inv 8 bits 
%endif

    lea     edx,[eax+eax]		; 2*inv 
    mov     [PARAM_DIVISOR],ebx ; d without twos 
    imul    eax,eax				; inv*inv 
    mov     esi,[PARAM_SRC]
    mov     edi,[PARAM_DST]
    imul    eax,ebx				; inv*inv*d 
    sub     edx,eax				; inv = 2*inv - inv*inv*d 
    lea     eax,[edx+edx]		; 2*inv 
    imul    edx,edx				; inv*inv 
    lea     esi,[esi+ebp*4]		; src end 
    lea     edi,[edi+ebp*4]		; dst end 
    neg     ebp					; -size 
    imul    edx,ebx				; inv*inv*d 
    sub     eax,edx				; inv = 2*inv - inv*inv*d 

%ifdef	ASSERT
    FR_push eax
    imul    eax,[PARAM_DIVISOR]
    cmp     eax,1
    FR_pop  eax
%endif

    mov     [VAR_INVERSE],eax
    mov     eax,[esi+ebp*4]		; src[0] 
    xor     ebx,ebx
    xor     edx,edx
    inc     ebp
    jz      Lone
    mov     edx,[esi+ebp*4]		; src[1] 
	shrd	eax,edx,cl
    mov     edx,[VAR_INVERSE]
    jmp     Lentry

    align   8
    nop							; k6 code alignment 
    nop

; eax  q 
; ebx  carry bit,0 or -1 
; ecx  shift 
; edx  carry limb 
; esi  src end 
; edi  dst end 
; ebp  counter,limbs,negative 

Ltop:
    mov     eax,[-4+esi+ebp*4]
    sub     edx,ebx				; accumulate carry bit 
    mov     ebx,[esi+ebp*4]
	shrd	eax,ebx,cl
    sub     eax,edx				; apply carry limb 
    mov     edx,[VAR_INVERSE]
    sbb     ebx,ebx
Lentry:
    imul    eax,edx
    mov     [-4+edi+ebp*4],eax
    mov     edx,[PARAM_DIVISOR]
    mul     edx
    inc     ebp
    jnz     Ltop
    mov     eax,[-4+esi]		; src high limb 
Lone: 
    shr     eax,cl
	FR_pop  esi
    add     eax,ebx				; apply carry bit 
    FR_pop  ebx
    sub     eax,edx				; apply carry limb 
    imul    eax,[VAR_INVERSE]
    mov     [-4+edi],eax
    pop     edi
    pop     ebp
    ret

%ifdef	PIC
Lmovl_eip_edx:	 
    mov     edx,[esp]
    ret
%endif

	end
