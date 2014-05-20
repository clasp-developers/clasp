
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
; Translation of AT&T syntax code by Brian Gladman 

%include	"x86i.inc"

%define PARAM_CARRY   esp+frame+16
%define PARAM_DIVISOR esp+frame+12
%define PARAM_SIZE    esp+frame+8
%define PARAM_SRC     esp+frame+4

    section .text

    global  ___gmpn_mod_1c
%ifdef	DLL
	export	___gmpn_mod_1c
%endif

    align   16
___gmpn_mod_1c:     
%assign	frame   0
    mov     ecx,[PARAM_SIZE]
	FR_push ebx
    mov     ebx,[PARAM_SRC]
	FR_push esi
    mov     esi,[PARAM_DIVISOR]
    or      ecx,ecx
    mov     edx,[PARAM_CARRY]
    jnz     Ltop
    pop     esi
    mov     eax,edx
    pop     ebx
    ret

	global  ___gmpn_mod_1
%ifdef	DLL
	export	___gmpn_mod_1
%endif
    align   16
___gmpn_mod_1: 

%assign	frame   0        
    mov     ecx,[PARAM_SIZE]
	FR_push ebx
    mov     ebx,[PARAM_SRC]
	FR_push esi
    or      ecx,ecx
    jz      Ldone_zero
    mov     esi,[PARAM_DIVISOR]
    mov     eax,[-4+ebx+ecx*4]  ; src high limb 
    cmp     eax,esi
    sbb     edx,edx				; -1 if high<divisor 
    add     ecx,edx				; skip one division if high<divisor 
    jz      Ldone_eax
    and     edx,eax				; carry if high<divisor 

; eax  scratch (quotient) 
; ebx  src 
; ecx  counter 
; edx  carry (remainder) 
; esi  divisor 
; edi 
; ebp 

Ltop:
    mov     eax,[-4+ebx+ecx*4]
    div     esi
	dec		ecx
	jnz		Ltop
    mov     eax,edx
Ldone_eax:
    pop     esi
    pop     ebx
    ret
Ldone_zero:
    pop     esi
    xor     eax,eax
    pop     ebx
    ret

	end
