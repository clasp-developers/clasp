
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

%define PARAM_CARRY esp+frame+16
%define PARAM_SIZE  esp+frame+12
%define PARAM_SRC   esp+frame+8
%define PARAM_DST   esp+frame+4
%assign frame		0

; multiplicative inverse of 3,modulo 2^32 
; ceil(b/3) and ceil(b*2/3) where b=2^32 

%define	INVERSE_3		0xAAAAAAAB
%define	ONE_THIRD_CEIL	0x55555556
%define	TWO_THIRDS_CEIL	0xAAAAAAAB

    section .text
    
    global  ___gmpn_divexact_by3c
%ifdef	DLL
	export	___gmpn_divexact_by3c
%endif

    align   8
___gmpn_divexact_by3c: 
    mov     ecx,[PARAM_SRC]
    FR_push ebp
    mov     ebp,[PARAM_SIZE]
    FR_push edi
    mov     edi,[PARAM_DST]
    FR_push esi
    mov     esi,INVERSE_3
	FR_push ebx
    lea     ecx,[ecx+ebp*4]
    mov     ebx,[PARAM_CARRY]
    lea     edi,[edi+ebp*4]
    neg     ebp

; eax  scratch,low product 
; ebx  carry limb (0 to 3) 
; ecx  &src[size] 
; edx  scratch,high product 
; esi  multiplier 
; edi  &dst[size] 
; ebp  counter,limbs,negative 

	align   8
Ltop:	 
    mov     eax,[ecx+ebp*4]
    sub     eax,ebx
    setc    bl
    imul    esi
    cmp     eax,ONE_THIRD_CEIL
    mov     [edi+ebp*4],eax
    sbb     ebx,-1				; +1 if eax>=ceil(b/3) 
    cmp     eax,TWO_THIRDS_CEIL
    sbb     ebx,-1				; +1 if eax>=ceil(b*2/3) 
    inc     ebp
    jnz     Ltop
    mov     eax,ebx
    pop     ebx
    pop     esi
    pop     edi
    pop     ebp
    ret

	end
