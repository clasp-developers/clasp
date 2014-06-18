
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

%include "..\x86i.inc" 

	extern	___gmp_modlimb_invert_table
	global  ___gmpn_modexact_1c_odd 
    global  ___gmpn_modexact_1_odd 

%ifdef	DLL
	export	___gmpn_modexact_1c_odd
	export	___gmpn_modexact_1_odd
%endif

%define	PARAM_CARRY     esp+frame+16 
%define PARAM_DIVISOR	esp+frame+12 
%define PARAM_SIZE      esp+frame+8 
%define PARAM_SRC       esp+frame+4 

;   Not enough room under modexact_1 to make these re-use the parameter 
;   space,unfortunately. 

%define	SAVE_EBX    esp+frame-4 
%define SAVE_ESI    esp+frame-8 
%define SAVE_EDI    esp+frame-12 
%define STACK_SPACE	12 
%define	frame		0 

	section .text

	align   16

___gmpn_modexact_1c_odd: 
    mov     ecx,[PARAM_CARRY]
    jmp     Lstart_1c

	align   16

___gmpn_modexact_1_odd: 
    xor     ecx,ecx
Lstart_1c: 
    mov     eax,[PARAM_DIVISOR]
    sub     esp,STACK_SPACE
	FR_sesp	STACK_SPACE

    mov     [SAVE_ESI],esi
    mov     esi,[PARAM_SRC]
    shr     eax,1				;  d/2 
    mov     [SAVE_EDI],edi
    and     eax,127

%ifdef	PIC
    call    Lmovl_eip_edi
    add     edi,_GLOBAL_OFFSET_TABLE_
    mov     edi,[___gmp_modlimb_invert_table+edi]
    movzx   edi,byte [eax+edi]							;  inv 8 bits 
%else
    movzx   edi,byte [___gmp_modlimb_invert_table+eax]	;  inv 8 bits 
%endif

    xor     edx,edx				;  initial extra carry 
    lea     eax,[edi+edi]		;  2*inv 
    imul    edi,edi				;  inv*inv 
    mov     [SAVE_EBX],ebx
    mov     ebx,[PARAM_SIZE]
    imul    edi,[PARAM_DIVISOR] ;  inv*inv*d 
    sub     eax,edi				;  inv = 2*inv - inv*inv*d 
    lea     edi,[eax+eax]		;  2*inv 
    imul    eax,eax				;  inv*inv 
    imul    eax,[PARAM_DIVISOR] ;  inv*inv*d 
    lea     esi,[esi+ebx*4]		;  src end 
    neg     ebx					;  -size 
    sub     edi,eax				;  inv = 2*inv - inv*inv*d 
        
;  The dependent chain here is 
; 
;      subl    %edx,%eax        1 
;      imull   %edi,%eax        4 
;      mull    PARAM_DIVISOR    5 
;                             ---- 
;        total                 10 
; 
;  and this is the measured speed.  No special scheduling is necessary,out 
;  of order execution hides the load latency. 
;
;  eax scratch (src limb) 
;  ebx counter,limbs,negative 
;  ecx carry bit,0 or 1 
;  edx carry limb,high of last product 
;  esi &src[size] 
;  edi inverse 
;  ebp 

Ltop: 
    mov     eax,[esi+ebx*4]
    sub     eax,ecx
    sbb     ecx,ecx
    sub     eax,edx
    sbb     ecx,0
    imul    eax,edi
    neg     ecx
    mul     dword [PARAM_DIVISOR]
    inc     ebx
    jnz     Ltop
    mov     esi,[SAVE_ESI]
    lea     eax,[ecx+edx]
    mov     edi,[SAVE_EDI]
    mov     ebx,[SAVE_EBX]
    add     esp,STACK_SPACE
    ret

%ifdef	PIC
Lmovl_eip_edi: 
    mov     edi,[esp]
    ret
%endif

	end
