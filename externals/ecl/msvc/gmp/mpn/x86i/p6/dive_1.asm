
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

%include "..\x86i.inc" 

	extern	___gmp_modlimb_invert_table
	global  ___gmpn_divexact_1 

%ifdef	DLL
	export	___gmpn_divexact_1
%endif

%define	PARAM_DIVISOR	esp+frame+16 
%define PARAM_SIZE      esp+frame+12 
%define PARAM_SRC       esp+frame+8 
%define PARAM_DST       esp+frame+4 

%define SAVE_EBX        esp+frame-4 
%define SAVE_ESI        esp+frame-8 
%define SAVE_EDI		esp+frame-12 
%define SAVE_EBP		esp+frame-16 
%define VAR_INVERSE		esp+frame-20 
%define STACK_SPACE		20 
%define frame			0 

	section .text

	align   16

___gmpn_divexact_1: 
	mov     eax,[PARAM_DIVISOR]
    sub     esp,STACK_SPACE
	FR_sesp	STACK_SPACE
    mov     [SAVE_ESI],esi
    mov     esi,[PARAM_SRC]
    mov     [SAVE_EBX],ebx
    mov     ebx,[PARAM_SIZE]
    bsf     ecx,eax         ;  trailing twos 
    mov     [SAVE_EBP],ebp
    shr     eax,cl          ;  d without twos 
    mov     edx,eax
    shr     eax,1           ;  d/2 without twos 
    mov     [PARAM_DIVISOR],edx
    and     eax,127

%ifdef	PIC
    call    Lmovl_eip_ebp
    add     ebp,_GLOBAL_OFFSET_TABLE_
    mov     ebp,[___gmp_modlimb_invert_table+edx+ebp]
    movzx   ebp,byte [eax+ebp]							;  inv 8 bits 
%else
	movzx   ebp,byte [___gmp_modlimb_invert_table+eax]	;  inv 8 bits 
%endif

    lea     eax,[ebp+ebp]		;  2*inv 
    imul    ebp,ebp				;  inv*inv 
    mov     [SAVE_EDI],edi
    mov     edi,[PARAM_DST]
    lea     esi,[esi+ebx*4]		;  src end 
    imul    ebp,[PARAM_DIVISOR]	;  inv*inv*d 
    sub     eax,ebp				;  inv = 2*inv - inv*inv*d 
    lea     ebp,[eax+eax]		;  2*inv 
    imul    eax,eax				;  inv*inv 
    lea     edi,[edi+ebx*4]		;  dst end 
    neg     ebx					;  -size 
    mov     [PARAM_DST],edi
    imul    eax,[PARAM_DIVISOR] ;  inv*inv*d 
    sub     ebp,eax				;  inv = 2*inv - inv*inv*d 

    mov     [VAR_INVERSE],ebp
    mov     eax,[esi+ebx*4]		;  src[0] 
    or      ecx,ecx
    jnz     Leven
    jmp     Lodd_entry			;  ecx initial carry is zero 

;  The dependent chain here is 
; 
;      subl    %edx,%eax        1 
;      imull   %ebp,%eax        4 
;      mull    PARAM_DIVISOR    5 
;                             ---- 
;        total                 10 
; 
;  and this is the measured speed.  No special scheduling is necessary,out 
;  of order execution hides the load latency. 
;
;  eax scratch (src limb) 
;  ebx counter,limbs,negative 
;  ecx carry bit 
;  edx carry limb,high of last product 
;  esi &src[size] 
;  edi &dst[size] 

Lodd_top: 
    mul     dword [PARAM_DIVISOR]
    mov     eax,[esi+ebx*4]
    sub     eax,ecx
    sbb     ecx,ecx
    sub     eax,edx
    sbb     ecx,0
Lodd_entry: 
    imul    eax,[VAR_INVERSE]
    mov     [edi+ebx*4],eax
    neg     ecx
    inc     ebx
    jnz     Lodd_top
    mov     esi,[SAVE_ESI]
    mov     edi,[SAVE_EDI]
    mov     ebp,[SAVE_EBP]
    mov     ebx,[SAVE_EBX]
    add     esp,STACK_SPACE
    ret

;  eax src[0] 
;  ebx counter,limbs,negative 
;  ecx shift 

Leven: 
    xor     ebp,ebp         ;  initial carry bit 
    xor     edx,edx         ;  initial carry limb (for size==1) 
    inc     ebx
    jz      Leven_one
    mov     edi,[esi+ebx*4]	;  src[1] 
	shrd	eax,edi,cl
    jmp     Leven_entry

;  eax scratch 
;  ebx counter,limbs,negative 
;  ecx shift 
;  edx scratch 
;  esi &src[size] 
;  edi &dst[size] and scratch 
;  ebp carry bit 

Leven_top: 
    mov     edi,[esi+ebx*4]
    mul     dword [PARAM_DIVISOR]
    mov     eax,[-4+esi+ebx*4]
	shrd	eax,edi,cl
    sub     eax,ebp
    sbb     ebp,ebp
    sub     eax,edx
    sbb     ebp,0

Leven_entry: 
    imul    eax,[VAR_INVERSE]
    mov     edi,[PARAM_DST]
    neg     ebp
    mov     [-4+edi+ebx*4],eax
    inc     ebx
    jnz     Leven_top
    mul     dword [PARAM_DIVISOR]
    mov     eax,[-4+esi]
Leven_one: 
    shr     eax,cl
    mov     esi,[SAVE_ESI]
    sub     eax,ebp
    mov     ebp,[SAVE_EBP]
    sub     eax,edx
    mov     ebx,[SAVE_EBX]
    imul    eax,[VAR_INVERSE]
    mov     [-4+edi],eax
    mov     edi,[SAVE_EDI]
    add     esp,STACK_SPACE
    ret

%ifdef	PIC
Lmovl_eip_ebp: 
    mov     ebp,[esp]
    ret
%endif

	end
