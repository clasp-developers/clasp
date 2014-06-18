
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

%define PARAM_CARRY   esp+frame+24
%define PARAM_DIVISOR esp+frame+20
%define PARAM_SIZE    esp+frame+16
%define PARAM_SRC     esp+frame+12
%define PARAM_XSIZE   esp+frame+8
%define PARAM_DST     esp+frame+4

	section .text

    global  ___gmpn_divrem_1c
%ifdef	DLL
	export	___gmpn_divrem_1c
%endif

    align   16
___gmpn_divrem_1c:       
%assign       frame   0
    mov     ecx,[PARAM_SIZE]
	FR_push edi
    mov     edi,[PARAM_SRC]
	FR_push esi
    mov     esi,[PARAM_DIVISOR]
	FR_push ebx
    mov     ebx,[PARAM_DST]
	FR_push ebp
    mov     ebp,[PARAM_XSIZE]
    or      ecx,ecx
    mov     edx,[PARAM_CARRY]
    jz      Lfraction
    lea     ebx,[-4+ebx+ebp*4]  ; dst one limb below integer part 
    jmp     Linteger_top
		
	global  ___gmpn_divrem_1
%ifdef	DLL
	export	___gmpn_divrem_1
%endif
    align   16
___gmpn_divrem_1: 

%assign       frame   0
    mov     ecx,[PARAM_SIZE]
	FR_push edi
    mov     edi,[PARAM_SRC]
    FR_push esi
    mov     esi,[PARAM_DIVISOR]
    or      ecx,ecx
    jz      Lsize_zero
	FR_push ebx
    mov     eax,[-4+edi+ecx*4]  ; src high limb 
    xor     edx,edx
    mov     ebx,[PARAM_DST]
	FR_push ebp
    mov     ebp,[PARAM_XSIZE]
    cmp     eax,esi
    lea     ebx,[-4+ebx+ebp*4]  ; dst one limb below integer part 
    jae     Linteger_entry

; high<divisor,so high of dst is zero,and avoid one div 

    mov     [ebx+ecx*4],edx
    dec     ecx
    mov     edx,eax
    jz      Lfraction
	 
; eax  scratch (quotient) 
; ebx  dst+4*xsize-4 
; ecx  counter 
; edx  scratch (remainder) 
; esi  divisor 
; edi  src 
; ebp  xsize 

Linteger_top:
    mov     eax,[-4+edi+ecx*4]
Linteger_entry:	 
    div     esi
    mov     [ebx+ecx*4],eax
	dec		ecx
	jnz		Linteger_top
Lfraction:
    or      ecx,ebp
    jz      Ldone
    mov     ebx,[PARAM_DST]

; eax  scratch (quotient) 
; ebx  dst 
; ecx  counter 
; edx  scratch (remainder) 
; esi  divisor 
; edi 
; ebp 

Lfraction_top:	 
    xor     eax,eax
    div     esi
    mov     [-4+ebx+ecx*4],eax
    dec		ecx
    jnz		Lfraction_top
Ldone:
    pop     ebp
    mov     eax,edx
    pop     ebx
    pop     esi
    pop     edi
    ret
Lsize_zero: 
    mov     ecx,[PARAM_XSIZE]
    xor     eax,eax
    mov     edi,[PARAM_DST]
    cld							; better safe than sorry,see mpn/x86/README 
    rep		stosd
    pop     esi
    pop     edi
    ret

	end
