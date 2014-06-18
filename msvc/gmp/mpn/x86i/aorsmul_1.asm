
;  Copyright 1992, 1994, 1997, 1999, 2000, 2001, 2002 Free Software
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

%define	PARAM_MULTIPLIER	esp+frame+16
%define	PARAM_SIZE			esp+frame+12
%define	PARAM_SRC			esp+frame+8
%define	PARAM_DST			esp+frame+4
%assign frame				16

%macro	mac_sub 3

	global  %1%3
%ifdef	DLL
	export	%1%3
%endif

	align   8
%1%3:
    push    edi
    push    esi
    push    ebx
    push    ebp
    mov     edi,[PARAM_DST]
    mov     esi,[PARAM_SRC]
    mov     ecx,[PARAM_SIZE]
    xor     ebx,ebx
    and     ecx,3
    jz      %%2
%%1:
    mov     eax,[esi]
    mul     dword [PARAM_MULTIPLIER]
    lea     esi,[4+esi]
    add     eax,ebx
    mov     ebx,0
    adc     edx,ebx
	%2		[edi],eax
    adc     ebx,edx		; propagate carry into cylimb 
    lea     edi,[4+edi]
    dec     ecx
    jnz     %%1
%%2:
    mov     ecx,[PARAM_SIZE]
    shr     ecx,2
    jz      %%4

	align   8
%%3:
	mov     eax,[esi]
    mul     dword [PARAM_MULTIPLIER]
    add     ebx,eax
    mov     ebp,0
    adc     ebp,edx
    mov     eax,[4+esi]
    mul     dword [PARAM_MULTIPLIER]
	%2		[edi],ebx
    adc     ebp,eax		; new lo + cylimb 
    mov     ebx,0
    adc     ebx,edx
    mov     eax,[8+esi]
    mul     dword [PARAM_MULTIPLIER]
	%2		[4+edi],ebp
    adc     ebx,eax		; new lo + cylimb 
    mov     ebp,0
    adc     ebp,edx
    mov     eax,[12+esi]
    mul     dword [PARAM_MULTIPLIER]
	%2		[8+edi],ebx
    adc     ebp,eax   ; new lo + cylimb 
    mov     ebx,0
    adc     ebx,edx
	%2		[12+edi],ebp
    adc     ebx,0		; propagate carry into cylimb 
    lea     esi,[16+esi]
    lea     edi,[16+edi]
    dec     ecx
    jnz     %%3
%%4:
	mov     eax,ebx
    pop     ebp
    pop     ebx
    pop     esi
    pop     edi
    ret
%endmacro

	section .text

	mac_sub	___g,add,mpn_addmul_1
	mac_sub	___g,sub,mpn_submul_1

	end
