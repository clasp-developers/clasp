
;  Copyright 1999, 2000, 2001 Free Software Foundation, Inc.
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

%include	"..\x86i.inc"

	section	.text

	global	___gmpn_copyi
%ifdef	DLL
	export	___gmpn_copyi
%endif

	align	8
___gmpn_copyi:
    mov     ecx, [12+esp]
    cmp     ecx, 150
    jg      rm
    mov     eax, [8+esp]
    mov     edx, [4+esp]
    mov     [12+esp],ebx
    test    ecx,ecx
    jz      nd
oop: 
    mov     ebx, [eax]
    lea     eax, [4+eax]
    add     ecx, -1
    mov     [edx],ebx
    lea     edx, [4+edx]
    jnz     oop
nd: 
    mov     ebx, [12+esp]
    ret
rm:
    cld
    mov     eax,esi
    mov     esi, [8+esp]
    mov     edx,edi
    mov     edi, [4+esp]
    rep movsd
    mov     esi,eax
    mov     edi,edx
    ret

	end
