
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

	section .text

	global	___gmpn_mul_basecase
%ifdef	DLL
	export	___gmpn_mul_basecase
%endif

	align	8
___gmpn_mul_basecase: 
    mov     eax,[8+esp]
    mov     [8+esp],ebx
    pxor    mm0,mm0
    mov     edx,[16+esp]
    mov     [16+esp],esi
    mov     ebx,[4+esp]
    mov     [4+esp],ebp
    mov     esi,eax
    movd    mm7,[edx]
    mov     ecx,[12+esp]
    mov     ebp,[20+esp]
    mov     [20+esp],edi
    mov     edi,ebx

mul1: 
    movd    mm1,[eax]
    add     eax,4
	pmuludq mm1,mm7
	paddq   mm0,mm1
    movd    [ebx],mm0
    add     ebx,4
    psrlq   mm0,32
    sub     ecx,1
    jnz     mul1
    movd    [ebx],mm0
    sub     ebp,1
    jz      done

outer: 
    mov     eax,esi
    lea     ebx,[4+edi]
    add     edi,4
    movd    mm7,[4+edx]
    add     edx,4
    pxor    mm0,mm0
    mov     ecx,[12+esp]

inner: 
    movd    mm1,[eax]
    lea     eax,[4+eax]
    movd    mm2,[ebx]
	pmuludq mm1,mm7
	paddq   mm1,mm2
	paddq   mm0,mm1
    sub     ecx,1
    movd    [ebx],mm0
    psrlq   mm0,32
    lea     ebx,[4+ebx]
    jnz     inner
    movd    [ebx],mm0
    sub     ebp,1
    jnz     outer

done: 
    mov     ebx,[8+esp]
    mov     esi,[16+esp]
    mov     edi,[20+esp]
    mov     ebp,[4+esp]
    emms
    ret

	end
