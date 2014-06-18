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

	global	___gmpn_sqr_basecase
%ifdef	DLL
	export	___gmpn_sqr_basecase
%endif

	align	8
___gmpn_sqr_basecase: 
    mov     edx,[12+esp]
    mov     eax,[8+esp]
    mov     ecx,[4+esp]
    cmp     edx,2
    je      two_limbs
    ja      three_or_more
    mov     eax,[eax]
    mul     eax
    mov     [ecx],eax
    mov     [4+ecx],edx
    ret

two_limbs: 
    movd    mm1,[eax]
    movd    mm0,[4+eax]
	pmuludq mm0,mm1
	pmuludq mm1,mm1
    movd    mm2,[4+eax]
	pmuludq mm2,mm2
    movd    [ecx],mm1
    psrlq   mm1,32
    pcmpeqd mm3,mm3
    psrlq   mm3,32
    pand    mm3,mm0
    psrlq   mm0,32
    psllq   mm3,1
	paddq   mm1,mm3
    movd    [4+ecx],mm1
    pcmpeqd mm4,mm4
    psrlq   mm4,32
    pand    mm4,mm2
    psrlq   mm2,32
    psllq   mm0,1
    psrlq   mm1,32
	paddq   mm0,mm1
	paddq   mm0,mm4
    movd    [8+ecx],mm0
    psrlq   mm0,32
	paddq   mm0,mm2
    movd    [12+ecx],mm0
	emms
    ret

three_or_more: 
    sub     esp,12
    pxor    mm0,mm0
    movd    mm7,[eax]
    mov     [8+esp],esi
    mov     [4+esp],edi
    mov     [esp],ebp
    mov     esi,eax
    mov     edi,ecx
    sub     edx,1

mul1: 
    movd    mm1,[4+eax]
    add     eax,4
	pmuludq mm1,mm7
	paddq   mm0,mm1
    movd    [4+ecx],mm0
    add     ecx,4
    psrlq   mm0,32
    sub     edx,1
    jnz     mul1
    mov     ebp,[24+esp]
    sub     ebp,3
    jz      corner

outer: 
    movd    mm7,[4+esi]
    movd    [4+ecx],mm0
    lea     eax,[8+esi]
    add     esi,4
    lea     ecx,[8+edi]
    add     edi,8
    lea     edx,[1+ebp]
    pxor    mm0,mm0

inner: 
    movd    mm1,[eax]
    lea     eax,[4+eax]
    movd    mm2,[4+ecx]
	pmuludq mm1,mm7
	paddq   mm1,mm2
	paddq   mm0,mm1
    sub     edx,1
    movd    [4+ecx],mm0
    psrlq   mm0,32
    lea     ecx,[4+ecx]
    jnz     inner
    sub     ebp,1
    jnz     outer

corner: 
    movd    mm1,[4+esi]
    movd    mm2,[8+esi]
	pmuludq mm1,mm2
    mov     eax,[20+esp]
    movd    mm2,[eax]
	pmuludq mm2,mm2
    pcmpeqd mm7,mm7
    psrlq   mm7,32
    mov     edx,[16+esp]
    movd    mm3,[4+edx]
	paddq   mm0,mm1
    movd    [12+edi],mm0
    psrlq   mm0,32
    movd    [16+edi],mm0
    movd    [edx],mm2
    psrlq   mm2,32
    psllq   mm3,1
	paddq   mm2,mm3
    movd    [4+edx],mm2
    psrlq   mm2,32
    mov     ecx,[24+esp]
    sub     ecx,2

diag: 
    movd    mm0,[4+eax]
    add     eax,4
	pmuludq mm0,mm0
    movq    mm1,mm7
    pand    mm1,mm0
    psrlq   mm0,32
    movd    mm3,[8+edx]
    psllq   mm3,1
	paddq   mm1,mm3
	paddq   mm2,mm1
    movd    [8+edx],mm2
    psrlq   mm2,32
    movd    mm3,[12+edx]
    psllq   mm3,1
	paddq   mm0,mm3
	paddq   mm2,mm0
    movd    [12+edx],mm2
    add     edx,8
    psrlq   mm2,32
    sub     ecx,1
    jnz     diag
    movd    mm0,[4+eax]
	pmuludq mm0,mm0
    pand    mm7,mm0
    psrlq   mm0,32
    movd    mm3,[8+edx]
    psllq   mm3,1
	paddq   mm7,mm3
	paddq   mm2,mm7
    movd    [8+edx],mm2
    psrlq   mm2,32
	paddq   mm2,mm0
    movd    [12+edx],mm2
    mov     esi,[8+esp]
    mov     edi,[4+esp]
    mov     ebp,[esp]
    add     esp,12
    emms
    ret

	end
