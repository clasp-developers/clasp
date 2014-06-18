
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

%include "..\..\x86i.inc"
%define	p_space	16

	extern	___gmp_modlimb_invert_table

	section .text

	global	___gmpn_divexact_1
%ifdef	DLL
	export	___gmpn_divexact_1
%endif

	align	16
___gmpn_divexact_1: 
    mov     edx,[12+esp]
    mov     eax,[8+esp]
    mov     ecx,[16+esp]
    sub     edx,1
    jnz     two_or_more
    mov     eax,[eax]
    xor     edx,edx
    div     ecx
    mov     ecx,[4+esp]
    mov     [ecx],eax
    ret
    
two_or_more: 
    mov     eax,ecx
    bsf     ecx,ecx
    shr     eax,cl
    movd    mm6,eax
    movd    mm7,ecx
    shr     eax,1
    and     eax,127
    movzx   eax,byte [___gmp_modlimb_invert_table+eax]
    movd    mm5,eax
    movd    mm0,eax
	pmuludq mm5,mm5
	pmuludq mm5,mm6
    paddd   mm0,mm0
    psubd   mm0,mm5
    pxor    mm5,mm5
    paddd   mm5,mm0
	pmuludq mm0,mm0
    pcmpeqd mm4,mm4
    psrlq   mm4,32
	pmuludq mm0,mm6
    paddd   mm5,mm5
    mov     eax,[8+esp]
    mov     ecx,[4+esp]
    pxor    mm1,mm1
    psubd   mm5,mm0
    pxor    mm0,mm0

top: 
    movd    mm2,[eax]
    movd    mm3,[4+eax]
    add     eax,4
    punpckldq mm2,mm3
    psrlq   mm2,mm7
    pand    mm2,mm4
	psubq   mm2,mm0
	psubq   mm2,mm1
    movq    mm0,mm2
    psrlq   mm0,63
	pmuludq mm2,mm5
    movd    [ecx],mm2
    add     ecx,4
    movq    mm1,mm6
	pmuludq mm1,mm2
    psrlq   mm1,32
    sub     edx,1
    jnz     top

done: 
    movd    mm2,[eax]
    psrlq   mm2,mm7
	psubq   mm2,mm0
	psubq   mm2,mm1
	pmuludq mm2,mm5
    movd    [ecx],mm2
    emms
    ret
    
    end
