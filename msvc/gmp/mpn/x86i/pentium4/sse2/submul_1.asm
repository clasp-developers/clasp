
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

	section .text

	global	___gmpn_submul_1c
%ifdef	DLL
	export	___gmpn_submul_1c
%endif

	align	16
___gmpn_submul_1c:
    movd    mm1,[20+esp]
	mov     eax,[8+esp]
	pcmpeqd mm0,mm0
	movd    mm7,[16+esp]
	pcmpeqd mm6,mm6
	mov     edx,[4+esp]
	psrlq   mm0,32
	mov     ecx,[12+esp]
	psllq   mm6,32
	psubq   mm6,mm0
	psubq   mm0,mm1
oop1: 
    movd    mm1,[eax]
    lea     eax,[4+eax]
    movd    mm2,[edx]
	paddq   mm2,mm6
	pmuludq mm1,mm7
	psubq   mm2,mm1
	paddq   mm0,mm2
    sub     ecx,1
    movd    [edx],mm0
    psrlq   mm0,32
    lea     edx,[4+edx]
    jnz     oop1
    movd    eax,mm0
    not     eax
    emms
    ret

	global	___gmpn_submul_1
%ifdef	DLL
	export	___gmpn_submul_1
%endif
	align	16
___gmpn_submul_1:

	pxor    mm1,mm1
	mov     eax,[8+esp]
	pcmpeqd mm0,mm0
	movd    mm7,[16+esp]
	pcmpeqd mm6,mm6
	mov     edx,[4+esp]
	psrlq   mm0,32
	mov     ecx,[12+esp]
	psllq   mm6,32
	psubq   mm6,mm0
	psubq   mm0,mm1
oop2: 
    movd    mm1,[eax]
    lea     eax,[4+eax]
    movd    mm2,[edx]
	paddq   mm2,mm6
	pmuludq mm1,mm7
	psubq   mm2,mm1
	paddq   mm0,mm2
    sub     ecx,1
    movd    [edx],mm0
    psrlq   mm0,32
    lea     edx,[4+edx]
    jnz     oop2
    movd    eax,mm0
    not     eax
    emms
    ret

	end
