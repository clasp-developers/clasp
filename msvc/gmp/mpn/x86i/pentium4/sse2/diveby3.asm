
;  Copyright 2001, 2002 Free Software Foundation, Inc.
; 
;  This file is part of the GNU MP Library.
; 
;  The GNU MP Library is free software; you can redistribute it and/or
;  modify it under the terms of the GNU Library General Public License as
;  published by the Free Software Foundation; either version 2 of the
;  License, or (at your option) any later version.
; 
;  The GNU MP Library is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  Library General Public License for more details.
; 
;  You should have received a copy of the GNU Library General Public
;  License along with the GNU MP Library; see the file COPYING.LIB.  If
;  not, write to the Free Software Foundation, Inc., 59 Temple Place -
;  Suite 330, Boston, MA 02111-1307, USA.
;
; Translation of AT&T syntax code by Brian Gladman 

	section	.text

	global	___gmpn_divexact_by3c	
%ifdef	DLL
	export	___gmpn_divexact_by3c
%endif

	align	16
___gmpn_divexact_by3c: 
    mov     eax,[8+esp]
    pxor    mm0,mm0
    movd    mm1,[16+esp]
    pcmpeqd mm6,mm6
    movd    mm7,[val]
    mov     edx,[4+esp]
    psrlq   mm6,32
    mov     ecx,[12+esp]

top: 
    movd    mm2,[eax]
    add     eax,4
	psubq   mm2,mm0
	psubq   mm2,mm1
    movq    mm0,mm2
    psrlq   mm0,63
	pmuludq mm2,mm7
    movd    [edx],mm2
    add     edx,4
    movq    mm1,mm6
    pand    mm1,mm2
    pand    mm2,mm6
    psllq   mm1,1
	paddq   mm1,mm2
    psrlq   mm1,32
    sub     ecx,1
    jnz     top
    paddd   mm0,mm1
    movd    eax,mm0
    emms
    ret

	section	.data
val:
	dd	0xAAAAAAAB

	end
