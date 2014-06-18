
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

	section	.text

	global	___gmpn_mod_34lsub1
%ifdef	DLL
	export	___gmpn_mod_34lsub1
%endif

	align	16
___gmpn_mod_34lsub1: 
    mov     ecx,[8+esp]
    mov     edx,[4+esp]
    mov     eax,[edx]
    sub     ecx,2
    ja      three_or_more
    jne     one
    mov     edx,[4+edx]
    mov     ecx,eax
    shr     eax,24
    and     ecx,0x00FFFFFF
    add     eax,ecx
    mov     ecx,edx
    shl     edx,8
    shr     ecx,16
    add     eax,ecx
    and     edx,0x00FFFF00
    add     eax,edx
one:ret

three_or_more: 
    pxor    mm0,mm0
    pxor    mm1,mm1
    pxor    mm2,mm2
    pcmpeqd mm7,mm7
    psrlq   mm7,32
    pcmpeqd mm6,mm6
    psrlq   mm6,40

top: 
	movd    mm3,[edx]
	paddq   mm0,mm3
	movd    mm3,[4+edx]
	paddq   mm1,mm3
    movd    mm3,[8+edx]
	paddq   mm2,mm3
    add     edx,12
    sub     ecx,3
    ja      top
    add     ecx,1
    js      combine
    movd    mm3,[edx]
	paddq   mm0,mm3
	jz      combine
	movd    mm3,[4+edx]
	paddq   mm1,mm3

combine: 
    movq    mm3,mm7
    pand    mm3,mm0
    movq    mm4,mm7
    pand    mm4,mm1
    movq    mm5,mm7
    pand    mm5,mm2
    psrlq   mm0,32
    psrlq   mm1,32
    psrlq   mm2,32
	paddq   mm4,mm0
	paddq   mm5,mm1
	paddq   mm3,mm2
    psllq   mm4,8
    psllq   mm5,16
	paddq   mm3,mm4
	paddq	mm3,mm5
    pand    mm6,mm3
    psrlq   mm3,24
	paddq   mm3,mm6
    movd    eax,mm3
    emms
    ret

	end
