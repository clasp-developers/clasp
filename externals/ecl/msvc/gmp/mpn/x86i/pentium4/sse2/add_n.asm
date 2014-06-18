
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

	global	___gmpn_add_nc
%ifdef	DLL
	export	___gmpn_add_nc
%endif

	align	8
___gmpn_add_nc:
    movd    mm0,[20+esp]
    mov     eax,[8+esp]
    mov     [8+esp],ebx
    mov     ebx,[12+esp]
    mov     edx,[4+esp]
    mov     ecx,[16+esp]
    lea     eax,[eax+ecx*4]
    lea     ebx,[ebx+ecx*4]
    lea     edx,[edx+ecx*4]
    neg     ecx
t1: movd    mm1,[eax+ecx*4]
	movd    mm2,[ebx+ecx*4]
	paddq   mm1,mm2
	paddq   mm0,mm1
    movd    [edx+ecx*4],mm0
    psrlq   mm0,32
    add     ecx,1
    jnz     t1
    movd    eax,mm0
    mov     ebx,[8+esp]
    emms
    ret

	global	___gmpn_add_n
%ifdef	DLL
	export	___gmpn_add_n
%endif

	align	8
___gmpn_add_n:	
	pxor    mm0,mm0
    mov     eax,[8+esp]
    mov     [8+esp],ebx
    mov     ebx,[12+esp]
    mov     edx,[4+esp]
    mov     ecx,[16+esp]
    lea     eax,[eax+ecx*4]
    lea     ebx,[ebx+ecx*4]
    lea     edx,[edx+ecx*4]
    neg     ecx
t2:	movd    mm1,[eax+ecx*4]
	movd    mm2,[ebx+ecx*4]
	paddq   mm1,mm2
	paddq   mm0,mm1
    movd    [edx+ecx*4],mm0
    psrlq   mm0,32
    add     ecx,1
    jnz     t2
    movd    eax,mm0
    mov     ebx,[8+esp]
    emms
    ret

	end
