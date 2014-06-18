
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

	global	___gmpn_addmul_1
	global	___gmpn_addmul_1c

%ifdef	DLL
	export	___gmpn_addmul_1
	export	___gmpn_addmul_1c
%endif

	align	16
___gmpn_addmul_1c: 
    movd    mm0,[20+esp]
    jmp     start_1c

	align	16
___gmpn_addmul_1: 
	pxor    mm0,mm0

start_1c: 
    mov     eax,[8+esp]
    mov     ecx,[12+esp]
    mov     edx,[4+esp]
    movd    mm7,[16+esp]

oop: 
    movd    mm1,[eax]
    lea     eax,[4+eax]
    movd    mm2,[edx]
	pmuludq mm1,mm7
	paddq   mm2,mm1
	paddq   mm0,mm2
	sub     ecx,1
	movd    [edx],mm0
	psrlq   mm0,32
	lea     edx,[4+edx]
	jnz     oop
	movd    eax,mm0
	emms
	ret
	
	end
