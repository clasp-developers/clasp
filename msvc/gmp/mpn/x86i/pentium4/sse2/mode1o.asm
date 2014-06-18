
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

	extern	___gmp_modlimb_invert_table
	global	___gmpn_modexact_1_odd
	global	___gmpn_modexact_1c_odd
	
%ifdef	DLL
	export	___gmpn_modexact_1_odd
	export	___gmpn_modexact_1c_odd
%endif

	align	16
___gmpn_modexact_1c_odd: 
    movd    mm1,[16+esp]
    jmp     start_1c

	align	16
___gmpn_modexact_1_odd: 
	pxor    mm1,mm1

start_1c: 
    mov     eax,[12+esp]
    movd    mm7,[12+esp]
    shr     eax,1
    and     eax,127
    movzx   eax,byte [___gmp_modlimb_invert_table+eax]
    movd    mm6,eax
    movd    mm0,eax
	pmuludq mm6,mm6
	pmuludq mm6,mm7
    paddd   mm0,mm0
    psubd   mm0,mm6
    pxor    mm6,mm6
    paddd   mm6,mm0
	pmuludq mm0,mm0
	pmuludq mm0,mm7
    paddd   mm6,mm6
    mov     eax,[4+esp]
    mov     ecx,[8+esp]
    psubd   mm6,mm0
    pxor    mm0,mm0

top: 
    movd    mm2,[eax]
    add     eax,4
	psubq   mm2,mm0
	psubq   mm2,mm1
    movq    mm0,mm2
    psrlq   mm0,63
	pmuludq mm2,mm6
    movq    mm1,mm7
	pmuludq mm1,mm2
    psrlq   mm1,32
    sub     ecx,1
    jnz     top

done: 
	paddq   mm0,mm1
    movd    eax,mm0
    emms
    ret

	end
