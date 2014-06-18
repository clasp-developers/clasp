
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

%macro	ph_fun 1
%ifdef	PIC
    mov			edx,0xAAAAAAAA
    movd		mm7,edx
    punpckldq	mm7,mm7
    mov			edx,0x33333333
    movd		mm6,edx
    punpckldq	mm6,mm6
    mov			edx,0x0F0F0F0F
    movd		mm5,edx
    punpckldq	mm5,mm5
%else
    movq		mm7,[L_AA]
    movq		mm6,[L_33]
    movq		mm5,[L_0F]
%endif
    mov			ecx,[esp+PARAM_SIZE]
    mov			eax,[esp+PARAM_SRC]
%if %1 == 1
    mov			edx,[esp+PARAM_SRC2]
%endif
    pxor		mm4,mm4
    pxor		mm0,mm0
    sub			ecx,1
    ja			%%L_top

%%L_last:
    movd		mm1,[eax+ecx*4]
%if %1 == 1
    movd		mm2,[edx+ecx*4]
    pxor		mm1,mm2
%endif
    jmp			%%L_loaded

%%L_top:
    movd		mm1,[eax]
    movd		mm2,[eax+4]
    punpckldq	mm1,mm2
    add			eax,8
%if %1 == 1
    movd		mm2,[edx]
    movd		mm3,[edx+4]
    punpckldq	mm2,mm3
    pxor		mm1,mm2
    add			edx,8
%endif
%%L_loaded:
    movq		mm2,mm7
    pand		mm2,mm1
    psrlq		mm2,1
    psubd		mm1,mm2
    movq		mm2,mm6
    pand		mm2,mm1
    psrlq		mm1,2
    pand		mm1,mm6
    paddd		mm1,mm2
    movq		mm2,mm5
    pand		mm2,mm1
    psrlq		mm1,4
    pand		mm1,mm5
    paddd		mm1,mm2
    psadbw		mm1,mm4
    paddd		mm0,mm1
    sub			ecx,2
    jg			%%L_top
    jz			%%L_last
    movd		eax,mm0
    emms
    ret
%endmacro

%ifndef PIC
    data
    align 8

L_AA:	dq	0xAAAAAAAAAAAAAAAA
L_33:	dq	0x3333333333333333
L_0F:	dq	0x0F0F0F0F0F0F0F0F
%endif

    text

%define	PARAM_SIZE	 8
%define PARAM_SRC	 4
    global	___gmpn_popcount
%ifdef	DLL
    export	___gmpn_popcount
%endif
    align   16
___gmpn_popcount:
    ph_fun	0

%define	PARAM_SIZE	12
%define	PARAM_SRC2	 8
%define	PARAM_SRC	 4
    global	___gmpn_hamdist
%ifdef	DLL
    export	___gmpn_hamdist
%endif
    align   16
___gmpn_hamdist:
    ph_fun	1

    end
