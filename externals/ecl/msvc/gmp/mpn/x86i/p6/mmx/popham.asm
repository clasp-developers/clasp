
;  Copyright 2000, 2002 Free Software Foundation, Inc.
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

%define       REG_AAAAAAAAAAAAAAAA    mm7 
%define       REG_3333333333333333    mm6 
%define       REG_0F0F0F0F0F0F0F0F    mm5 
%define       REG_000000FF000000FF    mm4 

%ifndef	PIC

	section	.data
	align   8

Lrodata_AAAAAAAAAAAAAAAA: 
    dd      0AAAAAAAAh
    dd      0AAAAAAAAh

Lrodata_3333333333333333: 
    dd      033333333h
    dd      033333333h

Lrodata_0F0F0F0F0F0F0F0F: 
    dd      00F0F0F0Fh
    dd      00F0F0F0Fh

Lrodata_000000FF000000FF: 
    dd      0000000FFh
    dd      0000000FFh

%endif

%macro	ph_fun 3

	section .text
	align   32
	global	%1%2

%ifdef	DLL
	export	%1%2
%endif

%if	%3 == 0
%ifdef	PIC
	nop		;  avoid shrl crossing a 32-byte boundary 
%endif
%endif

%1%2: 
	mov     ecx, [PARAM_SIZE]
%ifdef	PIC
    mov     eax, 0xAAAAAAAA
    mov     edx, 0x33333333
    movd    mm7,eax
    movd    mm6,edx
    mov     eax, 0x0F0F0F0F
    mov     edx, 0x000000FF
    punpckldq mm7,mm7
    punpckldq mm6,mm6
    movd    mm5,eax
    movd    mm4,edx
    punpckldq mm5,mm5
    punpckldq mm4,mm4
%else
    movq    mm7,[Lrodata_AAAAAAAAAAAAAAAA]
    movq    mm6,[Lrodata_3333333333333333]
    movq    mm5,[Lrodata_0F0F0F0F0F0F0F0F]
    movq    mm4,[Lrodata_000000FF000000FF]
%endif
	mov     eax,[PARAM_SRC]
%if	%3 == 1
	mov		edx,[PARAM_SRC2]
%endif
    pxor    mm2,mm2
	shr     ecx,1
	jnc     %%Ltop
	movd	mm1,[eax+ecx*8]	; Zdisp(  movd,0,(%eax,%ecx,8),%mm1)
%if	%3 == 1
	movd	mm0,[edx+ecx*8]	; Zdisp(  movd,0,(%edx,%ecx,8),%mm0)"
    pxor    mm1,mm0
%endif
    inc     ecx
    jmp     %%Lloaded

	align   16
%if	%3 == 0
	nop
%endif

;  eax src 
;  ebx 
;  ecx counter,qwords,decrementing 
;  edx [hamdist] src2 
; 
;  mm0 (scratch) 
;  mm1 (scratch) 
;  mm2 total (low dword) 
;  mm3 
;  mm4 \ 
;  mm5 | special constants 
;  mm6 | 
;  mm7 / 

%%Ltop: 
	movq    mm1,[eax+ecx*8-8]
%if %3 == 1
	pxor    mm1,[edx+ecx*8-8]
%endif
%%Lloaded: 
    movq    mm0,mm1
    pand    mm1,REG_AAAAAAAAAAAAAAAA
    psrlq   mm1,1
%if	%3 == 1
	nop
%endif
    psubd   mm0,mm1						;  bit pairs 
%if %3 == 1
	nop
%endif
    movq    mm1,mm0
    psrlq   mm0,2
    pand    mm0,REG_3333333333333333
    pand    mm1,REG_3333333333333333
    paddd   mm0,mm1						;  nibbles 
    movq    mm1,mm0
    psrlq   mm0,4
    pand    mm0,REG_0F0F0F0F0F0F0F0F
    pand    mm1,REG_0F0F0F0F0F0F0F0F
    paddd   mm0,mm1						;  bytes 
    movq    mm1,mm0
    psrlq   mm0,8
    paddb   mm0,mm1						;  words 
    movq    mm1,mm0
    psrlq   mm0,16
    paddd   mm0,mm1						;  dwords 
    pand    mm0,REG_000000FF000000FF
    paddd   mm2,mm0						;  low to total 
    psrlq   mm0,32
    paddd   mm2,mm0						;  high to total 
    loop    %%Ltop
    movd    eax,mm2
    emms
    ret
%endmacro

%define	PARAM_SIZE  esp+frame+8 
%define PARAM_SRC   esp+frame+4 
%define	frame		0

	ph_fun	___g,mpn_popcount,0

%define	PARAM_SIZE  esp+frame+12 
%define PARAM_SRC2  esp+frame+8 
%define PARAM_SRC   esp+frame+4 
%define	frame		0

	ph_fun	___g,mpn_hamdist,1

	end
