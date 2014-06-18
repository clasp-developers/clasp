
;  Copyright 1999, 2000, 2002 Free Software Foundation, Inc.
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

%include	"x86i.inc"

%define PARAM_M2		esp+frame+12
%define PARAM_M1		esp+frame+8
%define PARAM_LOWPTR	esp+frame+4
%assign frame			0

	section .text
	
	global	___gmpn_umul_ppmm
%ifdef	DLL
	export	___gmpn_umul_ppmm
%endif

	align   8
___gmpn_umul_ppmm:
    mov     ecx,[PARAM_LOWPTR]
    mov     eax,[PARAM_M1]
    mul     dword [PARAM_M2]
    mov     [ecx],eax
    mov     eax,edx
    ret

	end
