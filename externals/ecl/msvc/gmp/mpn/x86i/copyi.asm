
;  Copyright 1999, 2000, 2001, 2002 Free Software Foundation, Inc.
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

	global  ___gmpn_copyi

%ifdef	DLL
	export	___gmpn_copyi
%endif

%define PARAM_SIZE    esp+frame+12
%define PARAM_SRC     esp+frame+8
%define PARAM_DST     esp+frame+4
%assign       frame   0

	section .text
    align   32

; eax  saved esi 
; ebx 
; ecx  counter 
; edx  saved edi 
; esi  src 
; edi  dst 
; ebp 

___gmpn_copyi: 
    mov     ecx,[PARAM_SIZE]
    mov     eax,esi
    mov     esi,[PARAM_SRC]
    mov     edx,edi
    mov     edi,[PARAM_DST]
    cld						; better safe than sorry,see mpn/x86/README 
    rep	movsd
    mov     esi,eax
    mov     edi,edx
    ret

	end
