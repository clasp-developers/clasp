
;  Copyright 1999, 2000, 2001 Free Software Foundation, Inc.
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

	global	___gmpn_copyd
%ifdef	DLL
	export	___gmpn_copyd
%endif

	align	8
___gmpn_copyd:

    mov     ecx,[12+esp]
    mov     eax,[8+esp]
    mov     edx,[4+esp]
    mov     [12+esp],ebx
    add     ecx,-1
    js      nd
oop: 
    mov     ebx,[eax+ecx*4]
    mov     [edx+ecx*4],ebx
    add     ecx,-1
    jns     oop
nd: 
    mov     ebx,[12+esp]
    ret

	end
