
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

%include "..\x86i.inc" 

	global  ___gmpn_copyd 

%ifdef	DLL
	export	___gmpn_copyd
%endif

%define	PARAM_SIZE	esp+frame+12 
%define PARAM_SRC   esp+frame+8 
%define PARAM_DST   esp+frame+4 

%define	SAVE_ESI	PARAM_SIZE
%define	SAVE_EDI	PARAM_SRC
%define	frame		0 

	section .text
	align   16

___gmpn_copyd: 
	mov     ecx,[PARAM_SIZE]
    mov     [SAVE_ESI],esi
    mov     esi,[PARAM_SRC]
    mov     [SAVE_EDI],edi
    mov     edi,[PARAM_DST]
    sub     ecx,1
    jb      Lzero
    mov     eax,[esi+ecx*4]			;  src[size-1] 
    jz      Lone
    mov     edx,[-4+esi+ecx*4]		;  src[size-2] 
    sub     ecx,2
    jbe     Ldone_loop              ;  2 or 3 limbs only 
        
;  The usual overlap is 
;
;      high                   low 
;      +------------------+ 
;      |               dst| 
;      +------------------+ 
;            +------------------+ 
;            |               src| 
;            +------------------+ 
;
;  We can use an incrementing copy in the following circumstances. 
;
;      src+4*size<=dst,since then the regions are disjoint 
;
;      src==dst,clearly (though this shouldn't occur normally) 
;
;      src>dst,since in that case it's a requirement of the 
;               parameters that src>=dst+size*4,and hence the 
;               regions are disjoint 
;
;  eax prev high limb 
;  ebx 
;  ecx counter,size-3 down to 0 or -1,inclusive,by 2s 
;  edx prev low limb 
;  esi src 
;  edi dst 
;  ebp 

    lea     edx,[edi+ecx*4]
    cmp     esi,edi
    jae     Luse_movsl			;  src >= dst 
    cmp     edx,edi
    mov     edx,[4+esi+ecx*4]	;  src[size-2] again 
    jbe     Luse_movsl			;  src+4*size <= dst 
Ltop: 
    mov     [8+edi+ecx*4],eax
    mov     eax,[esi+ecx*4]
    mov     [4+edi+ecx*4],edx
    mov     edx,[-4+esi+ecx*4]
    sub     ecx,2
    jnbe    Ltop
Ldone_loop: 
    mov     [8+edi+ecx*4],eax
    mov     [4+edi+ecx*4],edx

;  copy low limb (needed if size was odd,but will already have been 
;  done in the loop if size was even) 

    mov     eax,[esi]
Lone: 
    mov     [edi],eax
    mov     edi,[SAVE_EDI]
    mov     esi,[SAVE_ESI]
	ret
Luse_movsl: 
    add     ecx,3
    cld
    rep		movsd
Lzero: 
    mov     esi,[SAVE_ESI]
    mov     edi,[SAVE_EDI]
    ret

	end
