
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

%include "..\x86i.inc" 

%define	UNROLL_LOG2		4
%define	UNROLL_COUNT	(1 << UNROLL_LOG2)
%define	UNROLL_MASK		UNROLL_COUNT-1  
%define	UNROLL_BYTES	4*UNROLL_COUNT

%ifdef	PIC
%define	UNROLL_THRESHOLD	5 
%else
%define	UNROLL_THRESHOLD	5 
%endif

%define	PARAM_CARRY			esp+frame+20 
%define PARAM_MULTIPLIER	esp+frame+16 
%define PARAM_SIZE			esp+frame+12 
%define PARAM_SRC			esp+frame+8 
%define PARAM_DST			esp+frame+4 

%macro	mul_fun	4

	global  %1%3
	global  %1%4

%ifdef	DLL
	export	%1%3
	export  %1%4
%endif

	align   32
%define	frame	0
%1%4:
    FR_push ebx
    mov     ebx,[PARAM_CARRY]
	jmp     %%Lstart_nc

%define	frame	0
%1%3:
	FR_push ebx
    xor     ebx,ebx	;  initial carry 
%%Lstart_nc: 
    mov     ecx,[PARAM_SIZE]
    FR_push esi
    mov     esi,[PARAM_SRC]
    FR_push	edi
    mov     edi,[PARAM_DST]
    FR_push ebp
    cmp     ecx,UNROLL_THRESHOLD
    mov     ebp,[PARAM_MULTIPLIER]
    jae     %%Lunroll

;  simple loop 
;  this is offset 0x22,so close enough to aligned 
;  eax scratch 
;  ebx carry 
;  ecx counter 
;  edx scratch 
;  esi src 
;  edi dst 
;  ebp multiplier 

%%Lsimple: 
    mov     eax,[esi]
    add     edi,4
    mul     ebp
    add     eax,ebx
    adc     edx,0
	%2		[edi-4],eax
    mov     ebx,edx
    adc     ebx,0
    dec     ecx
    lea     esi,[4+esi]
    jnz     %%Lsimple
    pop     ebp
    pop     edi
    pop     esi
    mov     eax,ebx
    pop     ebx
    ret

;  VAR_JUMP holds the computed jump temporarily because there's not enough 
;  registers when doing the mul for the initial two carry limbs. 
; 
;  The add/adc for the initial carry in %ebx is necessary only for the 
;  mpn_add/submul_1c entry points.  Duplicating the startup code to 
;  eliminiate this for the plain mpn_add/submul_1 doesn't seem like a good 
;  idea. 
;
;   overlapping with parameters already fetched 

%define	VAR_COUNTER	PARAM_SIZE
%define	VAR_JUMP	PARAM_DST

; VAL1 = ifelse(UNROLL_BYTES,256,128)
%define	VAL1	128
; VAL2 = ifelse(UNROLL_BYTES,256,-128)
%define	VAL2   -128

;  this is offset 0x43,so close enough to aligned 
;  eax 
;  ebx initial carry 
;  ecx size 
;  edx 
;  esi src 
;  edi dst 
;  ebp 

%%Lunroll: 
    mov     edx,ecx
    dec     ecx
    sub     edx,2
    neg     ecx
    shr     edx,UNROLL_LOG2
    and     ecx,UNROLL_MASK
    mov     [VAR_COUNTER],edx
    mov     edx,ecx
        
;  15 code bytes per limb 

%ifdef	PIC
	call    %%Lhere
%%Lhere: 
    shl     edx,4
    neg     ecx
    lea     edx,[edx+ecx*1]
    add     edx,%%Lentry-%%Lhere
    add     edx,[esp]
	add		esp,4
%else
	shl     edx,4
    neg     ecx
	lea		edx,[%%Lentry+edx+ecx]
%endif
    mov     eax,[esi]			;  src low limb 
    mov     [VAR_JUMP],edx
	lea		esi,[VAL1+4+esi+ecx*4]
    mul     ebp
    add     eax,ebx				;  initial carry (from _1c) 
    adc     edx,0
    mov     ebx,edx				;  high carry 
	lea		edi,[VAL1+edi+ecx*4]
    mov     edx,[VAR_JUMP]
    test    ecx,1
    mov     ecx,eax				;  low carry 
	cmovnz	ecx,ebx
	cmovnz	ebx,eax
    jmp     edx

;  eax scratch 
;  ebx carry hi 
;  ecx carry lo 
;  edx scratch 
;  esi src 
;  edi dst 
;  ebp multiplier 
;
;  VAR_COUNTER loop counter 
;
;  15 code bytes per limb 

%define	CHUNK_COUNT	2 

	align   32
%%Ltop: 
	add     edi,UNROLL_BYTES
%%Lentry: 
%assign	disp	VAL2
%rep	UNROLL_COUNT/CHUNK_COUNT
	mov		eax,[byte disp+esi]
	mul     ebp
	%2		[byte disp+edi],ecx
    adc     ebx,eax
    mov     ecx,edx
    adc     ecx,0
    mov     eax,[byte disp+4+esi]
    mul     ebp
	%2		[byte disp+4+edi],ebx
    adc     ecx,eax
    mov     ebx,edx
    adc     ebx,0
%assign		disp	disp+4*CHUNK_COUNT
%endrep

    dec     dword [VAR_COUNTER]
    lea     esi,[UNROLL_BYTES+esi]
    jns     %%Ltop

%assign	disp	UNROLL_BYTES+VAL2
	%2		[disp+edi],ecx
    mov     eax,ebx
    pop     ebp
    pop     edi
    pop     esi
    pop     ebx
    adc     eax,0
    ret
%endmacro

	section .text

	mul_fun	___g,add,mpn_addmul_1,mpn_addmul_1c
	mul_fun	___g,sub,mpn_submul_1,mpn_submul_1c
	
	end
