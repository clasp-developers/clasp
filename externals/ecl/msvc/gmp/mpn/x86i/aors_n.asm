
;  Copyright 1992, 1994, 1995, 1996, 1999, 2000, 2001, 2002 Free Software
;  Foundation, Inc.
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

; mp_limb_t M4_function_n (mp_ptr dst,mp_srcptr src1,mp_srcptr src2,
; mp_size_t size); 
; mp_limb_t M4_function_nc (mp_ptr dst,mp_srcptr src1,mp_srcptr src2,
; mp_size_t size,mp_limb_t carry); 

%define PARAM_SPACE   20
%define PARAM_CARRY   esp+frame+20
%define PARAM_SIZE    esp+frame+16
%define PARAM_SRC2    esp+frame+12
%define PARAM_SRC1    esp+frame+8
%define PARAM_DST     esp+frame+4

%macro	mac_sub 4

	global  %1%4
%ifdef	DLL
	export	%1%4
%endif

    align   8
%1%4:
%assign	frame   0
	FR_push edi
	FR_push esi
    mov     edi,[PARAM_DST]
    mov     esi,[PARAM_SRC1]
    mov     edx,[PARAM_SRC2]
    mov     ecx,[PARAM_SIZE]
    mov     eax,ecx
    shr     ecx,3				; compute count for unrolled %%4 
    neg     eax
    and     eax,7				; get index where to start %%4 
    jz      %%3					; necessary special case for 0 
    inc     ecx					; adjust %%4 count 
    shl     eax,2				; adjustment for pointers... 
    sub     edi,eax				; ... since they are offset ... 
    sub     esi,eax				; ... by a constant when we ... 
    sub     edx,eax				; ... enter the %%4 
    shr     eax,2				; restore previous value 

; Calculate start address in %%4

%ifdef	PIC
    call    %%1
%%1:
	lea     eax,[%%4-%%1-3+eax+eax*8]
    add     eax,[esp]
    add     esp,4
%else
	lea     eax,[%%4-3+eax+eax*8]
%endif

; These lines initialize carry from the 5th parameter.  Should be 
; possible to simplify. 

	FR_push ebp    
	mov     ebp,[PARAM_CARRY]    
    shr     ebp,1				; shift bit 0 into carry 
	FR_pop  ebp
    jmp     eax					; jump into %%4 

	global	%1%3
%ifdef	DLL
	export	%1%3
%endif
	align   8	
%1%3:
%assign	frame	0
	FR_push edi
    FR_push esi
    mov     edi,[PARAM_DST]
    mov     esi,[PARAM_SRC1]
    mov     edx,[PARAM_SRC2]
    mov     ecx,[PARAM_SIZE]
    mov     eax,ecx
    shr     ecx,3				; compute count for unrolled %%4 
    neg     eax
    and     eax,7				; get index where to start %%4 
    jz      %%4					; necessary special case for 0 
    inc     ecx					; adjust %%4 count 
    shl     eax,2				; adjustment for pointers... 
    sub     edi,eax				; ... since they are offset ... 
    sub     esi,eax				; ... by a constant when we ... 
    sub     edx,eax				; ... enter the %%4 
    shr     eax,2				; restore previous value 

; Calculate start address in %%4 for PIC.  
; Due to limitations in some assemblers,%%4-%%2-3 
; cannot be put into the leal 

%ifdef	PIC
	call    %%2
%%2:
	lea     eax,[%%4-%%2-3+eax+eax*8]
    add     eax,[esp]
    add     esp,4
%else
    lea     eax,[%%4-3+eax+eax*8]
%endif
	jmp     eax					; jump into %%4 
%%3:
	FR_push ebp
    mov     ebp,[PARAM_CARRY]
    shr     ebp,1				; shift bit 0 into carry 
	FR_pop  ebp

	align   8
%%4:
	mov     eax,[esi]
	%2		eax,[edx]
    mov     [edi],eax
    mov     eax,[4+esi]
	%2		eax,[edx+4]
    mov     [4+edi],eax
    mov     eax,[8+esi]
	%2		eax,[edx+8]
    mov     [8+edi],eax
    mov     eax,[12+esi]
    %2		eax,[edx+12]
    mov     [12+edi],eax
    mov     eax,[16+esi]
	%2		eax,[edx+16]
    mov     [16+edi],eax
    mov     eax,[20+esi]
	%2		eax,[edx+20]
    mov     [20+edi],eax
    mov     eax,[24+esi]
	%2		eax,[edx+24]
    mov     [24+edi],eax
    mov     eax,[28+esi]
    %2		eax,[edx+28]
    mov     [28+edi],eax
    lea     edi,[32+edi]
    lea     esi,[32+esi]
    lea     edx,[32+edx]
    dec     ecx
    jnz     %%4
    sbb     eax,eax
    neg     eax
    pop     esi
    pop     edi
    ret		
%endmacro

	section .text

	mac_sub	___g,adc,mpn_add_n,mpn_add_nc
	mac_sub	___g,sbb,mpn_sub_n,mpn_sub_nc
	
    end
