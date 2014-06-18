
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

%include "..\x86i.inc" 

	global  ___gmpn_divexact_by3c 

%ifdef	DLL
	export	___gmpn_divexact_by3c
%endif

%define	PARAM_CARRY esp+frame+16 
%define PARAM_SIZE  esp+frame+12 
%define PARAM_SRC   esp+frame+8 
%define PARAM_DST   esp+frame+4 
%define	frame		0 

;   multiplicative inverse of 3,modulo 2^32 
;   ceil(b/3),ceil(b*2/3) and floor(b*2/3) where b=2^32 
%define	INVERSE_3			0xAAAAAAAB
%define	ONE_THIRD_CEIL		0x55555556
%define	TWO_THIRDS_CEIL		0xAAAAAAAB
%define	TWO_THIRDS_FLOOR	0xAAAAAAAA

	section .text

	align   8   

___gmpn_divexact_by3c: 
    mov     ecx,[PARAM_SRC]
    mov     edx,[PARAM_SIZE]
    dec     edx
    jnz     Ltwo_or_more
    mov     edx,[ecx]
    mov     eax,[PARAM_CARRY]			;  risk of cache bank clash here 
    mov     ecx,[PARAM_DST]
    sub     edx,eax
    sbb     eax,eax						;  0 or -1 
    imul    edx,edx,INVERSE_3
    neg     eax							;  0 or 1 
    cmp     edx,ONE_THIRD_CEIL
    sbb     eax,-1						;  +1 if edx>=ceil(b/3) 
    cmp     edx,TWO_THIRDS_CEIL
    sbb     eax,-1						;  +1 if edx>=ceil(b*2/3) 
    mov     [ecx],edx
    ret

;  eax 
;  ebx 
;  ecx src 
;  edx size-1 
;  esi 
;  edi 
;  ebp 

Ltwo_or_more: 
	FR_push	ebx
	FR_push	esi
	FR_push	edi
	FR_push	ebp
    mov     edi,[PARAM_DST]
    mov     esi,[PARAM_CARRY]
    mov     eax,[ecx]				;  src low limb 
    xor     ebx,ebx
	sub     eax,esi
    mov     esi,TWO_THIRDS_FLOOR
    lea     ecx,[ecx+edx*4]			;  &src[size-1] 
    lea     edi,[edi+edx*4]			;  &dst[size-1] 
    adc     ebx,0					;  carry,0 or 1 
    neg     edx						;  -(size-1) 

;  The loop needs a source limb ready at the top,which leads to one limb 
;  handled separately at the end,and the special case above for size==1. 
;  There doesn't seem to be any scheduling that would keep the speed but move 
;  the source load and carry subtract up to the top. 
; 
;  The destination cache line prefetching adds 1 cycle to the loop but is 
;  considered worthwhile.  The slowdown is a factor of 1.07,but will prevent 
;  repeated write-throughs if the destination isn't in L1.  A version using 
;  an outer loop to prefetch only every 8 limbs (a cache line) proved to be 
;  no faster,due to unavoidable branch mispreditions in the inner loop. 
; 
;  setc is 2 cycles on P54,so an adcl is used instead.  If the movl $0,%ebx 
;  could be avoided then the src limb fetch could pair up and save a cycle. 
;  This would probably mean going to a two limb loop with the carry limb 
;  alternately positive or negative,since an sbbl %ebx,%ebx will leave a 
;  value which is in the opposite sense to the preceding sbbl/adcl %ebx,%eax. 
; 
;  A register is used for TWO_THIRDS_FLOOR because a cmp can't be done as 
;  "cmpl %edx,$n" with the immediate as the second operand. 
; 
;  The "4" source displacement is in the loop rather than the setup because 
;  this gets Ltop aligned to 8 bytes at no cost. 

;  eax source limb,carry subtracted 
;  ebx carry (0 or 1) 
;  ecx &src[size-1] 
;  edx counter,limbs,negative 
;  esi TWO_THIRDS_FLOOR 
;  edi &dst[size-1] 
;  ebp scratch (result limb) 

	align   8
Ltop: 
    imul    ebp,eax,INVERSE_3
    cmp     ebp,ONE_THIRD_CEIL
    mov     eax,[edi+edx*4]		;  dst cache line prefetch 
    sbb     ebx,-1				;  +1 if ebp>=ceil(b/3) 
    cmp     esi,ebp
    mov     eax,[4+ecx+edx*4]	;  next src limb 
    sbb     eax,ebx				;  and further -1 if ebp>=ceil(b*2/3) 
    mov     ebx,0
    adc     ebx,0				;  new carry 
    mov     [edi+edx*4],ebp
    inc     edx
    jnz     Ltop
    imul    edx,eax,INVERSE_3
    cmp     edx,ONE_THIRD_CEIL
    mov     [edi],edx
    sbb     ebx,-1				;  +1 if edx>=ceil(b/3) 
    cmp     edx,TWO_THIRDS_CEIL
    sbb     ebx,-1				;  +1 if edx>=ceil(b*2/3) 
    pop     ebp
    mov     eax,ebx
    pop     edi
    pop     esi
    pop     ebx
    ret

	end
