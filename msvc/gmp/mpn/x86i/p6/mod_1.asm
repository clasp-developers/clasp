
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

%include "..\x86i.inc" 

    global  ___gmpn_preinv_mod_1 
    global  ___gmpn_mod_1c     
	global  ___gmpn_mod_1 

%ifdef	DLL
	export	___gmpn_mod_1c
	export	___gmpn_mod_1
%endif

%define	MUL_NORM_THRESHOLD      4 
%define MUL_UNNORM_THRESHOLD	5 

%define	MUL_NORM_DELTA	MUL_NORM_THRESHOLD - MUL_UNNORM_THRESHOLD

%define	PARAM_INVERSE	esp+frame+16 
%define PARAM_CARRY     esp+frame+16 
%define PARAM_DIVISOR   esp+frame+12 
%define PARAM_SIZE      esp+frame+8 
%define PARAM_SRC       esp+frame+4 

%define SAVE_EBX        esp+frame-4 
%define SAVE_ESI        esp+frame-8 
%define SAVE_EDI        esp+frame-12 
%define SAVE_EBP        esp+frame-16 

%define	VAR_NORM        esp+frame-20 
%define VAR_INVERSE     esp+frame-24 
%define STACK_SPACE     24 

	section .text
	
	align   16

%define frame	0 

___gmpn_preinv_mod_1: 
    mov     edx,[PARAM_SRC]
    sub     esp,STACK_SPACE
	FR_sesp	STACK_SPACE
    mov     [SAVE_EBX],ebx
    mov     ebx,[PARAM_SIZE]
    mov     [SAVE_EBP],ebp
    mov     ebp,[PARAM_DIVISOR]
    mov     [SAVE_ESI],esi
    mov     eax,[PARAM_INVERSE]
    mov     [SAVE_EDI],edi
    mov     edi,[-4+edx+ebx*4]	;  src high limb 
    mov     [VAR_NORM],dword 0
    lea     ecx,[-8+edx+ebx*4]	;  &src[size-2] 
    mov     esi,edi
    sub     edi,ebp				;  high-divisor 
	cmovc	edi,esi
    dec     ebx
    jnz     Lpreinv_entry
    jmp     Ldone_edi
     
	align   16

%define       frame   0 

___gmpn_mod_1c: 
    mov     ecx,[PARAM_SIZE]
    sub     esp,STACK_SPACE
	FR_sesp	STACK_SPACE
    mov     [SAVE_EBP],ebp
    mov     eax,[PARAM_DIVISOR]
    mov     [SAVE_ESI],esi
    mov     edx,[PARAM_CARRY]
    mov     esi,[PARAM_SRC]
    or      ecx,ecx
    jz      Ldone_edx       ;  result==carry if size==0 
    sar     eax,31
    mov     ebp,[PARAM_DIVISOR]
    and     eax,MUL_NORM_DELTA
    add     eax,MUL_UNNORM_THRESHOLD
    cmp     ecx,eax
    jb      Ldivide_top

;  The carry parameter pretends to be the src high limb. 
    mov     [SAVE_EBX],ebx
    lea     ebx,[1+ecx]    ;  size+1 
    mov     eax,edx         ;  carry 
    jmp     Lmul_by_inverse_1c

	align   16

%define       frame   0
 
___gmpn_mod_1: 
    mov     ecx,[PARAM_SIZE]
    sub     esp,STACK_SPACE
	FR_sesp	STACK_SPACE
    mov     edx,0				;  initial carry (if can't skip a div) 
    mov     [SAVE_ESI],esi
    mov     eax,[PARAM_SRC]
    mov     [SAVE_EBP],ebp
    mov     ebp,[PARAM_DIVISOR]
    mov     esi,[PARAM_DIVISOR]
    or      ecx,ecx
    jz      Ldone_edx
    mov     eax,[-4+eax+ecx*4]	;  src high limb 
    sar     ebp,31
    and     ebp,MUL_NORM_DELTA
    add     ebp,MUL_UNNORM_THRESHOLD
    cmp     eax,esi				;  carry flag if high<divisor 
	cmovc	edx,eax
    mov     esi,[PARAM_SRC]
    sbb     ecx,0				;  size-1 to skip one div 
    jz      Ldone_eax			;  done if had size==1 
    cmp     ecx,ebp
    mov     ebp,[PARAM_DIVISOR]
    jae     Lmul_by_inverse

;  eax scratch (quotient) 
;  ebx 
;  ecx counter,limbs,decrementing 
;  edx scratch (remainder) 
;  esi src 
;  edi 
;  ebp divisor 

Ldivide_top: 
    mov     eax,[-4+esi+ecx*4]
    div     ebp
    dec     ecx
    jnz     Ldivide_top
Ldone_edx: 
    mov     eax,edx
Ldone_eax: 
    mov     esi,[SAVE_ESI]
    mov     ebp,[SAVE_EBP]
    add     esp,STACK_SPACE
    ret

;  eax src high limb 
;  ebx 
;  ecx 
;  edx 
;  esi src 
;  edi 
;  ebp divisor 

Lmul_by_inverse: 
    mov     [SAVE_EBX],ebx
    mov     ebx,[PARAM_SIZE]
Lmul_by_inverse_1c: 
    bsr     ecx,ebp				;  31-l 
    mov     [SAVE_EDI],edi
    xor     ecx,31				;  l 
    mov     [VAR_NORM],ecx
    shl     ebp,cl				;  d normalized 
    mov     edi,eax				;  src high -> n2 
    sub     eax,ebp
	cmovnc	edi,eax
    mov     eax,-1
    mov     edx,-1
    sub     edx,ebp				;  (b-d)-1 so  edx:eax = b*(b-d)-1 
    lea     ecx,[-8+esi+ebx*4] ;  &src[size-2] 
    div     ebp					;  floor (b*(b-d)-1) / d 
Lpreinv_entry: 
    mov     [VAR_INVERSE],eax

;  No special scheduling of loads is necessary in this loop,out of order 
;  execution hides the latencies already. 
; 
;  The way q1+1 is generated in %ebx and d is moved to %eax for the multiply 
;  seems fastest.  The obvious change to generate q1+1 in %eax and then just 
;  multiply by %ebp (as per mpn/x86/pentium/mod_1.asm in fact) runs 1 cycle 
;  slower,for no obvious reason. 

;  eax n10 (then scratch) 
;  ebx scratch (nadj,q1) 
;  ecx src pointer,decrementing 
;  edx scratch 
;  esi n10 
;  edi n2 
;  ebp divisor 

	align   16
Linverse_top: 
    mov     eax,[ecx]			;  next src limb 
    mov     esi,eax
    sar     eax,31				;  -n1 
    mov     ebx,ebp
    and     ebx,eax				;  -n1 & d 
    neg     eax					;  n1 
    add     eax,edi				;  n2+n1 
    mul     dword [VAR_INVERSE] ;  m*(n2+n1) 
    add     ebx,esi				;  nadj = n10 + (-n1 & d),ignoring overflow 
    sub     ecx,4
    add     eax,ebx				;  m*(n2+n1) + nadj,low giving carry flag 
    lea     ebx,[1+edi]			;  n2+1 
    mov     eax,ebp				;  d 
    adc     ebx,edx				;  1 + high(n2<<32 + m*(n2+n1) + nadj) = q1+1 
    jz      Lq1_ff
    mul     ebx					;  (q1+1)*d 
    sub     esi,eax				;  low n - (q1+1)*d 
    sbb     edi,edx				;  high n - (q1+1)*d,0 or -1 
    and     edi,ebp				;  d if underflow 
    add     edi,esi				;  remainder with addback if necessary 
    cmp     ecx,[PARAM_SRC]
    jae     Linverse_top

;  %edi is the remainder modulo d*2^n and now must be reduced to 
;  0<=r<d by calculating r*2^n mod d*2^n and then right shifting by 
;  n.  If d was already normalized on entry so that n==0 then nothing 
;  is needed here.  The chance of n==0 is low,but it's true of say 
;  PP from gmp-impl.h. 
;
;  eax 
;  ebx 
;  ecx 
;  edx 
;  esi  
;  edi remainder 
;  ebp divisor (normalized) 

Linverse_loop_done: 
    mov     ecx,[VAR_NORM]
    mov     esi,0
    or      ecx,ecx
    jz      Ldone_edi

;  Here use %edi=n10 and %esi=n2,opposite to the loop above. 
;
;  The q1=0xFFFFFFFF case is handled with an sbbl to adjust q1+1 
;  back,rather than q1_ff special case code.  This is simpler and 
;  costs only 2 uops. 

	shld	esi,edi,cl
    shl     edi,cl
    mov     eax,edi				;  n10 
    mov     ebx,ebp				;  d 
    sar     eax,31				;  -n1 
    and     ebx,eax				;  -n1 & d 
    neg     eax					;  n1 
    add     ebx,edi				;  nadj = n10 + (-n1 & d),ignoring overflow 
    add     eax,esi				;  n2+n1 
    mul     dword [VAR_INVERSE]	;  m*(n2+n1) 
    add     eax,ebx				;  m*(n2+n1) + nadj,low giving carry flag 
    lea     ebx,[1+esi]			;  n2+1 
    adc     ebx,edx				;  1 + high(n2<<32 + m*(n2+n1) + nadj) = q1+1 
    sbb     ebx,0
    mov     eax,ebp				;  d 
    mul     ebx					;  (q1+1)*d 
    mov     ebx,[SAVE_EBX]
    sub     edi,eax				;  low  n - (q1+1)*d is remainder 
    sbb     esi,edx				;  high n - (q1+1)*d,0 or -1 
    and     esi,ebp
    mov     ebp,[SAVE_EBP]
    lea     eax,[esi+edi]		;  remainder 
    mov     esi,[SAVE_ESI]
    shr     eax,cl				;  denorm remainder 
    mov     edi,[SAVE_EDI]
    add     esp,STACK_SPACE
    ret
Ldone_edi: 
    mov     ebx,[SAVE_EBX]
    mov     eax,edi
    mov     esi,[SAVE_ESI]
    mov     edi,[SAVE_EDI]
    mov     ebp,[SAVE_EBP]
    add     esp,STACK_SPACE
    ret

;  Special case for q1=0xFFFFFFFF,giving q=0xFFFFFFFF meaning the low dword 
;  of q*d is simply -d and the remainder n-q*d = n10+d. 
; 
;  This is reached only very rarely. 
;
;  eax (divisor) 
;  ebx (q1+1 == 0) 
;  ecx src pointer 
;  edx 
;  esi n10 
;  edi (n2) 
;  ebp divisor 

Lq1_ff: 
    lea     edi,[ebp+esi]  ;  n-q*d remainder -> next n2 
    cmp     ecx,[PARAM_SRC]
    jae     Linverse_top
    jmp     Linverse_loop_done

	end
