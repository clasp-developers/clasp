
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

%include "..\..\x86i.inc" 

	global  ___gmpn_preinv_divrem_1 
    global  ___gmpn_divrem_1c 
    global  ___gmpn_divrem_1 

%ifdef	DLL
	export	___gmpn_divrem_1c
	export	___gmpn_divrem_1
%endif

%define	MUL_THRESHOLD		4 
%define	PARAM_PREINV_SHIFT      esp+frame+28 
%define PARAM_PREINV_INVERSE	esp+frame+24 
%define PARAM_CARRY     esp+frame+24 
%define PARAM_DIVISOR   esp+frame+20 
%define PARAM_SIZE      esp+frame+16 
%define PARAM_SRC       esp+frame+12 
%define PARAM_XSIZE     esp+frame+8 
%define PARAM_DST       esp+frame+4 

%define SAVE_EBX        esp+frame-4 
%define SAVE_ESI        esp+frame-8 
%define SAVE_EDI        esp+frame-12 
%define SAVE_EBP        esp+frame-16 

%define VAR_NORM        esp+frame-20 
%define VAR_INVERSE     esp+frame-24 
%define VAR_SRC			esp+frame-28 
%define VAR_DST			esp+frame-32 
%define VAR_DST_STOP    esp+frame-36 
%define STACK_SPACE		36 
%define frame			0 

	section .text

	align   16   

___gmpn_preinv_divrem_1: 
    mov     ecx,[PARAM_XSIZE]
    sub     esp,STACK_SPACE
	FR_sesp	STACK_SPACE
    mov     [SAVE_ESI],esi
    mov     esi,[PARAM_SRC]
    mov     [SAVE_EBX],ebx
    mov     ebx,[PARAM_SIZE]
    mov     [SAVE_EBP],ebp
    mov     ebp,[PARAM_DIVISOR]
    mov     [SAVE_EDI],edi
    mov     edx,[PARAM_DST]
    mov     eax,[-4+esi+ebx*4]	;  src high limb 
    xor     edi,edi				;  initial carry (if can't skip a div) 
	lea     edx,[8+edx+ecx*4]	;  &dst[xsize+2] 
	xor     ecx,ecx
    mov     [VAR_DST_STOP],edx	;  &dst[xsize+2] 
    cmp     eax,ebp				;  high cmp divisor 
	cmovc	edi,eax
	cmovnc	ecx,eax				;  (the latter in case src==dst) 
    mov     [-12+edx+ebx*4],ecx	;  dst high limb 
	sbb     ebx,0				;  skip one division if high<divisor 
    mov     ecx,[PARAM_PREINV_SHIFT]
    lea     edx,[-8+edx+ebx*4]	;  &dst[xsize+size] 
    mov     eax,32
    mov     [VAR_DST],edx		;  &dst[xsize+size] 
    shl     ebp,cl				;  d normalized 
    sub     eax,ecx
    mov     [VAR_NORM],ecx
    movd    mm7,eax				;  rshift 
    mov     eax,[PARAM_PREINV_INVERSE]
    jmp     Lstart_preinv

	align   16

%define       frame   0 

___gmpn_divrem_1c: 
    mov     edx,[PARAM_CARRY]
    mov     ecx,[PARAM_SIZE]
    sub     esp,STACK_SPACE
%define frame   STACK_SPACE 
    mov     [SAVE_EBX],ebx
    mov     ebx,[PARAM_XSIZE]
    mov     [SAVE_EDI],edi
    mov     edi,[PARAM_DST]
    mov     [SAVE_EBP],ebp
    mov     ebp,[PARAM_DIVISOR]
    mov     [SAVE_ESI],esi
    mov     esi,[PARAM_SRC]
    lea     edi,[-4+edi+ebx*4]
    jmp     Lstart_1c

;  offset 0x31,close enough to aligned 

%define       frame   0 

___gmpn_divrem_1: 
    mov     ecx,[PARAM_SIZE]
    mov     edx,0				;  initial carry (if can't skip a div) 
    sub     esp,STACK_SPACE
%define frame   STACK_SPACE 
    mov     [SAVE_EBP],ebp
    mov     ebp,[PARAM_DIVISOR]
    mov     [SAVE_EBX],ebx
    mov     ebx,[PARAM_XSIZE]
    mov     [SAVE_ESI],esi
    mov     esi,[PARAM_SRC]
    or      ecx,ecx				;  size 
    mov     [SAVE_EDI],edi
    mov     edi,[PARAM_DST]
    lea     edi,[-4+edi+ebx*4]	;  &dst[xsize-1] 
    jz      Lno_skip_div		;  if size==0 
    mov     eax,[-4+esi+ecx*4]	;  src high limb 
    xor     esi,esi
    cmp     eax,ebp				;  high cmp divisor 
	cmovc	edx,eax
	cmovnc	esi,eax				;  (the latter in case src==dst) 
    mov     [edi+ecx*4],esi		;  dst high limb 
    sbb     ecx,0				;  size-1 if high<divisor 
    mov     esi,[PARAM_SRC]		;  reload 
Lno_skip_div: 

;  eax  
;  ebx xsize 
;  ecx size 
;  edx carry 
;  esi src 
;  edi &dst[xsize-1] 
;  ebp divisor 

Lstart_1c: 
    lea     eax,[ebx+ecx]		;  size+xsize 
    cmp     eax,MUL_THRESHOLD
    jae     Lmul_by_inverse
    or      ecx,ecx
    jz      Ldivide_no_integer

;  eax scratch (quotient) 
;  ebx xsize 
;  ecx counter 
;  edx scratch (remainder) 
;  esi src 
;  edi &dst[xsize-1] 
;  ebp divisor 

Ldivide_integer: 
    mov     eax,[-4+esi+ecx*4]
    div     ebp
    mov     [edi+ecx*4],eax
    dec     ecx
    jnz     Ldivide_integer
Ldivide_no_integer: 
    mov     edi,[PARAM_DST]
    or      ebx,ebx
    jnz     Ldivide_fraction
Ldivide_done: 
    mov     esi,[SAVE_ESI]
    mov     edi,[SAVE_EDI]
    mov     ebx,[SAVE_EBX]
    mov     eax,edx
    mov     ebp,[SAVE_EBP]
    add     esp,STACK_SPACE
    ret

;  eax scratch (quotient) 
;  ebx counter 
;  ecx 
;  edx scratch (remainder) 
;  esi 
;  edi dst 
;  ebp divisor 

Ldivide_fraction: 
    mov     eax,0
    div     ebp
    mov     [-4+edi+ebx*4],eax
    dec     ebx
    jnz     Ldivide_fraction
    jmp     Ldivide_done

;  eax 
;  ebx xsize 
;  ecx size 
;  edx carry 
;  esi src 
;  edi &dst[xsize-1] 
;  ebp divisor 

Lmul_by_inverse: 
    lea     ebx,[12+edi]   ;  &dst[xsize+2],loop dst stop 
    mov     [VAR_DST_STOP],ebx
    lea     edi,[4+edi+ecx*4] ;  &dst[xsize+size] 
    mov     [VAR_DST],edi
    mov     ebx,ecx         ;  size 
    bsr     ecx,ebp         ;  31-l 
    mov     edi,edx         ;  carry 
    lea     eax,[1+ecx]    ;  32-l 
    xor     ecx,31         ;  l 
    mov     [VAR_NORM],ecx
    mov     edx,-1
    shl     ebp,cl          ;  d normalized 
    movd    mm7,eax
    mov     eax,-1
    sub     edx,ebp         ;  (b-d)-1 giving edx:eax = b*(b-d)-1 
    div     ebp             ;  floor (b*(b-d)-1) / d 

;  eax inverse 
;  ebx size 
;  ecx shift 
;  edx 
;  esi src 
;  edi carry 
;  ebp divisor 
;
;  mm7 rshift 

Lstart_preinv: 
    mov     [VAR_INVERSE],eax
    or      ebx,ebx         ;  size 
    lea     eax,[-12+esi+ebx*4] ;  &src[size-3] 
    mov     [VAR_SRC],eax
    jz      Lstart_zero
    mov     esi,[8+eax]    ;  src high limb 
    cmp     ebx,1
    jz      Lstart_one
Lstart_two_or_more: 
    mov     edx,[4+eax]    ;  src second highest limb 
	shld	edi,esi,cl
	shld	esi,edx,cl
    cmp     ebx,2
    je      Linteger_two_left
    jmp     Linteger_top

Lstart_one: 
	shld	edi,esi,cl
    shl     esi,cl          ;  n10 = high << l 
    jmp     Linteger_one_left

Lstart_zero: 
;  Can be here with xsize==0 if mpn_preinv_divrem_1 had size==1 and 
;  skipped a division. 

    shl     edi,cl          ;  n2 = carry << l 
    mov     eax,edi         ;  return value for zero_done 
    cmp     [PARAM_XSIZE],dword 0
    je      Lzero_done
    jmp     Lfraction_some

;  This loop runs at about 25 cycles,which is probably sub-optimal,and 
;  certainly more than the dependent chain would suggest.  A better loop,or 
;  a better rough analysis of what's possible,would be welcomed. 
; 
;  In the current implementation,the following successively dependent 
;  micro-ops seem to exist. 
; 
;                     uops 
;              n2+n1   1   (addl) 
;              mul     5 
;              q1+1    3   (addl/adcl) 
;              mul     5 
;              sub     3   (subl/sbbl) 
;              addback 2   (cmov) 
;                     --- 
;                     19 
; 
;  Lack of registers hinders explicit scheduling and it might be that the 
;  normal out of order execution isn't able to hide enough under the mul 
;  latencies. 
; 
;  Using sarl/negl to pick out n1 for the n2+n1 stage is a touch faster than 
;  cmov (and takes one uop off the dependent chain).  A sarl/andl/addl 
;  combination was tried for the addback (despite the fact it would lengthen 
;  the dependent chain) but found to be no faster. 

;  eax scratch 
;  ebx scratch (nadj,q1) 
;  ecx scratch (src,dst) 
;  edx scratch 
;  esi n10 
;  edi n2 
;  ebp d 
;
;  mm0 scratch (src qword) 
;  mm7 rshift for normalization 

	align   16
Linteger_top: 
    mov     eax,esi
    mov     ebx,ebp
    sar     eax,31				;  -n1 
    mov     ecx,[VAR_SRC]
    and     ebx,eax				;  -n1 & d 
    neg     eax					;  n1 
    add     ebx,esi				;  nadj = n10 + (-n1 & d),ignoring overflow 
    add     eax,edi				;  n2+n1 
    movq    mm0,[ecx]			;  next src limb and the one below it 
    mul     dword [VAR_INVERSE] ;  m*(n2+n1) 
    sub     ecx,4
    mov     [VAR_SRC],ecx
    add     eax,ebx				;  m*(n2+n1) + nadj,low giving carry flag 
    mov     eax,ebp				;  d 
    lea     ebx,[1+edi]			;  n2+1 
    adc     ebx,edx				;  1 + high(n2<<32 + m*(n2+n1) + nadj) = q1+1 
    jz      Lq1_ff
    mul     ebx					;  (q1+1)*d 
    mov     ecx,[VAR_DST]
    psrlq   mm0,mm7
    sub     esi,eax
    mov     eax,[VAR_DST_STOP]
    sbb     edi,edx				;  n - (q1+1)*d 
    mov     edi,esi				;  remainder -> n2 
    lea     edx,[ebp+esi]
	cmovc	edi,edx
    movd    esi,mm0
    sbb     ebx,0    ;  q 
    sub     ecx,4
    mov     [ecx],ebx
    cmp     ecx,eax
    mov     [VAR_DST],ecx
    jne     Linteger_top
Linteger_loop_done: 
 
;  Here,and in integer_one_left below,an sbbl $0 is used rather than a jz 
;  q1_ff special case.  This make the code a bit smaller and simpler,and 
;  costs only 2 cycles (each). 

;  eax scratch 
;  ebx scratch (nadj,q1) 
;  ecx scratch (src,dst) 
;  edx scratch 
;  esi n10 
;  edi n2 
;  ebp divisor 
;
;  mm7 rshift 

Linteger_two_left: 
    mov     eax,esi
    mov     ebx,ebp
    sar     eax,31				;  -n1 
    mov     ecx,[PARAM_SRC]
    and     ebx,eax				;  -n1 & d 
    neg     eax					;  n1 
    add     ebx,esi				;  nadj = n10 + (-n1 & d),ignoring overflow 
    add     eax,edi				;  n2+n1 
    mul     dword [VAR_INVERSE] ;  m*(n2+n1) 
    movd    mm0,[ecx]			;  src low limb 
    mov     ecx,[VAR_DST_STOP]
    add     eax,ebx				;  m*(n2+n1) + nadj,low giving carry flag 
    lea     ebx,[1+edi]			;  n2+1 
    mov     eax,ebp				;  d 
    adc     ebx,edx				;  1 + high(n2<<32 + m*(n2+n1) + nadj) = q1+1 
    sbb     ebx,0
    mul     ebx					;  (q1+1)*d 
    psllq   mm0,32
    psrlq   mm0,mm7
    sub     esi,eax
    sbb     edi,edx				;  n - (q1+1)*d 
    mov     edi,esi				;  remainder -> n2 
    lea     edx,[ebp+esi]
	cmovc	edi,edx
    movd    esi,mm0
    sbb     ebx,0				;  q 
    mov     [-4+ecx],ebx

;  eax scratch 
;  ebx scratch (nadj,q1) 
;  ecx scratch (dst) 
;  edx scratch 
;  esi n10 
;  edi n2 
;  ebp divisor 
;
;  mm7 rshift 

Linteger_one_left: 
    mov     eax,esi
    mov     ebx,ebp
    sar     eax,31				;  -n1 
    mov     ecx,[VAR_DST_STOP]
    and     ebx,eax				;  -n1 & d 
    neg     eax					;  n1 
    add     ebx,esi				;  nadj = n10 + (-n1 & d),ignoring overflow 
    add     eax,edi				;  n2+n1 
    mul     dword [VAR_INVERSE]	;  m*(n2+n1) 
    add     eax,ebx				;  m*(n2+n1) + nadj,low giving carry flag 
    lea     ebx,[1+edi]			;  n2+1 
    mov     eax,ebp				;  d 
    adc     ebx,edx				;  1 + high(n2<<32 + m*(n2+n1) + nadj) = q1+1 
    sbb     ebx,0				;  q1 if q1+1 overflowed 
    mul     ebx
    sub     esi,eax
    mov     eax,[PARAM_XSIZE]
    sbb     edi,edx				;  n - (q1+1)*d 
    mov     edi,esi				;  remainder -> n2 
    lea     edx,[ebp+esi]
	cmovc	edi,edx
    sbb     ebx,0				;  q 
    mov     [-8+ecx],ebx
    sub     ecx,8
    or      eax,eax				;  xsize 
    jnz     Lfraction_some
    mov     eax,edi
Lfraction_done: 
    mov     ecx,[VAR_NORM]
Lzero_done: 
    mov     ebp,[SAVE_EBP]
    mov     edi,[SAVE_EDI]
    mov     esi,[SAVE_ESI]
    mov     ebx,[SAVE_EBX]
    add     esp,STACK_SPACE
    shr     eax,cl
    emms
    ret

;  Special case for q1=0xFFFFFFFF,giving q=0xFFFFFFFF meaning the low dword 
;  of q*d is simply -d and the remainder n-q*d = n10+d 
;
;  eax (divisor) 
;  ebx (q1+1 == 0) 
;  ecx 
;  edx 
;  esi n10 
;  edi n2 
;  ebp divisor 

Lq1_ff: 
    mov     ecx,[VAR_DST]
    mov     edx,[VAR_DST_STOP]
    sub     ecx,4
    mov     [VAR_DST],ecx
    psrlq   mm0,mm7
    lea     edi,[ebp+esi]		;  n-q*d remainder -> next n2 
    mov     [ecx],dword -1
    movd    esi,mm0				;  next n10 
    cmp     edx,ecx
    jne     Linteger_top
    jmp     Linteger_loop_done

; 
;  In the current implementation,the following successively dependent 
;  micro-ops seem to exist. 
; 
;                     uops 
;              mul     5 
;              q1+1    1   (addl) 
;              mul     5 
;              sub     3   (negl/sbbl) 
;              addback 2   (cmov) 
;                     --- 
;                     16 
; 
;  The loop in fact runs at about 17.5 cycles.  Using a sarl/andl/addl for 
;  the addback was found to be a touch slower. 

;  eax 
;  ebx 
;  ecx 
;  edx 
;  esi 
;  edi carry 
;  ebp divisor 

	align   16
Lfraction_some: 
    mov     esi,[PARAM_DST]
    mov     ecx,[VAR_DST_STOP]	;  &dst[xsize+2] 
    mov     eax,edi
    sub     ecx,8				;  &dst[xsize] 

;  eax n2,then scratch 
;  ebx scratch (nadj,q1) 
;  ecx dst,decrementing 
;  edx scratch 
;  esi dst stop point 
;  edi n2 
;  ebp divisor 

	align   16
Lfraction_top: 
    mul     dword [VAR_INVERSE]	;  m*n2 
    mov     eax,ebp				;  d 
    sub     ecx,4				;  dst 
    lea     ebx,[edi+1]
    add     ebx,edx				;  1 + high(n2<<32 + m*n2) = q1+1 
    mul     ebx					;  (q1+1)*d 
    neg     eax					;  low of n - (q1+1)*d 
    sbb     edi,edx				;  high of n - (q1+1)*d,caring only about carry 
    lea     edx,[ebp+eax]
	cmovc	eax,edx
    sbb     ebx,0				;  q 
    mov     edi,eax				;  remainder->n2 
    cmp     ecx,esi
    mov     [ecx],ebx			;  previous q 
    jne     Lfraction_top
    jmp     Lfraction_done

	end
