
;   Intel P6 mpn_sqr_basecase -- square an mpn number. 
;
;   Copyright 1999,2000,2002 Free Software Foundation,Inc. 
;   
;   This file is part of the GNU MP Library. 
;   
;   The GNU MP Library is free software; you can redistribute it and/or 
;   modify it under the terms of the GNU Lesser General Public License as 
;   published by the Free Software Foundation; either version 2.1 of the 
;   License,or (at your option) any later version. 
;   
;   The GNU MP Library is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of 
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU 
;   Lesser General Public License for more details. 
;   
;   You should have received a copy of the GNU Lesser General Public 
;   License along with the GNU MP Library; see the file COPYING.LIB.  If 
;   not,write to the Free Software Foundation,Inc.,59 Temple Place - 
;   Suite 330,Boston,MA 02111-1307,USA. 
;
;  P6: approx 4.0 cycles per cross product,or 7.75 cycles per triangular 
;      product (measured on the speed difference between 20 and 40 limbs,
;      which is the Karatsuba recursing range). 
;
;   These are the same as in mpn/x86/k6/sqr_basecase.asm,see that file for 
;   a description.  The only difference here is that UNROLL_COUNT can go up 
;   to 64 (not 63) making SQR_KARATSUBA_THRESHOLD_MAX 67. 
;
;  void mpn_sqr_basecase (mp_ptr dst,mp_srcptr src,mp_size_t size); 
; 
;  The algorithm is basically the same as mpn/generic/sqr_basecase.c,but a 
;  lot of function call overheads are avoided,especially when the given size 
;  is small. 
; 
;  The code size might look a bit excessive,but not all of it is executed so 
;  it won't all get into the code cache.  The 1x1,2x2 and 3x3 special cases 
;  clearly apply only to those sizes; mid sizes like 10x10 only need part of 
;  the unrolled addmul; and big sizes like 40x40 that do use the full 
;  unrolling will least be making good use of it,because 40x40 will take 
;  something like 7000 cycles. 

%include "..\\x86i.inc" 

%define	SQR_KARATSUBA_THRESHOLD	10 
%define	SQR_KARATSUBA_THRESHOLD_MAX	67 

%ifdef	SQR_KARATSUBA_THRESHOLD_OVERRIDE
%define	SQR_KARATSUBA_THRESHOLD	SQR_KARATSUBA_THRESHOLD_OVERRIDE
%endif

%define	UNROLL_COUNT	SQR_KARATSUBA_THRESHOLD-3

FR_def	PARAM_SIZE,12
FR_def	PARAM_SRC,8
FR_def	PARAM_DST,4

	section .text
    global  ___gmpn_sqr_basecase 

	align   32
	
___gmpn_sqr_basecase: 
    mov     edx,[PARAM_SIZE]
    mov     eax,[PARAM_SRC]
    cmp     edx,2
    mov     ecx,[PARAM_DST]
    je      Ltwo_limbs
    mov     eax,[eax]
    ja      Lthree_or_more

;  one limb only 
;  eax src limb 
;  ebx 
;  ecx dst 
;  edx 

    mul     eax
    mov     [ecx],eax
    mov     [4+ecx],edx
    ret

;  eax src 
;  ebx 
;  ecx dst 
;  edx 

FR_def	SAVE_ESI, -4
FR_def	SAVE_EBX, -8
FR_def	SAVE_EDI,-12
FR_def	SAVE_EBP,-16

%define STACK_SPACE	16 
%define	frame		16

Ltwo_limbs: 
    sub     esp,frame
    mov     [SAVE_ESI],esi
    mov     esi,eax
    mov     eax,[eax]
    mul     eax				;  src[0]^2 
    mov     [ecx],eax		;  dst[0] 
    mov     eax,[4+esi]
    mov     [SAVE_EBX],ebx
    mov     ebx,edx			;  dst[1] 
    mul     eax				;  src[1]^2 
    mov     [SAVE_EDI],edi
    mov     edi,eax			;  dst[2] 
    mov     eax,[esi]
    mov     [SAVE_EBP],ebp
    mov     ebp,edx			;  dst[3] 
    mul     dword [4+esi]	;  src[0]*src[1] 
    add     ebx,eax
    mov     esi,[SAVE_ESI]
    adc     edi,edx
    adc     ebp,0
    add     eax,ebx
    mov     ebx,[SAVE_EBX]
    adc     edx,edi
    mov     edi,[SAVE_EDI]
    adc     ebp,0
    mov     [4+ecx],eax
    mov     [12+ecx],ebp
    mov     ebp,[SAVE_EBP]
    mov     [8+ecx],edx
    add     esp,frame
    ret

;  eax src low limb 
;  ebx 
;  ecx dst 
;  edx size 

%define       frame   0 

Lthree_or_more: 
    FR_push	esi
    cmp     edx,4
    mov     esi,[PARAM_SRC]
    jae     Lfour_or_more

;  three limbs 
;
;  eax src low limb 
;  ebx 
;  ecx dst 
;  edx 
;  esi src 
;  edi 
;  ebp 

%undef	SAVE_EBP
%undef	SAVE_EDI
%undef	SAVE_EBX

	FR_push	ebp,SAVE_EBP
	FR_push	edi,SAVE_EDI
    mul     eax				;  src[0] ^ 2 
    mov     [ecx],eax
    mov     [4+ecx],edx
    mov     eax,[4+esi]
    xor     ebp,ebp
    mul     eax				;  src[1] ^ 2 
    mov     [8+ecx],eax
    mov     [12+ecx],edx
    mov     eax,[8+esi]
	FR_push	ebx,SAVE_EBX
    mul     eax				;  src[2] ^ 2 
    mov     [16+ecx],eax
    mov     [20+ecx],edx
    mov     eax,[esi]
    mul     dword [4+esi]	;  src[0] * src[1] 
    mov     ebx,eax
    mov     edi,edx
    mov     eax,[esi]
    mul     dword [8+esi]	;  src[0] * src[2] 
    add     edi,eax
    mov     ebp,edx
    adc     ebp,0
    mov     eax,[4+esi]
    mul     dword [8+esi]	;  src[1] * src[2] 
    xor     esi,esi
    add     ebp,eax

;  eax 
;  ebx dst[1] 
;  ecx dst 
;  edx dst[4] 
;  esi zero,will be dst[5] 
;  edi dst[2] 
;  ebp dst[3] 

    adc     edx,0
    add     ebx,ebx
    adc     edi,edi
    adc     ebp,ebp
    adc     edx,edx
    mov     eax,[4+ecx]
    adc     esi,0
    add     eax,ebx
    mov     [4+ecx],eax
    mov     eax,[8+ecx]
    adc     eax,edi
    mov     ebx,[12+ecx]
    adc     ebx,ebp
    mov     edi,[16+ecx]
    mov     [8+ecx],eax
    mov     ebp,[SAVE_EBP]
    mov     [12+ecx],ebx
    mov     ebx,[SAVE_EBX]
    adc     edi,edx
    mov     eax,[20+ecx]
    mov     [16+ecx],edi
    mov     edi,[SAVE_EDI]
    adc     eax,esi			;  no carry out of this 
    mov     esi,[SAVE_ESI]
    mov     [20+ecx],eax
    add     esp,frame
    ret

;  eax src low limb 
;  ebx 
;  ecx 
;  edx size 
;  esi src 
;  edi 
;  ebp 

%define	VAR_COUNTER esp+frame-20 
%define VAR_JMP		esp+frame-24 
%define STACK_SPACE 24 
%define frame		4 

;  First multiply src[0]*src[1..size-1] and store at dst[1..size]. 

Lfour_or_more: 
    sub     esp,STACK_SPACE-frame
%define	frame   STACK_SPACE 
    mov     ecx,1
    mov     [SAVE_EDI],edi
    mov     edi,[PARAM_DST]
    mov     [SAVE_EBX],ebx
    sub     ecx,edx				;  -(size-1) 
    mov     [SAVE_EBP],ebp
    mov     ebx,0				;  initial carry 
    lea     esi,[esi+edx*4]		;  &src[size] 
    mov     ebp,eax				;  multiplier 
    lea     edi,[-4+edi+edx*4]	;  &dst[size-1] 

;  This loop runs at just over 6 c/l. 
;
;  eax scratch 
;  ebx carry 
;  ecx counter,limbs,negative,-(size-1) to -1 
;  edx scratch 
;  esi &src[size] 
;  edi &dst[size-1] 
;  ebp multiplier 

Lmul_1: 
    mov     eax,ebp
    mul     dword [esi+ecx*4]
    add     eax,ebx
    mov     ebx,0
    adc     ebx,edx
    mov     [4+edi+ecx*4],eax
    inc     ecx
    jnz     Lmul_1
    mov     [4+edi],ebx

;  Addmul src[n]*src[n+1..size-1] at dst[2*n-1...],for each n=1..size-2. 
; 
;  The last two addmuls,which are the bottom right corner of the product 
;  triangle,are left to the end.  These are src[size-3]*src[size-2,size-1] 
;  and src[size-2]*src[size-1].  If size is 4 then it's only these corner 
;  cases that need to be done. 
; 
;  The unrolled code is the same as mpn_addmul_1(),see that routine for some 
;  comments. 
; 
;  VAR_COUNTER is the outer loop,running from -(size-4) to -1,inclusive. 
; 
;  VAR_JMP is the computed jump into the unrolled code,stepped by one code 
;  chunk each outer loop. 
;
;   This is also hard-coded in the address calculation below. 
;
;   With &src[size] and &dst[size-1] pointers,the displacements in the 
;   unrolled code fit in a byte for UNROLL_COUNT values up to 32,but above 
;   that an offset must be added to them. 
;
;  eax 
;  ebx carry 
;  ecx 
;  edx 
;  esi &src[size] 
;  edi &dst[size-1] 
;  ebp 

%define	CODE_BYTES_PER_LIMB	15 

%if	UNROLL_COUNT > 32
%define	OFFSET	UNROLL_COUNT-32
%else
%define	OFFSET	0
%endif

    mov     ecx,[PARAM_SIZE]
    sub     ecx,4
    jz      Lcorner
    mov     edx,ecx
    neg     ecx
    shl     ecx,4
%if	OFFSET != 0
	sub  esi,OFFSET
%endif	
	
%ifdef	PIC
    call    Lhere
Lhere: 
    add     ecx,[esp]
    add     ecx,Lunroll_inner_end-Lhere-2*CODE_BYTES_PER_LIMB
    add     ecx,edx
	add		esp,4
%else
	lea     ecx,[Lunroll_inner_end-2*CODE_BYTES_PER_LIMB+ecx+edx]
%endif
	neg     edx

%if	OFFSET != 0
	sub  edi,OFFSET
%endif

;  The calculated jump mustn't be before the start of the available 
;  code.  This is the limit that UNROLL_COUNT puts on the src operand 
;  size,but checked here using the jump address directly. 

%ifdef	ASSERT
	mov		eax,Lunroll_inner_start
	cmp		ecx,eax
	jae		Lunroll_outer_top
	jmp		exit
%endif

;  eax 
;  ebx high limb to store 
;  ecx VAR_JMP 
;  edx VAR_COUNTER,limbs,negative 
;  esi &src[size],constant 
;  edi dst ptr,second highest limb of last addmul 
;  ebp 

	align   16
Lunroll_outer_top: 
    mov     ebp,[-12+OFFSET+esi+edx*4] ;  multiplier 
    mov     [VAR_COUNTER],edx
    mov     eax,[-8+OFFSET+esi+edx*4] ;  first limb of multiplicand 
    mul     ebp

%if	UNROLL_COUNT % 2 == 1
%define	cmovX	cmovz
%else
%define	cmovX	cmovnz
%endif

    test    cl,1
    mov     ebx,edx  ;  high carry 
    lea     edi,[4+edi]
    mov     edx,ecx  ;  jump 
    mov     ecx,eax  ;  low carry 
    lea     edx,[CODE_BYTES_PER_LIMB+edx]
	cmovX	ecx,ebx
	cmovX	ebx,eax
    mov     [VAR_JMP],edx
    jmp     edx

;  Must be on an even address here so the low bit of the jump address 
;  will indicate which way around ecx/ebx should start. 

;  eax scratch 
;  ebx carry high 
;  ecx carry low 
;  edx scratch 
;  esi src pointer 
;  edi dst pointer 
;  ebp multiplier 
;
;  15 code bytes each limb 
;  ecx/ebx reversed on each chunk 

	align   2
Lunroll_inner_start: 

%assign	i	UNROLL_COUNT
%rep	UNROLL_COUNT
%assign	disp	OFFSET-4*i
	%if	i % 2 == 0
	mov		eax,[byte disp+esi]
	mul		ebp
	add		[byte disp+edi],ebx
	adc		ecx,eax
	mov		ebx,edx
	adc		ebx,0
%else
	;  this one comes out last
	mov		eax,[byte disp+esi]
	mul		ebp
	add		[byte disp+edi],ecx
	adc		ebx,eax
	mov		ecx,edx
	adc		ecx,0
%endif
%assign	i	i-1		
%endrep

Lunroll_inner_end: 
    add     [OFFSET+edi],ebx
    mov     edx,[VAR_COUNTER]
    adc     ecx,0
    mov     [OFFSET+4+edi],ecx
    mov     ecx,[VAR_JMP]
    inc     edx
    jnz     Lunroll_outer_top
%if	OFFSET != 0
    add     esi,OFFSET
    add     edi,OFFSET
%endif

;  eax 
;  ebx 
;  ecx 
;  edx 
;  esi &src[size] 
;  edi &dst[2*size-5] 
;  ebp 

	align   16
Lcorner: 
    mov     eax,[-12+esi]
    mul     dword [-8+esi]
    add     [edi],eax
    mov     eax,[-12+esi]
    mov     ebx,0
    adc     ebx,edx
    mul     dword [-4+esi]
    add     ebx,eax
    mov     eax,[-8+esi]
    adc     edx,0
    add     [4+edi],ebx
    mov     ebx,0
    adc     ebx,edx
    mul     dword [-4+esi]
    mov     ecx,[PARAM_SIZE]
    add     eax,ebx
    adc     edx,0
    mov     [8+edi],eax
    mov     [12+edi],edx
    mov     edi,[PARAM_DST]

;  Left shift of dst[1..2*size-2],the bit shifted out becomes dst[2*size-1]. 

    sub     ecx,1			;  size-1 
    xor     eax,eax         ;  ready for final adcl,and clear carry 
    mov     edx,ecx
    mov     esi,[PARAM_SRC]

;  eax 
;  ebx 
;  ecx counter,size-1 to 1 
;  edx size-1 (for later use) 
;  esi src (for later use) 
;  edi dst,incrementing 
;  ebp 

Llshift: 
    rcl     dword [4+edi],1
    rcl     dword [8+edi],1
    lea     edi,[8+edi]
    dec     ecx
    jnz     Llshift
    adc     eax,eax
    mov     [4+edi],eax			;  dst most significant limb 
    mov     eax,[esi]			;  src[0] 
    lea     esi,[4+esi+edx*4]	;  &src[size] 
    sub     ecx,edx				;  -(size-1) 

;  Now add in the squares on the diagonal,src[0]^2,src[1]^2,...,
;  src[size-1]^2.  dst[0] hasn't yet been set at all yet,and just gets the 
;  low limb of src[0]^2. 

	mul     eax
    mov     [edi+ecx*8],eax   ;  dst[0] 

;  eax scratch 
;  ebx scratch 
;  ecx counter,negative 
;  edx carry 
;  esi &src[size] 
;  edi dst[2*size-2] 
;  ebp 

Ldiag: 
    mov     eax,[esi+ecx*4]
    mov     ebx,edx
    mul     eax
    add     [4+edi+ecx*8],ebx
    adc     [8+edi+ecx*8],eax
    adc     edx,0
    inc     ecx
    jnz     Ldiag
    mov     esi,[SAVE_ESI]
    mov     ebx,[SAVE_EBX]
    add     [4+edi],edx     ;  dst most significant limb 
    mov     edi,[SAVE_EDI]
    mov     ebp,[SAVE_EBP]
    add     esp,frame
    ret

	end
