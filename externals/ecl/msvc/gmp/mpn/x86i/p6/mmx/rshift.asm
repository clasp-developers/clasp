
;  Copyright 2001 Free Software Foundation, Inc.
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
	
	global  ___gmpn_rshift 

%ifdef	DLL
	export	___gmpn_rshift
%endif

%define	PARAM_SHIFT esp+frame+16 
%define PARAM_SIZE  esp+frame+12 
%define PARAM_SRC   esp+frame+8 
%define PARAM_DST   esp+frame+4 
%define frame		8 

;   Minimum 5,because the unrolled loop can't handle less. 
%define	UNROLL_THRESHOLD  5 

	section .text
	align   8

___gmpn_rshift: 
    push    ebx
    push    edi
    mov     eax,[PARAM_SIZE]
    mov     edx,[PARAM_DST]
    mov     ebx,[PARAM_SRC]
    mov     ecx,[PARAM_SHIFT]
	cmp     eax,UNROLL_THRESHOLD
	jae     Lunroll
    dec     eax
    mov     edi,[ebx]		;  src low limb 
    jnz     Lsimple
	shrd	eax,edi,cl
    shr     edi,cl
    mov     [edx],edi       ;  dst low limb 
    pop     edi             ;  risk of data cache bank clash 
    pop     ebx
    ret

;  eax size-1 
;  ebx src 
;  ecx shift 
;  edx dst 
;  esi 
;  edi 
;  ebp 

	align   8
Lsimple: 
    movd    mm5,[ebx]       ;  src[0] 
    lea     ebx,[ebx+eax*4]  ;  &src[size-1] 
    movd    mm6,ecx         ;  rshift 
    lea     edx,[-4+edx+eax*4] ;  &dst[size-2] 
    psllq   mm5,32
    neg     eax

;  This loop is 5 or 8 cycles,with every second load unaligned and a wasted 
;  cycle waiting for the mm0 result to be ready.  For comparison a shrdl is 4 
;  cycles and would be 8 in a simple loop.  Using mmx helps the return value 
;  and last limb calculations too. 

;  eax counter,limbs,negative 
;  ebx &src[size-1] 
;  ecx return value 
;  edx &dst[size-2] 
;
;  mm0 scratch 
;  mm5 return value 
;  mm6 shift 

Lsimple_top: 
    movq    mm0,[ebx+eax*4]
    inc     eax
    psrlq   mm0,mm6
    movd    [edx+eax*4],mm0
    jnz     Lsimple_top
    movd    mm0,[ebx]
    psrlq   mm5,mm6         ;  return value 
    psrlq   mm0,mm6
    pop     edi
    movd    eax,mm5
    pop     ebx
    movd    [4+edx],mm0
    emms
    ret

;  eax size 
;  ebx src 
;  ecx shift 
;  edx dst 
;  esi 
;  edi 
;  ebp 

	align   8
Lunroll: 
    movd    mm5,[ebx]       ;  src[0] 
    mov     edi,4
    movd    mm6,ecx         ;  rshift 
    test    ebx,edi
    psllq   mm5,32
    jz      Lstart_src_aligned

;  src isn't aligned,process low limb separately (marked xxx) and 
;  step src and dst by one limb,making src aligned. 
;
;  source                  ebx 
;  --+-------+-------+-------+ 
;            |          xxx  | 
;  --+-------+-------+-------+ 
;          4mod8   0mod8   4mod8 
;
;          dest            edx 
;          --+-------+-------+ 
;            |       |  xxx  |   
;          --+-------+-------+ 

    movq    mm0,[ebx]       ;  unaligned load 
    psrlq   mm0,mm6
    add     ebx,4
    dec     eax
    movd    [edx],mm0
    add     edx,4
Lstart_src_aligned: 
    movq    mm1,[ebx]
    test    edx,edi
    psrlq   mm5,mm6         ;  retval 
    jz      Lstart_dst_aligned

;  dst isn't aligned,add 4 to make it so,and pretend the shift is 
;  32 bits extra.  Low limb of dst (marked xxx) handled here 
;  separately. 
;
;           source          ebx 
;           --+-------+-------+ 
;             |      mm1      | 
;           --+-------+-------+ 
;                   4mod8   0mod8 
;
;   dest                    edx 
;   --+-------+-------+-------+ 
;                     |  xxx  |         
;   --+-------+-------+-------+ 
;           4mod8   0mod8   4mod8 

    movq    mm0,mm1
    add     ecx,32         ;  new shift 
    psrlq   mm0,mm6
    movd    mm6,ecx
    movd    [edx],mm0
    add     edx,4
Lstart_dst_aligned: 
    movq    mm3,[8+ebx]
    neg     ecx
    movq    mm2,mm3			;  mm2 src qword 
    add     ecx,64
    movd    mm7,ecx
    psrlq   mm1,mm6
    lea     ebx,[-12+ebx+eax*4]
    lea     edx,[-20+edx+eax*4]
    psllq   mm3,mm7
    sub     eax,7			;  size-7 
    por     mm3,mm1         ;  mm3 ready to store 
    neg     eax             ;  -(size-7) 
    jns     Lfinish

;  This loop is the important bit,the rest is just support.  Careful 
;  instruction scheduling achieves the claimed 1.75 c/l.  The 
;  relevant parts of the pairing rules are: 
;
;  - mmx loads and stores execute only in the U pipe 
;  - only one mmx shift in a pair 
;  - wait one cycle before storing an mmx register result 
;  - the usual address generation interlock 
;
;  Two qword calculations are slightly interleaved.  The instructions 
;  marked "C" belong to the second qword,and the "C prev" one is for 
;  the second qword from the previous iteration. 

;  eax counter,limbs,negative 
;  ebx &src[size-12] 
;  ecx 
;  edx &dst[size-12] 
;  esi 
;  edi 
;
;  mm0 
;  mm1 
;  mm2 src qword from -8(%ebx,%eax,4) 
;  mm3 dst qword ready to store to -8(%edx,%eax,4) 
;
;  mm5 return value 
;  mm6 rshift 
;  mm7 lshift 

	align   8
Lunroll_loop: 
    movq    mm0,[ebx+eax*4]
    psrlq   mm2,mm6
    movq    mm1,mm0
    psllq   mm0,mm7
    movq    [-8+edx+eax*4],mm3
    por     mm0,mm2

	movq    mm3,[ebx+eax*4+8]
	psrlq   mm1,mm6
    movq    [edx+eax*4],mm0
	movq    mm2,mm3
	psllq   mm3,mm7
    add     eax,4
	por     mm3,mm1
    js      Lunroll_loop

Lfinish: 
;  eax 0 to 3 representing respectively 3 to 0 limbs remaining 

    test    al,2
    jnz     Lfinish_no_two
    movq    mm0,[ebx+eax*4]
    psrlq   mm2,mm6
    movq    mm1,mm0
    psllq   mm0,mm7
    movq    [-8+edx+eax*4],mm3  ;  prev 
    por     mm0,mm2
    movq    mm2,mm1
    movq    mm3,mm0
    add     eax,2
Lfinish_no_two: 

;  eax 2 or 3 representing respectively 1 or 0 limbs remaining 
;
;  mm2 src prev qword,from -8(%ebx,%eax,4) 
;  mm3 dst qword,for -8(%edx,%eax,4) 

    test    al,1
    pop     edi
    movd    eax,mm5  ;  retval 
    jnz     Lfinish_zero

;  One extra limb,destination was aligned. 
;
;  source                ebx 
;  +-------+---------------+-- 
;  |       |      mm2      | 
;  +-------+---------------+-- 
;
;  dest                                  edx 
;  +-------+---------------+---------------+-- 
;  |       |               |      mm3      | 
;  +-------+---------------+---------------+-- 
;
;  mm6 = shift 
;  mm7 = ecx = 64-shift 

;  One extra limb,destination was unaligned. 
;
;  source                ebx 
;  +-------+---------------+-- 
;  |       |      mm2      | 
;  +-------+---------------+-- 
;
;  dest                          edx 
;  +---------------+---------------+-- 
;  |               |      mm3      | 
;  +---------------+---------------+-- 
;
;  mm6 = shift+32 
;  mm7 = ecx = 64-(shift+32) 

;  In both cases there's one extra limb of src to fetch and combine 
;  with mm2 to make a qword at 8(%edx),and in the aligned case 
;  there's a further extra limb of dst to be formed. 


    movd    mm0,[8+ebx]
    psrlq   mm2,mm6
    movq    mm1,mm0
    psllq   mm0,mm7
    movq    [edx],mm3
    por     mm0,mm2
    psrlq   mm1,mm6
    and     ecx,32
    pop     ebx
    jz      Lfinish_one_unaligned

    ;  dst was aligned,must store one extra limb 
    movd    [16+edx],mm1
Lfinish_one_unaligned: 

    movq    [8+edx],mm0
    emms
    ret
Lfinish_zero: 

;  No extra limbs,destination was aligned. 
;
;  source        ebx 
;  +---------------+-- 
;  |      mm2      | 
;  +---------------+-- 
;
;  dest                        edx+4 
;  +---------------+---------------+-- 
;  |               |      mm3      | 
;  +---------------+---------------+-- 
;
;  mm6 = shift 
;  mm7 = ecx = 64-shift 

;  No extra limbs,destination was unaligned. 
;
;  source        ebx 
;  +---------------+-- 
;  |      mm2      | 
;  +---------------+-- 
;
;  dest                edx+4 
;  +-------+---------------+-- 
;  |       |      mm3      | 
;  +-------+---------------+-- 
;
;  mm6 = shift+32 
;  mm7 = 64-(shift+32) 

;  The movd for the unaligned case is clearly the same data as the 
;  movq for the aligned case,it's just a choice between whether one 
;  or two limbs should be written. 

    movq    [4+edx],mm3
    psrlq   mm2,mm6
    movd    [12+edx],mm2
    and     ecx,32
    pop     ebx
    jz      Lfinish_zero_unaligned
    movq    [12+edx],mm2
Lfinish_zero_unaligned: 
    emms
    ret

	end
