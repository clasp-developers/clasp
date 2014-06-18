
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

%define	PARAM_SHIFT esp+frame+16 
%define PARAM_SIZE  esp+frame+12 
%define PARAM_SRC   esp+frame+8 
%define PARAM_DST   esp+frame+4 
%define	frame		8 

;   minimum 5,because the unrolled loop can't handle less 
%define       UNROLL_THRESHOLD  5 

	section .text

	global	___gmpn_lshift
%ifdef	DLL
	export	___gmpn_lshift
%endif

	align   8
___gmpn_lshift:
    push    ebx
    push    edi
    mov     eax,[PARAM_SIZE]
    mov     edx,[PARAM_DST]
    mov     ebx,[PARAM_SRC]
    mov     ecx,[PARAM_SHIFT]
	cmp     eax,UNROLL_THRESHOLD
    jae     Lunroll
    mov     edi,[-4+ebx+eax*4]	;  src high limb 
    dec     eax
    jnz     Lsimple
	shld	eax,edi,cl
    shl     edi,cl
    mov     [edx],edi			;  dst low limb 
    pop     edi					;  risk of data cache bank clash 
    pop     ebx
    ret

;  eax size-1 
;  ebx src 
;  ecx shift 
;  edx dst 
;  esi 
;  edi 
;  ebp 

Lsimple: 
    movd    mm5,[ebx+eax*4] ;  src high limb 
    movd    mm6,ecx         ;  lshift 
    neg     ecx
    psllq   mm5,mm6
    add     ecx,32
    movd    mm7,ecx
    psrlq   mm5,32          ;  retval 

;  eax counter,limbs,negative 
;  ebx src 
;  ecx 
;  edx dst 
;  esi 
;  edi 
; 
;  mm0 scratch 
;  mm5 return value 
;  mm6 shift 
;  mm7 32-shift 

Lsimple_top: 
    movq    mm0,[ebx+eax*4-4]
    dec     eax
    psrlq   mm0,mm7
    movd    [4+edx+eax*4],mm0
    jnz     Lsimple_top
    movd    mm0,[ebx]
    movd    eax,mm5
    psllq   mm0,mm6
    pop     edi
    pop     ebx
    movd    [edx],mm0
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
    movd    mm5,[ebx+eax*4-4]	;  src high limb 
    lea     edi,[ebx+eax*4]
    movd    mm6,ecx				;  lshift 
    and     edi,4
    psllq   mm5,mm6
    jz      Lstart_src_aligned

;  src isn't aligned,process high limb separately (marked xxx) to 
;  make it so. 
; 
;   source     -8(ebx,%eax,4) 
;                   | 
;   +-------+-------+-------+-- 
;   |               |           
;   +-------+-------+-------+-- 
;         0mod8   4mod8   0mod8 
; 
;   dest 
;      -4(edx,%eax,4) 
;           | 
;   +-------+-------+-- 
;   |  xxx  |       |   
;   +-------+-------+-- 

    movq    mm0,[ebx+eax*4-8]		;  unaligned load 
    psllq   mm0,mm6
    dec     eax
    psrlq   mm0,32
    movd    [edx+eax*4],mm0
Lstart_src_aligned: 
    movq    mm1,[ebx+eax*4-8]		;  src high qword 
    lea     edi,[edx+eax*4]
    and     edi,4
    psrlq   mm5,32					;  return value 
    movq    mm3,[ebx+eax*4-16]		;  src second highest qword 
    jz      Lstart_dst_aligned

;  dst isn't aligned,subtract 4 to make it so,and pretend the shift 
;  is 32 bits extra.  High limb of dst (marked xxx) handled here 
;  separately. 
; 
;   source     -8(ebx,%eax,4) 
;                   | 
;   +-------+-------+-- 
;   |      mm1      |   
;   +-------+-------+-- 
;                 0mod8   4mod8 
; 
;   dest 
;      -4(edx,%eax,4) 
;           | 
;   +-------+-------+-------+-- 
;   |  xxx  |               | 
;   +-------+-------+-------+-- 
;         0mod8   4mod8   0mod8 

    movq    mm0,mm1
    add     ecx,32         ;  new shift 
    psllq   mm0,mm6
    movd    mm6,ecx
    psrlq   mm0,32

;  wasted cycle here waiting for %mm0 

    movd    [-4+edx+eax*4],mm0
    sub     edx,4
Lstart_dst_aligned: 

    psllq   mm1,mm6
    neg     ecx				;  -shift 
    add     ecx,64			;  64-shift 
    movq    mm2,mm3
    movd    mm7,ecx
    sub     eax,8			;  size-8 
    psrlq   mm3,mm7
    por     mm3,mm1         ;  mm3 ready to store 
    jc      Lfinish

;  The comments in mpn_rshift apply here too. 

;  eax counter,limbs 
;  ebx src 
;  ecx 
;  edx dst 
;  esi 
;  edi 
; 
;  mm0 
;  mm1 
;  mm2 src qword from 16(%ebx,%eax,4) 
;  mm3 dst qword ready to store to 24(%edx,%eax,4) 
; 
;  mm5 return value 
;  mm6 lshift 
;  mm7 rshift 

	align   8
Lunroll_loop: 
    movq    mm0,[ebx+eax*4+8]
    psllq   mm2,mm6
    movq    mm1,mm0
    psrlq   mm0,mm7
    movq    [24+edx+eax*4],mm3
    por     mm0,mm2
    movq    mm3,[ebx+eax*4]
    psllq   mm1,mm6
    movq    [16+edx+eax*4],mm0
    movq    mm2,mm3 
	psrlq   mm3,mm7
    sub     eax,4
	por     mm3,mm1
    jnc     Lunroll_loop
Lfinish: 
;  eax -4 to -1 representing respectively 0 to 3 limbs remaining 

    test    al,2
    jz      Lfinish_no_two
    movq    mm0,[ebx+eax*4+8]
    psllq   mm2,mm6
    movq    mm1,mm0
    psrlq   mm0,mm7
    movq    [24+edx+eax*4],mm3  ;  prev 
    por     mm0,mm2
    movq    mm2,mm1
    movq    mm3,mm0
    sub     eax,2
Lfinish_no_two: 

;  eax -4 or -3 representing respectively 0 or 1 limbs remaining 
;  mm2 src prev qword,from 16(%ebx,%eax,4) 
;  mm3 dst qword,for 24(%edx,%eax,4) 

    test    al,1
    movd    eax,mm5  ;  retval 
    pop     edi
    jz      Lfinish_zero

;  One extra src limb,destination was aligned. 
;
;                  source                  ebx 
;                  --+---------------+-------+ 
;                    |      mm2      |       | 
;                  --+---------------+-------+ 
;
;  dest         edx+12           edx+4     edx 
;  --+---------------+---------------+-------+ 
;    |      mm3      |               |       | 
;  --+---------------+---------------+-------+ 
;
;  mm6 = shift 
;  mm7 = ecx = 64-shift 

;  One extra src limb,destination was unaligned. 
;
;                  source                  ebx 
;                  --+---------------+-------+ 
;                    |      mm2      |       | 
;                  --+---------------+-------+ 
;
;          dest         edx+12           edx+4 
;          --+---------------+---------------+ 
;            |      mm3      |               | 
;          --+---------------+---------------+ 
;
;  mm6 = shift+32 
;  mm7 = ecx = 64-(shift+32) 


;  In both cases there's one extra limb of src to fetch and combine 
;  with mm2 to make a qword at 4(%edx),and in the aligned case 
;  there's an extra limb of dst to be formed from that extra src limb 
;  left shifted. 

    movd    mm0,[ebx]
    psllq   mm2,mm6
    movq    [12+edx],mm3
    psllq   mm0,32
    movq    mm1,mm0
    psrlq   mm0,mm7
    por     mm0,mm2
    psllq   mm1,mm6
    movq    [4+edx],mm0
    psrlq   mm1,32
    and     ecx,32
    pop     ebx
    jz      Lfinish_one_unaligned
    movd    [edx],mm1
Lfinish_one_unaligned: 
    emms
    ret
Lfinish_zero: 

;  No extra src limbs,destination was aligned. 
;
;                  source          ebx 
;                  --+---------------+ 
;                    |      mm2      | 
;                  --+---------------+ 
;
;  dest          edx+8             edx 
;  --+---------------+---------------+ 
;    |      mm3      |               | 
;  --+---------------+---------------+ 
;
;  mm6 = shift 
;  mm7 = ecx = 64-shift 

;  No extra src limbs,destination was unaligned. 
;
;                source            ebx 
;                  --+---------------+ 
;                    |      mm2      | 
;                  --+---------------+ 
;
;          dest          edx+8   edx+4 
;          --+---------------+-------+ 
;            |      mm3      |       | 
;          --+---------------+-------+ 
;
;  mm6 = shift+32 
;  mm7 = ecx = 64-(shift+32) 

;  The movd for the unaligned case writes the same data to 4(%edx) 
;  that the movq does for the aligned case. 

    movq    [8+edx],mm3
    and     ecx,32
    psllq   mm2,mm6
    jz      Lfinish_zero_unaligned
    movq    [edx],mm2
Lfinish_zero_unaligned: 
    psrlq   mm2,32
    pop     ebx
    movd    eax,mm5  ;  retval 
    movd    [4+edx],mm2
    emms
    ret

	end
