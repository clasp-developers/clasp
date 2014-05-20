	.machine	"ppc"
	.toc
	.csect .text[PR]

	.align 2
	.globl opal_atomic_mb
	.globl .opal_atomic_mb
	.csect  [DS],3
opal_atomic_mb:
	.long  .opal_atomic_mb, TOC[tc0], 0
	.csect  [PR]
	.align  2
.opal_atomic_mb:
	sync
	blr


	.globl opal_atomic_rmb
	.globl .opal_atomic_rmb
	.csect  [DS],3
opal_atomic_rmb:
	.long  .opal_atomic_rmb, TOC[tc0], 0
	.csect  [PR]
	.align  2
.opal_atomic_rmb:
	lwsync
	blr


	.globl opal_atomic_wmb
	.globl .opal_atomic_wmb
	.csect  [DS],3
opal_atomic_wmb:
	.long  .opal_atomic_wmb, TOC[tc0], 0
	.csect  [PR]
	.align  2
.opal_atomic_wmb:
	eieio
	blr


	.globl opal_atomic_cmpset_32
	.globl .opal_atomic_cmpset_32
	.csect  [DS],3
opal_atomic_cmpset_32:
	.long  .opal_atomic_cmpset_32, TOC[tc0], 0
	.csect  [PR]
	.align  2
.opal_atomic_cmpset_32:
	L1: lwarx   0, 0, 3  
	   cmpw    0, 0, 4  
	   bne-    L2
	   stwcx.  5, 0, 3  
	   bne-    L1
	L2:
	xor 3,0,4
	subfic 5,3,0
	adde 3,5,3
	blr


	.globl opal_atomic_cmpset_acq_32
	.globl .opal_atomic_cmpset_acq_32
	.csect  [DS],3
opal_atomic_cmpset_acq_32:
	.long  .opal_atomic_cmpset_acq_32, TOC[tc0], 0
	.csect  [PR]
	.align  2
.opal_atomic_cmpset_acq_32:
	L3: lwarx   0, 0, 3  
	   cmpw    0, 0, 4  
	   bne-    L4         
	   stwcx.  5, 0, 3  
	   bne-    L3
	sync 
	L4:
	xor 3,0,4
	subfic 5,3,0
	adde 3,5,3
	lwsync
	blr


	.globl opal_atomic_cmpset_rel_32
	.globl .opal_atomic_cmpset_rel_32
	.csect  [DS],3
opal_atomic_cmpset_rel_32:
	.long  .opal_atomic_cmpset_rel_32, TOC[tc0], 0
	.csect  [PR]
	.align  2
.opal_atomic_cmpset_rel_32:
	eieio
	L5: lwarx   0, 0, 3  
	   cmpw    0, 0, 4  
	   bne-    L6
	   stwcx.  5, 0, 3  
	   bne-    L5
	sync 
	L6:
	xor 3,0,4
	subfic 5,3,0
	adde 3,5,3
	blr



	.globl opal_atomic_add_32
	.globl .opal_atomic_add_32
	.csect  [DS],3
opal_atomic_add_32:
	.long  .opal_atomic_add_32, TOC[tc0], 0
	.csect  [PR]
	.align  2
.opal_atomic_add_32:
	L13:   lwarx 0, 0, 3 
	     add  0, 4, 0                
	     stwcx.   0, 0, 3              
	     bne-  L13
	mr	3,0
	blr


	.globl opal_atomic_sub_32
	.globl .opal_atomic_sub_32
	.csect  [DS],3
opal_atomic_sub_32:
	.long  .opal_atomic_sub_32, TOC[tc0], 0
	.csect  [PR]
	.align  2
.opal_atomic_sub_32:
	L14:   lwarx 0,0,3
	     subf  0,4,0                
	     stwcx.   0,0,3              
	     bne-  L14             
	mr	3,0
	blr

	.globl opal_sys_timer_get_cycles
	.globl .opal_sys_timer_get_cycles
	.csect  [DS],3
opal_sys_timer_get_cycles:
	.long  .opal_sys_timer_get_cycles, TOC[tc0], 0
	.csect  [PR]
	.align  2
.opal_sys_timer_get_cycles:
	L15:
	mftbu 0
	mftb 11
	mftbu 2
	cmpw 7,2,0
	bne+ 7,L15
	li 4,0
	li 9,0
	or 3,2,9
	or 4,4,11
	blr
