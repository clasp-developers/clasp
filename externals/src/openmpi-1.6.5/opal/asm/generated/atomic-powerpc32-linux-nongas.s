	.text

	.align 2
	.globl opal_atomic_mb
	.type opal_atomic_mb, @function
opal_atomic_mb:
	sync
	blr
	.size opal_atomic_mb, .-opal_atomic_mb


	.globl opal_atomic_rmb
	.type opal_atomic_rmb, @function
opal_atomic_rmb:
	lwsync
	blr
	.size opal_atomic_rmb, .-opal_atomic_rmb


	.globl opal_atomic_wmb
	.type opal_atomic_wmb, @function
opal_atomic_wmb:
	eieio
	blr
	.size opal_atomic_wmb, .-opal_atomic_wmb


	.globl opal_atomic_cmpset_32
	.type opal_atomic_cmpset_32, @function
opal_atomic_cmpset_32:
	.L1: lwarx   0, 0, 3  
	   cmpw    0, 0, 4  
	   bne-    .L2
	   stwcx.  5, 0, 3  
	   bne-    .L1
	.L2:
	xor 3,0,4
	subfic 5,3,0
	adde 3,5,3
	blr
	.size opal_atomic_cmpset_32, .-opal_atomic_cmpset_32


	.globl opal_atomic_cmpset_acq_32
	.type opal_atomic_cmpset_acq_32, @function
opal_atomic_cmpset_acq_32:
	.L3: lwarx   0, 0, 3  
	   cmpw    0, 0, 4  
	   bne-    .L4         
	   stwcx.  5, 0, 3  
	   bne-    .L3
	sync 
	.L4:
	xor 3,0,4
	subfic 5,3,0
	adde 3,5,3
	lwsync
	blr
	.size opal_atomic_cmpset_acq_32, .-opal_atomic_cmpset_acq_32


	.globl opal_atomic_cmpset_rel_32
	.type opal_atomic_cmpset_rel_32, @function
opal_atomic_cmpset_rel_32:
	eieio
	.L5: lwarx   0, 0, 3  
	   cmpw    0, 0, 4  
	   bne-    .L6
	   stwcx.  5, 0, 3  
	   bne-    .L5
	sync 
	.L6:
	xor 3,0,4
	subfic 5,3,0
	adde 3,5,3
	blr
	.size opal_atomic_cmpset_rel_32, .-opal_atomic_cmpset_rel_32



	.globl opal_atomic_add_32
	.type opal_atomic_add_32, @function
opal_atomic_add_32:
	.L13:   lwarx 0, 0, 3 
	     add  0, 4, 0                
	     stwcx.   0, 0, 3              
	     bne-  .L13
	mr	3,0
	blr
	.size opal_atomic_add_32, .-opal_atomic_add_32


	.globl opal_atomic_sub_32
	.type opal_atomic_sub_32, @function
opal_atomic_sub_32:
	.L14:   lwarx 0,0,3
	     subf  0,4,0                
	     stwcx.   0,0,3              
	     bne-  .L14             
	mr	3,0
	blr
	.size opal_atomic_sub_32, .-opal_atomic_sub_32

	.globl opal_sys_timer_get_cycles
	.type opal_sys_timer_get_cycles, @function
opal_sys_timer_get_cycles:
	.L15:
	mftbu 0
	mftb 11
	mftbu 2
	cmpw 7,2,0
	bne+ 7,.L15
	li 4,0
	li 9,0
	or 3,2,9
	or 4,4,11
	blr
	.size opal_sys_timer_get_cycles, .-opal_sys_timer_get_cycles
