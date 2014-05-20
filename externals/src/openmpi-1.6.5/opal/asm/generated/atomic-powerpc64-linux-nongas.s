	.text

	.align 2
	.globl .opal_atomic_mb
	.type .opal_atomic_mb, @function
.opal_atomic_mb:
	sync
	blr
	.size .opal_atomic_mb, .-.opal_atomic_mb


	.globl .opal_atomic_rmb
	.type .opal_atomic_rmb, @function
.opal_atomic_rmb:
	lwsync
	blr
	.size .opal_atomic_rmb, .-.opal_atomic_rmb


	.globl .opal_atomic_wmb
	.type .opal_atomic_wmb, @function
.opal_atomic_wmb:
	eieio
	blr
	.size .opal_atomic_wmb, .-.opal_atomic_wmb


	.globl .opal_atomic_cmpset_32
	.type .opal_atomic_cmpset_32, @function
.opal_atomic_cmpset_32:
	.L1: lwarx   0, 0, 3  
	   cmpw    0, 0, 4  
	   bne-    .L2         
	   stwcx.  5, 0, 3  
	   bne-    .L1
	.L2:
	cmpw 7,0,4
	mfcr 3
	rlwinm 3,3,31,1
	blr
	.size .opal_atomic_cmpset_32, .-.opal_atomic_cmpset_32
	

	.globl .opal_atomic_cmpset_acq_32
	.type .opal_atomic_cmpset_acq_32, @function
.opal_atomic_cmpset_acq_32:
	mflr 0
	std 29,-24(1)
	std 0,16(1)
	stdu 1,-144(1)
	bl .opal_atomic_cmpset_32
	mr 29,3
	bl .opal_atomic_rmb
	mr 3,29
	addi 1,1,144
	ld 0,16(1)
	mtlr 0
	ld 29,-24(1)
	blr
	.size .opal_atomic_cmpset_acq_32, .-.opal_atomic_cmpset_acq_32


	.globl .opal_atomic_cmpset_rel_32
	.type .opal_atomic_cmpset_rel_32, @function
.opal_atomic_cmpset_rel_32:
	mflr 0
	std 27,-40(1)
	std 28,-32(1)
	std 29,-24(1)
	std 0,16(1)
	stdu 1,-160(1)
	mr 29,3
	mr 28,4
	mr 27,5
	bl .opal_atomic_wmb
	mr 3,29
	mr 4,28
	mr 5,27
	bl .opal_atomic_cmpset_32
	addi 1,1,160
	ld 0,16(1)
	mtlr 0
	ld 27,-40(1)
	ld 28,-32(1)
	ld 29,-24(1)
	blr
	.size .opal_atomic_cmpset_rel_32, .-.opal_atomic_cmpset_rel_32


	.globl .opal_atomic_cmpset_64
	.type .opal_atomic_cmpset_64, @function
.opal_atomic_cmpset_64:
	.L3: ldarx   0, 0, 3  
	   cmpd    0, 0, 4  
	   bne-    .L4
	   stdcx.  5, 0, 3  
	   bne-    .L3
	.L4:
	xor 3,4,0
	subfic 5,3,0
	adde 3,5,3
	blr
	.size .opal_atomic_cmpset_64, .-.opal_atomic_cmpset_64


	.globl .opal_atomic_cmpset_acq_64
	.type .opal_atomic_cmpset_acq_64, @function
.opal_atomic_cmpset_acq_64:
        .L7: ldarx   0, 0, 3
           cmpd    0, 0, 4
           bne-    .L8
           stdcx.  5, 0, 3
           bne-    .L7
        .L8:
        lwsync
        xor 3,4,0
        subfic 5,3,0
        adde 3,5,3
        blr
	.size .opal_atomic_cmpset_acq_64, .-.opal_atomic_cmpset_acq_64


	.globl .opal_atomic_cmpset_rel_64
	.type .opal_atomic_cmpset_rel_64, @function
.opal_atomic_cmpset_rel_64:
        eieio
        .L9: ldarx   0, 0, 3
           cmpd    0, 0, 4
           bne-    .L10
           stdcx.  5, 0, 3
           bne-    .L9
        .L10:
        xor 3,4,0
        subfic 5,3,0
        adde 3,5,3
        blr
	.size .opal_atomic_cmpset_rel_64, .-.opal_atomic_cmpset_rel_64


	.globl .opal_atomic_add_32
	.type .opal_atomic_add_32, @function
.opal_atomic_add_32:
	.L5: lwarx 0, 0, 3 
	     add  0, 4, 0                
	     stwcx.   0, 0, 3              
	     bne-  .L5
	
	mr 3,0
	blr
	.size .opal_atomic_add_32, .-.opal_atomic_add_32


	.globl .opal_atomic_sub_32
	.type .opal_atomic_sub_32, @function
.opal_atomic_sub_32:
	.L6:   lwarx 0,0,3
	     subf  0,4,0                
	     stwcx.   0,0,3              
	     bne-  .L6
	
	mr 3,0
	blr
	.size .opal_atomic_sub_32, .-.opal_atomic_sub_32

	.globl .opal_sys_timer_get_cycles
	.type .opal_sys_timer_get_cycles, @function
.opal_sys_timer_get_cycles:
	.L11:
        mftbu 2
        rldicl 2,2,0,32
        mftb 0
        rldicl 9,0,0,32
        mftbu 0
        rldicl 0,0,0,32
        cmpw 7,0,2
        bne 7,.L11
        sldi 3,0,32
        or 3,3,9
        blr
	.size .opal_sys_timer_get_cycles, .-.opal_sys_timer_get_cycles
