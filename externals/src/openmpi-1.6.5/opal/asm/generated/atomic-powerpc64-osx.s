	.text

	.align 2
	.globl _opal_atomic_mb
_opal_atomic_mb:
	sync
	blr


	.globl _opal_atomic_rmb
_opal_atomic_rmb:
	lwsync
	blr


	.globl _opal_atomic_wmb
_opal_atomic_wmb:
	eieio
	blr


	.globl _opal_atomic_cmpset_32
_opal_atomic_cmpset_32:
	L1: lwarx   r0, 0, r3  
	   cmpw    0, r0, r4  
	   bne-    L2         
	   stwcx.  r5, 0, r3  
	   bne-    L1
	L2:
	cmpw cr7,r0,r4
	mfcr r3
	rlwinm r3,r3,31,1
	blr
	

	.globl _opal_atomic_cmpset_acq_32
_opal_atomic_cmpset_acq_32:
	mflr r0
	std r29,-24(r1)
	std r0,16(r1)
	stdu r1,-144(r1)
	bl _opal_atomic_cmpset_32
	mr r29,r3
	bl _opal_atomic_rmb
	mr r3,r29
	addi r1,r1,144
	ld r0,16(r1)
	mtlr r0
	ld r29,-24(r1)
	blr


	.globl _opal_atomic_cmpset_rel_32
_opal_atomic_cmpset_rel_32:
	mflr r0
	std r27,-40(r1)
	std r28,-32(r1)
	std r29,-24(r1)
	std r0,16(r1)
	stdu r1,-160(r1)
	mr r29,r3
	mr r28,r4
	mr r27,r5
	bl _opal_atomic_wmb
	mr r3,r29
	mr r4,r28
	mr r5,r27
	bl _opal_atomic_cmpset_32
	addi r1,r1,160
	ld r0,16(r1)
	mtlr r0
	ld r27,-40(r1)
	ld r28,-32(r1)
	ld r29,-24(r1)
	blr


	.globl _opal_atomic_cmpset_64
_opal_atomic_cmpset_64:
	L3: ldarx   r0, 0, r3  
	   cmpd    0, r0, r4  
	   bne-    L4
	   stdcx.  r5, 0, r3  
	   bne-    L3
	L4:
	xor r3,r4,r0
	subfic r5,r3,0
	adde r3,r5,r3
	blr


	.globl _opal_atomic_cmpset_acq_64
_opal_atomic_cmpset_acq_64:
        L7: ldarx   r0, 0, r3
           cmpd    0, r0, r4
           bne-    L8
           stdcx.  r5, 0, r3
           bne-    L7
        L8:
        lwsync
        xor r3,r4,r0
        subfic r5,r3,0
        adde r3,r5,r3
        blr


	.globl _opal_atomic_cmpset_rel_64
_opal_atomic_cmpset_rel_64:
        eieio
        L9: ldarx   r0, 0, r3
           cmpd    0, r0, r4
           bne-    L10
           stdcx.  r5, 0, r3
           bne-    L9
        L10:
        xor r3,r4,r0
        subfic r5,r3,0
        adde r3,r5,r3
        blr


	.globl _opal_atomic_add_32
_opal_atomic_add_32:
	L5: lwarx r0, 0, r3 
	     add  r0, r4, r0                
	     stwcx.   r0, 0, r3              
	     bne-  L5
	
	mr r3,r0
	blr


	.globl _opal_atomic_sub_32
_opal_atomic_sub_32:
	L6:   lwarx r0,0,r3
	     subf  r0,r4,r0                
	     stwcx.   r0,0,r3              
	     bne-  L6
	
	mr r3,r0
	blr

	.globl _opal_sys_timer_get_cycles
_opal_sys_timer_get_cycles:
	L11:
        mftbu r2
        rldicl r2,r2,0,32
        mftb r0
        rldicl r9,r0,0,32
        mftbu r0
        rldicl r0,r0,0,32
        cmpw cr7,r0,r2
        bne cr7,L11
        sldi r3,r0,32
        or r3,r3,r9
        blr
