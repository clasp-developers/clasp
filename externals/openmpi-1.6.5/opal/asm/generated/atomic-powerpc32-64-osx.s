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
	xor r3,r0,r4
	subfic r5,r3,0
	adde r3,r5,r3
	blr


	.globl _opal_atomic_cmpset_acq_32
_opal_atomic_cmpset_acq_32:
	L3: lwarx   r0, 0, r3  
	   cmpw    0, r0, r4  
	   bne-    L4         
	   stwcx.  r5, 0, r3  
	   bne-    L3
	sync 
	L4:
	xor r3,r0,r4
	subfic r5,r3,0
	adde r3,r5,r3
	lwsync
	blr


	.globl _opal_atomic_cmpset_rel_32
_opal_atomic_cmpset_rel_32:
	eieio
	L5: lwarx   r0, 0, r3  
	   cmpw    0, r0, r4  
	   bne-    L6
	   stwcx.  r5, 0, r3  
	   bne-    L5
	sync 
	L6:
	xor r3,r0,r4
	subfic r5,r3,0
	adde r3,r5,r3
	blr

	.globl _opal_atomic_cmpset_64
_opal_atomic_cmpset_64:
	stw r4,-32(r1)
	stw r5,-28(r1)
	stw r6,-24(r1)
	stw r7,-20(r1)
	ld r5,-32(r1)
	ld r7,-24(r1)
	L7: ldarx   r9, 0, r3  
	   cmpd    0, r9, r5  
	   bne-    L8         
	   stdcx.  r7, 0, r3
	   bne-    L7
	L8:
	xor r3,r5,r9
	subfic r5,r3,0
	adde r3,r5,r3
	blr


	.globl _opal_atomic_cmpset_acq_64
_opal_atomic_cmpset_acq_64:
        stw r4,-32(r1)
        stw r5,-28(r1)
        stw r6,-24(r1)
        stw r7,-20(r1)
        ld r5,-32(r1)
        ld r7,-24(r1)

        L9: ldarx   r9, 0, r3  
           cmpd    0, r9, r5
           bne-    L10         
           stdcx.  r7, 0, r3  
           bne-    L9
        L10:
        xor r3,r5,r9
        subfic r5,r3,0
        adde r3,r5,r3
        blr
        lwsync
        blr


	.globl _opal_atomic_cmpset_rel_64
_opal_atomic_cmpset_rel_64:
        stw r4,-32(r1)
        stw r5,-28(r1)
        stw r6,-24(r1)
        stw r7,-20(r1)
        ld r5,-32(r1)
        ld r7,-24(r1)

        eieio
        L11: ldarx   r9, 0, r3  
           cmpd    0, r9, r5  
           bne-    L12         
           stdcx.  r7, 0, r3  
           bne-    L11
        L12:
        xor r3,r5,r9
        subfic r5,r3,0
        adde r3,r5,r3
        blr
        lwsync
        blr


	.globl _opal_atomic_add_32
_opal_atomic_add_32:
	L13:   lwarx r0, 0, r3 
	     add  r0, r4, r0                
	     stwcx.   r0, 0, r3              
	     bne-  L13
	mr	r3,r0
	blr


	.globl _opal_atomic_sub_32
_opal_atomic_sub_32:
	L14:   lwarx r0,0,r3
	     subf  r0,r4,r0                
	     stwcx.   r0,0,r3              
	     bne-  L14             
	mr	r3,r0
	blr

	.globl _opal_sys_timer_get_cycles
_opal_sys_timer_get_cycles:
	L15:
	mftbu r0
	mftb r11
	mftbu r2
	cmpw cr7,r2,r0
	bne+ cr7,L15
	li r4,0
	li r9,0
	or r3,r2,r9
	or r4,r4,r11
	blr
