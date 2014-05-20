START_FILE
	TEXT

	ALIGN(4)
START_FUNC(opal_atomic_mb)
	sync
	blr
END_FUNC(opal_atomic_mb)


START_FUNC(opal_atomic_rmb)
	lwsync
	blr
END_FUNC(opal_atomic_rmb)


START_FUNC(opal_atomic_wmb)
	eieio
	blr
END_FUNC(opal_atomic_wmb)


START_FUNC(opal_atomic_cmpset_32)
	LSYM(1) lwarx   r0, 0, r3  
	   cmpw    0, r0, r4  
	   bne-    REFLSYM(2)
	   stwcx.  r5, 0, r3  
	   bne-    REFLSYM(1)
	LSYM(2)
	xor r3,r0,r4
	subfic r5,r3,0
	adde r3,r5,r3
	blr
END_FUNC(opal_atomic_cmpset_32)


START_FUNC(opal_atomic_cmpset_acq_32)
	LSYM(3) lwarx   r0, 0, r3  
	   cmpw    0, r0, r4  
	   bne-    REFLSYM(4)         
	   stwcx.  r5, 0, r3  
	   bne-    REFLSYM(3)
	sync 
	LSYM(4)
	xor r3,r0,r4
	subfic r5,r3,0
	adde r3,r5,r3
	lwsync
	blr
END_FUNC(opal_atomic_cmpset_acq_32)


START_FUNC(opal_atomic_cmpset_rel_32)
	eieio
	LSYM(5) lwarx   r0, 0, r3  
	   cmpw    0, r0, r4  
	   bne-    REFLSYM(6)
	   stwcx.  r5, 0, r3  
	   bne-    REFLSYM(5)
	sync 
	LSYM(6)
	xor r3,r0,r4
	subfic r5,r3,0
	adde r3,r5,r3
	blr
END_FUNC(opal_atomic_cmpset_rel_32)

#START_64BIT
START_FUNC(opal_atomic_cmpset_64)
	stw r4,-32(r1)
	stw r5,-28(r1)
	stw r6,-24(r1)
	stw r7,-20(r1)
	ld r5,-32(r1)
	ld r7,-24(r1)
	LSYM(7) ldarx   r9, 0, r3  
	   cmpd    0, r9, r5  
	   bne-    REFLSYM(8)         
	   stdcx.  r7, 0, r3
	   bne-    REFLSYM(7)
	LSYM(8)
	xor r3,r5,r9
	subfic r5,r3,0
	adde r3,r5,r3
	blr
END_FUNC(opal_atomic_cmpset_64)


START_FUNC(opal_atomic_cmpset_acq_64)
        stw r4,-32(r1)
        stw r5,-28(r1)
        stw r6,-24(r1)
        stw r7,-20(r1)
        ld r5,-32(r1)
        ld r7,-24(r1)

        LSYM(9) ldarx   r9, 0, r3  
           cmpd    0, r9, r5
           bne-    REFLSYM(10)         
           stdcx.  r7, 0, r3  
           bne-    REFLSYM(9)
        LSYM(10)
        xor r3,r5,r9
        subfic r5,r3,0
        adde r3,r5,r3
        blr
        lwsync
        blr
END_FUNC(opal_atomic_cmpset_acq_64)


START_FUNC(opal_atomic_cmpset_rel_64)
        stw r4,-32(r1)
        stw r5,-28(r1)
        stw r6,-24(r1)
        stw r7,-20(r1)
        ld r5,-32(r1)
        ld r7,-24(r1)

        eieio
        LSYM(11) ldarx   r9, 0, r3  
           cmpd    0, r9, r5  
           bne-    REFLSYM(12)         
           stdcx.  r7, 0, r3  
           bne-    REFLSYM(11)
        LSYM(12)
        xor r3,r5,r9
        subfic r5,r3,0
        adde r3,r5,r3
        blr
        lwsync
        blr
END_FUNC(opal_atomic_cmpset_rel_64)
#END_64BIT


START_FUNC(opal_atomic_add_32)
	LSYM(13)   lwarx r0, 0, r3 
	     add  r0, r4, r0                
	     stwcx.   r0, 0, r3              
	     bne-  REFLSYM(13)
	mr	r3,r0
	blr
END_FUNC(opal_atomic_add_32)


START_FUNC(opal_atomic_sub_32)
	LSYM(14)   lwarx r0,0,r3
	     subf  r0,r4,r0                
	     stwcx.   r0,0,r3              
	     bne-  REFLSYM(14)             
	mr	r3,r0
	blr
END_FUNC(opal_atomic_sub_32)

START_FUNC(opal_sys_timer_get_cycles)
	LSYM(15)
	mftbu r0
	mftb r11
	mftbu r2
	cmpw cr7,r2,r0
	bne+ cr7,REFLSYM(15)
	li r4,0
	li r9,0
	or r3,r2,r9
	or r4,r4,r11
	blr
END_FUNC(opal_sys_timer_get_cycles)
