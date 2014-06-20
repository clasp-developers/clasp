START_FILE
	TEXT

	ALIGN(4)

	
START_FUNC(opal_atomic_mb)
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	membar #LoadLoad | #LoadStore | #StoreStore | #StoreLoad
	retl
	nop
END_FUNC(opal_atomic_mb)


START_FUNC(opal_atomic_rmb)
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	membar #LoadLoad
	retl
	nop
END_FUNC(opal_atomic_rmb)


START_FUNC(opal_atomic_wmb)
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	membar #StoreStore
	retl
	nop
END_FUNC(opal_atomic_wmb)


START_FUNC(opal_atomic_cmpset_32)
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	casa [%o0] 0x80, %o1, %o2
	xor	%o2, %o1, %o2
	subcc	%g0, %o2, %g0
	retl
	subx	%g0, -1, %o0
END_FUNC(opal_atomic_cmpset_32)


START_FUNC(opal_atomic_cmpset_acq_32)
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	casa [%o0] 0x80, %o1, %o2
	xor     %o2, %o1, %o2
	subcc   %g0, %o2, %g0
	subx    %g0, -1, %o0
	membar #LoadLoad
	retl
	sra     %o0, 0, %o0
END_FUNC(opal_atomic_cmpset_acq_32)


START_FUNC(opal_atomic_cmpset_rel_32)
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	membar #StoreStore
	casa [%o0] 0x80, %o1, %o2
	xor     %o2, %o1, %o2
	subcc   %g0, %o2, %g0
	retl
	subx    %g0, -1, %o0
END_FUNC(opal_atomic_cmpset_rel_32)


START_FUNC(opal_atomic_cmpset_64)
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	casxa [%o0] 0x80, %o1, %o2
	mov     0, %o0
	xor     %o2, %o1, %o2
	retl
	movre   %o2, 1, %o0
END_FUNC(opal_atomic_cmpset_64)


START_FUNC(opal_atomic_cmpset_acq_64)
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	casxa [%o0] 0x80, %o1, %o2
	mov     0, %o0
	xor     %o2, %o1, %o2
	movre   %o2, 1, %o0
	membar #LoadLoad
	retl
	sra     %o0, 0, %o0
END_FUNC(opal_atomic_cmpset_acq_64)


START_FUNC(opal_atomic_cmpset_rel_64)
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	membar #StoreStore
	casxa [%o0] 0x80, %o1, %o2
	mov     0, %o0
	xor     %o2, %o1, %o2
	retl
	movre   %o2, 1, %o0
END_FUNC(opal_atomic_cmpset_rel_64)


START_FUNC(opal_sys_timer_get_cycles)
        save    %sp,-176,%sp
        rd      %tick,%o0
        ret     ! Result =  %i0
        restore %o0,0,%o0
END_FUNC(opal_sys_timer_get_cycles)
