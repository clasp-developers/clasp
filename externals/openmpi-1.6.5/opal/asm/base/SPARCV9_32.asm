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
	save	%sp, -128, %sp
	!#PROLOGUE# 1
	mov	%i3, %o4
	mov	%i4, %o5
	st	%i1, [%fp-32]
	st	%i2, [%fp-28]
	std	%o4, [%fp-24]
	ldx [%fp-24], %g1               
	ldx [%fp-32], %g2               
	casxa [%i0] 0x80, %g2, %g1 
	stx %g1, [%fp-24]               

	ld	[%fp-24], %i5
	ld	[%fp-32], %g1
	cmp	%i5, %g1
	bne	REFLSYM(12)
	mov	0, %i0
	ld	[%fp-20], %i2
	ld	[%fp-28], %i1
	cmp	%i2, %i1
	be,a	REFLSYM(12)
	mov	1, %i0
LSYM(12)
	ret
	restore
END_FUNC(opal_atomic_cmpset_64)


START_FUNC(opal_atomic_cmpset_acq_64)
	!#PROLOGUE# 0
	save	%sp, -128, %sp
	!#PROLOGUE# 1
	mov	%i1, %o4
	mov	%i2, %o5
	mov	%i3, %o2
	mov	%i4, %o3
	std	%o4, [%fp-32]
	std	%o2, [%fp-24]
	ldx [%fp-24], %g1               
	ldx [%fp-32], %g2               
	casxa [%i0] 0x80, %g2, %g1 
	stx %g1, [%fp-24]               

	ld	[%fp-24], %i5
	ld	[%fp-32], %g1
	cmp	%i5, %g1
	bne	REFLSYM(16)
	mov	0, %i0
	ld	[%fp-20], %i2
	ld	[%fp-28], %i1
	cmp	%i2, %i1
	be,a	REFLSYM(16)
	mov	1, %i0
LSYM(16)
	membar #LoadLoad
	ret
	restore
END_FUNC(opal_atomic_cmpset_acq_64)


START_FUNC(opal_atomic_cmpset_rel_64)
	!#PROLOGUE# 0
	save	%sp, -128, %sp
	!#PROLOGUE# 1
	mov	%i1, %o4
	mov	%i2, %o5
	mov	%i3, %o2
	mov	%i4, %o3
	membar #StoreStore
	std	%o4, [%fp-32]
	std	%o2, [%fp-24]
	ldx [%fp-24], %g1               
	ldx [%fp-32], %g2               
	casxa [%i0] 0x80, %g2, %g1 
	stx %g1, [%fp-24]               

	ld	[%fp-24], %i5
	ld	[%fp-32], %g1
	cmp	%i5, %g1
	bne	REFLSYM(21)
	mov	0, %i0
	ld	[%fp-20], %i2
	ld	[%fp-28], %i1
	cmp	%i2, %i1
	be,a	REFLSYM(21)
	mov	1, %i0
LSYM(21)
	ret
	restore
END_FUNC(opal_atomic_cmpset_rel_64)


START_FUNC(opal_sys_timer_get_cycles)
        save    %sp,-96,%sp
        rd      %tick,%o0
        srlx    %o0,32,%o1
        or      %g0,%o1,%i0
        ret     ! Result =  %i0
        restore %o0,0,%o1
END_FUNC(opal_sys_timer_get_cycles)
