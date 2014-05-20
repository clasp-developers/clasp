START_FILE
	TEXT

	ALIGN(4)

START_FUNC(opal_atomic_mb)
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	retl
	nop
END_FUNC(opal_atomic_mb)


START_FUNC(opal_atomic_rmb)
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	retl
	nop
END_FUNC(opal_atomic_rmb)


START_FUNC(opal_atomic_wmb)
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	retl
	nop
END_FUNC(opal_atomic_wmb)


START_FUNC(opal_atomic_init)
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	stb	%o1, [%o0]
	retl
	nop
END_FUNC(opal_atomic_init)


START_FUNC(opal_atomic_trylock)
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	ldstub [%o0], %o0
	and	%o0, 0xff, %o0
	subcc	%g0, %o0, %g0
	retl
	subx	%g0, -1, %o0
END_FUNC(opal_atomic_trylock)


START_FUNC(opal_atomic_lock)
	!#PROLOGUE# 0
	save	%sp, -112, %sp
	!#PROLOGUE# 1
LSYM(retry)               
	ldstub [%i0], %l0    
	tst    %l0          
	be     REFLSYM(out)
	nop                  
LSYM(loop)
	ldub   [%i0], %l0    
	tst    %l0          
	bne    REFLSYM(loop)          
	nop                  
	ba,a   REFLSYM(retry)         
LSYM(out)                 
	nop
	ret
	restore
END_FUNC(opal_atomic_lock)


START_FUNC(opal_atomic_unlock)
	!#PROLOGUE# 0
	!#PROLOGUE# 1
		stbar             
	stb %g0, [%o0]    
	
	retl
	nop
END_FUNC(opal_atomic_unlock)
