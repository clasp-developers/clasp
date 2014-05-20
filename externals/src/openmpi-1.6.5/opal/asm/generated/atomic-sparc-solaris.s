	.text

	.align 4

	.globl opal_atomic_mb
	.type opal_atomic_mb, #function
opal_atomic_mb:
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	retl
	nop
	.size opal_atomic_mb, .-opal_atomic_mb


	.globl opal_atomic_rmb
	.type opal_atomic_rmb, #function
opal_atomic_rmb:
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	retl
	nop
	.size opal_atomic_rmb, .-opal_atomic_rmb


	.globl opal_atomic_wmb
	.type opal_atomic_wmb, #function
opal_atomic_wmb:
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	retl
	nop
	.size opal_atomic_wmb, .-opal_atomic_wmb


	.globl opal_atomic_init
	.type opal_atomic_init, #function
opal_atomic_init:
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	stb	%o1, [%o0]
	retl
	nop
	.size opal_atomic_init, .-opal_atomic_init


	.globl opal_atomic_trylock
	.type opal_atomic_trylock, #function
opal_atomic_trylock:
	!#PROLOGUE# 0
	!#PROLOGUE# 1
	ldstub [%o0], %o0
	and	%o0, 0xff, %o0
	subcc	%g0, %o0, %g0
	retl
	subx	%g0, -1, %o0
	.size opal_atomic_trylock, .-opal_atomic_trylock


	.globl opal_atomic_lock
	.type opal_atomic_lock, #function
opal_atomic_lock:
	!#PROLOGUE# 0
	save	%sp, -112, %sp
	!#PROLOGUE# 1
.Lretry:               
	ldstub [%i0], %l0    
	tst    %l0          
	be     .Lout
	nop                  
.Lloop:
	ldub   [%i0], %l0    
	tst    %l0          
	bne    .Lloop          
	nop                  
	ba,a   .Lretry         
.Lout:                 
	nop
	ret
	restore
	.size opal_atomic_lock, .-opal_atomic_lock


	.globl opal_atomic_unlock
	.type opal_atomic_unlock, #function
opal_atomic_unlock:
	!#PROLOGUE# 0
	!#PROLOGUE# 1
		stbar             
	stb %g0, [%o0]    
	
	retl
	nop
	.size opal_atomic_unlock, .-opal_atomic_unlock
