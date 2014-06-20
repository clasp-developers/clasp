	.text

	.globl opal_atomic_mb
	.type opal_atomic_mb, @function
opal_atomic_mb:
	pushq	%rbp
	movq	%rsp, %rbp
	leave
	ret
	.size opal_atomic_mb, .-opal_atomic_mb


	.globl opal_atomic_rmb
	.type opal_atomic_rmb, @function
opal_atomic_rmb:
	pushq	%rbp
	movq	%rsp, %rbp
	leave
	ret
	.size opal_atomic_rmb, .-opal_atomic_rmb


	.globl opal_atomic_wmb
	.type opal_atomic_wmb, @function
opal_atomic_wmb:
	pushq	%rbp
	movq	%rsp, %rbp
	leave
	ret
	.size opal_atomic_wmb, .-opal_atomic_wmb


	.globl opal_atomic_cmpset_32
	.type opal_atomic_cmpset_32, @function
opal_atomic_cmpset_32:
        movl    %esi, %eax
        lock; cmpxchgl %edx,(%rdi)   
        sete     %dl
        movzbl  %dl, %eax
        ret
	.size opal_atomic_cmpset_32, .-opal_atomic_cmpset_32


	.globl opal_atomic_cmpset_64
	.type opal_atomic_cmpset_64, @function
opal_atomic_cmpset_64:
        movq    %rsi, %rax
        lock; cmpxchgq %rdx,(%rdi)   
        sete     %dl
        movzbl  %dl, %eax
        ret
	.size opal_atomic_cmpset_64, .-opal_atomic_cmpset_64


	.globl opal_sys_timer_get_cycles
	.type opal_sys_timer_get_cycles, @function
opal_sys_timer_get_cycles:
        rdtsc
        salq    $32, %rdx
        mov     %eax, %eax
        orq     %rdx, %rax
        ret
	.size opal_sys_timer_get_cycles, .-opal_sys_timer_get_cycles
