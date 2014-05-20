START_FILE
	TEXT

START_FUNC(opal_atomic_mb)
	pushq	%rbp
	movq	%rsp, %rbp
	leave
	ret
END_FUNC(opal_atomic_mb)


START_FUNC(opal_atomic_rmb)
	pushq	%rbp
	movq	%rsp, %rbp
	leave
	ret
END_FUNC(opal_atomic_rmb)


START_FUNC(opal_atomic_wmb)
	pushq	%rbp
	movq	%rsp, %rbp
	leave
	ret
END_FUNC(opal_atomic_wmb)


START_FUNC(opal_atomic_cmpset_32)
        movl    %esi, %eax
        lock; cmpxchgl %edx,(%rdi)   
        sete     %dl
        movzbl  %dl, %eax
        ret
END_FUNC(opal_atomic_cmpset_32)


START_FUNC(opal_atomic_cmpset_64)
        movq    %rsi, %rax
        lock; cmpxchgq %rdx,(%rdi)   
        sete     %dl
        movzbl  %dl, %eax
        ret
END_FUNC(opal_atomic_cmpset_64)


START_FUNC(opal_sys_timer_get_cycles)
        rdtsc
        salq    $32, %rdx
        mov     %eax, %eax
        orq     %rdx, %rax
        ret
END_FUNC(opal_sys_timer_get_cycles)
