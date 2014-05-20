	.text

	.globl _opal_atomic_mb
_opal_atomic_mb:
	pushq	%rbp
	movq	%rsp, %rbp
	leave
	ret


	.globl _opal_atomic_rmb
_opal_atomic_rmb:
	pushq	%rbp
	movq	%rsp, %rbp
	leave
	ret


	.globl _opal_atomic_wmb
_opal_atomic_wmb:
	pushq	%rbp
	movq	%rsp, %rbp
	leave
	ret


	.globl _opal_atomic_cmpset_32
_opal_atomic_cmpset_32:
        movl    %esi, %eax
        lock; cmpxchgl %edx,(%rdi)   
        sete     %dl
        movzbl  %dl, %eax
        ret


	.globl _opal_atomic_cmpset_64
_opal_atomic_cmpset_64:
        movq    %rsi, %rax
        lock; cmpxchgq %rdx,(%rdi)   
        sete     %dl
        movzbl  %dl, %eax
        ret


	.globl _opal_sys_timer_get_cycles
_opal_sys_timer_get_cycles:
        rdtsc
        salq    $32, %rdx
        mov     %eax, %eax
        orq     %rdx, %rax
        ret
