	.text

	.globl opal_atomic_mb
	.type opal_atomic_mb, @function
opal_atomic_mb:
	pushl	%ebp
	movl	%esp, %ebp
	leave
	ret
	.size opal_atomic_mb, .-opal_atomic_mb


	.globl opal_atomic_rmb
	.type opal_atomic_rmb, @function
opal_atomic_rmb:
	pushl	%ebp
	movl	%esp, %ebp
	leave
	ret
	.size opal_atomic_rmb, .-opal_atomic_rmb


	.globl opal_atomic_wmb
	.type opal_atomic_wmb, @function
opal_atomic_wmb:
	pushl	%ebp
	movl	%esp, %ebp
	leave
	ret
	.size opal_atomic_wmb, .-opal_atomic_wmb


	.globl opal_atomic_cmpset_32
	.type opal_atomic_cmpset_32, @function
opal_atomic_cmpset_32:
	pushl   %ebp
	movl    %esp, %ebp
	movl    8(%ebp), %edx
	movl    16(%ebp), %ecx
	movl    12(%ebp), %eax
	lock; cmpxchgl %ecx,(%edx)
	sete     %dl

	movzbl  %dl, %eax
	leave
	ret
	.size opal_atomic_cmpset_32, .-opal_atomic_cmpset_32


	.globl opal_atomic_cmpset_64
	.type opal_atomic_cmpset_64, @function
opal_atomic_cmpset_64:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$32, %esp
	movl	%ebx, -12(%ebp)
	movl	%esi, -8(%ebp)
	movl	%edi, -4(%ebp)
	movl	8(%ebp), %edi
	movl	12(%ebp), %eax
	movl	16(%ebp), %edx
	movl	%eax, -24(%ebp)
	movl	%edx, -20(%ebp)
	movl	20(%ebp), %eax
	movl	24(%ebp), %edx
	movl	%eax, -32(%ebp)
	movl	%edx, -28(%ebp)
	movl	-24(%ebp), %ebx
	movl	-20(%ebp), %edx
	movl	-32(%ebp), %esi
	movl	-28(%ebp), %ecx
	movl	%ebx, %eax
	push %ebx            
	movl %esi, %ebx        
	lock; cmpxchg8b (%edi)
	sete %dl               
	pop %ebx             
	
	movzbl	%dl, %eax
	movl	-12(%ebp), %ebx
	movl	-8(%ebp), %esi
	movl	-4(%ebp), %edi
	movl	%ebp, %esp
	popl	%ebp
	ret
	.size opal_atomic_cmpset_64, .-opal_atomic_cmpset_64


	.globl opal_atomic_add_32
	.type opal_atomic_add_32, @function
opal_atomic_add_32:
        pushl   %ebp
        movl    %esp, %ebp
        movl    8(%ebp), %eax
        movl    12(%ebp), %edx
        lock; addl %edx,(%eax)
        movl    (%eax), %eax
        leave
        ret
	.size opal_atomic_add_32, .-opal_atomic_add_32


	.globl opal_atomic_sub_32
	.type opal_atomic_sub_32, @function
opal_atomic_sub_32:
        pushl   %ebp
        movl    %esp, %ebp
        movl    8(%ebp), %eax
        movl    12(%ebp), %edx
        lock; subl %edx,(%eax)
        movl    (%eax), %eax
        leave
        ret
	.size opal_atomic_sub_32, .-opal_atomic_sub_32


	.globl opal_sys_timer_get_cycles
	.type opal_sys_timer_get_cycles, @function
opal_sys_timer_get_cycles:
        pushl   %ebp
        movl    %esp, %ebp
        rdtsc
        popl    %ebp
        ret
	.size opal_sys_timer_get_cycles, .-opal_sys_timer_get_cycles

	.section	.note.GNU-stack,"",@progbits
