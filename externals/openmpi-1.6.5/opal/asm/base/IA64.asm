START_FILE

	.pred.safe_across_calls p1-p5,p16-p63
	.text
	.align 16
	.global opal_atomic_mb#
	.proc opal_atomic_mb#
opal_atomic_mb:
	.prologue
	.body
	mf
	br.ret.sptk.many b0
	;;
	.endp opal_atomic_mb#
	.align 16
	.global opal_atomic_rmb#
	.proc opal_atomic_rmb#
opal_atomic_rmb:
	.prologue
	.body
	mf
	br.ret.sptk.many b0
	;;
	.endp opal_atomic_rmb#
	.align 16
	.global opal_atomic_wmb#
	.proc opal_atomic_wmb#
opal_atomic_wmb:
	.prologue
	.body
	mf
	br.ret.sptk.many b0
	;;
	.endp opal_atomic_wmb#
	.align 16
	.global opal_atomic_cmpset_acq_32#
	.proc opal_atomic_cmpset_acq_32#
opal_atomic_cmpset_acq_32:
	.prologue
	.body
	mov ar.ccv=r33;;
	cmpxchg4.acq r32=[r32],r34,ar.ccv
	;;
	cmp4.eq p6, p7 = r32, r33
	;;
	(p6) addl r8 = 1, r0
	(p7) mov r8 = r0
	br.ret.sptk.many b0
	;;
	.endp opal_atomic_cmpset_acq_32#
	.align 16
	.global opal_atomic_cmpset_rel_32#
	.proc opal_atomic_cmpset_rel_32#
opal_atomic_cmpset_rel_32:
	.prologue
	.body
	mov ar.ccv=r33;;
	cmpxchg4.rel r32=[r32],r34,ar.ccv
	;;
	cmp4.eq p6, p7 = r32, r33
	;;
	(p6) addl r8 = 1, r0
	(p7) mov r8 = r0
	br.ret.sptk.many b0
	;;
	.endp opal_atomic_cmpset_rel_32#
	.align 16
	.global opal_atomic_cmpset_acq_64#
	.proc opal_atomic_cmpset_acq_64#
opal_atomic_cmpset_acq_64:
	.prologue
	.body
	mov ar.ccv=r33;;
	cmpxchg8.acq r32=[r32],r34,ar.ccv
	;;
	sxt4 r32 = r32
	;;
	cmp.eq p6, p7 = r33, r32
	;;
	(p6) addl r8 = 1, r0
	(p7) mov r8 = r0
	br.ret.sptk.many b0
	;;
	.endp opal_atomic_cmpset_acq_64#
	.align 16
	.global opal_atomic_cmpset_rel_64#
	.proc opal_atomic_cmpset_rel_64#
opal_atomic_cmpset_rel_64:
	.prologue
	.body
	mov ar.ccv=r33;;
	cmpxchg8.rel r32=[r32],r34,ar.ccv
	;;
	sxt4 r32 = r32
	;;
	cmp.eq p6, p7 = r33, r32
	;;
	(p6) addl r8 = 1, r0
	(p7) mov r8 = r0
	br.ret.sptk.many b0
	;;
	.endp opal_atomic_cmpset_rel_64#
        .align 16
        .global opal_sys_timer_get_cycles#
        .proc opal_sys_timer_get_cycles#
opal_sys_timer_get_cycles:
        .prologue
        .body
        mov r8=ar.itc
        br.ret.sptk.many b0
        ;;
        .endp opal_sys_timer_get_cycles#
	.ident	"GCC: (GNU) 3.2.3 20030502 (Red Hat Linux 3.2.3-49)"
