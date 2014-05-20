START_FILE
       TEXT

       ALIGN(4)
START_FUNC(opal_atomic_mb)
       dmb
       bx      lr
END_FUNC(opal_atomic_mb)


START_FUNC(opal_atomic_rmb)
       dmb
       bx      lr
END_FUNC(opal_atomic_rmb)


START_FUNC(opal_atomic_wmb)
       dmb
       bx      lr
END_FUNC(opal_atomic_wmb)


START_FUNC(opal_atomic_cmpset_32)
       LSYM(1)
       ldrex   r3, [r0]
       cmp     r1, r3
       bne     REFLSYM(2)
       strex   r12, r2, [r0]
       cmp     r12, #0
       bne     REFLSYM(1)
       mov     r0, #1
       LSYM(2)
       movne   r0, #0
       bx      lr
END_FUNC(opal_atomic_cmpset_32)


START_FUNC(opal_atomic_cmpset_acq_32)
       LSYM(3)
       ldrex   r3, [r0]
       cmp     r1, r3
       bne     REFLSYM(4)
       strex   r12, r2, [r0]
       cmp     r12, #0
       bne     REFLSYM(3)
       dmb
       mov     r0, #1
       LSYM(4)
       movne   r0, #0
       bx      lr
END_FUNC(opal_atomic_cmpset_acq_32)


START_FUNC(opal_atomic_cmpset_rel_32)
       LSYM(5)
       ldrex   r3, [r0]
       cmp     r1, r3
       bne     REFLSYM(6)
       dmb
       strex   r12, r2, [r0]
       cmp     r12, #0
       bne     REFLSYM(4)
       mov     r0, #1
       LSYM(6)
       movne   r0, #0
       bx      lr
END_FUNC(opal_atomic_cmpset_rel_32)

#START_64BIT
START_FUNC(opal_atomic_cmpset_64)
       push    {r4-r7}
       ldrd    r6, r7, [sp, #16]
       LSYM(7)
       ldrexd  r4, r5, [r0]
       cmp     r4, r2
       it      eq
       cmpeq   r5, r3
       bne     REFLSYM(8)
       strexd  r1, r6, r7, [r0]
       cmp     r1, #0
       bne     REFLSYM(7)
       mov     r0, #1
       LSYM(8)
       movne   r0, #0
       pop     {r4-r7}
       bx      lr
END_FUNC(opal_atomic_cmpset_64)

START_FUNC(opal_atomic_cmpset_acq_64)
       push    {r4-r7}
       ldrd    r6, r7, [sp, #16]
       LSYM(9)
       ldrexd  r4, r5, [r0]
       cmp     r4, r2
       it      eq
       cmpeq   r5, r3
       bne     REFLSYM(10)
       strexd  r1, r6, r7, [r0]
       cmp     r1, #0
       bne     REFLSYM(9)
       dmb
       mov     r0, #1
       LSYM(10)
       movne   r0, #0
       pop     {r4-r7}
       bx      lr
END_FUNC(opal_atomic_cmpset_acq_64)


START_FUNC(opal_atomic_cmpset_rel_64)
       push    {r4-r7}
       ldrd    r6, r7, [sp, #16]
       LSYM(11)
       ldrexd  r4, r5, [r0]
       cmp     r4, r2
       it      eq
       cmpeq   r5, r3
       bne     REFLSYM(12)
       dmb
       strexd  r1, r6, r7, [r0]
       cmp     r1, #0
       bne     REFLSYM(11)
       mov     r0, #1
       LSYM(12)
       movne   r0, #0
       pop     {r4-r7}
       bx      lr
END_FUNC(opal_atomic_cmpset_rel_64)
#END_64BIT


START_FUNC(opal_atomic_add_32)
       LSYM(13)
       ldrex   r2, [r0]
       add     r2, r2, r1
       strex   r3, r2, [r0]
       cmp     r3, #0
       bne     REFLSYM(13)
       mov     r0, r2
       bx      lr
END_FUNC(opal_atomic_add_32)


START_FUNC(opal_atomic_sub_32)
       LSYM(14)
       ldrex   r2, [r0]
       sub     r2, r2, r1
       strex   r3, r2, [r0]
       cmp     r3, #0
       bne     REFLSYM(14)
       mov     r0, r2
       bx      lr
END_FUNC(opal_atomic_sub_32)
