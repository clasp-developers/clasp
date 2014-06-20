/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2010 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2010 Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_SYS_ARCH_ATOMIC_H
#define OMPI_SYS_ARCH_ATOMIC_H 1

/*
 * On ia32, we use cmpxchg.
 */

#if OPAL_WANT_SMP_LOCKS
#define SMPLOCK "lock; "
#define MB() __asm__ __volatile__("": : :"memory")
#else
#define SMPLOCK
#define MB()
#endif


/**********************************************************************
 *
 * Define constants for IA32
 *
 *********************************************************************/
#define OPAL_HAVE_ATOMIC_MEM_BARRIER 1

#define OPAL_HAVE_ATOMIC_CMPSET_32 1

#define OPAL_HAVE_ATOMIC_MATH_32 1
#define OPAL_HAVE_ATOMIC_ADD_32 1
#define OPAL_HAVE_ATOMIC_SUB_32 1

#define OPAL_HAVE_ATOMIC_CMPSET_64 1

#undef OPAL_HAVE_INLINE_ATOMIC_CMPSET_64
#define OPAL_HAVE_INLINE_ATOMIC_CMPSET_64 0

/**********************************************************************
 *
 * Memory Barriers
 *
 *********************************************************************/
#if OMPI_GCC_INLINE_ASSEMBLY

static inline void opal_atomic_mb(void)
{
    MB();
}


static inline void opal_atomic_rmb(void)
{
    MB();
}


static inline void opal_atomic_wmb(void)
{
    MB();
}

#endif /* OMPI_GCC_INLINE_ASSEMBLY */


/**********************************************************************
 *
 * Atomic math operations
 *
 *********************************************************************/
#if OMPI_GCC_INLINE_ASSEMBLY

static inline int opal_atomic_cmpset_32(volatile int32_t *addr,
                                        int32_t oldval,
                                        int32_t newval)
{
   unsigned char ret;
   __asm__ __volatile__ (
                       SMPLOCK "cmpxchgl %3,%2   \n\t"
                               "sete     %0      \n\t"
                       : "=qm" (ret), "+a" (oldval), "+m" (*addr)
                       : "q"(newval)
                       : "memory", "cc");
   
   return (int)ret;
}

#endif /* OMPI_GCC_INLINE_ASSEMBLY */

#define opal_atomic_cmpset_acq_32 opal_atomic_cmpset_32
#define opal_atomic_cmpset_rel_32 opal_atomic_cmpset_32

#if OMPI_GCC_INLINE_ASSEMBLY

#if 0

/* some versions of GCC won't let you use ebx period (even though they
   should be able to save / restore for the life of the inline
   assembly).  For the beta, just use the non-inline version */

#ifndef ll_low /* GLIBC provides these somewhere, so protect */
#define ll_low(x)       *(((unsigned int*)&(x))+0)
#define ll_high(x)      *(((unsigned int*)&(x))+1)
#endif

/* On Linux the EBX register is used by the shared libraries
 * to keep the global offset. In same time this register is 
 * required by the cmpxchg8b instruction (as an input parameter).
 * This conflict force us to save the EBX before the cmpxchg8b 
 * and to restore it afterward.
 */
static inline int opal_atomic_cmpset_64(volatile int64_t *addr,
                                        int64_t oldval,
                                        int64_t newval)
{
   /* 
    * Compare EDX:EAX with m64. If equal, set ZF and load ECX:EBX into
    * m64. Else, clear ZF and load m64 into EDX:EAX.
    */
    unsigned char ret;

    __asm__ __volatile__(
		    "push %%ebx            \n\t"
                    "movl %4, %%ebx        \n\t"
		    SMPLOCK "cmpxchg8b (%1)  \n\t"
		    "sete %0               \n\t"
		    "pop %%ebx             \n\t"
		    : "=qm"(ret)
		    : "D"(addr), "a"(ll_low(oldval)), "d"(ll_high(oldval)),
		      "r"(ll_low(newval)), "c"(ll_high(newval))
		    : "cc", "memory", "ebx");
    return (int) ret;
}
#endif /* if 0 */

#endif /* OMPI_GCC_INLINE_ASSEMBLY */

#define opal_atomic_cmpset_acq_64 opal_atomic_cmpset_64
#define opal_atomic_cmpset_rel_64 opal_atomic_cmpset_64

#if OMPI_GCC_INLINE_ASSEMBLY

/**
 * atomic_add - add integer to atomic variable
 * @i: integer value to add
 * @v: pointer of type int
 *
 * Atomically adds @i to @v.
 */
static inline int32_t opal_atomic_add_32(volatile int32_t* v, int i)
{
    int ret = i;
   __asm__ __volatile__(
                        SMPLOCK "xaddl %1,%0"
                        :"=m" (*v), "+r" (ret)
                        :"m" (*v)
                        :"memory", "cc"
                        );
   return (ret+i);
}


/**
 * atomic_sub - subtract the atomic variable
 * @i: integer value to subtract
 * @v: pointer of type int
 *
 * Atomically subtracts @i from @v.
 */
static inline int32_t opal_atomic_sub_32(volatile int32_t* v, int i)
{
    int ret = -i;
   __asm__ __volatile__(
                        SMPLOCK "xaddl %1,%0"
                        :"=m" (*v), "+r" (ret)
                        :"m" (*v)
                        :"memory", "cc"
                        );
   return (ret-i);
}

#endif /* OMPI_GCC_INLINE_ASSEMBLY */

#endif /* ! OMPI_SYS_ARCH_ATOMIC_H */
