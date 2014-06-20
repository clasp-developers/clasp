/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_SYS_ARCH_ATOMIC_H
#define OMPI_SYS_ARCH_ATOMIC_H 1

/*
 * On ia64, we use cmpxchg, which supports acquire/release semantics natively.
 */


#define MB() __asm__ __volatile__("mf": : :"memory")


/**********************************************************************
 *
 * Define constants for IA64
 *
 *********************************************************************/
#define OPAL_HAVE_ATOMIC_MEM_BARRIER 1

#define OPAL_HAVE_ATOMIC_CMPSET_32 1
#define OPAL_HAVE_ATOMIC_CMPSET_64 1

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

#define ia64_cmpxchg4_acq(ptr, new, old)                 \
({                               \
   __u64 ia64_intri_res;                        \
   ia64_intri_res;                           \
})

static inline int opal_atomic_cmpset_acq_32( volatile int32_t *addr,
                                             int32_t oldval, int32_t newval)
{
    int64_t ret;

    __asm__ __volatile__ ("mov ar.ccv=%0;;" :: "rO"(oldval));
    __asm__ __volatile__ ("cmpxchg4.acq %0=[%1],%2,ar.ccv":
                          "=r"(ret) : "r"(addr), "r"(newval) : "memory");

    return ((int32_t)ret == oldval);
}


static inline int opal_atomic_cmpset_rel_32( volatile int32_t *addr,
                                             int32_t oldval, int32_t newval)
{
    int64_t ret;

    __asm__ __volatile__ ("mov ar.ccv=%0;;" :: "rO"(oldval));
    __asm__ __volatile__ ("cmpxchg4.rel %0=[%1],%2,ar.ccv":
                  "=r"(ret) : "r"(addr), "r"(newval) : "memory");

    return ((int32_t)ret == oldval);
}

#endif /* OMPI_GCC_INLINE_ASSEMBLY */


#define opal_atomic_cmpset_32 opal_atomic_cmpset_acq_32

#if OMPI_GCC_INLINE_ASSEMBLY

static inline int opal_atomic_cmpset_acq_64( volatile int64_t *addr,
                                             int64_t oldval, int64_t newval)
{
    int64_t ret;

    __asm__ __volatile__ ("mov ar.ccv=%0;;" :: "rO"(oldval));
    __asm__ __volatile__ ("cmpxchg8.acq %0=[%1],%2,ar.ccv":
                  "=r"(ret) : "r"(addr), "r"(newval) : "memory");

    return ((int32_t)ret == oldval);
}


static inline int opal_atomic_cmpset_rel_64( volatile int64_t *addr,
                                             int64_t oldval, int64_t newval)
{
    int64_t ret;

    __asm__ __volatile__ ("mov ar.ccv=%0;;" :: "rO"(oldval));
    __asm__ __volatile__ ("cmpxchg8.rel %0=[%1],%2,ar.ccv":
                  "=r"(ret) : "r"(addr), "r"(newval) : "memory");

    return ((int32_t)ret == oldval);
}

#endif /* OMPI_GCC_INLINE_ASSEMBLY */

#define opal_atomic_cmpset_64 opal_atomic_cmpset_acq_64

#endif /* ! OMPI_SYS_ARCH_ATOMIC_H */
