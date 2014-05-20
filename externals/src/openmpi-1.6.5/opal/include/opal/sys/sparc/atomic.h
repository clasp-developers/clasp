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


#if OPAL_WANT_SMP_LOCKS
#define MB() __asm__  __volatile__ ("" : : : "memory")
#else
#define MB()
#endif

#ifdef OMPI_GENERATE_ASM_FILE
struct opal_atomic_lock_t {
    union {
        volatile int lock;         /**< The lock address (an integer) */
        volatile unsigned char sparc_lock; /**< The lock address on sparc */
        char padding[sizeof(int)]; /**< Array for optional padding */
    } u;
};
typedef struct opal_atomic_lock_t opal_atomic_lock_t;
#endif

/**********************************************************************
 *
 * Define constants for Sparc
 *
 *********************************************************************/
#define OPAL_HAVE_ATOMIC_MEM_BARRIER 1

#define OPAL_HAVE_ATOMIC_CMPSET_32 0
#define OPAL_HAVE_ATOMIC_CMPSET_64 0

#define OPAL_HAVE_ATOMIC_MATH_32 1
#define OPAL_HAVE_ATOMIC_SUB_32 1
#define OPAL_HAVE_ATOMIC_ADD_32 1

#define OPAL_HAVE_ATOMIC_SPINLOCKS 1

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
 * Atomic spinlocks
 *
 *********************************************************************/
#if OMPI_GCC_INLINE_ASSEMBLY

/* for these, the lock is held whenever lock.sparc_lock != 0.  We
   attempt to leave it as OPAL_LOCKED whenever possible */


static inline void opal_atomic_init(opal_atomic_lock_t* lock, int value)
{
    lock->u.sparc_lock = (unsigned char) value;
}


static inline int opal_atomic_trylock(opal_atomic_lock_t *lock)
{
    unsigned char result;

    /* try to load the lock byte (atomically making the memory byte
       contain all 1s).  If the byte used to be 0, we now have the
       lock.  Otherwise, someone else has the lock.  Either way, the
       lock is now held. */
    __asm__ __volatile__ ("\t"
                          "ldstub [%1], %0"
                          : "+r"(result)
                          : "r"(&(lock->u.sparc_lock))
                          : "memory");
    return (result == 0);
}


static inline void opal_atomic_lock(opal_atomic_lock_t *lock)
{
    /* From page 264 of The SPARC Architecture Manual, Version 8 */
    __asm__ __volatile__ (
                          "1:                   \n\t"
                          "ldstub [%0], %%l0    \n\t"
                          "tst    %%l0          \n\t"
                          "be     3f            \n\t"
                          "nop                  \n"
                          "2:                   \n\t"
                          "ldub   [%0], %%l0    \n\t"
                          "tst    %%l0          \n\t"
                          "bne    2b            \n\t"
                          "nop                  \n\t"
                          "ba,a   1b            \n"
                          "3:                   \n\t"
                          "nop"
                          :
                          : "r"(&(lock->u.sparc_lock))
                          : "%l0", "memory");
}


static inline void opal_atomic_unlock(opal_atomic_lock_t *lock)
{
    /* 0 out that byte in memory */
    __asm__ __volatile__ ("\t"
                          "stbar             \n\t"
                          "stb %%g0, [%0]    \n\t"
                          :
                          : "r"(&(lock->u.sparc_lock))
                          : "memory");
}

#endif /* OMPI_GCC_INLINE_ASSEMBLY */


#endif /* ! OMPI_SYS_ARCH_ATOMIC_H */
