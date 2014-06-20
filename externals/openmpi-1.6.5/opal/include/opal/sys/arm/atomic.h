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
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 * Copyright (c) 2010      ARM ltd.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/*
 * ARMv5 and earlier lack robust atomic operations and therefore this file uses
 * Linux kernel support where needed.  The kernel also provides memory barriers
 * and this file uses them for ARMv5 and earlier processors, which lack the
 * memory barrier instruction.  These kernel functions are available on kernel
 * versions 2.6.15 and greater; using them will result in undefined behavior on
 * older kernels.
 * See Documentation/arm/kernel_user_helpers.txt in the kernel tree for details
 */

#ifndef OMPI_SYS_ARCH_ATOMIC_H
#define OMPI_SYS_ARCH_ATOMIC_H 1

#if OPAL_WANT_SMP_LOCKS

#if (OPAL_ASM_ARM_VERSION >= 7)

#define OPAL_HAVE_ATOMIC_MEM_BARRIER 1
/* use the DMB instruction if available... */

#define MB()  __asm__ __volatile__ ("dmb" : : : "memory")
#define RMB() __asm__ __volatile__ ("dmb" : : : "memory")
#define WMB() __asm__ __volatile__ ("dmb" : : : "memory")

#elif (OPAL_ASM_ARM_VERSION == 6)

#define OPAL_HAVE_ATOMIC_MEM_BARRIER 1
/* ...or the v6-specific equivalent... */

#define MB()  __asm__ __volatile__ ("mcr p15, 0, r0, c7, c10, 5" : : : "memory")
#define RMB() MB()
#define WMB() MB()

#else

#define OPAL_HAVE_ATOMIC_MEM_BARRIER 1
/* ...otherwise use the Linux kernel-provided barrier */

#define MB() (*((void (*)(void))(0xffff0fa0)))()
#define RMB() MB()
#define WMB() MB()

#endif

#else

#define MB()
#define RMB()
#define WMB()

#endif /* OPAL_WANT_SMP_LOCKS */



/**********************************************************************
 *
 * Memory Barriers
 *
 *********************************************************************/

#if (OPAL_HAVE_ATOMIC_MEM_BARRIER == 1)

static inline
void opal_atomic_mb(void)
{
    MB();
}


static inline
void opal_atomic_rmb(void)
{
    RMB();
}


static inline
void opal_atomic_wmb(void)
{
    WMB();
}

#endif


/**********************************************************************
 *
 * Atomic math operations
 *
 *********************************************************************/

#if (OMPI_GCC_INLINE_ASSEMBLY && (OPAL_ASM_ARM_VERSION >= 6))

#define OPAL_HAVE_ATOMIC_CMPSET_32 1
#define OPAL_HAVE_ATOMIC_MATH_32 1
static inline int opal_atomic_cmpset_32(volatile int32_t *addr,
                                        int32_t oldval, int32_t newval)
{
  int32_t ret, tmp;

   __asm__ __volatile__ (
                         "1:  ldrex   %0, [%2]        \n"
                         "    cmp     %0, %3          \n"
                         "    bne     2f              \n"
                         "    strex   %1, %4, [%2]    \n"
                         "    cmp     %1, #0          \n"
                         "    bne     1b              \n"
                         "2:                          \n"

                         : "=&r" (ret), "=&r" (tmp)
                         : "r" (addr), "r" (oldval), "r" (newval)
                         : "cc", "memory");

   return (ret == oldval);
}

/* these two functions aren't inlined in the non-gcc case because then
   there would be two function calls (since neither cmpset_32 nor
   atomic_?mb can be inlined).  Instead, we "inline" them by hand in
   the assembly, meaning there is one function call overhead instead
   of two */
static inline int opal_atomic_cmpset_acq_32(volatile int32_t *addr,
                                            int32_t oldval, int32_t newval)
{
    int rc;

    rc = opal_atomic_cmpset_32(addr, oldval, newval);
    opal_atomic_rmb();

    return rc;
}


static inline int opal_atomic_cmpset_rel_32(volatile int32_t *addr,
                                            int32_t oldval, int32_t newval)
{
    opal_atomic_wmb();
    return opal_atomic_cmpset_32(addr, oldval, newval);
}

#if (OPAL_ASM_SUPPORT_64BIT == 1)

#define OPAL_HAVE_ATOMIC_CMPSET_64 1
static inline int opal_atomic_cmpset_64(volatile int64_t *addr,
                                        int64_t oldval, int64_t newval)
{
  int64_t ret;
  int tmp;


   __asm__ __volatile__ (
                         "1:  ldrexd  %0, %H0, [%2]           \n"
                         "    cmp     %0, %3                  \n"
                         "    it      eq                      \n"
                         "    cmpeq   %H0, %H3                \n"
                         "    bne     2f                      \n"
                         "    strexd  %1, %4, %H4, [%2]       \n"
                         "    cmp     %1, #0                  \n"
                         "    bne     1b                      \n"
                         "2:                                    \n"

                         : "=&r" (ret), "=&r" (tmp)
                         : "r" (addr), "r" (oldval), "r" (newval)
                         : "cc", "memory");

   return (ret == oldval);
}

/* these two functions aren't inlined in the non-gcc case because then
   there would be two function calls (since neither cmpset_64 nor
   atomic_?mb can be inlined).  Instead, we "inline" them by hand in
   the assembly, meaning there is one function call overhead instead
   of two */
static inline int opal_atomic_cmpset_acq_64(volatile int64_t *addr,
                                            int64_t oldval, int64_t newval)
{
    int rc;

    rc = opal_atomic_cmpset_64(addr, oldval, newval);
    opal_atomic_rmb();

    return rc;
}


static inline int opal_atomic_cmpset_rel_64(volatile int64_t *addr,
                                            int64_t oldval, int64_t newval)
{
    opal_atomic_wmb();
    return opal_atomic_cmpset_64(addr, oldval, newval);
}

#endif


#define OPAL_HAVE_ATOMIC_ADD_32 1
static inline int32_t opal_atomic_add_32(volatile int32_t* v, int inc)
{
   int32_t t;
   int tmp;

   __asm__ __volatile__(
                         "1:  ldrex   %0, [%2]        \n"
                         "    add     %0, %0, %3      \n"
                         "    strex   %1, %0, [%2]    \n"
                         "    cmp     %1, #0          \n"
                         "    bne     1b              \n"

                         : "=&r" (t), "=&r" (tmp)
                         : "r" (v), "r" (inc)
                         : "cc", "memory");


   return t;
}

#define OPAL_HAVE_ATOMIC_SUB_32 1
static inline int32_t opal_atomic_sub_32(volatile int32_t* v, int dec)
{
   int32_t t;
   int tmp;

   __asm__ __volatile__(
                         "1:  ldrex   %0, [%2]        \n"
                         "    sub     %0, %0, %3      \n"
                         "    strex   %1, %0, [%2]    \n"
                         "    cmp     %1, #0          \n"
                         "    bne     1b              \n"

                         : "=&r" (t), "=&r" (tmp)
                         : "r" (v), "r" (dec)
                         : "cc", "memory");

   return t;
}

#else /* OPAL_ASM_ARM_VERSION <=5 or no GCC inline assembly */

#define OPAL_HAVE_ATOMIC_CMPSET_32 1
#define __kuser_cmpxchg (*((int (*)(int, int, volatile int*))(0xffff0fc0)))
static inline int opal_atomic_cmpset_32(volatile int32_t *addr,
                                        int32_t oldval, int32_t newval)
{
    return !(__kuser_cmpxchg(oldval, newval, addr));
}

static inline int opal_atomic_cmpset_acq_32(volatile int32_t *addr,
                                            int32_t oldval, int32_t newval)
{
    /* kernel function includes all necessary memory barriers */
    return opal_atomic_cmpset_32(addr, oldval, newval);
}

static inline int opal_atomic_cmpset_rel_32(volatile int32_t *addr,
                                            int32_t oldval, int32_t newval)
{
    /* kernel function includes all necessary memory barriers */
    return opal_atomic_cmpset_32(addr, oldval, newval);
}

#endif

#endif /* ! OMPI_SYS_ARCH_ATOMIC_H */
