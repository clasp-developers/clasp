/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#include <windows.h>
#if defined(HAVE_WDM_H)
#include <wdm.h>
#endif  /* HAVE_WDM_H */

/**********************************************************************
 *
 * Memory Barriers
 *
 *********************************************************************/
#define OPAL_HAVE_ATOMIC_MEM_BARRIER 1

static inline void opal_atomic_mb(void)
{    
#if defined(HAVE_WDM_H)
   return KeMemoryBarrier();
#endif  /* HAVE_WDM_H */
}

static inline void opal_atomic_rmb(void)
{
#if defined(HAVE_WDM_H)
   return KeMemoryBarrier();
#endif  /* HAVE_WDM_H */
}

static inline void opal_atomic_wmb(void)
{    
#if defined(HAVE_WDM_H)
   return KeMemoryBarrier();
#endif  /* HAVE_WDM_H */
}

/**********************************************************************
 *
 * Atomic math operations
 *
 *********************************************************************/

#define OPAL_HAVE_ATOMIC_CMPSET_32 1
static inline int opal_atomic_cmpset_acq_32( volatile int32_t *addr,
                                             int32_t oldval, int32_t newval)
{
#if HAVE_INTERLOCKEDCOMPAREEXCHANGEACQUIRE
   int32_t ret = InterlockedCompareExchangeAcquire( (long volatile*)addr,
                                                    (long)newval, (long)oldval);
#else
   int32_t ret = InterlockedCompareExchange( (long volatile*)addr,
                                             (long)newval, (long)oldval );
#endif  /* HAVE_INTERLOCKEDCOMPAREEXCHANGEACQUIRE */
   return (oldval == ret) ? 1: 0;
}


static inline int opal_atomic_cmpset_rel_32( volatile int32_t *addr,
                                             int32_t oldval, int32_t newval)
{
#if HAVE_INTERLOCKEDCOMPAREEXCHANGERELEASE
   int32_t ret = InterlockedCompareExchangeRelease( (long volatile*)addr,
                                                    (long)newval, (long)oldval );
#else
   int32_t ret = InterlockedCompareExchange( (long volatile*)addr,
                                             (long)newval, (long)oldval );
#endif  /* HAVE_INTERLOCKEDCOMPAREEXCHANGERELEASE */
   return (oldval == ret) ? 1: 0;
}

static inline int opal_atomic_cmpset_32( volatile int32_t *addr,
                                         int32_t oldval, int32_t newval)
{
   int32_t ret = InterlockedCompareExchange ((long volatile*) addr,
                                             (long) newval, (long) oldval);

   return (oldval == ret) ? 1: 0;
}

#define OPAL_HAVE_ATOMIC_MATH_32 1

#define OPAL_HAVE_ATOMIC_ADD_32 1
static inline int32_t opal_atomic_add_32(volatile int32_t *addr, int32_t delta)
{
   return InterlockedExchangeAdd ((LONG volatile *) addr,
                                  (LONG) delta);
}

#define OPAL_HAVE_ATOMIC_SUB_32 1
static inline int32_t opal_atomic_sub_32(volatile int32_t *addr, int32_t delta)
{
   return InterlockedExchangeAdd( (LONG volatile *) addr,
                                  (LONG) (-delta));
}

#if HAVE_INTERLOCKEDCOMPAREEXCHANGE64
#define OPAL_HAVE_ATOMIC_CMPSET_64 1
static inline int opal_atomic_cmpset_acq_64( volatile int64_t *addr,
                                             int64_t oldval, int64_t newval)
{
/* The address should be 64 bits aligned otherwise ...
 * http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dllproc/base/interlockedcompareexchange64.asp
 */
#if HAVE_INTERLOCKEDCOMPAREEXCHANGEACQUIRE64
	int64_t ret = InterlockedCompareExchangeAcquire64 ((int64_t volatile*) addr,
                                                      (int64_t) newval, (int64_t) oldval);
#else
    int64_t ret = InterlockedCompareExchange64 ((int64_t volatile*) addr,
                                                 (int64_t) newval, (int64_t) oldval);
#endif  /* HAVE_INTERLOCKEDCOMPAREEXCHANGEACQUIRE64 */
    return (oldval == ret) ? 1: 0;
}

static inline int opal_atomic_cmpset_rel_64( volatile int64_t *addr,
                                             int64_t oldval, int64_t newval)
{
#if HAVE_INTERLOCKEDCOMPAREEXCHANGERELEASE64
    int64_t ret = InterlockedCompareExchangeRelease64 ((int64_t volatile*) addr,
                                                       (int64_t) newval, (int64_t) oldval);
#else
    int64_t ret = InterlockedCompareExchange64 ((int64_t volatile*) addr,
                                                 (int64_t) newval, (int64_t) oldval);
#endif  /* HAVE_INTERLOCKEDCOMPAREEXCHANGERELEASE64 */
    return (oldval == ret) ? 1: 0;
}


static inline int opal_atomic_cmpset_64( volatile int64_t *addr,
                                         int64_t oldval, int64_t newval)
{
    int64_t ret = InterlockedCompareExchange64 ((int64_t volatile*) addr,
                                                 (int64_t) newval, (int64_t) oldval);
    return (oldval == ret) ? 1: 0;
}

#define OPAL_HAVE_ATOMIC_MATH_64 1
#define OPAL_HAVE_ATOMIC_ADD_64 1
static inline int64_t opal_atomic_add_64(volatile int64_t *addr, int64_t delta)
{
    return InterlockedExchangeAdd64 ((int64_t volatile *) addr,
                                     (int64_t) delta);
}

#define OPAL_HAVE_ATOMIC_SUB_64 1
static inline int64_t opal_atomic_sub_64(volatile int64_t *addr, int64_t delta)
{
    return InterlockedExchangeAdd64 ((int64_t volatile *) addr,
                                     (int64_t) (-delta));
}

#else

#define OPAL_HAVE_ATOMIC_CMPSET_64 0
#define OPAL_HAVE_ATOMIC_MATH_64 0
#define OPAL_HAVE_ATOMIC_ADD_64 0
#define OPAL_HAVE_ATOMIC_SUB_64 0

#endif  /* HAVE_INTERLOCKEDCOMPAREEXCHANGE64 */

#endif /* ! OMPI_SYS_ARCH_ATOMIC_H */
