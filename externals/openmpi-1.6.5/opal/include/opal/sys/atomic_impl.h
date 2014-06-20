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
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/* Inline C implementation of the functions defined in atomic.h */

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

/**********************************************************************
 *
 * Atomic math operations
 *
 * All the architectures provide a compare_and_set atomic operations. If
 * they dont provide atomic additions and/or substractions then we can
 * define these operations using the atomic compare_and_set.
 *
 * Some architectures does not provide support for the 64 bits
 * atomic operations. Until we find a better solution let's just
 * undefine all those functions if there is no 64 bit cmpset
 *
 *********************************************************************/
#if OPAL_HAVE_ATOMIC_CMPSET_32

#if !defined(OPAL_HAVE_ATOMIC_ADD_32)
#define OPAL_HAVE_ATOMIC_ADD_32 1
static inline int32_t
opal_atomic_add_32(volatile int32_t *addr, int delta)
{
   int32_t oldval;
   
   do {
      oldval = *addr;
   } while (0 == opal_atomic_cmpset_32(addr, oldval, oldval + delta));
   return (oldval + delta);
}
#endif  /* OPAL_HAVE_ATOMIC_CMPSET_32 */


#if !defined(OPAL_HAVE_ATOMIC_SUB_32)
#define OPAL_HAVE_ATOMIC_SUB_32 1
static inline int32_t
opal_atomic_sub_32(volatile int32_t *addr, int delta)
{
   int32_t oldval;
   
   do {
      oldval = *addr;
   } while (0 == opal_atomic_cmpset_32(addr, oldval, oldval - delta));
   return (oldval - delta);
}
#endif  /* OPAL_HAVE_ATOMIC_SUB_32 */

#endif /* OPAL_HAVE_ATOMIC_CMPSET_32 */


#if OPAL_HAVE_ATOMIC_CMPSET_64

#if !defined(OPAL_HAVE_ATOMIC_ADD_64)
#define OPAL_HAVE_ATOMIC_ADD_64 1
static inline int64_t
opal_atomic_add_64(volatile int64_t *addr, int64_t delta)
{
   int64_t oldval;
   
   do {
      oldval = *addr;
   } while (0 == opal_atomic_cmpset_64(addr, oldval, oldval + delta));
   return (oldval + delta);
}
#endif  /* OPAL_HAVE_ATOMIC_ADD_64 */


#if !defined(OPAL_HAVE_ATOMIC_SUB_64)
#define OPAL_HAVE_ATOMIC_SUB_64 1
static inline int64_t
opal_atomic_sub_64(volatile int64_t *addr, int64_t delta)
{
    int64_t oldval;

    do {
        oldval = *addr;
    } while (0 == opal_atomic_cmpset_64(addr, oldval, oldval - delta));
    return (oldval - delta);
}
#endif  /* OPAL_HAVE_ATOMIC_SUB_64 */

#else

#if !defined(OPAL_HAVE_ATOMIC_ADD_64)
#define OPAL_HAVE_ATOMIC_ADD_64 0
#endif

#if !defined(OPAL_HAVE_ATOMIC_SUB_64)
#define OPAL_HAVE_ATOMIC_SUB_64 0
#endif

#endif  /* OPAL_HAVE_ATOMIC_CMPSET_64 */


#if (OPAL_HAVE_ATOMIC_CMPSET_32 || OPAL_HAVE_ATOMIC_CMPSET_64)

static inline int
opal_atomic_cmpset_xx(volatile void* addr, int64_t oldval,
                      int64_t newval, size_t length)
{
   switch( length ) {
#if OPAL_HAVE_ATOMIC_CMPSET_32
   case 4:
      return opal_atomic_cmpset_32( (volatile int32_t*)addr,
                                    (int32_t)oldval, (int32_t)newval );
#endif  /* OPAL_HAVE_ATOMIC_CMPSET_32 */

#if OPAL_HAVE_ATOMIC_CMPSET_64
   case 8:
      return opal_atomic_cmpset_64( (volatile int64_t*)addr,
                                    (int64_t)oldval, (int64_t)newval );
#endif  /* OPAL_HAVE_ATOMIC_CMPSET_64 */
   default:
       /* This should never happen, so deliberately abort (hopefully
          leaving a coreful for analysis) */
       abort();
   }
   return 0;  /* always fail */
}


static inline int
opal_atomic_cmpset_acq_xx(volatile void* addr, int64_t oldval,
                          int64_t newval, size_t length)
{
   switch( length ) {
#if OPAL_HAVE_ATOMIC_CMPSET_32
   case 4:
      return opal_atomic_cmpset_acq_32( (volatile int32_t*)addr,
                                        (int32_t)oldval, (int32_t)newval );
#endif  /* OPAL_HAVE_ATOMIC_CMPSET_32 */

#if OPAL_HAVE_ATOMIC_CMPSET_64
   case 8:
      return opal_atomic_cmpset_acq_64( (volatile int64_t*)addr,
                                        (int64_t)oldval, (int64_t)newval );
#endif  /* OPAL_HAVE_ATOMIC_CMPSET_64 */
   default:
       /* This should never happen, so deliberately abort (hopefully
          leaving a coreful for analysis) */
       abort();
   }
   return 0;  /* always fail */
}


static inline int
opal_atomic_cmpset_rel_xx(volatile void* addr, int64_t oldval,
                          int64_t newval, size_t length)
{
   switch( length ) {
#if OPAL_HAVE_ATOMIC_CMPSET_32
   case 4:
      return opal_atomic_cmpset_rel_32( (volatile int32_t*)addr,
                                        (int32_t)oldval, (int32_t)newval );
#endif  /* OPAL_HAVE_ATOMIC_CMPSET_32 */

#if OPAL_HAVE_ATOMIC_CMPSET_64
   case 8:
      return opal_atomic_cmpset_rel_64( (volatile int64_t*)addr,
                                        (int64_t)oldval, (int64_t)newval );
#endif  /* OPAL_HAVE_ATOMIC_CMPSET_64 */
   default:
       /* This should never happen, so deliberately abort (hopefully
          leaving a coreful for analysis) */
       abort();
   }
   return 0;  /* always fail */
}


static inline int
opal_atomic_cmpset_ptr(volatile void* addr, 
                       void* oldval, 
                       void* newval)
{
#if SIZEOF_VOID_P == 4 && OPAL_HAVE_ATOMIC_CMPSET_32
    return opal_atomic_cmpset_32((int32_t*) addr, (unsigned long) oldval, 
                                 (unsigned long) newval);
#elif SIZEOF_VOID_P == 8 && OPAL_HAVE_ATOMIC_CMPSET_64
    return opal_atomic_cmpset_64((int64_t*) addr, (unsigned long) oldval, 
                                 (unsigned long) newval);
#else
    abort();
    return 0;
#endif
}


static inline int
opal_atomic_cmpset_acq_ptr(volatile void* addr, 
                           void* oldval, 
                           void* newval)
{
#if SIZEOF_VOID_P == 4 && OPAL_HAVE_ATOMIC_CMPSET_32
    return opal_atomic_cmpset_acq_32((int32_t*) addr, (unsigned long) oldval, 
                                     (unsigned long) newval);
#elif SIZEOF_VOID_P == 8 && OPAL_HAVE_ATOMIC_CMPSET_64
    return opal_atomic_cmpset_acq_64((int64_t*) addr, (unsigned long) oldval, 
                                     (unsigned long) newval);
#else
    abort();
    return 0;
#endif
}


static inline int opal_atomic_cmpset_rel_ptr(volatile void* addr, 
                                             void* oldval, 
                                             void* newval)
{
#if SIZEOF_VOID_P == 4 && OPAL_HAVE_ATOMIC_CMPSET_32
    return opal_atomic_cmpset_rel_32((int32_t*) addr, (unsigned long) oldval, 
                                     (unsigned long) newval);
#elif SIZEOF_VOID_P == 8 && OPAL_HAVE_ATOMIC_CMPSET_64
    return opal_atomic_cmpset_rel_64((int64_t*) addr, (unsigned long) oldval, 
                                     (unsigned long) newval);
#else
    abort();
    return 0;
#endif
}

#endif /* (OPAL_HAVE_ATOMIC_CMPSET_32 || OPAL_HAVE_ATOMIC_CMPSET_64) */

#if OPAL_HAVE_ATOMIC_MATH_32 || OPAL_HAVE_ATOMIC_MATH_64


static inline void
opal_atomic_add_xx(volatile void* addr, int32_t value, size_t length)
{
   switch( length ) {
#if OPAL_HAVE_ATOMIC_CMPSET_32
   case 4:
      opal_atomic_add_32( (volatile int32_t*)addr, (int32_t)value );
      break;
#endif  /* OPAL_HAVE_ATOMIC_CMPSET_32 */

#if OPAL_HAVE_ATOMIC_CMPSET_64
   case 8:
      opal_atomic_add_64( (volatile int64_t*)addr, (int64_t)value );
      break;
#endif  /* OPAL_HAVE_ATOMIC_CMPSET_64 */
   default:
       /* This should never happen, so deliberately abort (hopefully
          leaving a coreful for analysis) */
       abort();
   }
}


static inline void
opal_atomic_sub_xx(volatile void* addr, int32_t value, size_t length)
{
   switch( length ) {
#if OPAL_HAVE_ATOMIC_CMPSET_32
   case 4:
      opal_atomic_sub_32( (volatile int32_t*)addr, (int32_t)value );
      break;
#endif  /* OPAL_HAVE_ATOMIC_CMPSET_32 */

#if OPAL_HAVE_ATOMIC_CMPSET_64 
   case 8:
      opal_atomic_sub_64( (volatile int64_t*)addr, (int64_t)value );
      break;
#endif  /* OPAL_HAVE_ATOMIC_CMPSET_64 */
   default:
       /* This should never happen, so deliberately abort (hopefully
          leaving a coreful for analysis) */
       abort();
   }
}

#if SIZEOF_VOID_P == 4 && OPAL_HAVE_ATOMIC_CMPSET_32
static inline int32_t opal_atomic_add_ptr( volatile void* addr, 
                                           void* delta )
{
    return opal_atomic_add_32((int32_t*) addr, (unsigned long) delta);
}
#elif SIZEOF_VOID_P == 8 && OPAL_HAVE_ATOMIC_CMPSET_64
static inline int64_t opal_atomic_add_ptr( volatile void* addr, 
                                           void* delta )
{
    return opal_atomic_add_64((int64_t*) addr, (unsigned long) delta);
}
#else
static inline int32_t opal_atomic_add_ptr( volatile void* addr, 
                                           void* delta )
{
    abort();
    return 0;
}
#endif

#if SIZEOF_VOID_P == 4 && OPAL_HAVE_ATOMIC_CMPSET_32
static inline int32_t opal_atomic_sub_ptr( volatile void* addr, 
                                           void* delta )
{
    return opal_atomic_sub_32((int32_t*) addr, (unsigned long) delta);
}
#elif SIZEOF_VOID_P == 8 && OPAL_HAVE_ATOMIC_CMPSET_64
static inline int64_t opal_atomic_sub_ptr( volatile void* addr, 
                                           void* delta )
{
    return opal_atomic_sub_64((int64_t*) addr, (unsigned long) delta);
}
#else
static inline int32_t opal_atomic_sub_ptr( volatile void* addr, 
                                           void* delta )
{
    abort();
    return 0;
}
#endif

#endif /* OPAL_HAVE_ATOMIC_MATH_32 || OPAL_HAVE_ATOMIC_MATH_64 */

/**********************************************************************
 *
 * Atomic spinlocks
 *
 *********************************************************************/
#ifdef OPAL_NEED_INLINE_ATOMIC_SPINLOCKS

/* 
 * Lock initialization function. It set the lock to UNLOCKED.
 */
static inline void
opal_atomic_init( opal_atomic_lock_t* lock, int32_t value )
{
   lock->u.lock = value;
}


static inline int
opal_atomic_trylock(opal_atomic_lock_t *lock)
{
   return opal_atomic_cmpset_acq_32( &(lock->u.lock),
                                  OPAL_ATOMIC_UNLOCKED, OPAL_ATOMIC_LOCKED);
}


static inline void
opal_atomic_lock(opal_atomic_lock_t *lock)
{
   while( !opal_atomic_cmpset_acq_32( &(lock->u.lock),
                                  OPAL_ATOMIC_UNLOCKED, OPAL_ATOMIC_LOCKED) ) {
      while (lock->u.lock == OPAL_ATOMIC_LOCKED) {
         /* spin */ ;
      }
   }
}


static inline void
opal_atomic_unlock(opal_atomic_lock_t *lock)
{
   opal_atomic_wmb();
   lock->u.lock=OPAL_ATOMIC_UNLOCKED;
}

#endif /* OPAL_HAVE_ATOMIC_SPINLOCKS */
