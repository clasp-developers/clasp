/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef  ORTE_MUTEX_H
#define  ORTE_MUTEX_H

#include "orte_config.h"

#include "opal/sys/atomic.h"
#include "opal/threads/mutex.h"
#if OPAL_ENABLE_DEBUG
#include "opal/util/output.h"
#endif

BEGIN_C_DECLS

/* Lock a mutex */
#define ORTE_THREAD_LOCK(mutex) opal_mutex_lock(mutex)

/**
 * Try to lock a mutex
 * Returns 0 if mutex was locked, non-zero otherwise.
 */
#define ORTE_THREAD_TRYLOCK(mutex) opal_mutex_trylock(mutex)

/** Unlock a mutex */
#define ORTE_THREAD_UNLOCK(mutex) opal_mutex_unlock(mutex)


/* Lock a mutex */
#define ORTE_THREAD_SCOPED_LOCK(mutex, action)  \
    do {                                        \
        opal_mutex_lock(mutex);                 \
        (action);                               \
        opal_mutex_unlock(mutex);               \
    } while (0)

/* Use an atomic operation for increment/decrement */

#define ORTE_THREAD_ADD32(x,y) opal_atomic_add_32(x,y)

#define ORTE_THREAD_ADD64(x,y) opal_atomic_add_64(x,y)

#define ORTE_THREAD_ADD_SIZE_T(x,y) opal_atomic_add_size_t(x,y)

#define ORTE_CMPSET(x, y, z) ((*(x) == (y)) ? ((*(x) = (z)), 1) : 0)

#if OPAL_HAVE_ATOMIC_CMPSET_32
#define ORTE_ATOMIC_CMPSET_32(x, y, z) opal_atomic_cmpset_32(x, y, z)
# endif

# if OPAL_HAVE_ATOMIC_CMPSET_64
#define ORTE_ATOMIC_CMPSET_64(x, y, z) opal_atomic_cmpset_64(x, y, z)
#endif

#if OPAL_HAVE_ATOMIC_CMPSET_32 || OPAL_HAVE_ATOMIC_CMPSET_64
#define ORTE_ATOMIC_CMPSET(x, y, z) opal_atomic_cmpset(x, y, z)
#endif

END_C_DECLS

#endif                          /* ORTE_MUTEX_H */
