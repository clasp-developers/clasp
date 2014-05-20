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
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef  OPAL_MUTEX_WINDOWS_H
#define  OPAL_MUTEX_WINDOWS_H 1

/**
 * @file:
 *
 * Mutual exclusion functions: Windows implementation.
 *
 * Functions for locking of critical sections.
 *
 * On Windows, base everything on InterlockedExchange().
 */

#include "opal_config.h"
#include "opal/class/opal_object.h"
#include "opal/sys/atomic.h"

BEGIN_C_DECLS

struct opal_mutex_t {
    opal_object_t super;
    volatile LONG m_lock;

#if !OPAL_HAVE_THREAD_SUPPORT && OPAL_ENABLE_DEBUG
    int m_lock_debug;
    const char *m_lock_file;
    int m_lock_line;
#endif
};

OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_mutex_t);


static inline int opal_mutex_trylock(opal_mutex_t *m)
{
	return (0 == InterlockedExchange(&m->m_lock, 1) ? 1 : 0);
}


static inline void opal_mutex_lock(opal_mutex_t *m)
{
    while (InterlockedExchange(&m->m_lock, 1)) {
        while (m->m_lock == 1) {
            /* spin */;
        }
    }
}


static inline void opal_mutex_unlock(opal_mutex_t *m)
{
    InterlockedExchange(&m->m_lock, 0);
}


static inline int opal_mutex_atomic_trylock(opal_mutex_t *m)
{
    return opal_mutex_trylock(m);
}


static inline void opal_mutex_atomic_lock(opal_mutex_t *m)
{
   opal_mutex_lock(m);
}


static inline void opal_mutex_atomic_unlock(opal_mutex_t *m)
{
    opal_mutex_unlock(m);
}

END_C_DECLS

#endif  /* OPAL_MUTEX_WINDOWS_H */
