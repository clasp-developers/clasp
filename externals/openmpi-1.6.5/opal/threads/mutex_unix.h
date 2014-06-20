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
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef  OPAL_MUTEX_UNIX_H
#define  OPAL_MUTEX_UNIX_H 1

/**
 * @file:
 *
 * Mutual exclusion functions: Unix implementation.
 *
 * Functions for locking of critical sections.
 *
 * On unix, use pthreads or our own atomic operations as
 * available.
 */

#include "opal_config.h"

#if OPAL_HAVE_POSIX_THREADS
#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif
#include <errno.h>
#include <stdio.h>
#elif OPAL_HAVE_SOLARIS_THREADS
#include <thread.h>
#include <synch.h>
#endif

#include "opal/class/opal_object.h"
#include "opal/sys/atomic.h"

BEGIN_C_DECLS

struct opal_mutex_t {
    opal_object_t super;

#if OPAL_HAVE_POSIX_THREADS
    pthread_mutex_t m_lock_pthread;
#elif OPAL_HAVE_SOLARIS_THREADS
    mutex_t m_lock_solaris;
#endif

#if !OPAL_HAVE_THREAD_SUPPORT && OPAL_ENABLE_DEBUG
    int m_lock_debug;
    const char *m_lock_file;
    int m_lock_line;
#endif

    opal_atomic_lock_t m_lock_atomic;
};
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_mutex_t);

/************************************************************************
 *
 * mutex operations (non-atomic versions)
 *
 ************************************************************************/

#if OPAL_HAVE_POSIX_THREADS

/************************************************************************
 * POSIX threads
 ************************************************************************/

static inline int opal_mutex_trylock(opal_mutex_t *m)
{
#if OPAL_ENABLE_DEBUG
    int ret = pthread_mutex_trylock(&m->m_lock_pthread);
    if (ret == EDEADLK) {
        errno = ret;
        perror("opal_mutex_trylock()");
        abort();
    }
    return ret;
#else
    return pthread_mutex_trylock(&m->m_lock_pthread);
#endif
}

static inline void opal_mutex_lock(opal_mutex_t *m)
{
#if OPAL_ENABLE_DEBUG
    int ret = pthread_mutex_lock(&m->m_lock_pthread);
    if (ret == EDEADLK) {
        errno = ret;
        perror("opal_mutex_lock()");
        abort();
    }
#else
    pthread_mutex_lock(&m->m_lock_pthread);
#endif
}

static inline void opal_mutex_unlock(opal_mutex_t *m)
{
#if OPAL_ENABLE_DEBUG
    int ret = pthread_mutex_unlock(&m->m_lock_pthread);
    if (ret == EPERM) {
        errno = ret;
        perror("opal_mutex_unlock");
        abort();
    }
#else
    pthread_mutex_unlock(&m->m_lock_pthread);
#endif
}

#elif OPAL_HAVE_SOLARIS_THREADS

/************************************************************************
 * Solaris threads
 ************************************************************************/


static inline int opal_mutex_trylock(opal_mutex_t *m)
{
    return mutex_trylock(&m->m_lock_solaris);
}

static inline void opal_mutex_lock(opal_mutex_t *m)
{
    mutex_lock(&m->m_lock_solaris);
}

static inline void opal_mutex_unlock(opal_mutex_t *m)
{
    mutex_unlock(&m->m_lock_solaris);
}

#elif OPAL_HAVE_ATOMIC_SPINLOCKS

/************************************************************************
 * Spin Locks
 ************************************************************************/

static inline int opal_mutex_trylock(opal_mutex_t *m)
{
    return opal_atomic_trylock(&m->m_lock_atomic);
}

static inline void opal_mutex_lock(opal_mutex_t *m)
{
    opal_atomic_lock(&m->m_lock_atomic);
}

static inline void opal_mutex_unlock(opal_mutex_t *m)
{
    opal_atomic_unlock(&m->m_lock_atomic);
}

#else

#error No mutex definition

#endif


/************************************************************************
 *
 * mutex operations (atomic versions)
 *
 ************************************************************************/

#if OPAL_HAVE_ATOMIC_SPINLOCKS

/************************************************************************
 * Spin Locks
 ************************************************************************/

static inline int opal_mutex_atomic_trylock(opal_mutex_t *m)
{
    return opal_atomic_trylock(&m->m_lock_atomic);
}

static inline void opal_mutex_atomic_lock(opal_mutex_t *m)
{
    opal_atomic_lock(&m->m_lock_atomic);
}

static inline void opal_mutex_atomic_unlock(opal_mutex_t *m)
{
    opal_atomic_unlock(&m->m_lock_atomic);
}

#else

/************************************************************************
 * Standard locking
 ************************************************************************/

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

#endif

END_C_DECLS

#endif                          /* OPAL_MUTEX_UNIX_H */
