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

#include "opal_config.h"

#include "opal/threads/mutex.h"

/*
 * If we have progress threads, always default to using threads.
 * Otherwise, wait and see if some upper layer wants to use threads.
 */
bool opal_uses_threads = (bool) OPAL_ENABLE_PROGRESS_THREADS;
bool opal_mutex_check_locks = false;


#ifdef __WINDOWS__

static void opal_mutex_construct(opal_mutex_t *m)
{
    InterlockedExchange(&m->m_lock, 0);
#if !OPAL_HAVE_THREAD_SUPPORT && OPAL_ENABLE_DEBUG
    m->m_lock_debug = 0;
    m->m_lock_file = NULL;
    m->m_lock_line = 0;
#endif  /* !OPAL_HAVE_THREAD_SUPPORT && OPAL_ENABLE_DEBUG */
}

static void opal_mutex_destruct(opal_mutex_t *m)
{
}

#else

static void opal_mutex_construct(opal_mutex_t *m)
{
#if OPAL_HAVE_POSIX_THREADS

#if OPAL_ENABLE_DEBUG
    pthread_mutexattr_t attr;
    pthread_mutexattr_init(&attr);

    /* set type to ERRORCHECK so that we catch recursive locks */
#if OMPI_HAVE_PTHREAD_MUTEX_ERRORCHECK_NP
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK_NP);
#elif OMPI_HAVE_PTHREAD_MUTEX_ERRORCHECK
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK);
#endif /* OMPI_HAVE_PTHREAD_MUTEX_ERRORCHECK_NP */

    pthread_mutex_init(&m->m_lock_pthread, &attr);
    pthread_mutexattr_destroy(&attr);

#else

    /* Without debugging, choose the fastest available mutexes */
    pthread_mutex_init(&m->m_lock_pthread, NULL);

#endif /* OPAL_ENABLE_DEBUG */

#elif OPAL_HAVE_SOLARIS_THREADS
    mutex_init(&m->m_lock_solaris, USYNC_THREAD, NULL);
#endif

#if OPAL_ENABLE_DEBUG && !OPAL_HAVE_THREAD_SUPPORT
    m->m_lock_debug = 0;
    m->m_lock_file = NULL;
    m->m_lock_line = 0;
#endif

#if OPAL_HAVE_ATOMIC_SPINLOCKS
    opal_atomic_init( &m->m_lock_atomic, OPAL_ATOMIC_UNLOCKED );
#endif
}

static void opal_mutex_destruct(opal_mutex_t *m)
{
#if OPAL_HAVE_POSIX_THREADS
    pthread_mutex_destroy(&m->m_lock_pthread);
#elif OPAL_HAVE_SOLARIS_THREADS
    mutex_destroy(&m->m_lock_solaris);
#endif
}

#endif /* __WINDOWS__ */

OBJ_CLASS_INSTANCE(opal_mutex_t,
                   opal_object_t,
                   opal_mutex_construct,
                   opal_mutex_destruct);
