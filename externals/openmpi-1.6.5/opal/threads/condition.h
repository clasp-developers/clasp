/* 
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
#ifndef OPAL_CONDITION_SPINLOCK_H
#define OPAL_CONDITION_SPINLOCK_H

#include "opal_config.h"
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_TIME_H
#include <time.h>
#endif
#if OPAL_HAVE_POSIX_THREADS
#include <pthread.h>
#elif OPAL_HAVE_SOLARIS_THREADS
#include <thread.h>
#include <synch.h>
#endif

#include "opal/threads/mutex.h"
#include "opal/runtime/opal_progress.h"

#include "opal/runtime/opal_cr.h"

BEGIN_C_DECLS

/*
 * Combine pthread support w/ polled progress to allow run-time selection
 * of threading vs. non-threading progress.
 */

struct opal_condition_t {
    opal_object_t super;
    volatile int c_waiting;
    volatile int c_signaled;
#if OPAL_HAVE_POSIX_THREADS
    pthread_cond_t c_cond;
#elif OPAL_HAVE_SOLARIS_THREADS
    cond_t c_cond;
#endif
};
typedef struct opal_condition_t opal_condition_t;

OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_condition_t);


static inline int opal_condition_wait(opal_condition_t *c, opal_mutex_t *m)
{
    int rc = 0;
    c->c_waiting++;

#if OPAL_ENABLE_DEBUG && !OPAL_HAVE_THREAD_SUPPORT
    if (opal_mutex_check_locks && 0 == m->m_lock_debug) {                                         \
        opal_output(0, "Warning -- mutex not locked in condition_wait"); \
    }                                                                   \
    m->m_lock_debug--;
#endif

    if (opal_using_threads()) {
#if OPAL_HAVE_POSIX_THREADS && OPAL_ENABLE_PROGRESS_THREADS
        rc = pthread_cond_wait(&c->c_cond, &m->m_lock_pthread);
#elif OPAL_HAVE_SOLARIS_THREADS && OPAL_ENABLE_PROGRESS_THREADS
        rc = cond_wait(&c->c_cond, &m->m_lock_solaris);
#else
        if (c->c_signaled) {
            c->c_waiting--;
            opal_mutex_unlock(m);
            opal_progress();
            OPAL_CR_TEST_CHECKPOINT_READY_STALL();
            opal_mutex_lock(m);
            return 0;
        }
        while (c->c_signaled == 0) {
            opal_mutex_unlock(m);
            opal_progress();
            OPAL_CR_TEST_CHECKPOINT_READY_STALL();
            opal_mutex_lock(m);
        }
#endif
    } else {
        while (c->c_signaled == 0) {
            opal_progress();
            OPAL_CR_TEST_CHECKPOINT_READY_STALL();
        }
    }

#if OPAL_ENABLE_DEBUG && !OPAL_HAVE_THREAD_SUPPORT
    m->m_lock_debug++;
#endif

    c->c_signaled--;
    c->c_waiting--;
    return rc;
}

static inline int opal_condition_timedwait(opal_condition_t *c,
                                           opal_mutex_t *m,
                                           const struct timespec *abstime)
{
    struct timeval tv;
    struct timeval absolute;
    int rc = 0;

#if OPAL_ENABLE_DEBUG && !OPAL_HAVE_THREAD_SUPPORT
    if (opal_mutex_check_locks && 0 == m->m_lock_debug) {                                         \
        opal_output(0, "Warning -- mutex not locked in condition_wait"); \
    }                                                                   \
    m->m_lock_debug--;
#endif

    c->c_waiting++;
    if (opal_using_threads()) {
#if OPAL_HAVE_POSIX_THREADS && OPAL_ENABLE_PROGRESS_THREADS
        rc = pthread_cond_timedwait(&c->c_cond, &m->m_lock_pthread, abstime);
#elif OPAL_HAVE_SOLARIS_THREADS && OPAL_ENABLE_PROGRESS_THREADS
        /* deal with const-ness */
        timestruc_t to;
        to.tv_sec = abstime->tv_sec;
        to.tv_nsec = abstime->tv_nsec;
        rc = cond_timedwait(&c->c_cond, &m->m_lock_solaris, &to);
#else
        absolute.tv_sec = abstime->tv_sec;
        absolute.tv_usec = abstime->tv_nsec * 1000;
        gettimeofday(&tv,NULL);
        if (c->c_signaled == 0) {
            do {
                opal_mutex_unlock(m);
                opal_progress();
                gettimeofday(&tv,NULL);
                opal_mutex_lock(m);
                } while (c->c_signaled == 0 &&  
                         (tv.tv_sec <= absolute.tv_sec ||
                          (tv.tv_sec == absolute.tv_sec && tv.tv_usec < absolute.tv_usec)));
        }
#endif
    } else {
        absolute.tv_sec = abstime->tv_sec;
        absolute.tv_usec = abstime->tv_nsec * 1000;
        gettimeofday(&tv,NULL);
        if (c->c_signaled == 0) {
            do {
                opal_progress();
                gettimeofday(&tv,NULL);
                } while (c->c_signaled == 0 &&  
                         (tv.tv_sec <= absolute.tv_sec ||
                          (tv.tv_sec == absolute.tv_sec && tv.tv_usec < absolute.tv_usec)));
        }
    }

#if OPAL_ENABLE_DEBUG && !OPAL_HAVE_THREAD_SUPPORT
    m->m_lock_debug++;
#endif

    if (c->c_signaled != 0) c->c_signaled--;
    c->c_waiting--;
    return rc;
}

static inline int opal_condition_signal(opal_condition_t *c)
{
    if (c->c_waiting) {
        c->c_signaled++;
#if OPAL_HAVE_POSIX_THREADS && OPAL_ENABLE_PROGRESS_THREADS
        if(opal_using_threads()) {
            pthread_cond_signal(&c->c_cond);
        }
#elif OPAL_HAVE_SOLARIS_THREADS && OPAL_ENABLE_PROGRESS_THREADS
        if(opal_using_threads()) {
            cond_signal(&c->c_cond);
        }
#endif
    }
    return 0;
}

static inline int opal_condition_broadcast(opal_condition_t *c)
{
    c->c_signaled = c->c_waiting;
#if OPAL_HAVE_POSIX_THREADS && OPAL_ENABLE_PROGRESS_THREADS
    if(opal_using_threads()) {
        if( 1 == c->c_waiting ) {
            pthread_cond_signal(&c->c_cond);
        } else {
            pthread_cond_broadcast(&c->c_cond);
        }
    }
#elif OPAL_HAVE_SOLARIS_THREADS && OPAL_ENABLE_PROGRESS_THREADS
    if(opal_using_threads()) {
        cond_broadcast(&c->c_cond);
    }
#endif
    return 0;
}

END_C_DECLS

#endif

