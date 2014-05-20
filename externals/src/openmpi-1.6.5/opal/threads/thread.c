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

#include "opal_config.h"

#include "opal/threads/threads.h"
#include "opal/constants.h"


static void opal_thread_construct(opal_thread_t *t);

OBJ_CLASS_INSTANCE(opal_thread_t,
                   opal_object_t,
                   opal_thread_construct, NULL);


/*
 * Constructor
 */
static void opal_thread_construct(opal_thread_t *t)
{
    t->t_run = 0;
#ifdef __WINDOWS__
    t->t_handle = (HANDLE)NULL;
#elif OPAL_HAVE_POSIX_THREADS
    t->t_handle = (pthread_t) -1;
#elif OPAL_HAVE_SOLARIS_THREADS
    t->t_handle = (thread_t) -1;
#endif
}


#ifdef __WINDOWS__

/************************************************************************
 * Windows threads
 ************************************************************************/

int opal_thread_start(opal_thread_t *t)
{
    DWORD tid;

    if (OPAL_ENABLE_DEBUG) {
        if (NULL == t->t_run || t->t_handle != (HANDLE) -1L) {
            return OPAL_ERR_BAD_PARAM;
        }
    }

    t->t_handle = CreateThread(NULL,    /* default security attributes */
                               0,       /* default stack size */
                               (LPTHREAD_START_ROUTINE) t->t_run,
                               t,       /* argument */
                               0,       /* default creation flags */
                               &tid);

    if (t->t_handle == NULL) {
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}


int opal_thread_join(opal_thread_t *t, void **thr_return)
{
    DWORD rc;

    if (WaitForSingleObject(t->t_handle, INFINITE) != WAIT_OBJECT_0) {
        return OPAL_ERROR;
    }
    if (!GetExitCodeThread(t->t_handle, &rc)) {
        return OPAL_ERROR;
    }

    if( NULL != thr_return ) {
        *thr_return = (void *)((intptr_t)rc);
    }

    return OPAL_SUCCESS;
}


bool opal_thread_self_compare(opal_thread_t *t)
{
    HANDLE thread_handle;
    thread_handle = GetCurrentThread();
    return (thread_handle == t->t_handle ? true : false);
}


opal_thread_t *opal_thread_get_self(void)
{
    opal_thread_t *t = OBJ_NEW(opal_thread_t);
    t->t_handle = GetCurrentThread();
    return t;
}



#elif OPAL_HAVE_POSIX_THREADS

/************************************************************************
 * POSIX threads
 ************************************************************************/

int opal_thread_start(opal_thread_t *t)
{
    int rc;

    if (OPAL_ENABLE_DEBUG) {
        if (NULL == t->t_run || t->t_handle != (pthread_t) -1) {
            return OPAL_ERR_BAD_PARAM;
        }
    }

    rc = pthread_create(&t->t_handle, NULL, (void*(*)(void*)) t->t_run, t);

    return (rc == 0) ? OPAL_SUCCESS : OPAL_ERROR;
}


int opal_thread_join(opal_thread_t *t, void **thr_return)
{
    int rc = pthread_join(t->t_handle, thr_return);
    t->t_handle = (pthread_t) -1;
    return (rc == 0) ? OPAL_SUCCESS : OPAL_ERROR;
}


bool opal_thread_self_compare(opal_thread_t *t)
{
    return t->t_handle == pthread_self();
}


opal_thread_t *opal_thread_get_self(void)
{
    opal_thread_t *t = OBJ_NEW(opal_thread_t);
    t->t_handle = pthread_self();
    return t;
}


#elif OPAL_HAVE_SOLARIS_THREADS

/************************************************************************
 * Solaris threads
 ************************************************************************/

int opal_thread_start(opal_thread_t *t)
{
    int rc;

    if (OPAL_ENABLE_DEBUG) {
        if (NULL == t->t_run || t->t_handle != (thread_t) -1) {
            return OPAL_ERR_BAD_PARAM;
        }
    }

    rc = thr_create(NULL, 0, (void*(*)(void*)) t->t_run, t, NULL,
                    &t->t_handle);

    return (rc == 0) ? OPAL_SUCCESS : OPAL_ERROR;
}


int opal_thread_join(opal_thread_t *t, void **thr_return)
{
    int rc = thr_join(t->t_handle, NULL, thr_return);
    t->t_handle = (thread_t) -1;
    return (rc == 0) ? OPAL_SUCCESS : OPAL_ERROR;
}


bool opal_thread_self_compare(opal_thread_t *t)
{
    return t->t_handle == thr_self();
}


opal_thread_t *opal_thread_get_self(void)
{
    opal_thread_t *t = OBJ_NEW(opal_thread_t);
    t->t_handle = thr_self();
    return t;
}


#else

/************************************************************************
 * No thread support
 ************************************************************************/

int opal_thread_start(opal_thread_t *t)
{
    return OPAL_ERROR;
}


int opal_thread_join(opal_thread_t *t, void **thr_return)
{
    return OPAL_ERROR;
}


bool opal_thread_self_compare(opal_thread_t *t)
{
    return true;
}

opal_thread_t *opal_thread_get_self(void)
{
    return NULL;
}


#endif
