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
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#define OMPI_BUILDING 0
#include "opal_config.h"

#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif
#include <stdlib.h>
#include <stdio.h>

#include "opal/sys/atomic.h"

#define TEST_REPS 500

int atomic_verbose = 0;

struct start_info {
    int tid;
    int count;
    opal_atomic_lock_t *lock;
};

static int atomic_spinlock_test(opal_atomic_lock_t *lock, int count, int id);

#if OPAL_HAVE_POSIX_THREADS
static void* atomic_spinlock_start(void* arg)
{
    struct start_info *data = (struct start_info*) arg;

    return (void*) (unsigned long) atomic_spinlock_test(data->lock, data->count,
                                        data->tid);
}
#endif


static int
atomic_spinlock_test_th(opal_atomic_lock_t *lock, int count, int id, int thr_count)
{
#if OPAL_HAVE_POSIX_THREADS
    pthread_t *th;
    int tid, ret = 0;
    struct start_info *data;

    th = (pthread_t *) malloc(thr_count * sizeof(pthread_t));
    if (!th) { perror("malloc"); exit(EXIT_FAILURE); }
    data = (struct start_info *) malloc(thr_count * sizeof(struct start_info));
    if (!th) { perror("malloc"); exit(EXIT_FAILURE); }

    for (tid = 0; tid < thr_count; tid++) {
        data[tid].tid = tid;
        data[tid].count = count;
        data[tid].lock = lock;

        if (pthread_create(&th[tid], NULL, atomic_spinlock_start, (void *) &(data[tid])) != 0) {
            perror("pthread_create");
            exit(EXIT_FAILURE);
        }
    }

    /* -- wait for the thread set to finish -- */
    for (tid = 0; tid < thr_count; tid++) {
        void *thread_return;

        if (pthread_join(th[tid], &thread_return) != 0) {
            perror("pthread_join");
            exit(EXIT_FAILURE);
        }

        ret += (int) (unsigned long) thread_return;
    }
    free(data);
    free(th);

    return ret;
#else
    return 77;
#endif
}


static int
atomic_spinlock_test(opal_atomic_lock_t *lock, int count, int id)
{
    int i;

    for (i = 0 ; i < count ; ++i) {
        opal_atomic_lock(lock);
        if (atomic_verbose) { printf("id %03d has the lock (lock)\n", id); }
        opal_atomic_unlock(lock);

        while (opal_atomic_trylock(lock)) { ; }
        if (atomic_verbose) { printf("id %03d has the lock (trylock)\n", id); }
        opal_atomic_unlock(lock);
    }

    return 0;
}


int
main(int argc, char *argv[])
{
    int ret = 77;
    opal_atomic_lock_t lock;
    int num_threads = 1;

    if (argc != 2) {
        printf("*** Incorrect number of arguments.  Skipping test\n");
        return 77;
    }
    num_threads = atoi(argv[1]);

    opal_atomic_init(&lock, OPAL_ATOMIC_UNLOCKED);
    ret = atomic_spinlock_test_th(&lock, TEST_REPS, 0, num_threads);

    return ret;
}
