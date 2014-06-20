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

#undef NDEBUG
#define DEBUG

#include <assert.h>
#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "opal/sys/atomic.h"


/* default options */
int nreps = 100;
int nthreads = 2;
int enable_verbose = 0;

volatile int32_t vol32;
int32_t val32;
int32_t old32;
int32_t new32;

#if OPAL_HAVE_ATOMIC_MATH_64
volatile int64_t vol64;
int64_t val64;
int64_t old64;
int64_t new64;
#endif

volatile int volint;
int valint;
int oldint;
int newint;

volatile void *volptr;
void *oldptr;
void *newptr;


#if OPAL_HAVE_POSIX_THREADS
static void *thread_main(void *arg)
{
    int rank = (int) (unsigned long) arg;
    int i;

    /* thread tests */

    for (i = 0; i < nreps; i++) {
        opal_atomic_add_32(&val32, 5);
#if OPAL_HAVE_ATOMIC_MATH_64
        opal_atomic_add_64(&val64, 5);
#endif
        opal_atomic_add(&valint, 5);
    }

    return (void *) (unsigned long) (rank + 1000);
}
#endif


int main(int argc, char *argv[])
{
#if OPAL_HAVE_POSIX_THREADS
    int tid;
    pthread_t *th;
#endif
    
    if (argc != 2) {
        printf("*** Incorrect number of arguments.  Skipping test\n");
        return 77;
    }
    nthreads = atoi(argv[1]);


    /* first test single-threaded functionality */

    /* -- cmpset 32-bit tests -- */

    vol32 = 42, old32 = 42, new32 = 50;
    assert(opal_atomic_cmpset_32(&vol32, old32, new32) == 1);
    opal_atomic_rmb();
    assert(vol32 == new32);

    vol32 = 42, old32 = 420, new32 = 50;
    assert(opal_atomic_cmpset_32(&vol32, old32, new32) ==  0);
    opal_atomic_rmb();
    assert(vol32 == 42);

    vol32 = 42, old32 = 42, new32 = 50;
    assert(opal_atomic_cmpset_acq_32(&vol32, old32, new32) == 1);
    assert(vol32 == new32);

    vol32 = 42, old32 = 420, new32 = 50;
    assert(opal_atomic_cmpset_acq_32(&vol32, old32, new32) == 0);
    assert(vol32 == 42);

    vol32 = 42, old32 = 42, new32 = 50;
    assert(opal_atomic_cmpset_rel_32(&vol32, old32, new32) ==  1);
    opal_atomic_rmb();
    assert(vol32 == new32);

    vol32 = 42, old32 = 420, new32 = 50;
    assert(opal_atomic_cmpset_rel_32(&vol32, old32, new32) == 0);
    opal_atomic_rmb();
    assert(vol32 == 42);

    /* -- cmpset 64-bit tests -- */

#if OPAL_HAVE_ATOMIC_MATH_64
    vol64 = 42, old64 = 42, new64 = 50;
    assert(1 == opal_atomic_cmpset_64(&vol64, old64, new64));
    opal_atomic_rmb();
    assert(new64 == vol64);

    vol64 = 42, old64 = 420, new64 = 50;
    assert(opal_atomic_cmpset_64(&vol64, old64, new64) == 0);
    opal_atomic_rmb();
    assert(vol64 == 42);

    vol64 = 42, old64 = 42, new64 = 50;
    assert(opal_atomic_cmpset_acq_64(&vol64, old64, new64) == 1);
    assert(vol64 == new64);

    vol64 = 42, old64 = 420, new64 = 50;
    assert(opal_atomic_cmpset_acq_64(&vol64, old64, new64) == 0);
    assert(vol64 == 42);

    vol64 = 42, old64 = 42, new64 = 50;
    assert(opal_atomic_cmpset_rel_64(&vol64, old64, new64) == 1);
    opal_atomic_rmb();
    assert(vol64 == new64);

    vol64 = 42, old64 = 420, new64 = 50;
    assert(opal_atomic_cmpset_rel_64(&vol64, old64, new64) == 0);
    opal_atomic_rmb();
    assert(vol64 == 42);
#endif
    /* -- cmpset int tests -- */

    volint = 42, oldint = 42, newint = 50;
    assert(opal_atomic_cmpset(&volint, oldint, newint) == 1);
    opal_atomic_rmb();
    assert(volint ==newint);

    volint = 42, oldint = 420, newint = 50;
    assert(opal_atomic_cmpset(&volint, oldint, newint) == 0);
    opal_atomic_rmb();
    assert(volint == 42);

    volint = 42, oldint = 42, newint = 50;
    assert(opal_atomic_cmpset_acq(&volint, oldint, newint) == 1);
    assert(volint == newint);

    volint = 42, oldint = 420, newint = 50;
    assert(opal_atomic_cmpset_acq(&volint, oldint, newint) == 0);
    assert(volint == 42);

    volint = 42, oldint = 42, newint = 50;
    assert(opal_atomic_cmpset_rel(&volint, oldint, newint) == 1);
    opal_atomic_rmb();
    assert(volint == newint);

    volint = 42, oldint = 420, newint = 50;
    assert(opal_atomic_cmpset_rel(&volint, oldint, newint) == 0);
    opal_atomic_rmb();
    assert(volint == 42);


    /* -- cmpset ptr tests -- */

    volptr = (void *) 42, oldptr = (void *) 42, newptr = (void *) 50;
    assert(opal_atomic_cmpset_ptr(&volptr, oldptr, newptr) == 1);
    opal_atomic_rmb();
    assert(volptr == newptr);

    volptr = (void *) 42, oldptr = (void *) 420, newptr = (void *) 50;
    assert(opal_atomic_cmpset_ptr(&volptr, oldptr, newptr) == 0);
    opal_atomic_rmb();
    assert(volptr == (void *) 42);

    volptr = (void *) 42, oldptr = (void *) 42, newptr = (void *) 50;
    assert(opal_atomic_cmpset_acq_ptr(&volptr, oldptr, newptr) == 1);
    assert(volptr == newptr);

    volptr = (void *) 42, oldptr = (void *) 420, newptr = (void *) 50;
    assert(opal_atomic_cmpset_acq_ptr(&volptr, oldptr, newptr) == 0);
    assert(volptr == (void *) 42);

    volptr = (void *) 42, oldptr = (void *) 42, newptr = (void *) 50;
    assert(opal_atomic_cmpset_rel_ptr(&volptr, oldptr, newptr) == 1);
    opal_atomic_rmb();
    assert(volptr == newptr);

    volptr = (void *) 42, oldptr = (void *) 420, newptr = (void *) 50;
    assert(opal_atomic_cmpset_rel_ptr(&volptr, oldptr, newptr) == 0);
    opal_atomic_rmb();
    assert(volptr == (void *) 42);

    /* -- add_32 tests -- */

    val32 = 42;
    assert(opal_atomic_add_32(&val32, 5) == (42 + 5));
    opal_atomic_rmb();
    assert((42 + 5) == val32);

    /* -- add_64 tests -- */
#if OPAL_HAVE_ATOMIC_MATH_64
    val64 = 42;
    assert(opal_atomic_add_64(&val64, 5) == (42 + 5));
    opal_atomic_rmb();
    assert((42 + 5) == val64);
#endif
    /* -- add_int tests -- */

    valint = 42;
    opal_atomic_add(&valint, 5);
    opal_atomic_rmb();
    assert((42 + 5) == valint);


    /* threaded tests */

    val32 = 0;
#if OPAL_HAVE_ATOMIC_MATH_64
    val64 = 0ul;
#endif
    valint = 0;

    /* -- create the thread set -- */
#if OPAL_HAVE_POSIX_THREADS
    th = (pthread_t *) malloc(nthreads * sizeof(pthread_t));
    if (!th) {
        perror("malloc");
        exit(EXIT_FAILURE);
    }
    for (tid = 0; tid < nthreads; tid++) {
        if (pthread_create(&th[tid], NULL, thread_main, (void *) (unsigned long) tid) != 0) {
            perror("pthread_create");
            exit(EXIT_FAILURE);
        }
    }

    /* -- wait for the thread set to finish -- */

    for (tid = 0; tid < nthreads; tid++) {
        void *thread_return;

        if (pthread_join(th[tid], &thread_return) != 0) {
            perror("pthread_join");
            exit(EXIT_FAILURE);
        }
    }
    free(th);

    opal_atomic_rmb();
    assert((5 * nthreads * nreps) == val32);
#if OPAL_HAVE_ATOMIC_MATH_64
    opal_atomic_rmb();
    assert((5 * nthreads * nreps) ==  val64);
#endif
    opal_atomic_rmb();
    assert((5 * nthreads * nreps) == valint);
#endif

    return 0;
}
