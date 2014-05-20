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
 * Copyright (c) 2008-2010 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>
#include <time.h>

#include "support.h"
#include "opal/runtime/opal.h"
#include "opal/constants.h"
#include "opal/threads/threads.h"
#include "opal/threads/condition.h"
#include "opal/sys/atomic.h"


#if !OPAL_HAVE_THREAD_SUPPORT

/* If we don't have thread support, there's no point in running this
   test */

int main(int argc, char *argv[])
{
    printf("OMPI was compiled without thread support -- skipping this test\n");
    return 77;
}

#else

/* Only have the body of this test if we have thread support */

opal_mutex_t mutex;
opal_condition_t thr1_cond;
opal_condition_t thr2_cond;

static volatile int thr1_count = 0;
static volatile int thr2_count = 0;


#define TEST_COUNT 100000


static void* thr1_run(opal_object_t* obj)
{
    int i;
    clock_t c1, c2;
    opal_mutex_lock(&mutex);
    c1 = clock();
    for(i=0; i<TEST_COUNT; i++) {
        opal_condition_wait(&thr1_cond, &mutex); 
        opal_condition_signal(&thr2_cond);
        thr1_count++;
    }
    c2 = clock();
    opal_mutex_unlock(&mutex);
    fprintf(stderr, "thr1: time per iteration: %ld usec\n", (c2 - c1) / TEST_COUNT);
    return NULL;
}

static void* thr2_run(opal_object_t* obj)
{
    int i;
    clock_t c1, c2;
    opal_mutex_lock(&mutex);
    c1 = clock();
    for(i=0; i<TEST_COUNT; i++) {
        opal_condition_signal(&thr1_cond);
        opal_condition_wait(&thr2_cond, &mutex);
        thr2_count++;
    }
    c2 = clock();
    opal_mutex_unlock(&mutex);
    fprintf(stderr, "thr2: time per iteration: %ld usec\n", (c2 - c1) / TEST_COUNT);
    return NULL;
}


int main(int argc, char** argv)
{
    int rc;
    opal_thread_t* thr1;
    opal_thread_t* thr2;

    test_init("opal_condition_t");

    rc = opal_init(&argc, &argv);
    test_verify_int(OPAL_SUCCESS, rc);
    if (OPAL_SUCCESS != rc) {
        test_finalize();
        exit(1);
    }
    opal_set_using_threads(true);

    OBJ_CONSTRUCT(&mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&thr1_cond, opal_condition_t);
    OBJ_CONSTRUCT(&thr2_cond, opal_condition_t);
  
    thr1 = OBJ_NEW(opal_thread_t);
    thr2 = OBJ_NEW(opal_thread_t);
    thr1->t_run = thr1_run;
    thr2->t_run = thr2_run;

    rc = opal_thread_start(thr1);
    test_verify_int(OPAL_SUCCESS, rc);

    rc = opal_thread_start(thr2);
    test_verify_int(OPAL_SUCCESS, rc);
   
    rc = opal_thread_join(thr1, NULL);
    test_verify_int(OPAL_SUCCESS, rc);
    test_verify_int(TEST_COUNT, thr1_count);

    rc = opal_thread_join(thr2, NULL);
    test_verify_int(OPAL_SUCCESS, rc);
    test_verify_int(TEST_COUNT, thr2_count);

    opal_finalize();

    return test_finalize();
}

#endif /* OPAL_HAVE_THREAD_SUPPORT */
