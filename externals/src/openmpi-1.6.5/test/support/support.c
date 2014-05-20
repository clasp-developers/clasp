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

#include "opal_config.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>

#include "support.h"

/**
 * A testing support library to provide uniform reporting output
 */

static int opal_n_tests;
static int opal_n_success;
static int opal_n_failures;
static char *opal_description;

void test_init(const char *a)
{
    /* local variables */
    size_t len;

    /* save the descriptive string */
    len = strlen(a);
    opal_description = (char *) malloc(len + 1);
    assert(opal_description);

    strcpy(opal_description, a);

    /* initialize counters */
    opal_n_tests = 0;
    opal_n_success = 0;
    opal_n_failures = 0;

    return;

}


void test_success(void)
{
    opal_n_tests++;
    opal_n_success++;
}


void test_failure(const char *a)
{
    opal_n_tests++;
    opal_n_failures++;

    fprintf(stderr, " Failure : ");
    fprintf(stderr, "%s", a);
    fprintf(stderr, "\n");
    fflush(stderr);
}


int test_verify_str(const char *expected_result, const char *test_result)
{
    size_t len_expect, len_result;
    int return_value;

    return_value = 1;
    len_expect = expected_result ? strlen(expected_result) : 0;
    len_result = test_result ? strlen(test_result) : 0;

    if ((!(len_expect == len_result)) ||
        (0 != strcmp(expected_result, test_result))) {
        test_failure("Comparison failure");
        fprintf(stderr, " Expected result: %s\n", expected_result);
        fprintf(stderr, " Test result: %s\n", test_result);
        fflush(stderr);
        return_value = 0;
    } else {
        test_success();
    }

    return return_value;
}


int test_verify_int(int expected_result, int test_result)
{
    int return_value;

    return_value = 1;
    if (expected_result != test_result) {
        test_failure("Comparison failure");
        fprintf(stderr, " Expected result: %d\n", expected_result);
        fprintf(stderr, " Test result: %d\n", test_result);
        fflush(stderr);
        return_value = 0;
    } else {
        test_success();
    }

    return return_value;
}


int test_finalize(void)
{
    int return_value;

    return_value = 0;

    if (opal_n_tests == opal_n_success) {
        fprintf(stderr, "SUPPORT: OMPI Test Passed: %s: (%d tests)\n",
                opal_description, opal_n_tests);
        fflush(stderr);
    } else {
        fprintf(stderr,
                "SUPPORT: OMPI Test failed: %s (%d of %d failed)\n",
                opal_description, opal_n_failures, opal_n_tests);
        fflush(stderr);
        return_value = 1;
    }

    if (NULL != opal_description)
        free(opal_description);
        
    return return_value;
}


/* note this is for additional output that does NOT go to STDERR but STDOUT */
void test_comment (const char* userstr)
{
	fprintf(stdout, "%s:%s\n", opal_description, userstr);
}


void test_fail_stop(const char *msg, int status)
{
    test_failure(msg);
    test_finalize();
    exit(status);
}
