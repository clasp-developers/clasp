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

#ifndef OMPI_SUPPORT_H
#define OMPI_SUPPORT_H

#include <stdio.h>
#include <stdlib.h>

#define TEST_AND_REPORT(res, exp_res, str)  \
    if( res == exp_res ) test_success(); \
    else test_failure(str);

void test_init(const char *a);
void test_success(void);
void test_failure(const char *a);
int test_verify_str(const char *expected_result, const char *test_result);
int test_verify_int(int expected_result, int test_result);
int test_finalize(void);
void test_comment (const char* userstr);
void test_fail_stop(const char *msg, int status);



/*
 * test_verify: Non-fatal assertion macro.
 */

#define test_verify(MESSAGE, EXPR)                               \
    do {                                                         \
        if (!(EXPR)) {                                           \
            char s[256];                                         \
            sprintf(s, "%s:%d: %s: %s\n",                        \
                    __FILE__, __LINE__, MESSAGE, # EXPR);        \
            test_failure(s);                                     \
        } else {                                                 \
            test_success();                                      \
        }                                                        \
    } while (0)

#endif /* OMPI_SUPPORT_H */

