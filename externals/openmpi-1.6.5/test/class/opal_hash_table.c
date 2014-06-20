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
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#include <string.h>
#include "support.h"
#include "opal/class/opal_object.h"
#include "opal/class/opal_hash_table.h"
#include "opal/runtime/opal.h"
#include "opal/constants.h"

static FILE *error_out=NULL;

char *num_keys[] = {
    "1234", "1234",
    "5678", "5678",
    "12450", "12450",
    "45623", "45623",
    NULL
};


char *str_keys[] = {
    "foo", "bar",
    "2", "this cow jumped over the moon",
    "this is another key", "this is another value",
    "key key", "value value",
    NULL
};


char *perm_keys[] = {
    "abcdef", "abcdef",
    "bcdefa", "bcdefa",
    "cdefab", "cdefab",
    "defabc", "defabc",
    "efabcd", "efabcd",
    "fabcde", "fabcde",
    "badcfe", "badcfe",
    "badcef", "badcef",
    "abdcfe", "abdcfe",
    "bcdaef", "bcdaef",
    NULL
};

typedef union {
    uint32_t uvalue;
    void *vvalue;
} value_t;

static void validate_table(opal_hash_table_t *table, char *keys[], int is_numeric_keys)
{
    int         j, ret;
    value_t value;
    
    for ( j = 0; keys[j]; j += 2) {
        if ( 1 == is_numeric_keys ) {
            ret = opal_hash_table_get_value_uint32(table, atoi(keys[j]),
                                                   (void**) &value.uvalue);
        } else {
            ret = opal_hash_table_get_value_ptr(table, keys[j], 
                                                strlen(keys[j]), 
                                                &value.vvalue);
        }
        test_verify_str(keys[j+1], value.vvalue);
    }
    test_verify_int(j/2, opal_hash_table_get_size(table));
}

static void test_htable(opal_hash_table_t *table)
{
    int j;
    fprintf(error_out, "\nTesting integer keys...\n");
    for ( j = 0; num_keys[j]; j += 2)
    {
        opal_hash_table_set_value_uint32(table, atoi(num_keys[j]), num_keys[j+1]);
    }
    validate_table(table, num_keys, 1);
    
    /* remove all values for next test */
    opal_hash_table_remove_all(table);
    test_verify_int(0, opal_hash_table_get_size(table));
    
    fprintf(error_out, "\nTesting string keys...\n");
    for ( j = 0; str_keys[j]; j += 2)
    {
        opal_hash_table_set_value_ptr(table, str_keys[j], strlen(str_keys[j]), str_keys[j+1]);
    }
    validate_table(table, str_keys, 0);
    
    /* remove all values for next test */
    opal_hash_table_remove_all(table);
    test_verify_int(0, opal_hash_table_get_size(table));
    
    fprintf(error_out, "\nTesting collision resolution...\n");
    /* All of the string keys in keys array should
        have the same hash value. */
    for ( j = 0; perm_keys[j]; j += 2)
    {
        opal_hash_table_set_value_ptr(table, perm_keys[j], strlen(perm_keys[j]), perm_keys[j+1]);
    }

    validate_table(table, perm_keys, 0);
    
    /* remove all values for next test */
    opal_hash_table_remove_all(table);
    test_verify_int(0, opal_hash_table_get_size(table));
    
    fprintf(error_out, "\n\n");
}


static void test_dynamic(void)
{
    opal_hash_table_t     *table;
    
    table = OBJ_NEW(opal_hash_table_t);
    if ( NULL == table )
    {
        fprintf(error_out, "Error: Unable to create hash table.\n");
        exit(-1);
    }
    fprintf(error_out, "Testing with dynamically created table...\n");
    opal_hash_table_init(table, 4);
    test_htable(table);
    
    OBJ_RELEASE(table);
}


static void test_static(void)
{
    opal_hash_table_t     table;
    
    OBJ_CONSTRUCT(&table, opal_hash_table_t);
    opal_hash_table_init(&table, 128);

    fprintf(error_out, "Testing with statically created table...\n");
    test_htable(&table);

    OBJ_DESTRUCT(&table);
}


int main(int argc, char **argv)
{
    int rc;

    test_init("opal_hash_table_t");

    rc = opal_init(&argc, &argv);
    test_verify_int(OPAL_SUCCESS, rc);
    if (OPAL_SUCCESS != rc) {
        test_finalize();
        exit(1);
    }

#ifdef STANDALONE
    error_out = stderr;
#else
    error_out = fopen( "./opal_hash_table_test_out.txt", "w" );
    if( error_out == NULL ) error_out = stderr;
#endif
    
    test_dynamic();
    test_static();
#ifndef STANDALONE
    fclose( error_out );
#endif

    opal_finalize();
    
    return test_finalize();
}
