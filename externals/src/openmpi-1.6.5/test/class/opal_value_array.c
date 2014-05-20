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

/*
 * This test is intended to test the opal_value_array class
 */

#include "opal_config.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "support.h"
#include "opal/class/opal_value_array.h"
#include "opal/runtime/opal.h"

#define NUM_ITEMS 10


int main(int argc, char **argv)
{
    uint64_t i, val;
    uint64_t count;
    opal_value_array_t array;

    test_init("opal_value_array_t");

    i = opal_init(&argc, &argv);
    test_verify_int(OPAL_SUCCESS, i);
    if (OPAL_SUCCESS != i) {
        test_finalize();
        exit(1);
    }

    OBJ_CONSTRUCT(&array, opal_value_array_t);

    opal_value_array_init(&array, sizeof(uint64_t));
    test_verify_int(0, opal_value_array_get_size(&array));

    /* add several items to the array */
    for(i=0; i < NUM_ITEMS; i++) {
        opal_value_array_append_item(&array, &i);
    }
    test_verify_int(NUM_ITEMS, opal_value_array_get_size(&array));

    /* verify contents */
    for(i=0; i < NUM_ITEMS; i++) {
        val = OPAL_VALUE_ARRAY_GET_ITEM(&array, uint64_t, i);
        if (val != i) {
            test_failure("Comparison failure");
            fprintf(stderr, " Expected result: %lld\n", (long long) i);
            fprintf(stderr, " Test result: %lld\n", (long long) val);
            fflush(stderr);
        }
    }

    /* re-init array with new type */
    opal_value_array_init(&array, sizeof(uint64_t));
    test_verify_int(0, opal_value_array_get_size(&array));

    /* set fixed size */
    opal_value_array_set_size(&array, NUM_ITEMS);
    
    /* initialize array */
    count = 0;
    for(i=0; i < NUM_ITEMS; i++) {
        OPAL_VALUE_ARRAY_SET_ITEM(&array, uint64_t, i, count++);
    }

    /* grow it */
    for(i=0; i < NUM_ITEMS; i++) {
        opal_value_array_append_item(&array, &count);
        count++;
    }
    /* check size */
    test_verify_int(count, opal_value_array_get_size(&array));

    /* validate contents */
    for(i=0; i < count; i++) {
        test_verify_int(i, OPAL_VALUE_ARRAY_GET_ITEM(&array, uint64_t, i));
    }
    
    /* remove an item */
    opal_value_array_remove_item(&array, NUM_ITEMS);

    /* check size */
    test_verify_int(count-1, opal_value_array_get_size(&array));

    /* validate contents */
    for(i=0; i < count-1; i++) {
        if(i >= NUM_ITEMS) {
            test_verify_int(i+1, OPAL_VALUE_ARRAY_GET_ITEM(&array, uint64_t, i));
        } else {
            test_verify_int(i, OPAL_VALUE_ARRAY_GET_ITEM(&array, uint64_t, i));
        }
    }
    
    OBJ_DESTRUCT(&array);

    opal_finalize();

    return test_finalize();
}
