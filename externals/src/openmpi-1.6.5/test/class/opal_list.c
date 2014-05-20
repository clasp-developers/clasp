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
#include <assert.h>

#include "support.h"
#include "opal/class/opal_list.h"
#include "opal/runtime/opal.h"
#include "opal/constants.h"

/*
 * Data type used for testing
 */
typedef struct test_data {
    /* link list data structure */
    opal_list_item_t ll_element;
    /* test data */
    size_t data;
} test_data_t;

OBJ_CLASS_INSTANCE(test_data_t,
                   opal_list_item_t,
                   NULL, NULL);

int main(int argc, char **argv)
{
    /* local variables */
    opal_list_t list, x;
    size_t indx,i,list_size, tmp_size_1, tmp_size_2,size_elements;
    int error_cnt, rc;
    test_data_t *elements, *ele;
    opal_list_item_t *item;

    rc = opal_init(&argc, &argv);
    test_verify_int(OPAL_SUCCESS, rc);
    if (OPAL_SUCCESS != rc) {
        test_finalize();
        exit(1);
    }

    test_init("opal_list_t");

    /* initialize list */
    OBJ_CONSTRUCT(&list, opal_list_t);
    OBJ_CONSTRUCT(&x, opal_list_t);

    /* check length of list */
    list_size=opal_list_get_size(&list);
    if( 0 == list_size ) {
        test_success();
    } else {
        test_failure(" opal_list_get_size");
    }

    /* check for empty */
    if (opal_list_is_empty(&list)) {
        test_success();
    } else {
        test_failure(" opal_list_is_empty(empty list)");
    }

    /* create test elements */
    size_elements=4;
    elements=(test_data_t *)malloc(sizeof(test_data_t)*size_elements);
    assert(elements);
    for(i=0 ; i < size_elements ; i++) {
        OBJ_CONSTRUCT(elements + i, test_data_t);
        (elements+i)->data=i;
    }

    /* populate list */
    for(i=0 ; i < size_elements ; i++) {
        opal_list_append(&list,(opal_list_item_t *)(elements+i));
    }
    list_size=opal_list_get_size(&list);
    if( list_size == size_elements ) {
        test_success();
    } else {
        test_failure(" populating list");
    }

    /* checking for empty on non-empty list */
    if (!opal_list_is_empty(&list)) {
        test_success();
    } else {
        test_failure(" opal_list_is_empty(non-empty list)");
    }

    /* check that list is ordered as expected */
    i=0;
    error_cnt=0;
    for(ele = (test_data_t *) opal_list_get_first(&list);
            ele != (test_data_t *) opal_list_get_end(&list);
            ele = (test_data_t *) ((opal_list_item_t *)ele)->opal_list_next) {
        if( ele->data != i )
            error_cnt++;
        i++;
    }
    if( 0 == error_cnt ) {
        test_success();
    } else {
        test_failure(" error in list order ");
    }

    /* check opal_list_get_first */
    ele = (test_data_t *)NULL;
    ele = (test_data_t *) opal_list_get_first(&list);
    assert(ele);
    if( 0 == ele->data ) {
        test_success();
    } else {
        test_failure(" error in opal_list_get_first");
    }
    i=0;
    for(ele = (test_data_t *) opal_list_get_first(&list);
            ele != (test_data_t *) opal_list_get_end(&list);
            ele = (test_data_t *) ((opal_list_item_t *)ele)->opal_list_next) {
        i++;
    }
    if( size_elements == i ) {
        test_success();
    } else {
        test_failure(" error in opal_list_get_first - list size changed ");
    }

    /* check opal_list_get_last */
    ele = (test_data_t *)NULL;
    ele = (test_data_t *) opal_list_get_last(&list);
    assert(ele);
    if( (size_elements-1) == ele->data ) {
        test_success();
    } else {
        test_failure(" error in opal_list_get_last");
    }
    i=0;
    for(ele = (test_data_t *) opal_list_get_first(&list);
            ele != (test_data_t *) opal_list_get_end(&list);
            ele = (test_data_t *) ((opal_list_item_t *)ele)->opal_list_next) {
        i++;
    }
    if( size_elements == i ) {
        test_success();
    } else {
        test_failure(" error in opal_list_get_first - list size changed ");
    }

    /* check opal_list_remove_first */
    ele = (test_data_t *)NULL;
    ele = (test_data_t *) opal_list_remove_first(&list);
    assert(ele);
    if( 0 == ele->data ) {
        test_success();
    } else {
        test_failure(" error in opal_list_remove_first");
    }
    i=0;
    for(ele = (test_data_t *) opal_list_get_first(&list);
            ele != (test_data_t *) opal_list_get_end(&list);
            ele = (test_data_t *) ((opal_list_item_t *)ele)->opal_list_next) {
        i++;
    }
    if( (size_elements-1) == i ) {
        test_success();
    } else {
        test_failure(" error in opal_list_remove_first - list size changed ");
    }

    /* test opal_list_prepend */
    opal_list_prepend(&list,(opal_list_item_t *)elements);
    ele = (test_data_t *)NULL;
    ele = (test_data_t *) opal_list_get_first(&list);
    assert(ele);
    if( 0 == ele->data ) {
        test_success();
    } else {
        test_failure(" error in opal_list_prepend");
    }
    i=0;
    for(ele = (test_data_t *) opal_list_get_first(&list);
            ele != (test_data_t *) opal_list_get_end(&list);
            ele = (test_data_t *) ((opal_list_item_t *)ele)->opal_list_next) {
        i++;
    }
    if( size_elements == i ) {
        test_success();
    } else {
        test_failure(" error in opal_list_prepend - list size changed ");
    }

    /* check opal_list_remove_last */
    ele = (test_data_t *)NULL;
    ele = (test_data_t *) opal_list_remove_last(&list);
    assert(ele);
    if( (size_elements-1) == ele->data ) {
        test_success();
    } else {
        test_failure(" error in opal_list_remove_last");
    }
    i=0;
    for(ele = (test_data_t *) opal_list_get_first(&list);
            ele != (test_data_t *) opal_list_get_end(&list);
            ele = (test_data_t *) ((opal_list_item_t *)ele)->opal_list_next) {
        i++;
    }
    if( (size_elements-1) == i ) {
        test_success();
    } else {
        test_failure(" error in opal_list_remove_last - list size changed ");
    }

    /* test opal_list_append */
    opal_list_append(&list,(opal_list_item_t *)(elements+size_elements-1));
    ele = (test_data_t *)NULL;
    ele = (test_data_t *) opal_list_get_last(&list);
    assert(ele);
    if( (size_elements-1) == ele->data ) {
        test_success();
    } else {
        test_failure(" error in opal_list_append");
    }
    i=0;
    for(ele = (test_data_t *) opal_list_get_first(&list);
            ele != (test_data_t *) opal_list_get_end(&list);
            ele = (test_data_t *) ((opal_list_item_t *)ele)->opal_list_next) {
        i++;
    }
    if( size_elements == i ) {
        test_success();
    } else {
        test_failure(" error in opal_list_append - list size changed ");
    }

    /* remove element from list */
    indx=size_elements/2;
    if( 0 == indx )
        indx=1;
    assert(2 <= size_elements);
    ele = (test_data_t *)NULL;
    ele = (test_data_t *) 
        opal_list_remove_item(&list,(opal_list_item_t *)(elements+indx));
    assert(ele);
    if( (indx-1) == ele->data ) {
        test_success();
    } else {
        test_failure(" error in opal_list_remove - previous");
    }
    ele=(test_data_t *)(((opal_list_item_t *)ele)->opal_list_next);
    if( (indx+1) == ele->data ) {
        test_success();
    } else {
        test_failure(" error in opal_list_remove - next");
    }
    i=0;
    for(ele = (test_data_t *) opal_list_get_first(&list);
            ele != (test_data_t *) opal_list_get_end(&list);
            ele = (test_data_t *) ((opal_list_item_t *)ele)->opal_list_next) {
        i++;
    }
    if( (size_elements-1) == i ) {
        test_success();
    } else {
        test_failure(" error in opal_list_remove - list size changed incorrectly");
    }

    /* test the insert function */
    i=opal_list_insert(&list,(opal_list_item_t *)(elements+indx),indx);
    if( 1 == i ) {
        test_success();
    } else {
        test_failure(" error in opal_list_remove_item \n");
    }

    i=0;
    for(ele = (test_data_t *) opal_list_get_first(&list);
            ele != (test_data_t *) opal_list_get_end(&list);
            ele = (test_data_t *) ((opal_list_item_t *)ele)->opal_list_next) {
        i++;
    }
    if( size_elements == i ) {
        test_success();
    } else {
        test_failure(" error in opal_list_insert - incorrect list length");
    }
    i=0;
    error_cnt=0;
    for(ele = (test_data_t *) opal_list_get_first(&list);
            ele != (test_data_t *) opal_list_get_end(&list);
            ele = (test_data_t *) ((opal_list_item_t *)ele)->opal_list_next) {
        if( ele->data != i )
            error_cnt++;
        i++;
    }
    if( 0 == error_cnt ) {
        test_success();
    } else {
        test_failure(" error in list order - opal_list_remove_item ");
    }

    /* test the splice and join functions  */
    list_size = opal_list_get_size(&list);
    for (i = 0, item = opal_list_get_first(&list) ; 
         i < list_size / 2 ; ++i, item = opal_list_get_next(item)) {
    }
    opal_list_splice(&x, opal_list_get_end(&x),
                     &list, item, opal_list_get_end(&list));
    tmp_size_1 = opal_list_get_size(&list);
    tmp_size_2 = opal_list_get_size(&x);
    if (tmp_size_1 != i) {
        test_failure(" error in splice (size of list)");
    } else if (tmp_size_2 != list_size - tmp_size_1) {
        test_failure(" error in splice (size of x)");
    } else {
        test_success();
    }

    opal_list_join(&list, opal_list_get_end(&list), &x);
    tmp_size_1 = opal_list_get_size(&list);
    tmp_size_2 = opal_list_get_size(&x);
    if (tmp_size_1 != list_size) {
        test_failure(" error in join (size of list)");
    } else if (tmp_size_2 != 0) {
        test_failure(" error in join (size of x)");
    } else {
        test_success();
    }

    if (NULL != elements) free(elements);

    opal_finalize();

    return test_finalize();
}
