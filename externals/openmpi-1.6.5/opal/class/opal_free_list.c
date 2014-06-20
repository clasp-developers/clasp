/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
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

#include "opal/class/opal_free_list.h"
#include "opal/runtime/opal.h"

static void opal_free_list_construct(opal_free_list_t* fl);
static void opal_free_list_destruct(opal_free_list_t* fl);

OBJ_CLASS_INSTANCE(opal_free_list_t,
                   opal_list_t,
                   opal_free_list_construct,
                   opal_free_list_destruct);
OBJ_CLASS_INSTANCE(opal_free_list_item_t,
                   opal_list_item_t,
                   NULL, NULL);

static void opal_free_list_construct(opal_free_list_t* fl)
{
    OBJ_CONSTRUCT(&fl->fl_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&fl->fl_condition, opal_condition_t);
    fl->fl_max_to_alloc = 0;
    fl->fl_num_allocated = 0;
    fl->fl_num_per_alloc = 0;
    fl->fl_num_waiting = 0;
    fl->fl_elem_size = 0;
    fl->fl_elem_class = 0;
    OBJ_CONSTRUCT(&(fl->fl_allocations), opal_list_t);
}

static void opal_free_list_destruct(opal_free_list_t* fl)
{
    opal_list_item_t *item;

    while (NULL != (item = opal_list_remove_first(&(fl->fl_allocations)))) {
        /* destruct the item (we constructed it), then free the memory chunk */
        OBJ_DESTRUCT(item);
        free(item);
    }

    OBJ_DESTRUCT(&fl->fl_allocations);
    OBJ_DESTRUCT(&fl->fl_condition);
    OBJ_DESTRUCT(&fl->fl_lock);
}

int opal_free_list_init(
    opal_free_list_t *flist,
    size_t elem_size,
    opal_class_t* elem_class,
    int num_elements_to_alloc,
    int max_elements_to_alloc,
    int num_elements_per_alloc)
{
    flist->fl_elem_size = elem_size;
    flist->fl_elem_class = elem_class;
    flist->fl_max_to_alloc = max_elements_to_alloc;
    flist->fl_num_allocated = 0;
    flist->fl_num_per_alloc = num_elements_per_alloc;
    if(num_elements_to_alloc)
        return opal_free_list_grow(flist, num_elements_to_alloc);
    return OPAL_SUCCESS;
}


int opal_free_list_grow(opal_free_list_t* flist, size_t num_elements)
{
    unsigned char* ptr;
    unsigned char* alloc_ptr;
    size_t i;
    size_t mod;

    if (flist->fl_max_to_alloc > 0 && flist->fl_num_allocated + num_elements > flist->fl_max_to_alloc)
        return OPAL_ERR_TEMP_OUT_OF_RESOURCE;

    alloc_ptr = (unsigned char *)malloc((num_elements * flist->fl_elem_size) + 
                                        sizeof(opal_list_item_t) +
                                        opal_cache_line_size);
    if(NULL == alloc_ptr)
        return OPAL_ERR_TEMP_OUT_OF_RESOURCE;

    /* make the alloc_ptr a list item, save the chunk in the allocations list, and
       have ptr point to memory right after the list item structure */
    OBJ_CONSTRUCT(alloc_ptr, opal_list_item_t);
    opal_list_append(&(flist->fl_allocations), (opal_list_item_t*) alloc_ptr);
    ptr = alloc_ptr + sizeof(opal_list_item_t);

    mod = (uintptr_t)ptr % opal_cache_line_size;
    if(mod != 0) {
        ptr += (opal_cache_line_size - mod);
    }

    if (NULL != flist->fl_elem_class) {
        for(i=0; i<num_elements; i++) {
            opal_free_list_item_t* item = (opal_free_list_item_t*)ptr;
            OBJ_CONSTRUCT_INTERNAL(item, flist->fl_elem_class);
            opal_list_append(&(flist->super), &(item->super));
            ptr += flist->fl_elem_size;
        }
    } else {
        for(i=0; i<num_elements; i++) {
            opal_free_list_item_t* item = (opal_free_list_item_t*)ptr;
            opal_list_append(&(flist->super), &(item->super));
            ptr += flist->fl_elem_size;
        }
    }
    flist->fl_num_allocated += num_elements;
    return OPAL_SUCCESS;
}



