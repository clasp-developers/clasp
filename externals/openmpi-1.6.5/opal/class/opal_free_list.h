/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#ifndef OPAL_FREE_LIST_H
#define OPAL_FREE_LIST_H

#include "opal_config.h"

#include "opal/class/opal_list.h"
#include "opal/threads/condition.h"
#include "opal/constants.h"
#include "opal/prefetch.h"

BEGIN_C_DECLS

struct opal_free_list_t
{
    opal_list_t super;
    size_t fl_max_to_alloc;
    size_t fl_num_allocated;
    size_t fl_num_per_alloc;
    size_t fl_num_waiting;
    size_t fl_elem_size;
    opal_class_t* fl_elem_class;
    opal_mutex_t fl_lock;
    opal_condition_t fl_condition; 
    opal_list_t fl_allocations;
};
typedef struct opal_free_list_t opal_free_list_t;
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_free_list_t);

struct opal_free_list_item_t
{ 
    opal_list_item_t super; 
}; 
typedef struct opal_free_list_item_t opal_free_list_item_t; 
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_free_list_item_t);

/**
 * Initialize a free list.
 *
 * @param free_list (IN)           Free list.
 * @param element_size (IN)        Size of each element.
 * @param element_class (IN)       opal_class_t of element - used to initialize allocated elements.
 * @param num_elements_to_alloc    Initial number of elements to allocate.
 * @param max_elements_to_alloc    Maximum number of elements to allocate.
 * @param num_elements_per_alloc   Number of elements to grow by per allocation.
 */
 
OPAL_DECLSPEC int opal_free_list_init(
    opal_free_list_t *free_list, 
    size_t element_size,
    opal_class_t* element_class,
    int num_elements_to_alloc,
    int max_elements_to_alloc,
    int num_elements_per_alloc);

OPAL_DECLSPEC int opal_free_list_grow(opal_free_list_t* flist, size_t num_elements) __opal_attribute_nonnull__(1);
    
/**
 * Attemp to obtain an item from a free list. 
 *
 * @param fl (IN)        Free list.
 * @param item (OUT)     Allocated item.
 * @param rc (OUT)       OPAL_SUCCESS or error status on failure.
 *
 * If the requested item is not available the free list is grown to 
 * accomodate the request - unless the max number of allocations has 
 * been reached.  If this is the case - an out of resource error is 
 * returned to the caller.
 */
 
#define OPAL_FREE_LIST_GET(fl, item, rc)                                \
{                                                                       \
    if(opal_using_threads()) {                                          \
        opal_mutex_lock(&((fl)->fl_lock));                              \
        item = (opal_free_list_item_t*)                                 \
            opal_list_remove_first(&((fl)->super));                     \
        if( OPAL_UNLIKELY(NULL == item) ) {                             \
            opal_free_list_grow((fl), (fl)->fl_num_per_alloc);          \
            item = (opal_free_list_item_t*)                             \
                opal_list_remove_first(&((fl)->super));                 \
        }                                                               \
        opal_mutex_unlock(&((fl)->fl_lock));                            \
    } else {                                                            \
        item = (opal_free_list_item_t*)                                 \
            opal_list_remove_first(&((fl)->super));                     \
        if( OPAL_UNLIKELY(NULL == item) ) {                             \
            opal_free_list_grow((fl), (fl)->fl_num_per_alloc);          \
            item = (opal_free_list_item_t*)                             \
                opal_list_remove_first(&((fl)->super));                 \
        }                                                               \
    }                                                                   \
    rc = (NULL == item) ?  OPAL_ERR_TEMP_OUT_OF_RESOURCE : OPAL_SUCCESS; \
}

/**
 * Blocking call to obtain an item from a free list.
 *
 * @param fl (IN)        Free list.
 * @param item (OUT)     Allocated item.
 * @param rc (OUT)       OPAL_SUCCESS or error status on failure.
 *
 * If the requested item is not available the free list is grown to 
 * accomodate the request - unless the max number of allocations has 
 * been reached. In this case the caller is blocked until an item
 * is returned to the list.
 */

#define OPAL_FREE_LIST_WAIT(fl, item, rc)                               \
    do {                                                                \
        OPAL_THREAD_LOCK(&((fl)->fl_lock));                             \
        while( NULL == (item = (opal_free_list_item_t*) opal_list_remove_first(&((fl)->super))) ) { \
            if( OPAL_LIKELY((fl)->fl_max_to_alloc <= (fl)->fl_num_allocated) ) { \
                (fl)->fl_num_waiting++;                                 \
                opal_condition_wait(&((fl)->fl_condition), &((fl)->fl_lock)); \
                (fl)->fl_num_waiting--;                                 \
            } else {                                                    \
                opal_free_list_grow((fl), (fl)->fl_num_per_alloc);      \
            }                                                           \
        }                                                               \
        OPAL_THREAD_UNLOCK(&((fl)->fl_lock));                           \
        rc = OPAL_SUCCESS;                                              \
    } while(0)


/**
 * Return an item to a free list. 
 *
 * @param fl (IN)        Free list.
 * @param item (OUT)     Allocated item.
 *
 */
 
#define OPAL_FREE_LIST_RETURN(fl, item)                                 \
    do {                                                                \
        OPAL_THREAD_LOCK(&(fl)->fl_lock);                               \
        opal_list_prepend(&((fl)->super), ((opal_list_item_t*) item));  \
        if( OPAL_UNLIKELY((fl)->fl_num_waiting > 0) ) {                 \
            opal_condition_signal(&((fl)->fl_condition));               \
        }                                                               \
        OPAL_THREAD_UNLOCK(&(fl)->fl_lock);                             \
    } while(0)

END_C_DECLS

#endif 

