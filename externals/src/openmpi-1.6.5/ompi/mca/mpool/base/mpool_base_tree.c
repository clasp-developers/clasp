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
 *                         All rights reserved.5A
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Voltaire. All rights reserved.
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 * Description of the Registration Cache framework
 */

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "orte/util/show_help.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"
#include "ompi/runtime/params.h"
#include "ompi/class/ompi_free_list.h"
#include "ompi/class/ompi_rb_tree.h"
#include "mpool_base_tree.h"


static int num_leaks = 0;
static char *leak_msg = NULL;

static int condition(void *value);
static void action(void *key, void *value);

OBJ_CLASS_INSTANCE(mca_mpool_base_tree_item_t, ompi_free_list_item_t, NULL, NULL); 

/* 
 * use globals for the tree and the tree_item free list.. 
 */
ompi_rb_tree_t mca_mpool_base_tree; 
ompi_free_list_t mca_mpool_base_tree_item_free_list;
static opal_mutex_t tree_lock;

/*
 *  simple minded compare function... 
 */
int mca_mpool_base_tree_node_compare(void * key1, void * key2)
{
    if(key1 < key2)
    {
        return -1;
    }
    else if(key1 > key2)
    {
        return 1;
    }
    else
    {
        return 0;
    }
}

/*
 * initialize the rb tree
 */
int mca_mpool_base_tree_init(void) { 
    int rc;
    OBJ_CONSTRUCT(&mca_mpool_base_tree, ompi_rb_tree_t); 
    OBJ_CONSTRUCT(&mca_mpool_base_tree_item_free_list, ompi_free_list_t); 
    OBJ_CONSTRUCT(&tree_lock, opal_mutex_t);
    rc = ompi_free_list_init_new(&mca_mpool_base_tree_item_free_list, 
            sizeof(mca_mpool_base_tree_item_t), 
            opal_cache_line_size,
            OBJ_CLASS(mca_mpool_base_tree_item_t), 
            0,opal_cache_line_size,
            0, -1 , 4, NULL);
    if(OMPI_SUCCESS == rc) { 
        rc = ompi_rb_tree_init(&mca_mpool_base_tree, mca_mpool_base_tree_node_compare);
    }
    return rc;
}

/* 
 * insert an item in the rb tree 
 */ 
int mca_mpool_base_tree_insert(mca_mpool_base_tree_item_t* item) { 
    int rc;

    OPAL_THREAD_LOCK(&tree_lock);
    rc = ompi_rb_tree_insert(&mca_mpool_base_tree, item->key, item);
    OPAL_THREAD_UNLOCK(&tree_lock);

    return rc;
}

/* 
 * remove an item from the rb tree 
 * Does not put the item back onto the free list. That
 * must be done separately by calling mca_mpool_base_tree_item_put.
 * This allows a caller to remove an item from the tree 
 * before safely cleaning up the item and only then returning it
 * to the free list. If the item is returned to the free list too soon
 * race conditions can occur
 *
 */
int mca_mpool_base_tree_delete(mca_mpool_base_tree_item_t* item) { 
    int rc;

    OPAL_THREAD_LOCK(&tree_lock);
    rc = ompi_rb_tree_delete(&mca_mpool_base_tree, item->key);
    OPAL_THREAD_UNLOCK(&tree_lock);

    return rc;
}

/**
 *  find the item in the rb tree  
 */
mca_mpool_base_tree_item_t* mca_mpool_base_tree_find(void* base) {
    mca_mpool_base_tree_item_t* item;

    OPAL_THREAD_LOCK(&tree_lock);
    item = (mca_mpool_base_tree_item_t*)ompi_rb_tree_find(&mca_mpool_base_tree,
            base);
    OPAL_THREAD_UNLOCK(&tree_lock);

    return item;
}
    
/* 
 * get a tree item from the free list 
 */
mca_mpool_base_tree_item_t* mca_mpool_base_tree_item_get(void) { 
    ompi_free_list_item_t* item = NULL;
    int rc;
    OMPI_FREE_LIST_GET(&mca_mpool_base_tree_item_free_list, 
                       item, 
                       rc); 
    if(OMPI_SUCCESS == rc) { 
        return (mca_mpool_base_tree_item_t*) item; 
    } else { 
        return NULL;
    }
}

/*
 * put an item back into the free list
 */
void mca_mpool_base_tree_item_put(mca_mpool_base_tree_item_t* item) { 
    OMPI_FREE_LIST_RETURN(&mca_mpool_base_tree_item_free_list,
                          &(item->super));
}


/*
 * Print a show_help kind of message for an items still left in the
 * tree
 */
void mca_mpool_base_tree_print(void)
{
    /* If they asked to show 0 leaks, then don't show anything.  */
    if (0 == ompi_debug_show_mpi_alloc_mem_leaks) {
        return;
    }

    num_leaks = 0;
    ompi_rb_tree_traverse(&mca_mpool_base_tree, condition, action);

    if (num_leaks <= ompi_debug_show_mpi_alloc_mem_leaks ||
        ompi_debug_show_mpi_alloc_mem_leaks < 0) {
        orte_show_help("help-mpool-base.txt", "all mem leaks",
                       true, ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                       orte_process_info.nodename,
                       orte_process_info.pid, leak_msg);
    } else {
        int i = num_leaks - ompi_debug_show_mpi_alloc_mem_leaks;
        orte_show_help("help-mpool-base.txt", "some mem leaks",
                       true, ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                       orte_process_info.nodename, 
                       orte_process_info.pid, leak_msg, i,
                       (i > 1) ? "s were" : " was",
                       (i > 1) ? "are" : "is");
    }
    free(leak_msg);
    leak_msg = NULL;
}


/* Condition function for rb traversal */
static int condition(void *value)
{
    return 1;
}


/* Action function for rb traversal */
static void action(void *key, void *value)
{
    char *tmp;
    mca_mpool_base_tree_item_t *item = (mca_mpool_base_tree_item_t *) value;

    if (++num_leaks <= ompi_debug_show_mpi_alloc_mem_leaks ||
        ompi_debug_show_mpi_alloc_mem_leaks < 0) {

        /* We know that we're supposed to make the first one; check on
           successive items if we're supposed to catenate more
           notices. */
        if (NULL == leak_msg) {
            asprintf(&leak_msg, "    %lu bytes at address 0x%lx",
                     (unsigned long) item->num_bytes,
                     (unsigned long) key);
        } else {
            asprintf(&tmp, "%s\n    %lu bytes at address 0x%lx",
                     leak_msg, (unsigned long) item->num_bytes,
                     (unsigned long) key);
            free(leak_msg);
            leak_msg = tmp;
        }
    }
}
