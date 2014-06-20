/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Voltaire All rights reserved.
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_ATOMIC_LIFO_H_HAS_BEEN_INCLUDED
#define OPAL_ATOMIC_LIFO_H_HAS_BEEN_INCLUDED

#include "opal_config.h"
#include "opal/class/opal_list.h"

#if OPAL_HAVE_THREAD_SUPPORT
#include "opal/sys/atomic.h"
#endif  /* OPAL_HAVE_THREAD_SUPPORT */

BEGIN_C_DECLS

/* Atomic Last In First Out lists. If we are in a multi-threaded environment then the
 * atomicity is insured via the compare-and-swap operation, if not we simply do a read
 * and/or a write.
 *
 * There is a trick. The ghost element at the end of the list. This ghost element has
 * the next pointer pointing to itself, therefore we cannot go past the end of the list.
 * With this approach we will never have a NULL element in the list, so we never have
 * to test for the NULL.
 */
struct opal_atomic_lifo_t
{
    opal_object_t     super;
    opal_list_item_t* opal_lifo_head;
    opal_list_item_t  opal_lifo_ghost;
};

typedef struct opal_atomic_lifo_t opal_atomic_lifo_t;

OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_atomic_lifo_t);


/* The ghost pointer will never change. The head will change via an atomic
 * compare-and-swap. On most architectures the reading of a pointer is an
 * atomic operation so we don't have to protect it.
 */
static inline bool opal_atomic_lifo_is_empty( opal_atomic_lifo_t* lifo )
{
    return (lifo->opal_lifo_head == &(lifo->opal_lifo_ghost) ? true : false);
}


/* Add one element to the LIFO. We will return the last head of the list
 * to allow the upper level to detect if this element is the first one in the
 * list (if the list was empty before this operation).
 */
static inline opal_list_item_t* opal_atomic_lifo_push( opal_atomic_lifo_t* lifo,
                                                       opal_list_item_t* item )
{
#if OPAL_HAVE_THREAD_SUPPORT
    do {
        item->opal_list_next = lifo->opal_lifo_head;
	opal_atomic_wmb();
        if( opal_atomic_cmpset_ptr( &(lifo->opal_lifo_head),
                                    (void*)item->opal_list_next,
                                    item ) ) {
            opal_atomic_cmpset_32((volatile int32_t*)&item->item_free, 1, 0);
            return (opal_list_item_t*)item->opal_list_next;
        }
        /* DO some kind of pause to release the bus */
    } while( 1 );
#else
    item->opal_list_next = lifo->opal_lifo_head;
    lifo->opal_lifo_head = item;
    return (opal_list_item_t*)item->opal_list_next;
#endif  /* OPAL_HAVE_THREAD_SUPPORT */
}

/* Retrieve one element from the LIFO. If we reach the ghost element then the LIFO
 * is empty so we return NULL.
 */
static inline opal_list_item_t* opal_atomic_lifo_pop( opal_atomic_lifo_t* lifo )
{
    opal_list_item_t* item;
#if OPAL_HAVE_THREAD_SUPPORT
    while((item = lifo->opal_lifo_head) != &(lifo->opal_lifo_ghost))
    {
	opal_atomic_rmb();
        if(!opal_atomic_cmpset_32((volatile int32_t*)&item->item_free, 0, 1))
            continue;
        if( opal_atomic_cmpset_ptr( &(lifo->opal_lifo_head),
                                    item,
                                    (void*)item->opal_list_next ) )
            break;
        opal_atomic_cmpset_32((volatile int32_t*)&item->item_free, 1, 0);
        /* Do some kind of pause to release the bus */
    } 
#else
    item = lifo->opal_lifo_head;
    lifo->opal_lifo_head = (opal_list_item_t*)item->opal_list_next;
#endif  /* OPAL_HAVE_THREAD_SUPPORT */
    if( item == &(lifo->opal_lifo_ghost) ) return NULL;
    item->opal_list_next = NULL;
    return item;
}

END_C_DECLS

#endif  /* OPAL_ATOMIC_LIFO_H_HAS_BEEN_INCLUDED */

