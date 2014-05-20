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

#include "opal_config.h"
#include "opal/class/opal_atomic_lifo.h"

static void opal_atomic_lifo_construct( opal_atomic_lifo_t* lifo )
{
    OBJ_CONSTRUCT( &(lifo->opal_lifo_ghost), opal_list_item_t );
    lifo->opal_lifo_ghost.opal_list_next = &(lifo->opal_lifo_ghost);
    lifo->opal_lifo_head = &(lifo->opal_lifo_ghost);
}

OBJ_CLASS_INSTANCE( opal_atomic_lifo_t,
                    opal_object_t,
                    opal_atomic_lifo_construct,
                    NULL );
