/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#ifdef HAVE_STDLIB_H
#  include <stdlib.h>
#endif
#include "mpi.h"
#include "ompi/peruse/peruse.h"
#include "ompi/peruse/peruse-internal.h"
#include "ompi/constants.h"

static opal_list_t peruse_handle_list;
static opal_mutex_t peruse_handle_list_lock;
int ompi_peruse_initialized = 0;
int ompi_peruse_finalized = 0;

static void ompi_peruse_handle_construct (ompi_peruse_handle_t* p)
{
    OBJ_CONSTRUCT (&(p->lock), opal_mutex_t);
    p->active = 0;
    p->event = PERUSE_EVENT_INVALID;
    p->type = PERUSE_TYPE_INVALID;
    p->comm = MPI_COMM_NULL;
    /* p->file = MPI_FILE_NULL */
    /* p->win = MPI_WIN_NULL; */
    p->fn = NULL;
    p->param = NULL;

    OPAL_THREAD_LOCK (&peruse_handle_list_lock);
    opal_list_append (&peruse_handle_list, (opal_list_item_t*)p);
    OPAL_THREAD_UNLOCK (&peruse_handle_list_lock);
}

static void ompi_peruse_handle_destruct (ompi_peruse_handle_t* p)
{
    OPAL_THREAD_LOCK (&peruse_handle_list_lock);
    opal_list_remove_item (&peruse_handle_list, (opal_list_item_t*)p);
    OPAL_THREAD_UNLOCK (&peruse_handle_list_lock);

    OBJ_DESTRUCT (&(p->lock));
}

OBJ_CLASS_INSTANCE(
    ompi_peruse_handle_t,
    opal_list_item_t,
    ompi_peruse_handle_construct,
    ompi_peruse_handle_destruct);


int ompi_peruse_init (void)
{
    if (ompi_peruse_initialized)
        return OMPI_SUCCESS;
    ompi_peruse_initialized = 1;

    OBJ_CONSTRUCT (&peruse_handle_list, opal_list_t);
    OBJ_CONSTRUCT (&peruse_handle_list_lock, opal_mutex_t);

    return OMPI_SUCCESS;
}


int ompi_peruse_finalize (void)
{
    if (!ompi_peruse_initialized)
        return OMPI_SUCCESS;

    ompi_peruse_finalized = 1;

    OBJ_DESTRUCT (&peruse_handle_list);
    OBJ_DESTRUCT (&peruse_handle_list_lock);

    return OMPI_SUCCESS;
}

