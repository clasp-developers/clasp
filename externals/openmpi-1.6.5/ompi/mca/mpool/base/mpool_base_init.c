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

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "ompi/mca/mpool/base/base.h"

OBJ_CLASS_INSTANCE(mca_mpool_base_selected_module_t, opal_list_item_t, NULL, NULL);
static bool mca_mpool_enable_progress_threads = true;
static bool mca_mpool_enable_mpi_threads = true;
         
/**
 * Function for weeding out mpool modules that don't want to run.
 *
 * Call the init function on all available components to find out if they
 * want to run.  Select all components that don't fail.  Failing modules
 * will be closed and unloaded.  The selected modules will be returned
 * to the caller in a opal_list_t.
 */
int mca_mpool_base_init(bool enable_progress_threads, bool enable_mpi_threads)
{
    mca_mpool_enable_progress_threads = enable_progress_threads;
    mca_mpool_enable_mpi_threads = enable_mpi_threads;
    return OMPI_SUCCESS;
}

