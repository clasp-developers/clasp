/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/*
 * utility functions for dealing with remote datatype and op structures
 */

/**
 * Convert a window index number into a module instance.
 */
static inline ompi_osc_rdma_module_t*
ompi_osc_rdma_windx_to_module(uint32_t windx)
{
    int ret;
    ompi_osc_rdma_module_t *module;

    /* find the right module and dispatch */
    OPAL_THREAD_LOCK(&mca_osc_rdma_component.c_lock);
    ret = opal_hash_table_get_value_uint32(&mca_osc_rdma_component.c_modules,
                                           windx,
                                           (void**) (&module));
    OPAL_THREAD_UNLOCK(&mca_osc_rdma_component.c_lock);
    if (OMPI_SUCCESS != ret) {
        opal_output(0, "Could not translate windx %d to a local MPI_Win instance",
                    windx);
        return NULL;
    }

    return module;
}
