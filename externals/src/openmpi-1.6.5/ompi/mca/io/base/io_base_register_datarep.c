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
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/mca/base/base.h"
#include "opal/class/opal_list.h"
#include "ompi/constants.h"
#include "ompi/mca/io/io.h"
#include "ompi/mca/io/base/base.h"


int
mca_io_base_register_datarep(char *datarep,
                             MPI_Datarep_conversion_function* read_fn,
                             MPI_Datarep_conversion_function* write_fn,
                             MPI_Datarep_extent_function* extent_fn,
                             void* state)
{
    opal_list_item_t *p;
    const mca_base_component_t *component;
    const mca_io_base_component_2_0_0_t *v200;
    int tmp, ret = OMPI_SUCCESS;

    /* Find the maximum additional number of bytes required by all io
       components for requests and make that the request size */

    for (p = opal_list_get_first(&mca_io_base_components_available); 
         p != opal_list_get_end(&mca_io_base_components_available); 
         p = opal_list_get_next(p)) {
        component = ((mca_base_component_priority_list_item_t *) 
                     p)->super.cli_component;

        /* Only know how to handle v2.0.0 components for now */
        if (component->mca_type_major_version == 2 &&
            component->mca_type_minor_version == 0 &&
            component->mca_type_release_version == 0) {
            v200 = (mca_io_base_component_2_0_0_t *) component;

            /* return first non-good error-code */
            tmp = v200->io_register_datarep(datarep, read_fn, write_fn, 
                                            extent_fn, state);
            ret = (ret == OMPI_SUCCESS) ? tmp : ret;
        }
    }

    return ret;
}

