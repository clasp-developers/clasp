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
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"

#include <stdio.h>

#include "ompi/constants.h"
#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "ompi/mca/topo/base/base.h"


int mca_topo_base_close(void) 
{
    /* We have to close all the components which are open. This might either 
       be the list of opened components or the list of available components. 
       Note that the components which are opened but are not available are 
       already closed */

    if (mca_topo_base_components_opened_valid) {
        mca_base_components_close (mca_topo_base_output,
                                   &mca_topo_base_components_opened, 
                                   NULL, false);
        mca_topo_base_components_opened_valid = false;
    } else if (mca_topo_base_components_available_valid) {
        mca_base_components_close (mca_topo_base_output,
                                   &mca_topo_base_components_available, 
                                   NULL, false);
        mca_topo_base_components_available_valid = false;
    }

    /* Close the output stream for this framework */
    opal_output_close (mca_topo_base_output);

    /*
     * All done
     */
    return OMPI_SUCCESS;
}
