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
#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"


#include "ompi/mca/io/io.h"
#include "ompi/mca/io/base/base.h"
#include "ompi/mca/io/base/io_base_request.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#ifdef __WINDOWS__
    const mca_base_component_t *mca_io_base_static_components[] = {NULL};
#else 
#include "ompi/mca/io/base/static-components.h"
#endif

/*
 * Global variables; most of which are loaded by back-ends of MCA
 * variables
 */
int mca_io_base_param = -1;
int mca_io_base_output = -1;

bool mca_io_base_components_opened_valid = false;
opal_list_t mca_io_base_components_opened;

bool mca_io_base_components_available_valid = false;
opal_list_t mca_io_base_components_available;


/*
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_io_base_open(void)
{
    /* Open an output stream for this framework */

    mca_io_base_output = opal_output_open(NULL);

    /* Create some parameters */

    if (0 >
        mca_base_param_reg_int_name("io","base_freelist_initial_size",
                                    "Initial MPI-2 IO request freelist size",
                                    false, false, 16, NULL) ||
        0 >
        mca_base_param_reg_int_name("io", "base_freelist_max_size",
                                    "Max size of the MPI-2 IO request freelist",
                                    false, false, 64, NULL) ||
        0 >
        mca_base_param_reg_int_name("io", "base_freelist_increment",
                                    "Increment size of the MPI-2 IO request freelist",
                                    false, false, 16, NULL)) {
        return OMPI_ERROR;
    }

    /* Open up all available components */

    if (OMPI_SUCCESS != 
        mca_base_components_open("io", mca_io_base_output,
                                 mca_io_base_static_components, 
                                 &mca_io_base_components_opened, true)) {
        return OMPI_ERROR;
    }
    mca_io_base_components_opened_valid = true;

    /* Find the index of the MCA "io" param for selection */
    
    mca_io_base_param = mca_base_param_find("io", "base", NULL);

    /* All done */
    
    return OMPI_SUCCESS;
}
