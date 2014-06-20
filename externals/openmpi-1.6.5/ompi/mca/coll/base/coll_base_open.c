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

#include "ompi/constants.h"
#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"


#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "ompi/mca/coll/base/static-components.h"


/*
 * Global variables; most of which are loaded by back-ends of MCA
 * variables
 */
int mca_coll_base_output = -1;

int mca_coll_base_crossover = 4;
int mca_coll_base_associative = 1;

bool mca_coll_base_components_opened_valid = false;
opal_list_t mca_coll_base_components_opened;

OBJ_CLASS_INSTANCE(mca_coll_base_module_t, opal_object_t, NULL, NULL);

/*
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_coll_base_open(void)
{
    /* Open an output stream for this framework */

    mca_coll_base_output = opal_output_open(NULL);

    /* Open up all available components */

    if (OMPI_SUCCESS !=
        mca_base_components_open("coll", mca_coll_base_output,
                                 mca_coll_base_static_components,
                                 &mca_coll_base_components_opened, true)) {
        return OMPI_ERROR;
    }
    mca_coll_base_components_opened_valid = true;

    /* All done */

    return OMPI_SUCCESS;
}
