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
#include "opal/mca/base/base.h"
#include "ompi/mca/coll/base/base.h"
#include "opal/util/output.h"


int mca_coll_base_close(void)
{
    /* Close all components that are still open.  This may be the opened
     * list (if we're in ompi_info), or it may be the available list (if
     * we're anywhere else). */

    if (mca_coll_base_components_opened_valid) {
        mca_base_components_close(mca_coll_base_output,
                                  &mca_coll_base_components_opened, 
                                  NULL, false);
        OBJ_DESTRUCT(&mca_coll_base_components_opened);
        mca_coll_base_components_opened_valid = false;
    } else if (mca_coll_base_components_available_valid) {
        mca_base_components_close(mca_coll_base_output,
                                  &mca_coll_base_components_available,
                                  NULL, false);
        OBJ_DESTRUCT(&mca_coll_base_components_available);
        mca_coll_base_components_available_valid = false;
    }
    opal_output_close(mca_coll_base_output);

    /* All done */

    return OMPI_SUCCESS;
}
