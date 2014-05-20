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
 * Copyright (c) 2011 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "opal_config.h"

#include <string.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/constants.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/maffinity/maffinity.h"
#include "opal/mca/maffinity/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "opal/mca/maffinity/base/static-components.h"


/*
 * Globals
 */
int opal_maffinity_base_output = -1;
bool opal_maffinity_base_components_opened_valid = false;
opal_list_t opal_maffinity_base_components_opened;
bool opal_maffinity_setup = false;

/*
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int opal_maffinity_base_open(void)
{
    int int_value;

    /* Debugging / verbose output */

    mca_base_param_reg_int_name("maffinity", "base_verbose", 
                                "Verbosity level of the maffinity framework",
                                false, false,
                                0, &int_value);
    if (0 != int_value) {
        opal_maffinity_base_output = opal_output_open(NULL);
    } else {
        opal_maffinity_base_output = -1;
    }

    opal_maffinity_base_components_opened_valid = false;

    /* Open up all available components */

    if (OPAL_SUCCESS !=
        mca_base_components_open("maffinity", opal_maffinity_base_output,
                                 mca_maffinity_base_static_components,
                                 &opal_maffinity_base_components_opened, 
                                 true)) {
        return OPAL_ERROR;
    }
    opal_maffinity_base_components_opened_valid = true;

    /* All done */

    return OPAL_SUCCESS;
}
