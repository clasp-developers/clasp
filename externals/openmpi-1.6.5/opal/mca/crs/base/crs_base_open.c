/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Evergrid, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/constants.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"
#include "opal/util/output.h"

#include "opal/mca/crs/base/static-components.h"

/*
 * Globals
 */
int  opal_crs_base_output  = -1;
opal_crs_base_module_t opal_crs = {
    NULL, /* crs_init               */
    NULL, /* crs_finalize           */
    NULL, /* crs_checkpoint         */
    NULL, /* crs_restart_cmd        */
    NULL, /* crs_disable_checkpoint */
    NULL, /* crs_enable_checkpoint  */
    NULL, /* crs_prelaunch          */
    NULL  /* crs_reg_thread         */
};
opal_list_t opal_crs_base_components_available;
opal_crs_base_component_t opal_crs_base_selected_component;
char * opal_crs_base_snapshot_dir = NULL;

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
int opal_crs_base_open(void)
{
    int ret, exit_status = OPAL_SUCCESS;
    int value;
    char *str_value = NULL;

    /* Debugging/Verbose output */
    mca_base_param_reg_int_name("crs",
                                "base_verbose",
                                "Verbosity level of the CRS framework",
                                false, false,
                                0, &value);
    if(0 != value) {
        opal_crs_base_output = opal_output_open(NULL);
    } else {
        opal_crs_base_output = -1;
    }
    opal_output_set_verbosity(opal_crs_base_output, value);

    /* Base snapshot directory */
    mca_base_param_reg_string_name("crs",
                                   "base_snapshot_dir",
                                   "The base directory to use when storing snapshots",
                                   true, false,
                                   strdup("/tmp"),
                                   &opal_crs_base_snapshot_dir);

    /* 
     * Which CRS component to open
     *  - NULL or "" = auto-select
     *  - "none" = Empty component
     *  - ow. select that specific component
     */
    mca_base_param_reg_string_name("crs", NULL,
                                   "Which CRS component to use (empty = auto-select)",
                                   false, false,
                                   "none", &str_value);
    
    /* Open up all available components */
    if (OPAL_SUCCESS != (ret = mca_base_components_open("crs", 
                                                        opal_crs_base_output, 
                                                        mca_crs_base_static_components,
                                                        &opal_crs_base_components_available,
                                                        true)) ) {
        if( OPAL_ERR_NOT_FOUND == ret &&
            NULL != str_value &&
            0 == strncmp(str_value, "none", strlen("none")) ) {
            exit_status = OPAL_SUCCESS;
        } else {
            exit_status = OPAL_ERROR;
        }
    }

    if( NULL != str_value ) {
        free(str_value);
    }
    return exit_status;
}
