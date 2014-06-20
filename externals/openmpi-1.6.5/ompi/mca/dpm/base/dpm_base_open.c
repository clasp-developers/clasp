/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include "opal/util/output.h"
#include "opal/mca/base/base.h"

#include "opal/mca/base/mca_base_param.h"


#include "ompi/mca/dpm/dpm.h"
#include "ompi/mca/dpm/base/base.h"

#include "ompi/mca/dpm/base/static-components.h"

/*
 * Globals
 */
OMPI_DECLSPEC int  ompi_dpm_base_output  = -1;
OMPI_DECLSPEC ompi_dpm_base_module_t ompi_dpm = {
    NULL,
    ompi_dpm_base_null_connect_accept,
    ompi_dpm_base_null_disconnect,
    ompi_dpm_base_null_spawn,
    ompi_dpm_base_null_dyn_init,
    ompi_dpm_base_null_dyn_finalize,
    ompi_dpm_base_null_mark_dyncomm,
    ompi_dpm_base_null_open_port,
    ompi_dpm_base_null_parse_port, 
    ompi_dpm_base_null_route_to_port,
    ompi_dpm_base_null_close_port,
    NULL
};
opal_list_t ompi_dpm_base_components_available;
ompi_dpm_base_component_t ompi_dpm_base_selected_component;

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
int ompi_dpm_base_open(void)
{
    /* Debugging/Verbose output */
    ompi_dpm_base_output = opal_output_open(NULL);

    /* Open up all available components */
    if (OPAL_SUCCESS !=
        mca_base_components_open("dpm", 
                                 ompi_dpm_base_output, 
                                 mca_dpm_base_static_components,
                                 &ompi_dpm_base_components_available,
                                 true)) {
        return OMPI_ERROR;
    }
    
    return OMPI_SUCCESS;
}
