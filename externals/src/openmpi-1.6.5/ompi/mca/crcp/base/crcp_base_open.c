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


#include "ompi/mca/crcp/crcp.h"
#include "ompi/mca/crcp/base/base.h"

#include "ompi/mca/crcp/base/static-components.h"

/*
 * Globals
 */
OMPI_DECLSPEC int  ompi_crcp_base_output  = -1;
OMPI_DECLSPEC ompi_crcp_base_module_t ompi_crcp = {
    NULL, /* crcp_init               */
    NULL  /* crcp_finalize           */
};
opal_list_t ompi_crcp_base_components_available;
ompi_crcp_base_component_t ompi_crcp_base_selected_component;

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
int ompi_crcp_base_open(void)
{
    char *str_value = NULL;

    ompi_crcp_base_output = opal_output_open(NULL);

    /* 
     * Which CRCP component to open
     *  - NULL or "" = auto-select
     *  - "none" = Empty component
     *  - ow. select that specific component
     * Note: Set the default to NULL here so ompi_info will work correctly,
     *       The 'real' default is set in base_select.c
     */
    mca_base_param_reg_string_name("crcp", NULL,
                                   "Which CRCP component to use (empty = auto-select)",
                                   false, false,
                                   NULL, &str_value);
    if( NULL != str_value ) {
        free(str_value);
    }

    /* Open up all available components */
    if (OPAL_SUCCESS !=
        mca_base_components_open("crcp", 
                                 ompi_crcp_base_output, 
                                 mca_crcp_base_static_components,
                                 &ompi_crcp_base_components_available,
                                 true)) {
        return OMPI_ERROR;
    }
    
    return OMPI_SUCCESS;
}
