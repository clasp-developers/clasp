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
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "opal_config.h"

#include "opal/constants.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/pstat/pstat.h"
#include "opal/mca/pstat/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "opal/mca/pstat/base/static-components.h"

/* unsupported functions */
static int opal_pstat_base_unsupported_init(void);
static int opal_pstat_base_unsupported_query(pid_t pid, opal_pstats_t *stats);
static int opal_pstat_base_unsupported_finalize(void);

/*
 * Globals
 */
int opal_pstat_base_output = -1;
opal_list_t opal_pstat_base_components_opened;
opal_pstat_base_component_t *opal_pstat_base_component = NULL;
opal_pstat_base_module_t opal_pstat = {
    opal_pstat_base_unsupported_init,
    opal_pstat_base_unsupported_query,
    opal_pstat_base_unsupported_finalize
};


/*
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int opal_pstat_base_open(void)
{
    opal_pstat_base_output = opal_output_open(NULL);

    /* Open up all available components */
    OBJ_CONSTRUCT( &opal_pstat_base_components_opened, opal_list_t );

    if (OPAL_SUCCESS !=
        mca_base_components_open("pstat", opal_pstat_base_output,
                                 mca_pstat_base_static_components,
                                 &opal_pstat_base_components_opened, 
                                 true)) {
        return OPAL_ERROR;
    }

    /* All done */

    return OPAL_SUCCESS;
}

static int opal_pstat_base_unsupported_init(void)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int opal_pstat_base_unsupported_query(pid_t pid, opal_pstats_t *stats)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int opal_pstat_base_unsupported_finalize(void)
{
    return OPAL_ERR_NOT_SUPPORTED;
}
