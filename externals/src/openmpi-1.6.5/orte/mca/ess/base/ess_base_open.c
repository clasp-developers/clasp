/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
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


#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"


#include "orte/mca/ess/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "orte/mca/ess/base/static-components.h"

opal_list_t orte_ess_base_components_available;
orte_ess_base_module_t orte_ess = {
    NULL, /* init     */
    NULL, /* finalize */
    NULL, /* abort    */
    NULL  /* ft_event */
};
int orte_ess_base_output;

int
orte_ess_base_open(void)
{
    orte_ess_base_output = opal_output_open(NULL);
    
    OBJ_CONSTRUCT(&orte_ess_base_components_available, opal_list_t);

    /* Open up all available components */
    if (ORTE_SUCCESS != 
        mca_base_components_open("ess", orte_ess_base_output, mca_ess_base_static_components, 
                                 &orte_ess_base_components_available,
                                 true)) {
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}
