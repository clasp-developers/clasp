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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
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
#include "opal/mca/base/mca_base_param.h"


#include "orte/mca/oob/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/oob/base/static-components.h"

#if ORTE_DISABLE_FULL_SUPPORT
/* have to include a bogus function here so that
 * the build system sees at least one function
 * in the library
 */
int mca_oob_base_open(void)
{
    return ORTE_SUCCESS;
}

#else

/*
 * Global variables
 */
mca_oob_t mca_oob;
int mca_oob_base_output = -1;
opal_list_t mca_oob_base_components;
opal_list_t mca_oob_base_modules;

bool orte_oob_base_already_opened = false;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_oob_base_open(void)
{
    /* Sanity check.  This may be able to be removed when the rml/oob
       interface is re-worked (the current infrastructure may invoke
       this function twice: once as a standalone, and once via the rml
       oob component). */
    if (orte_oob_base_already_opened) {
        return ORTE_SUCCESS;
    }

    /* register parameters */
    mca_oob_base_output = opal_output_open(NULL);
    
    /* Open up all available components */
    OBJ_CONSTRUCT(&mca_oob_base_components, opal_list_t);
    OBJ_CONSTRUCT(&mca_oob_base_modules, opal_list_t);
    
    if (ORTE_SUCCESS != 
        mca_base_components_open("oob", mca_oob_base_output,
                                 mca_oob_base_static_components, 
                                 &mca_oob_base_components, true)) {
        return ORTE_ERROR;
    }
    
    /* All done */
    orte_oob_base_already_opened = true;
    
    return ORTE_SUCCESS;
}

#endif /* ORTE_DISABLE_FULL_SUPPORT */

