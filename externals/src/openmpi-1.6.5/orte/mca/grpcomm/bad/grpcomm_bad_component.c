/* -*- C -*-
*
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
/** @file:
*
* The Open MPI Name Server
*
* The Open MPI Name Server provides unique name ranges for processes
* within the universe. Each universe will have one name server
* running within the seed daemon.  This is done to prevent the
* inadvertent duplication of names.
*/

/*
 * includes
 */
#include "orte_config.h"
#include "orte/constants.h"


#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"


#include "grpcomm_bad.h"

/*
 * Struct of function pointers that need to be initialized
 */
orte_grpcomm_base_component_t mca_grpcomm_bad_component = {
    {
        ORTE_GRPCOMM_BASE_VERSION_2_0_0,
        
        "bad", /* MCA module name */
        ORTE_MAJOR_VERSION,  /* MCA module major version */
        ORTE_MINOR_VERSION,  /* MCA module minor version */
        ORTE_RELEASE_VERSION,  /* MCA module release version */
        orte_grpcomm_bad_open,  /* module open */
        orte_grpcomm_bad_close, /* module close */
        orte_grpcomm_bad_component_query /* module query */
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


/* Open the component */
int orte_grpcomm_bad_open(void)
{
    return ORTE_SUCCESS;
}

int orte_grpcomm_bad_close(void)
{
    return ORTE_SUCCESS;
}

int orte_grpcomm_bad_component_query(mca_base_module_t **module, int *priority)
{
    /* we are a default, so set a low priority so we can be overridden */
    *priority = 10;
    *module = (mca_base_module_t *)&orte_grpcomm_bad_module;
    return ORTE_SUCCESS;    
}
