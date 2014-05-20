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
*/

/*
 * includes
 */
#include "orte_config.h"
#include "orte/constants.h"

#include "notifier_syslog.h"

/*
 * Struct of function pointers that need to be initialized
 */
orte_notifier_base_component_t mca_notifier_syslog_component = {
    {
        ORTE_NOTIFIER_BASE_VERSION_1_0_0,
        
        "syslog", /* MCA module name */
        ORTE_MAJOR_VERSION,  /* MCA module major version */
        ORTE_MINOR_VERSION,  /* MCA module minor version */
        ORTE_RELEASE_VERSION,  /* MCA module release version */
        orte_notifier_syslog_open,  /* module open */
        orte_notifier_syslog_close, /* module close */
        orte_notifier_syslog_component_query /* module query */
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

/* Open the component */
int orte_notifier_syslog_open(void)
{
    return ORTE_SUCCESS;
}

int orte_notifier_syslog_close(void)
{
    return ORTE_SUCCESS;
}

int orte_notifier_syslog_component_query(mca_base_module_t **module, int *priority)
{
    /* we are a lower-level default, so set a low priority so we can be overridden */
    *priority = 1;
    *module = (mca_base_module_t *)&orte_notifier_syslog_module;
    return ORTE_SUCCESS;    
}
