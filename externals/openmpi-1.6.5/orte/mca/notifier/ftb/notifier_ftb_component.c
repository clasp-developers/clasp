/* -*- C -*-
*
* Copyright (c) 2004-2011 The Trustees of Indiana University and Indiana
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
* This component proxies notification events to the Fault Tolerant
* Backplane (See http://www.mcs.anl.gov/research/cifts/).
* The ORTE notifier severity is translated to the corresponding
* FTB severity before the event is published to the FTB.  
*/

/*
 * includes
 */
#include "orte_config.h"
#include "orte/constants.h"

#include <string.h>

#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"
#include "opal/mca/base/mca_base_param.h"
#include "notifier_ftb.h"

static int orte_notifier_ftb_close(void);
static int orte_notifier_ftb_component_query(mca_base_module_t **module, int *priority);
static int orte_notifier_ftb_register(void);

/*
 * Struct of function pointers that need to be initialized
 */
orte_notifier_ftb_component_t mca_notifier_ftb_component = {
    {
        {
            ORTE_NOTIFIER_BASE_VERSION_1_0_0,

            "ftb", /* MCA module name */
            ORTE_MAJOR_VERSION,  /* MCA module major version */
            ORTE_MINOR_VERSION,  /* MCA module minor version */
            ORTE_RELEASE_VERSION,  /* MCA module release version */

            NULL,
            orte_notifier_ftb_close, /* module close */
            orte_notifier_ftb_component_query, /* module query */
            orte_notifier_ftb_register, /* module register */
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    },

    /* FTB client subscription style */
    "FTB_SUBSCRIPTION_NONE",

    /* Priority */
    10,
};

static int orte_notifier_ftb_close(void)
{

    if (NULL != mca_notifier_ftb_component.subscription_style) {
        free(mca_notifier_ftb_component.subscription_style);
    }

    return ORTE_SUCCESS;
}

static int orte_notifier_ftb_component_query(mca_base_module_t **module, int *priority)
{
    int ret;
    *priority = 0;
    *module = NULL;

    /* Fill the FTB client information structure */
    memset(&ftb_client_info, 0, sizeof(ftb_client_info));
    strcpy(ftb_client_info.event_space, "ftb.mpi.openmpi");

    /* We represent each client with a client name of the form
       openmpi/<hostname>/<PID> as a unique identifier in the
       FTB client namespace */
    sprintf(ftb_client_info.client_name, "ompi%u", orte_process_info.pid);

    sprintf(ftb_client_info.client_jobid, "%u", ORTE_PROC_MY_NAME->jobid);

    strncpy(ftb_client_info.client_subscription_style,
            mca_notifier_ftb_component.subscription_style,
            strlen(mca_notifier_ftb_component.subscription_style));

    /* We try to connect to the FTB backplane now, and we abort
       if we cannot connect for some reason. */
    if (FTB_SUCCESS != (ret = FTB_Connect(&ftb_client_info, &ftb_client_handle))) {
        switch (ret) {
        case FTB_ERR_SUBSCRIPTION_STYLE:
            orte_show_help("help-orte-notifier-ftb.txt",
                           "invalid subscription style",
                           true, ftb_client_info.client_subscription_style);

        case FTB_ERR_INVALID_VALUE:
            orte_show_help("help-orte-notifier-ftb.txt",
                           "invalid value",
                           true);

        default:
            orte_show_help("help-orte-notifier-ftb.txt",
                           "unable to connect",
                           true);
        }
        
        return ORTE_ERR_NOT_FOUND;
    }

    *priority = 10;
    *module = (mca_base_module_t *)&orte_notifier_ftb_module;

    return ORTE_SUCCESS;
}

static int orte_notifier_ftb_register(void)
{

    /* FTB client subscription style */
    mca_base_param_reg_string(&mca_notifier_ftb_component.super.base_version,
                              "subscription_style",
                              "FTB client subscription style. "
                              "Possible values are none, polling, notify and both (polling and notify).",
                              false, false,
                              mca_notifier_ftb_component.subscription_style,
                              &mca_notifier_ftb_component.subscription_style);

    /* Priority */
    mca_base_param_reg_int(&mca_notifier_ftb_component.super.base_version,
                           "priority",
                           "Priority of this component",
                           false, false, 
                           mca_notifier_ftb_component.priority,
                           &mca_notifier_ftb_component.priority);

    return ORTE_SUCCESS;
}
