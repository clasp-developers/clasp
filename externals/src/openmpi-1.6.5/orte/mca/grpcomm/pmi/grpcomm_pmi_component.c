/* -*- C -*-
 *
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Los Alamos National Security, LLC. All
 *                         rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <pmi.h>
#if WANT_CRAY_PMI2_EXT
#include <pmi2.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"

#include "grpcomm_pmi.h"

static int my_priority=5;  /* must be below "bad" module */
static bool started_by_me=false;

/*
 * Struct of function pointers that need to be initialized
 */
orte_grpcomm_base_component_t mca_grpcomm_pmi_component = {
    {
        ORTE_GRPCOMM_BASE_VERSION_2_0_0,
        
        "pmi", /* MCA module name */
        ORTE_MAJOR_VERSION,  /* MCA module major version */
        ORTE_MINOR_VERSION,  /* MCA module minor version */
        ORTE_RELEASE_VERSION,  /* MCA module release version */
        orte_grpcomm_pmi_open,  /* module open */
        orte_grpcomm_pmi_close, /* module close */
        orte_grpcomm_pmi_component_query /* module query */
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


/* Open the component */
int orte_grpcomm_pmi_open(void)
{
    mca_base_component_t *c = &mca_grpcomm_pmi_component.base_version;

    /* make the priority adjustable so users can select
     * pmi for use by apps without affecting daemons
     */
    mca_base_param_reg_int(c, "priority",
                           "Priority of the grpcomm pmi component",
                           false, false, my_priority,
                           &my_priority);

    return ORTE_SUCCESS;
}

int orte_grpcomm_pmi_close(void)
{
#if WANT_CRAY_PMI2_EXT
    if (started_by_me && PMI2_Initialized()) {
        PMI2_Finalize();
    }
#else
    PMI_BOOL initialized;

    /* if we weren't selected, cleanup */
    if (started_by_me &&
        PMI_SUCCESS == PMI_Initialized(&initialized) &&
        PMI_TRUE == initialized) {
        PMI_Finalize();
    }
#endif

    return ORTE_SUCCESS;
}

static bool pmi_startup(void)
{
#if WANT_CRAY_PMI2_EXT
    int spawned, size, rank, appnum;

    if (PMI2_Initialized()) {
        /* already initialized */
        return true;
    }
    /* if we can't startup PMI, we can't be used */
    if (PMI_SUCCESS != PMI2_Init(&spawned, &size, &rank, &appnum)) {
        return false;
    }
    /* flag that we started PMI */
    started_by_me = true;
    /* ignore the info - we'll pick it up elsewhere */
    return true;
#else
    PMI_BOOL initialized;

    if (PMI_SUCCESS != PMI_Initialized(&initialized)) {
        return false;
    }
    if (PMI_TRUE != initialized) {
        if (PMI_SUCCESS != PMI_Init(&initialized)) {
            return false;
        }
        /* flag that we started PMI */
        started_by_me = true;
    }
    return true;
#endif
}

int orte_grpcomm_pmi_component_query(mca_base_module_t **module, int *priority)
{
    /* for now, only use PMI when direct launched */
    if (NULL == orte_process_info.my_hnp_uri &&
        ORTE_PROC_IS_MPI &&
        pmi_startup()) {
        /* if PMI is available, use it */
        *priority = my_priority;
        *module = (mca_base_module_t *)&orte_grpcomm_pmi_module;
        return ORTE_SUCCESS;
    }

    /* we can't run */
    *priority = -1;
    *module = NULL;
    return ORTE_ERROR;
}
