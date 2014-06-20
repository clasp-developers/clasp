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
 * Copyright (c) 2007-2009 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <lsf/lsbatch.h>

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "ras_lsf.h"

/*
 * Local functions
 */

static int orte_ras_lsf_open(void);
static int orte_ras_lsf_close(void);
static int orte_ras_lsf_component_query(mca_base_module_t **module, int *priority);


orte_ras_base_component_t mca_ras_lsf_component = {
    {
        /* Indicate that we are a ras v2.0.0 component (which also
         implies a specific MCA version) */
        
        ORTE_RAS_BASE_VERSION_2_0_0,
        
        "lsf", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_ras_lsf_open,  /* component open  */
        orte_ras_lsf_close, /* component close */
        orte_ras_lsf_component_query
      },
      {
          /* The component is checkpoint ready */
          MCA_BASE_METADATA_PARAM_CHECKPOINT
      }
};


/**
  * component open/close/init function
  */
static int orte_ras_lsf_open(void)
{
    return ORTE_SUCCESS;
}


static int orte_ras_lsf_component_query(mca_base_module_t **module, int *priority)
{
    /* check if lsf is running here */
    if (NULL == getenv("LSB_JOBID") || lsb_init("ORTE launcher") < 0) {
        /* nope, not here */
        *module = NULL;
        return ORTE_ERROR;
    }
    
    *priority = 75;
    *module = (mca_base_module_t *) &orte_ras_lsf_module;
    return ORTE_SUCCESS;
}

/**
 *  Close all subsystems.
 */

static int orte_ras_lsf_close(void)
{
    return ORTE_SUCCESS;
}


