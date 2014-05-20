/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include "opal/class/opal_list.h"
#include "opal/util/output.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "ras_cm.h"


/*
 * Local functions
 */
static int allocate(opal_list_t *nodes);
static int finalize(void);


/*
 * Module APIs
 */
orte_ras_base_module_t orte_ras_cm_module = {
    allocate,
    finalize
};

/**
 * Since the system will be bootstrapping, there is
 * nothing to do here
 */
static int allocate(opal_list_t *nodes)
{
    OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                         "%s ras:cm:allocate: success",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    /* indicate that nodes will be discovered via bootstrap */
    return ORTE_ERR_SYSTEM_WILL_BOOTSTRAP;
}

/*
 * There's really nothing to do here
 */
static int finalize(void)
{
    
    OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                         "%s ras:cm:finalize: success (nothing to do)",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    return ORTE_SUCCESS;
}
