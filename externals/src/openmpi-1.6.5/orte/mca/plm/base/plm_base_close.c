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
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/util/argv.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/plm/base/base.h"
#include "orte/mca/plm/base/plm_private.h"

int orte_plm_base_finalize(void)
{
    int rc;
    
    /* Finalize the selected module */
    orte_plm.finalize();

    /* if we are the HNP, then stop our receive */
    if (ORTE_PROC_IS_HNP) {
        if (ORTE_SUCCESS != (rc = orte_plm_base_comm_stop())) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    return ORTE_SUCCESS;
}


int orte_plm_base_close(void)
{
    /* finalize selected module */
    if (orte_plm_base.selected) {
        orte_plm.finalize();
    }
    
    /* clearout the orted cmd locks */
    OBJ_DESTRUCT(&orte_plm_globals.orted_cmd_lock);
    OBJ_DESTRUCT(&orte_plm_globals.orted_cmd_cond);
    
    /* clearout the spawn locks */
    OBJ_DESTRUCT(&orte_plm_globals.spawn_lock);
    OBJ_DESTRUCT(&orte_plm_globals.spawn_cond);
    
#ifndef __WINDOWS__
    /* clearout the rsh support */
    orte_plm_base_local_slave_finalize();
#endif
    
    /* remove the rsh agent info */
    if (NULL != orte_plm_globals.rsh_agent_argv) {
        opal_argv_free(orte_plm_globals.rsh_agent_argv);
    }
    if (NULL != orte_plm_globals.rsh_agent_path) {
        free(orte_plm_globals.rsh_agent_path);
    }
    
    OBJ_DESTRUCT(&orte_plm_globals.slave_files);
    
    /* Close all open components */
    mca_base_components_close(orte_plm_globals.output, 
                              &orte_plm_base.available_components, 
                              NULL, true);
    OBJ_DESTRUCT(&orte_plm_base.available_components);
    
    return ORTE_SUCCESS;
}
