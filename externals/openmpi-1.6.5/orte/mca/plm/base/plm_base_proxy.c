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
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/dss/dss.h"
#include "orte/util/name_fns.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/plm/base/plm_private.h"

int orte_plm_proxy_init(void)
{
    return ORTE_SUCCESS;
}

int orte_plm_proxy_spawn(orte_job_t *jdata)
{
    opal_buffer_t buf;
    orte_plm_cmd_flag_t command;
    orte_std_cntr_t count;
    orte_process_name_t *target;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:proxy spawn child job",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* setup the buffer */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    
    /* tell the recipient we are sending a launch request */
    command = ORTE_PLM_LAUNCH_JOB_CMD;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &command, 1, ORTE_PLM_CMD))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* pack the jdata object */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &jdata, 1, ORTE_JOB))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
        
    }
    
    /* identify who gets this command - the HNP or the local orted */
    if (jdata->controls & ORTE_JOB_CONTROL_LOCAL_SLAVE) {
        target = ORTE_PROC_MY_DAEMON;
    } else {
        target = ORTE_PROC_MY_HNP;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:proxy sending spawn cmd to %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target)));
    
    /* tell the target to launch the job */
    if (0 > (rc = orte_rml.send_buffer(target, &buf, ORTE_RML_TAG_PLM, 0))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    OBJ_DESTRUCT(&buf);
    
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:proxy waiting for response",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* wait for the target's response */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    if (0 > (rc = orte_rml.recv_buffer(ORTE_NAME_WILDCARD, &buf, ORTE_RML_TAG_PLM_PROXY, 0))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }

    /* get the new jobid back in case the caller wants it */
    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &(jdata->jobid), &count, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    if (ORTE_JOBID_INVALID == jdata->jobid) {
        /* something went wrong on far end - go no further */
        rc = ORTE_ERR_FAILED_TO_START;
        goto CLEANUP;
    }
    
    /* good to go! */
    
CLEANUP:
    OBJ_DESTRUCT(&buf);
    
    return rc;
}

int orte_plm_proxy_finalize(void)
{
    return ORTE_SUCCESS;
}
