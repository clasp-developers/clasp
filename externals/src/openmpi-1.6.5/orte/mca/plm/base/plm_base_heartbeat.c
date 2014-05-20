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

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/dss/dss.h"
#include "opal/class/opal_pointer_array.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/name_fns.h"

#include "orte/mca/plm/base/plm_private.h"

#define HEARTBEAT_CK    2

void orte_plm_base_heartbeat(int fd, short event, void *arg)
{
    opal_buffer_t buf;
    orte_plm_cmd_flag_t command = ORTE_PLM_HEARTBEAT_CMD;
    opal_event_t *tmp = (opal_event_t*)arg;
    struct timeval now;
    int rc;
    
    /* setup the buffer */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    
    /* tell the HNP this is a heartbeat */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &command, 1, ORTE_PLM_CMD))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* send heartbeat to HNP */
    if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buf, ORTE_RML_TAG_PLM, 0))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* reset the timer */
    now.tv_sec = orte_heartbeat_rate;
    now.tv_usec = 0;
    opal_evtimer_add(tmp, &now);
    
CLEANUP:
    OBJ_DESTRUCT(&buf);
}

/* this function automatically gets periodically called
 * by the event library so we can check on the state
 * of the various orteds
 */
static void check_heartbeat(int fd, short dummy, void *arg)
{
    int v;
    orte_proc_t *proc;
    orte_job_t *daemons;
    struct timeval timeout;
    bool died = false;
    opal_event_t *tmp = (opal_event_t*)arg;
    struct timeval now;
    
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:base:check_heartbeat",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if we are aborting or shutting down, ignore this */
    if (orte_abnormal_term_ordered || 0 == orte_heartbeat_rate) {
        return;
    }
    
    /* get the job object for the daemons */
    if (NULL == (daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return;
    }
    
    /* get current time */
    gettimeofday(&timeout, NULL);
    
    /* cycle through the daemons - make sure we check them all
     * in case multiple daemons died so all of those that did die
     * can be appropriately flagged
     */
    for (v=1; v < daemons->procs->size; v++) {
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, v))) {
            continue;
        }
        if ((timeout.tv_sec - proc->beat) > HEARTBEAT_CK*orte_heartbeat_rate) {
            /* declare this orted dead */
            proc->state = ORTE_PROC_STATE_ABORTED;
            proc->exit_code = ORTE_ERROR_DEFAULT_EXIT_CODE;
            if (NULL == daemons->aborted_proc) {
                daemons->aborted_proc = proc;
            }
            ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
            died = true;
        }
    }
    
    /* if any daemon died, abort */
    if (died) {
        orte_plm_base_launch_failed(ORTE_PROC_MY_NAME->jobid, -1,
                                    ORTE_ERROR_DEFAULT_EXIT_CODE, ORTE_JOB_STATE_ABORTED);
        return;
    }
    
    /* reset the timer */
    now.tv_sec = HEARTBEAT_CK*orte_heartbeat_rate;
    now.tv_usec = 0;
    opal_evtimer_add(tmp, &now);
}

void orte_plm_base_start_heart(void)
{
    /* if the heartbeat rate > 0, then start the heart */
    if (0 < orte_heartbeat_rate) {
        ORTE_TIMER_EVENT(HEARTBEAT_CK*orte_heartbeat_rate, 0, check_heartbeat);
    }
}
