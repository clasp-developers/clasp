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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#include <stdlib.h>
#include <stdarg.h>

#include "opal/util/trace.h"

#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_locks.h"
#include "orte/mca/plm/plm.h"
#include "orte/util/name_fns.h"

#include "orte/mca/errmgr/base/errmgr_private.h"
#include "errmgr_default.h"

/*
 * This function gets called by the PLM when an orted notifies us
 * that a process has aborted
 * Various components will follow their own strategy for dealing with
 * this situation. For this component, we call the provided
 * err_cbfunc if they requested notification on proc aborted.
 * Otherwise, we simply kill the job.
 */
void orte_errmgr_default_proc_aborted(orte_process_name_t *name, int exit_code)
{
    int rc;
    orte_job_t *jdata;
    int i;
    
    /* get the job data object for this process */
    if (NULL == (jdata = orte_get_job_data_object(name->jobid))) {
        /* nothing we can do - abort things */
        goto PROCESS;
    }
    
    if (NULL != jdata->err_cbfunc && (ORTE_PROC_STATE_ABORTED & jdata->err_cbstates)) {
        OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base_output,
                             "%s errmgr:default: proc %s aborted with status %d - calling cbfunc",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(name), exit_code));
        jdata->err_cbfunc(name, ORTE_PROC_STATE_ABORTED, jdata->err_cbdata);
        return;
    }
    
PROCESS:
    /* if we are already in progress, then ignore this call */
    if (!opal_atomic_trylock(&orte_abort_inprogress_lock)) { /* returns 1 if already locked */
        OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base_output,
                             "%s errmgr:default: abort in progress, ignoring proc %s aborted with status %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(name), exit_code));
        
        return;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base_output,
                         "%s errmgr:default: proc %s aborted with status %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(name), exit_code));
    
    orte_job_term_ordered = true;
    
    /* if the proc is a daemon, then we are abnormally terminating */
    if (ORTE_PROC_MY_NAME->jobid == name->jobid) {
        orte_abnormal_term_ordered = true;
    }
    
    /* indicate that all jobs other than the one containing this
     * proc have been ordered to abort - this is necessary to avoid
     * duplicate ordering of "abort".
     *
     * NOTE: be sure to not include the 0 job data location as this
     * contains the daemons!
     */
    for (i=1; i < orte_job_data->size; i++) {
        /* the array may have holes in it as we are recovering
         * jobids as they complete, so check everything
         */
        if (NULL == (jdata = orte_get_job_data_object(name->jobid))) {
            continue;
        }
        if (ORTE_JOB_STATE_ABORTED != jdata->state &&
            ORTE_JOB_STATE_ABORTED_BY_SIG != jdata->state &&
            ORTE_JOB_STATE_ABORTED_WO_SYNC != jdata->state) {
            jdata->state = ORTE_JOB_STATE_ABORT_ORDERED;
        }
    }

    /* tell the plm to terminate all jobs */
    if (ORTE_SUCCESS != (rc = orte_plm.terminate_job(ORTE_JOBID_WILDCARD))) {
        ORTE_ERROR_LOG(rc);
    }
    
    /* set the exit status, just in case whomever called us failed
     * to do so - it can only be done once, so we are protected
     * from overwriting it
     */
    ORTE_UPDATE_EXIT_STATUS(exit_code);
    
    /* just return - let the daemons report back so we can properly
     * know when to actually exit
     */
}

/*
 * This function gets called by the PLM when an orted notifies us that
 * a job failed to start.
 * Various components will follow their own strategy for dealing with
 * this situation. For this component, we simply kill the job.
 */
void orte_errmgr_default_incomplete_start(orte_jobid_t job, int exit_code)
{
    int rc;
    orte_job_t *jdata;
    orte_process_name_t name;
    
    /* get the job data object for this process */
    if (NULL == (jdata = orte_get_job_data_object(job))) {
        /* nothing we can do - abort things */
        goto PROCESS;
    }
    
    if (NULL != jdata->err_cbfunc && (ORTE_PROC_STATE_FAILED_TO_START & jdata->err_cbstates)) {
        OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base_output,
                             "%s errmgr:cm: job %s reported incomplete start with status %d - calling cbfunc",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(job), exit_code));
        name.jobid = job;
        name.vpid = ORTE_VPID_WILDCARD;
        jdata->err_cbfunc(&name, ORTE_PROC_STATE_FAILED_TO_START, jdata->err_cbdata);
        return;
    }
    
PROCESS:
    /* if we are already in progress, then ignore this call */
    if (!opal_atomic_trylock(&orte_abort_inprogress_lock)) { /* returns 1 if already locked */
        OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base_output,
                             "%s errmgr:default: abort in progress, ignoring incomplete start on job %s with status %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(job), exit_code));
        return;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base_output,
                         "%s errmgr:default: job %s reported incomplete start with status %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job), exit_code));

    orte_job_term_ordered = true;
    
    /* tell the plm to terminate all jobs */
    if (ORTE_SUCCESS != (rc = orte_plm.terminate_job(ORTE_JOBID_WILDCARD))) {
        ORTE_ERROR_LOG(rc);
    }
    
    /* set the exit status, just in case whomever called us failed
     * to do so - it can only be done once, so we are protected
     * from overwriting it
     */
    ORTE_UPDATE_EXIT_STATUS(exit_code);
    
    /* just return - let the daemons report back so we can properly
     * know when to actually exit
     */
}

/*
 * Register a callback function upon a change to a specified job state.
 */
int orte_errmgr_default_register_callback(orte_jobid_t job,
                                          orte_proc_state_t state,
                                          orte_err_cb_fn_t cbfunc,
                                          void *cbdata)
{
    orte_job_t *jdata;
    
    /* get the job data object for this process */
    if (NULL == (jdata = orte_get_job_data_object(job))) {
        /* nothing we can do - abort things */
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* update the error callback data */
    jdata->err_cbfunc = cbfunc;
    jdata->err_cbstates = state;
    jdata->err_cbdata = cbdata;
    return ORTE_SUCCESS;
}
