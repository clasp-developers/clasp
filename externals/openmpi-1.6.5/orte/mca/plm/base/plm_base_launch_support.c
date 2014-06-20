/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Institut National de Recherche en Informatique
 *                         et Automatique. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */

#include "opal/util/argv.h"
#include "opal/runtime/opal_progress.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss.h"

#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/notifier/notifier.h"
#include "orte/mca/ras/ras.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/odls/odls.h"
#if OPAL_ENABLE_FT_CR == 1
#include "orte/mca/snapc/snapc.h"
#endif
#include "orte/mca/filem/filem.h"
#include "orte/mca/filem/base/base.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_locks.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/util/proc_info.h"
#include "orte/util/regex.h"

#include "orte/mca/odls/odls_types.h"

#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/base/base.h"

static bool active_job_completed_callback = false;

static char *pretty_print_timing(int64_t secs, int64_t usecs);
static void orte_plm_base_proc_state_notify(orte_proc_state_t state, orte_process_name_t *proc);

int orte_plm_base_setup_job(orte_job_t *jdata)
{
    orte_job_t *jdatorted;
    int rc;
    int32_t ljob;
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:setup_job for job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));

    /* if the job is not being restarted, prep it */
    if (ORTE_JOB_STATE_RESTART != jdata->state) {
        /* get a jobid for it */
        if (ORTE_SUCCESS != (rc = orte_plm_base_create_jobid(jdata))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* store it on the global job data pool */
        ljob = ORTE_LOCAL_JOBID(jdata->jobid);
        opal_pointer_array_set_item(orte_job_data, ljob, jdata);
        
        /* get its allocation */
        if (ORTE_SUCCESS != (rc = orte_ras.allocate(jdata))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    if (ORTE_SUCCESS != (rc = orte_rmaps.map_job(jdata))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }         

#if 0
    /* RHC: Please leave this code here - it is needed for
     * rare debugging that doesn't merit a separate debug-flag,
     * but is a pain to have to replicate when needed
     */
    {
        char *crud;
        orte_odls_job_t *jobdat;
        crud = orte_regex_encode_maps(jdata);
        opal_output(0, "maps regex: %s", (NULL == crud) ? "NULL" : crud);
        if (NULL == crud) {
            orte_never_launched = true;
            ORTE_UPDATE_EXIT_STATUS(0);
            orte_trigger_event(&orte_exit);
            return ORTE_ERROR;
        }
        orte_util_nidmap_init(NULL);
        orte_regex_decode_maps(crud, &jobdat);
        free(crud);
        /* print-out the map */
        orte_nidmap_dump();
        orte_jobmap_dump();
        /* printout the jobdat */
        opal_output(orte_clean_output, "****   DUMP OF JOBDAT %s (%d nodes %d procs)   ***",
                    ORTE_JOBID_PRINT(jobdat->jobid), (int)jobdat->num_nodes, (int)(jobdat->num_procs));
        opal_output(orte_clean_output, "\tNum slots: %d\tControl: %x\tStdin: %d",
                    (int)jobdat->total_slots_alloc, jobdat->controls, (int)jobdat->stdin_target);
        opal_output(orte_clean_output, "\tApp: %s", jobdat->apps[0]->app);
        opal_output(orte_clean_output, "\tCwd: %s", jobdat->apps[0]->cwd);
        crud = opal_argv_join(jobdat->apps[0]->argv, ',');
        opal_output(orte_clean_output, "\tArgv: %s", crud);
        free(crud);
        crud = opal_argv_join(jobdat->apps[0]->env, ',');
        opal_output(orte_clean_output, "\tEnv: %s", crud);
        free(crud);
        orte_never_launched = true;
        ORTE_UPDATE_EXIT_STATUS(0);
        orte_trigger_event(&orte_exit);
        return ORTE_ERROR;
    }

    {
        opal_byte_object_t bo;

        /* construct a nodemap */
        if (ORTE_SUCCESS != (rc = orte_util_encode_nodemap(&bo))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (ORTE_SUCCESS != (rc = orte_util_decode_nodemap(&bo))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* print-out the map */
        orte_nidmap_dump();
    }
#endif
    
    /* if we don't want to launch, now is the time to leave */
    if (orte_do_not_launch) {
        orte_never_launched = true;
        ORTE_UPDATE_EXIT_STATUS(0);
        orte_trigger_event(&orte_exit);
        return ORTE_ERROR;
    }
    
    /* quick sanity check - is the stdin target within range
     * of the job?
     */
    if (ORTE_VPID_WILDCARD != jdata->stdin_target &&
        ORTE_VPID_INVALID != jdata->stdin_target &&
        jdata->num_procs <= jdata->stdin_target) {
        /* this request cannot be met */
        orte_show_help("help-plm-base.txt", "stdin-target-out-of-range", true,
                       ORTE_VPID_PRINT(jdata->stdin_target),
                       ORTE_VPID_PRINT(jdata->num_procs));
        orte_never_launched = true;
        ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
        orte_trigger_event(&orte_exit);
        return ORTE_ERROR;
    }
    
    /* get the orted job data object */
    if (NULL == (jdatorted = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    if (orte_process_info.num_procs != jdatorted->num_procs) {
        /* more daemons are being launched - update the routing tree to
         * ensure that the HNP knows how to route messages via
         * the daemon routing tree - this needs to be done
         * here to avoid potential race conditions where the HNP
         * hasn't unpacked its launch message prior to being
         * asked to communicate.
         */
        orte_process_info.num_procs = jdatorted->num_procs;
        if (ORTE_SUCCESS != (rc = orte_routed.update_routing_tree())) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    /*** RHC: USER REQUEST TO TIE-OFF STDXXX TO /DEV/NULL
     *** WILL BE SENT IN LAUNCH MESSAGE AS PART OF CONTROLS FIELD.
     *** SO IF USER WANTS NO IO BEING SENT AROUND, THE ORTEDS
     *** WILL TIE IT OFF AND THE IOF WILL NEVER RECEIVE ANYTHING.
     *** THE IOF AUTOMATICALLY KNOWS TO OUTPUT ANY STDXXX
     *** DATA IT -DOES- RECEIVE TO THE APPROPRIATE FD, SO THERE
     *** IS NOTHING WE NEED DO HERE TO SETUP IOF
     ***/
    
#if OPAL_ENABLE_FT_CR == 1
    /*
     * Notify the Global SnapC component regarding new job
     */
    if( ORTE_SUCCESS != (rc = orte_snapc.setup_job(jdata->jobid) ) ) {
        /* Silent Failure :/ JJH */
        ORTE_ERROR_LOG(rc);
    }
#endif
    
    return ORTE_SUCCESS;
}

static struct timeval app_launch_start, app_launch_stop, launch_msg_sent;

int orte_plm_base_launch_apps(orte_jobid_t job)
{
    orte_job_t *jdata;
    orte_daemon_cmd_flag_t command;
    opal_buffer_t *buffer;
    int rc;
    orte_process_name_t name = {ORTE_JOBID_INVALID, 0};

    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:launch_apps for job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job)));

    if (orte_timing) {
        gettimeofday(&app_launch_start, NULL);
    }
    
    /* find the job's data record */
    if (ORTE_JOBID_INVALID == job) {
	jdata = orte_debugger_daemon;
    } else {
	if (NULL == (jdata = orte_get_job_data_object(job))) {
	    /* bad jobid */
	    ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
	    rc = ORTE_ERR_BAD_PARAM;
	    goto WAKEUP;
	}
    }

    /* setup the buffer */
    buffer = OBJ_NEW(opal_buffer_t);

    /* pack the add_local_procs command */
    command = ORTE_DAEMON_ADD_LOCAL_PROCS;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
        goto WAKEUP;
    }

    /* get the local launcher's required data */
    if (ORTE_SUCCESS != (rc = orte_odls.get_add_procs_data(buffer, job))) {
        ORTE_ERROR_LOG(rc);
        goto WAKEUP;
    }
    
    /* if we are timing, record the time we send this message */
    if (orte_timing) {
        gettimeofday(&launch_msg_sent, NULL);
    }
    
    /* send the command to the daemons */
    if (ORTE_SUCCESS != (rc = orte_grpcomm.xcast(ORTE_PROC_MY_NAME->jobid,
                                                 buffer, ORTE_RML_TAG_DAEMON))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
        goto WAKEUP;
    }
    OBJ_RELEASE(buffer);
    
    /* wait for all the daemons to report apps launched */
    if (ORTE_SUCCESS != (rc = orte_plm_base_report_launched(job))) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:launch failed for job %s on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(job), ORTE_ERROR_NAME(rc)));
        goto WAKEUP;
    }
    
    if (orte_timing) {
        int64_t maxsec, maxusec;

        gettimeofday(&app_launch_stop, NULL);
        /* subtract starting time to get time in microsecs for test */
        maxsec = app_launch_stop.tv_sec - app_launch_start.tv_sec;
        maxusec = app_launch_stop.tv_usec - app_launch_start.tv_usec;
        fprintf(orte_timing_output, "Time to launch apps: %s\n", pretty_print_timing(maxsec, maxusec));
    }
    
    /* complete wiring up the iof */
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:launch wiring up iof",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* push stdin - the IOF will know what to do with the specified target */
    name.jobid = job;
    name.vpid = jdata->stdin_target;
    
    if (ORTE_SUCCESS != (rc = orte_iof.push(&name, ORTE_IOF_STDIN, 0))) {
        ORTE_ERROR_LOG(rc);
        goto WAKEUP;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:launch completed for job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job)));
    
WAKEUP:
    /* wakeup anyone waiting for this */
    orte_plm_globals.spawn_complete = true;
    orte_plm_globals.spawn_status = rc;
    opal_condition_broadcast(&orte_plm_globals.spawn_cond);

    return rc;
}

/* daemons callback when they start - need to listen for them */
static int orted_num_callback;
static bool orted_failed_launch;
static orte_job_t *jdatorted;
static struct timeval daemonlaunchtime = {0,0}, daemonsetuptime = {0,0}, daemoncbtime = {0,0};

void orte_plm_base_launch_failed(orte_jobid_t job, pid_t pid,
                                 int status, orte_job_state_t state)
{
    orte_job_t *jdata;
    char *pidstr;
    int sts;
    
    if (!opal_atomic_trylock(&orte_abort_inprogress_lock)) { /* returns 1 if already locked */
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:base:launch_failed abort in progress, ignoring report",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        return;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:launch_failed for job %s, status %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job), status));

    /* no matter what, we must exit with a non-zero status */
    if (0 == status) {
        sts = ORTE_ERROR_DEFAULT_EXIT_CODE;
    } else {
        sts = status;
    }
    
    /* if we didn't even attempt to launch, then just quietly update
     * the job record and leave
     */
    if (ORTE_JOB_NEVER_LAUNCHED == state) {
        orte_never_launched = true;
        goto PROCESS;
    }
    
    /* if this is the daemon job that failed, set the flag indicating
     * that a daemon failed so we use the proper
     * methods for attempting to shutdown the rest of the system
     */
    if (ORTE_PROC_MY_NAME->jobid == job) {
        /* set the flag indicating that a daemon failed so we use the proper
         * methods for attempting to shutdown the rest of the system
         */
        orte_abnormal_term_ordered = true;
        if (0 < pid) {
            asprintf(&pidstr, "%d", (int)pid);
        } else {
            /* if the pid is negative, then we couldn't get a real pid
             * to report here - so tell someone that
             */
            pidstr = strdup("unknown");
        }
        if (WIFSIGNALED(status)) { /* died on signal */
#ifdef WCOREDUMP
            if (WCOREDUMP(status)) {
                orte_show_help("help-plm-base.txt", "daemon-died-signal-core", true,
                               pidstr, WTERMSIG(status));
                sts = WTERMSIG(status);
            } else {
                orte_show_help("help-plm-base.txt", "daemon-died-signal", true,
                               pidstr, WTERMSIG(status));
                sts = WTERMSIG(status);
            }
#else
            orte_show_help("help-plm-base.txt", "daemon-died-signal", true,
                            pidstr, WTERMSIG(status));
            sts = WTERMSIG(status);
#endif /* WCOREDUMP */
        } else {
            orte_show_help("help-plm-base.txt", "daemon-died-no-signal", true,
                           pidstr, WEXITSTATUS(status));
            sts = WEXITSTATUS(status);
        }
        orted_failed_launch = true;
        free(pidstr);
   }
    
PROCESS:
    /* Set the job state as indicated so orterun's exit status
       will be non-zero
     */
    /* find the job's data record */
    if (NULL == (jdata = orte_get_job_data_object(job))) {
        /* bad jobid */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        goto WAKEUP;
    }
    /* set the state */
    jdata->state = state;
    
WAKEUP:
    /* set orterun's exit code and wakeup so it can exit */
    ORTE_UPDATE_EXIT_STATUS(sts);
    orte_trigger_event(&orte_exit);
}

static void process_orted_launch_report(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    opal_buffer_t *buffer = mev->buffer;
    orte_process_name_t peer;
    char *rml_uri = NULL;
    int rc, idx;
    struct timeval recvtime;
    long secs, usecs;
    int64_t setupsec, setupusec;
    int64_t startsec, startusec;
    orte_proc_t *daemon=NULL;
    
    /* see if we need to timestamp this receipt */
    if (orte_timing) {
        gettimeofday(&recvtime, NULL);
    }
    
    /* unpack its contact info */
    idx = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &rml_uri, &idx, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        orted_failed_launch = true;
        goto CLEANUP;
    }

    /* set the contact info into the hash table */
    if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(rml_uri))) {
        ORTE_ERROR_LOG(rc);
        orted_failed_launch = true;
        goto CLEANUP;
    }

    rc = orte_rml_base_parse_uris(rml_uri, &peer, NULL );
    if( ORTE_SUCCESS != rc ) {
        ORTE_ERROR_LOG(rc);
        orted_failed_launch = true;
        goto CLEANUP;
    }

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:orted_report_launch from daemon %s via %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&peer),
                         ORTE_NAME_PRINT(&mev->sender)));
    
    /* update state and record for this daemon contact info */
    if (NULL == (daemon = (orte_proc_t*)opal_pointer_array_get_item(jdatorted->procs, peer.vpid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        orted_failed_launch = true;
        goto CLEANUP;
    }
    daemon->state = ORTE_PROC_STATE_RUNNING;
    daemon->rml_uri = rml_uri;

    /* if we are doing a timing test, unload the start and setup times of the daemon */
    if (orte_timing) {
        /* get the time stamp when the daemon first started */
        idx = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &startsec, &idx, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            orted_failed_launch = true;
            goto CLEANUP;
        }
        idx = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &startusec, &idx, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            orted_failed_launch = true;
            goto CLEANUP;
        }
        if (orte_timing_details) {
            
        }
        /* save the latest daemon to start */
        if (startsec > daemonlaunchtime.tv_sec) {
            daemonlaunchtime.tv_sec = startsec;
            daemonlaunchtime.tv_usec = startusec;
        } else if (startsec == daemonlaunchtime.tv_sec &&
                   startusec > daemonlaunchtime.tv_usec) {
            daemonlaunchtime.tv_usec = startusec;
        }
        /* get the time required for the daemon to setup - locally computed by each daemon */
        idx = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &setupsec, &idx, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            orted_failed_launch = true;
            goto CLEANUP;
        }
        idx = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &setupusec, &idx, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            orted_failed_launch = true;
            goto CLEANUP;
        }
        /* save the longest */
        if (setupsec > daemonsetuptime.tv_sec) {
            daemonsetuptime.tv_sec = setupsec;
            daemonsetuptime.tv_usec = setupusec;
        } else if (setupsec == daemonsetuptime.tv_sec &&
                   setupusec > daemonsetuptime.tv_usec) {
            daemonsetuptime.tv_usec = setupusec;
        }
        /* get the time stamp of when the daemon started to send this message to us */
        idx = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &setupsec, &idx, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            orted_failed_launch = true;
            goto CLEANUP;
        }
        idx = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &setupusec, &idx, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            orted_failed_launch = true;
            goto CLEANUP;
        }
        /* check the time for the callback to complete and save the longest */
        ORTE_COMPUTE_TIME_DIFF(secs, usecs, setupsec, setupusec, recvtime.tv_sec, recvtime.tv_usec);
        if (secs > daemoncbtime.tv_sec) {
            daemoncbtime.tv_sec = secs;
            daemoncbtime.tv_usec = usecs;
        } else if (secs == daemoncbtime.tv_sec &&
                   usecs > daemoncbtime.tv_usec) {
            daemoncbtime.tv_usec = usecs;
        }
    }
    
    /* if a tree-launch is underway, send the cmd back */
    if (NULL != orte_tree_launch_cmd) {
        orte_rml.send_buffer(&peer, orte_tree_launch_cmd, ORTE_RML_TAG_DAEMON, 0);
    }
    
CLEANUP:

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:orted_report_launch %s for daemon %s (via %s) at contact %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         orted_failed_launch ? "failed" : "completed",
                         ORTE_NAME_PRINT(&peer),
                         ORTE_NAME_PRINT(&mev->sender),
                         (NULL == daemon) ? "NULL" : daemon->rml_uri));

    /* release the message */
    OBJ_RELEASE(mev);

    if (orted_failed_launch) {
        if( NULL != rml_uri ) free(rml_uri);
        orte_errmgr.incomplete_start(ORTE_PROC_MY_NAME->jobid, ORTE_ERROR_DEFAULT_EXIT_CODE);
    } else {
        orted_num_callback++;
    }

}

static void orted_report_launch(int status, orte_process_name_t* sender,
                                opal_buffer_t *buffer,
                                orte_rml_tag_t tag, void *cbdata)
{
    int rc;
    
    /* don't process this right away - we need to get out of the recv before
     * we process the message as it may ask us to do something that involves
     * more messaging! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release when processed - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, process_orted_launch_report);
    
    /* reissue the recv */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ORTED_CALLBACK,
                                 ORTE_RML_NON_PERSISTENT, orted_report_launch, NULL);
    if (rc != ORTE_SUCCESS && rc != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(rc);
        orted_failed_launch = true;
    }
}

    
int orte_plm_base_daemon_callback(orte_std_cntr_t num_daemons)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:daemon_callback",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    orted_num_callback = 0;
    orted_failed_launch = false;
    /* get the orted job data object */
    if (NULL == (jdatorted = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }

    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ORTED_CALLBACK,
                                 ORTE_RML_NON_PERSISTENT, orted_report_launch, NULL);
    if (rc != ORTE_SUCCESS && rc != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    ORTE_PROGRESSED_WAIT(orted_failed_launch, orted_num_callback, num_daemons);
    
    /* cancel the lingering recv */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ORTED_CALLBACK))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* if we are timing, output the results */
    if (orte_timing) {
        int64_t sec, usec;
        ORTE_COMPUTE_TIME_DIFF(sec, usec, orte_plm_globals.daemonlaunchstart.tv_sec,
                               orte_plm_globals.daemonlaunchstart.tv_usec,
                               daemonlaunchtime.tv_sec, daemonlaunchtime.tv_usec);
        fprintf(orte_timing_output, "Daemon launch was completed in %s\n",
                pretty_print_timing(sec, usec));
        fprintf(orte_timing_output, "Daemon setup (from first exec statement to ready-for-commands) was completed in a maximum of %s\n",
                pretty_print_timing(daemonsetuptime.tv_sec, daemonsetuptime.tv_usec));
        fprintf(orte_timing_output, "Daemon callback message to HNP took a maximum time of %s to reach the HNP\n",
                pretty_print_timing(daemoncbtime.tv_sec, daemoncbtime.tv_usec));
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:daemon_callback completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if a tree-launch was underway, clear out the cmd */
    if (NULL != orte_tree_launch_cmd) {
        OBJ_RELEASE(orte_tree_launch_cmd);
    }
    
    return ORTE_SUCCESS;
}

/* the daemons actually report back that their procs have launched. Each
 * daemon will only send one message that contains the launch result
 * for their local procs.
 */
static bool app_launch_failed;
static struct timeval max_daemon_launch_msg_recvd = {0,0};
static orte_vpid_t num_daemons_reported=0;
static opal_event_t *dmn_report_ev=NULL;

/* catch timeout to allow cmds to progress */
static void timer_cb(int fd, short event, void *cbdata)
{
    /* free event */
    if (NULL != dmn_report_ev) {
        free(dmn_report_ev);
        dmn_report_ev = NULL;
    }
    /* declare time is up */
    app_launch_failed = true;
}

/* since the HNP also reports launch of procs, we need to separate out
 * the processing of the message vs its receipt so that the HNP
 * can call the processing part directly
 */
void orte_plm_base_app_report_launch(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    opal_buffer_t *buffer = mev->buffer;
    orte_std_cntr_t cnt;
    orte_jobid_t jobid;
    orte_vpid_t vpid;
    orte_proc_state_t state;
    orte_exit_code_t exit_code;
    pid_t pid;
    orte_job_t *jdata;
    orte_proc_t *proc;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:app_report_launch from daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&mev->sender)));
    
    /* got a response - cancel the timer */
    if (NULL != dmn_report_ev) {
        opal_event_del(dmn_report_ev);
        free(dmn_report_ev);
        dmn_report_ev = NULL;
    }
    
    /* unpack the jobid being reported */
    cnt = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &jobid, &cnt, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        app_launch_failed = true;
        orte_errmgr.incomplete_start(-1, -1); /* no way to know the jobid or exit code */
        return;
    }
    /* if the jobid is invalid, then we know that this is a failed
     * launch report from before we could even attempt to launch the
     * procs - most likely, while we were attempting to unpack the
     * launch cmd itself. In this case, just abort
     */
    if (ORTE_JOBID_INVALID == jobid) {
        jdata = NULL;
        app_launch_failed = true;
        goto CLEANUP;
    }
    
    num_daemons_reported++;

    /* get the job data object */
    if (NULL == (jdata = orte_get_job_data_object(jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        app_launch_failed = true;
        goto CLEANUP;
    }
    
    /* if we are timing, the daemon will have included the time it
     * recvd the launch msg - the maximum time between when we sent
     * that message and a daemon recvd it tells us the time reqd
     * to wireup the daemon comm network
     */
    if (orte_timing) {
        int64_t tmpsec, tmpusec;
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &tmpsec, &cnt, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            app_launch_failed = true;
            goto CLEANUP;
        }
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &tmpusec, &cnt, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            app_launch_failed = true;
            goto CLEANUP;
        }
        /* keep the maximum time */
        if (tmpsec > max_daemon_launch_msg_recvd.tv_sec) {
            max_daemon_launch_msg_recvd.tv_sec = tmpsec;
            max_daemon_launch_msg_recvd.tv_usec = tmpusec;
        } else if (tmpsec == max_daemon_launch_msg_recvd.tv_sec &&
                   tmpusec > max_daemon_launch_msg_recvd.tv_usec) {
            max_daemon_launch_msg_recvd.tv_usec = tmpusec;
        }
        if (orte_timing_details) {
            int64_t sec, usec;
            ORTE_COMPUTE_TIME_DIFF(sec, usec, launch_msg_sent.tv_sec, launch_msg_sent.tv_usec,
                                   tmpsec, tmpusec);
            fprintf(orte_timing_output, "Time for launch msg to reach daemon %s: %s\n",
                    ORTE_VPID_PRINT(mev->sender.vpid), pretty_print_timing(sec, usec));
        }
    }
    
    /* the daemon will report the vpid, state, and pid of each
     * process it launches - we need the pid in particular so
     * that any debuggers can attach to the process
     */
    cnt = 1;
    while (ORTE_SUCCESS == (rc = opal_dss.unpack(buffer, &vpid, &cnt, ORTE_VPID))) {
        if (ORTE_VPID_INVALID == vpid) {
            /* flag indicating we are done */
            break;
        }
        /* unpack the pid */
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &pid, &cnt, OPAL_PID))) {
            ORTE_ERROR_LOG(rc);
            app_launch_failed = true;
            goto CLEANUP;
        }
        /* if we are timing things, unpack the time this proc was started */
        if (orte_timing) {
            int64_t tmpsec, tmpusec;
            cnt = 1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &tmpsec, &cnt, OPAL_INT64))) {
                ORTE_ERROR_LOG(rc);
                app_launch_failed = true;
                goto CLEANUP;
            }
            cnt = 1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &tmpusec, &cnt, OPAL_INT64))) {
                ORTE_ERROR_LOG(rc);
                app_launch_failed = true;
                goto CLEANUP;
            }
            if (orte_timing_details) {
                time_t tmptime;
                char *tmpstr;
                tmptime = tmpsec;
                tmpstr = ctime(&tmptime);
                /* remove the newline and the year at the end */
                tmpstr[strlen(tmpstr)-6] = '\0';
                fprintf(orte_timing_output, "Time rank %s was launched: %s.%3lu\n",
                        ORTE_VPID_PRINT(vpid), tmpstr, (unsigned long)(tmpusec/1000));
            }
        }
        /* unpack the state */
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &state, &cnt, ORTE_PROC_STATE))) {
            ORTE_ERROR_LOG(rc);
            app_launch_failed = true;
            goto CLEANUP;
        }
        /* unpack the exit code */
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &exit_code, &cnt, ORTE_EXIT_CODE))) {
            ORTE_ERROR_LOG(rc);
            app_launch_failed = true;
            goto CLEANUP;
        }
        
        /* lookup the proc and update values */
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, vpid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            app_launch_failed = true;
            goto CLEANUP;
        }

        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:app_report_launched for proc %s from daemon %s: pid %lu state %0x exit %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&(proc->name)),
                             ORTE_NAME_PRINT(&mev->sender), (unsigned long)pid,
                             (int)state, (int)exit_code));
        
        proc->pid = pid;
        proc->state = state;
        proc->exit_code = exit_code;
        if (ORTE_PROC_STATE_FAILED_TO_START == state) {
            OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                 "%s plm:base:app_report_launched daemon %s reports proc %s failed to start",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&mev->sender),
                                 ORTE_NAME_PRINT(&(proc->name))));
            if (NULL == jdata->aborted_proc) {
                jdata->aborted_proc = proc;  /* only store this once */
                jdata->state = ORTE_JOB_STATE_FAILED_TO_START; /* update the job state */
            }
            /* increment the terminated counter */
            jdata->num_terminated++;
            /* ensure we have a non-zero exit code */
            if (0 == jdata->aborted_proc->exit_code) {
                jdata->aborted_proc->exit_code = ORTE_ERROR_DEFAULT_EXIT_CODE;
            }
            app_launch_failed = true;
            goto CLEANUP;
        }
        
        /* record that a proc reported */
        jdata->num_launched++;
    }
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
    
    if (orte_report_launch_progress) {
        if (0 == num_daemons_reported % 100 || num_daemons_reported == orte_process_info.num_procs) {
            opal_output(orte_clean_output, "Reported: %d (out of %d) daemons - %d (out of %d) procs",
                        (int)num_daemons_reported, (int)orte_process_info.num_procs,
                        (int)jdata->num_launched, (int)jdata->num_procs);
        }
    }

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:app_report_launch completed processing",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
CLEANUP:
    if (app_launch_failed) {
        if (NULL == jdata) {
            orte_errmgr.incomplete_start(ORTE_JOBID_INVALID, ORTE_ERROR_DEFAULT_EXIT_CODE);
        } else if (NULL == jdata->aborted_proc) {
            orte_errmgr.incomplete_start(jdata->jobid, ORTE_ERROR_DEFAULT_EXIT_CODE);
        } else {
            orte_errmgr.incomplete_start(jdata->jobid, jdata->aborted_proc->exit_code);
        }
    } else {
        /* restart the timer, if necessary */
        if (NULL != jdata && jdata->num_launched < jdata->num_procs && 0 < orte_startup_timeout) {
            ORTE_DETECT_TIMEOUT(&dmn_report_ev, orte_startup_timeout, 1000, 10000000, timer_cb);
        }
    }
}


static void app_report_launch(int status, orte_process_name_t* sender,
                              opal_buffer_t *buffer,
                              orte_rml_tag_t tag, void *cbdata)
{
    int rc;
    
    /* don't process this right away - we need to get out of the recv before
     * we process the message as it may ask us to do something that involves
     * more messaging! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release when processed - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, orte_plm_base_app_report_launch);

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:app_report_launch reissuing non-blocking recv",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* reissue the non-blocking receive */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_APP_LAUNCH_CALLBACK,
                                  ORTE_RML_NON_PERSISTENT, app_report_launch, NULL);
    if (rc != ORTE_SUCCESS && rc != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(rc);
        app_launch_failed = true;
    }

}

int orte_plm_base_report_launched(orte_jobid_t job)
{
    int rc;
    orte_job_t *jdata;
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:report_launched for job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job)));

    /* get the job data object */
    if (NULL == (jdata = orte_get_job_data_object(job))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* setup a timer - if we don't hear back from a daemon in the
     * defined time, then we know things have failed
     */
    if (0 < orte_startup_timeout) {
        ORTE_DETECT_TIMEOUT(&dmn_report_ev, orte_startup_timeout, 1000, 10000000, timer_cb);
    }

    /* we should get a callback from every daemon that is involved in
     * the launch. Fortunately, the mapper keeps track of this number
     * for us since num_nodes = num_participating_daemons
     */
    app_launch_failed = false;
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_APP_LAUNCH_CALLBACK,
                                 ORTE_RML_NON_PERSISTENT, app_report_launch, NULL);
    if (rc != ORTE_SUCCESS && rc != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    ORTE_PROGRESSED_WAIT(app_launch_failed, jdata->num_launched, jdata->num_procs);

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:report_launched all apps reported",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* cancel the lingering recv */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_APP_LAUNCH_CALLBACK))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* declare the job to be launched, but check to ensure
     * the procs haven't already reported in to avoid setting the
     * job back to an earlier state
     */
    if (jdata->state < ORTE_JOB_STATE_LAUNCHED) {
        jdata->state = ORTE_JOB_STATE_LAUNCHED;
    } else if (ORTE_JOB_STATE_TERMINATED < jdata->state) {
        /* job failed - indicate so */
        return ORTE_ERR_FATAL;
    }
    
    return ORTE_SUCCESS;
}

int orte_plm_base_setup_orted_cmd(int *argc, char ***argv)
{
    int i, loc;
    char **tmpv;
    
    /* set default location to be 0, indicating that
     * only a single word is in the cmd
     */
    loc = 0;
    /* split the command apart in case it is multi-word */
    tmpv = opal_argv_split(orte_launch_agent, ' ');
    for (i = 0; NULL != tmpv && NULL != tmpv[i]; ++i) {
        if (0 == strcmp(tmpv[i], "orted")) {
            loc = i;
        }
        opal_argv_append(argc, argv, tmpv[i]);
    }
    opal_argv_free(tmpv);
    
    return loc;
}


int orte_plm_base_orted_append_basic_args(int *argc, char ***argv,
                                          char *ess,
                                          int *proc_vpid_index,
                                          bool heartbeat, char *nodes)
{
    char *param = NULL;
    int loc_id;
    char * amca_param_path = NULL;
    char * amca_param_prefix = NULL;
    char * tmp_force = NULL;
    int i, cnt, rc;
    orte_job_t *jdata;
    char *rml_uri;
    unsigned long num_procs;
    
    /* check for debug flags */
    if (orte_debug_flag) {
        opal_argv_append(argc, argv, "--debug");
    }
    if (orte_debug_daemons_flag) {
        opal_argv_append(argc, argv, "--debug-daemons");
    }
    if (orte_debug_daemons_file_flag) {
        opal_argv_append(argc, argv, "--debug-daemons-file");
    }
    if (orted_spin_flag) {
        opal_argv_append(argc, argv, "--spin");
    }
    if (orte_report_bindings) {
        opal_argv_append(argc, argv, "--report-bindings");
    }
    
    if ((int)ORTE_VPID_INVALID != orted_debug_failure) {
        opal_argv_append(argc, argv, "--debug-failure");
        asprintf(&param, "%d", orted_debug_failure);
        opal_argv_append(argc, argv, param);
        free(param);
    }
    if (0 < orted_debug_failure_delay) {
        opal_argv_append(argc, argv, "--debug-failure-delay");
        asprintf(&param, "%d", orted_debug_failure_delay);
        opal_argv_append(argc, argv, param);
        free(param);
    }
    if (heartbeat && 0 < orte_heartbeat_rate) {
        /* tell the daemon to do a heartbeat */
        opal_argv_append(argc, argv, "--heartbeat");
        asprintf(&param, "%d", orte_heartbeat_rate);
        opal_argv_append(argc, argv, param);
        free(param);
    }
    
    /* tell the orted what ESS component to use */
    opal_argv_append(argc, argv, "-mca");
    opal_argv_append(argc, argv, "ess");
    opal_argv_append(argc, argv, ess);
    
    /* pass the daemon jobid */
    opal_argv_append(argc, argv, "-mca");
    opal_argv_append(argc, argv, "orte_ess_jobid");
    if (ORTE_SUCCESS != (rc = orte_util_convert_jobid_to_string(&param, ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    opal_argv_append(argc, argv, param);
    free(param);
    
    /* setup to pass the vpid */
    if (NULL != proc_vpid_index) {
        opal_argv_append(argc, argv, "-mca");
        opal_argv_append(argc, argv, "orte_ess_vpid");
        *proc_vpid_index = *argc;
        opal_argv_append(argc, argv, "<template>");
    }
    
    /* pass the total number of daemons that will be in the system */
    if (ORTE_PROC_IS_HNP) {
        jdata = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
        num_procs = jdata->num_procs;
    } else {
        num_procs = orte_process_info.num_procs;
    }
    opal_argv_append(argc, argv, "-mca");
    opal_argv_append(argc, argv, "orte_ess_num_procs");
    asprintf(&param, "%lu", num_procs);
    opal_argv_append(argc, argv, param);
    free(param);
    
    /* pass the uri of the hnp */
    if (ORTE_PROC_IS_HNP) {
        rml_uri = orte_rml.get_contact_info();
    } else {
        asprintf(&param, "\"%s\"", orte_rml.get_contact_info() );
        opal_argv_append(argc, argv, "--parent-uri");
        opal_argv_append(argc, argv, param);
        free(param);
    
        rml_uri = orte_process_info.my_hnp_uri;
    }
    asprintf(&param, "\"%s\"", rml_uri);
    opal_argv_append(argc, argv, "--hnp-uri");
    opal_argv_append(argc, argv, param);
    free(param);

    /* if given, pass the node list */
    if (NULL != nodes) {
        opal_argv_append(argc, argv, "-mca");
        opal_argv_append(argc, argv, "orte_nodelist");
        opal_argv_append(argc, argv, nodes);
    }
    
    /* pass along any cmd line MCA params provided to mpirun,
     * being sure to "purge" any that would cause problems
     * on backend nodes
     */
    if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_DAEMON) {
        cnt = opal_argv_count(orted_cmd_line);    
        for (i=0; i < cnt; i+=3) {
            /* if the specified option is more than one word, we don't
             * have a generic way of passing it as some environments ignore
             * any quotes we add, while others don't - so we ignore any
             * such options. In most cases, this won't be a problem as
             * they typically only apply to things of interest to the HNP.
             * Individual environments can add these back into the cmd line
             * as they know if it can be supported
             */
            if (NULL != strchr(orted_cmd_line[i+2], ' ')) {
                continue;
            }
            /* The daemon will attempt to open the PLM on the remote
             * end. Only a few environments allow this, so the daemon
             * only opens the PLM -if- it is specifically told to do
             * so by giving it a specific PLM module. To ensure we avoid
             * confusion, do not include any directives here
             */
            if (0 == strcmp(orted_cmd_line[i+1], "plm")) {
                continue;
            }
            /* must be okay - pass it along */
            opal_argv_append(argc, argv, orted_cmd_line[i]);
            opal_argv_append(argc, argv, orted_cmd_line[i+1]);
            opal_argv_append(argc, argv, orted_cmd_line[i+2]);
        }
    }

    /* if output-filename was specified, pass that along */
    if (NULL != orte_output_filename) {
        opal_argv_append(argc, argv, "--output-filename");
        opal_argv_append(argc, argv, orte_output_filename);
    }
    
    /* if --xterm was specified, pass that along */
    if (NULL != orte_xterm) {
        opal_argv_append(argc, argv, "--xterm");
        opal_argv_append(argc, argv, orte_xterm);
    }
    
    /* 
     * Pass along the Aggregate MCA Parameter Sets
     */
    /* Add the 'prefix' param */
    loc_id = mca_base_param_find("mca", NULL, "base_param_file_prefix");
    mca_base_param_lookup_string(loc_id, &amca_param_prefix);
    if( NULL != amca_param_prefix ) {
        /* Could also use the short version '-am'
        * but being verbose has some value
        */
        opal_argv_append(argc, argv, "-mca");
        opal_argv_append(argc, argv, "mca_base_param_file_prefix");
        opal_argv_append(argc, argv, amca_param_prefix);
    
        /* Add the 'path' param */
        loc_id = mca_base_param_find("mca", NULL, "base_param_file_path");
        mca_base_param_lookup_string(loc_id, &amca_param_path);
        if( NULL != amca_param_path ) {
            opal_argv_append(argc, argv, "-mca");
            opal_argv_append(argc, argv, "mca_base_param_file_path");
            opal_argv_append(argc, argv, amca_param_path);
        }
    
        /* Add the 'path' param */
        loc_id = mca_base_param_find("mca", NULL, "base_param_file_path_force");
        mca_base_param_lookup_string(loc_id, &tmp_force);
        if( NULL == tmp_force ) {
            /* Get the current working directory */
            tmp_force = (char *) malloc(sizeof(char) * OPAL_PATH_MAX);
            if( NULL == (tmp_force = getcwd(tmp_force, OPAL_PATH_MAX) )) {
                tmp_force = strdup("");
            }
        }
        opal_argv_append(argc, argv, "-mca");
        opal_argv_append(argc, argv, "mca_base_param_file_path_force");
        opal_argv_append(argc, argv, tmp_force);
    
        free(tmp_force);
    
        if( NULL != amca_param_path ) {
            free(amca_param_path);
            amca_param_path = NULL;
        }

        if( NULL != amca_param_prefix ) {
            free(amca_param_prefix);
            amca_param_prefix = NULL;
        }
    }

    return ORTE_SUCCESS;
}

static void process_check_job_completed(int fd, short event, void *data)
{
    orte_job_t *jdata = (orte_job_t*)data;

    active_job_completed_callback = false;
    orte_plm_base_check_job_completed(jdata);

    return;
}

void orte_plm_base_check_job_completed(orte_job_t *jdata)
{
    orte_proc_t *proc;
    int i;
    orte_std_cntr_t j;
    orte_job_t *job;
    orte_node_t *node;
    orte_job_map_t *map;
    orte_std_cntr_t index;
    bool one_still_alive;
    
    /* if the incoming job data pointer is NULL, then all we can do
     * is check all jobs for complete
     */
    if (NULL == jdata) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:check_job_completed called with NULL pointer",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        goto CHECK_ALL_JOBS;
    }
    
    /* if this job is not to be monitored, then ignore it */
    if (ORTE_JOB_CONTROL_DO_NOT_MONITOR & jdata->controls) {
        return;
    }

    /* Check if FileM is active. If so then keep processing. */
    if( orte_filem_base_is_active ) {
        opal_event_t *ev = NULL;
        struct timeval delay;

        if( active_job_completed_callback ) {
            return;
        }
        active_job_completed_callback = true;

        ev = (opal_event_t*)malloc(sizeof(opal_event_t));
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "WARNING: FileM Still Active! Waiting for it to finish..."));
        opal_evtimer_set(ev, process_check_job_completed, jdata);
        delay.tv_sec  = 5;
        delay.tv_usec = 0;
        opal_evtimer_add(ev, &delay);
        return;
    }

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:check_job_completed for job %s - num_terminated %lu  num_procs %lu",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid),
                         (unsigned long)jdata->num_terminated,
                         (unsigned long)jdata->num_procs));
    
    /* if this job was ordered to abort, or if its state was already recorded
     * as abnormally terminated, then do not update its state
     *
     * Treat termination of any process in a continuously operating job as
     * an error unless it was specifically commanded
     */
    if (jdata->state < ORTE_JOB_STATE_TERMINATED ||
        jdata->controls & ORTE_JOB_CONTROL_CONTINUOUS_OP) {
        for (i=0; i < jdata->procs->size; i++) {
            if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, i))) {
                /* the proc array may no longer be left justified, so
                 * we need to check everything
                 */
                continue;
            }

            /* notify the process status over the notifier */
            orte_plm_base_proc_state_notify(proc->state, &(proc->name));

            if (ORTE_PROC_STATE_FAILED_TO_START == proc->state) {
                jdata->state = ORTE_JOB_STATE_FAILED_TO_START;
                if (!jdata->abort) {
                    /* point to the lowest rank to cause the problem */
                    jdata->aborted_proc = proc;
                    /* retain the object so it doesn't get free'd */
                    OBJ_RETAIN(proc);
                    jdata->abort = true;
                    ORTE_UPDATE_EXIT_STATUS(proc->exit_code);
                }
                break;
            } else if (ORTE_PROC_STATE_ABORTED == proc->state) {
                jdata->state = ORTE_JOB_STATE_ABORTED;
                if (!jdata->abort) {
                    /* point to the lowest rank to cause the problem */
                    jdata->aborted_proc = proc;
                    /* retain the object so it doesn't get free'd */
                    OBJ_RETAIN(proc);
                    jdata->abort = true;
                    ORTE_UPDATE_EXIT_STATUS(proc->exit_code);
                }
                break;
            } else if (ORTE_PROC_STATE_ABORTED_BY_SIG == proc->state) {
                jdata->state = ORTE_JOB_STATE_ABORTED_BY_SIG;
                if (!jdata->abort) {
                    /* point to the lowest rank to cause the problem */
                    jdata->aborted_proc = proc;
                    /* retain the object so it doesn't get free'd */
                    OBJ_RETAIN(proc);
                    jdata->abort = true;
                    ORTE_UPDATE_EXIT_STATUS(proc->exit_code);
                }
                break;
            } else if (ORTE_PROC_STATE_TERM_WO_SYNC == proc->state) {
                jdata->state = ORTE_JOB_STATE_ABORTED_WO_SYNC;
                if (!jdata->abort) {
                    /* point to the lowest rank to cause the problem */
                    jdata->aborted_proc = proc;
                    /* retain the object so it doesn't get free'd */
                    OBJ_RETAIN(proc);
                    jdata->abort = true;
                    ORTE_UPDATE_EXIT_STATUS(proc->exit_code);
                    /* now treat a special case - if the proc exit'd without a required
                     * sync, it may have done so with a zero exit code. We want to ensure
                     * that the user realizes there was an error, so in this -one- case,
                     * we overwrite the process' exit code with the default error code
                     */
                    ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
                }
                break;
            } else if (ORTE_PROC_STATE_KILLED_BY_CMD == proc->state) {
                /* we ordered this proc to die, so it isn't an abnormal termination
                 * and we don't flag it as such - just check the remaining jobs to
                 * see if anyone is still alive
                 */
                if (jdata->num_terminated >= jdata->num_procs) {
                    /* this job has terminated - now we need to check to see if ALL
                     * the other jobs have also completed and wakeup if that is true
                     */
                    jdata->state = ORTE_JOB_STATE_KILLED_BY_CMD;
                }
                goto CHECK_ALL_JOBS;
            } else if (ORTE_PROC_STATE_UNTERMINATED < proc->state &&
                       jdata->controls & ORTE_JOB_CONTROL_CONTINUOUS_OP) {
                proc->state = ORTE_PROC_STATE_ABORTED;
                jdata->state = ORTE_JOB_STATE_ABORTED;
                if (!jdata->abort) {
                    /* point to the lowest rank to cause the problem */
                    jdata->aborted_proc = proc;
                    /* retain the object so it doesn't get free'd */
                    OBJ_RETAIN(proc);
                    jdata->abort = true;
                    ORTE_UPDATE_EXIT_STATUS(proc->exit_code);
                }
            }
        }
    }

    /* check the resulting job state and notify the appropriate places */
    
    if (ORTE_JOB_STATE_FAILED_TO_START == jdata->state) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:check_job_completed declared job %s failed to start by proc %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid),
                             (NULL == jdata->aborted_proc) ? "unknown" : ORTE_NAME_PRINT(&(jdata->aborted_proc->name))));
        /* report this to the errmgr - it will protect us from multiple calls */
        if (NULL == jdata->aborted_proc) {
            /* we don't know who caused us to abort */
            orte_errmgr.incomplete_start(jdata->jobid, ORTE_ERROR_DEFAULT_EXIT_CODE);
        } else {
            orte_errmgr.incomplete_start(jdata->jobid, jdata->aborted_proc->exit_code);
        }
        goto CHECK_ALL_JOBS;
    } else if (ORTE_JOB_STATE_ABORTED == jdata->state ||
               ORTE_JOB_STATE_ABORTED_BY_SIG == jdata->state ||
               ORTE_JOB_STATE_ABORTED_WO_SYNC == jdata->state) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:check_job_completed declared job %s aborted by proc %s with code %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid),
                             (NULL == jdata->aborted_proc) ? "unknown" : ORTE_NAME_PRINT(&(jdata->aborted_proc->name)),
                             (NULL == jdata->aborted_proc) ? ORTE_ERROR_DEFAULT_EXIT_CODE : jdata->aborted_proc->exit_code));
        /* report this to the errmgr */
        if (NULL == jdata->aborted_proc) {
            /* we don't know who caused us to abort */
            orte_errmgr.proc_aborted(ORTE_NAME_INVALID, ORTE_ERROR_DEFAULT_EXIT_CODE);
        } else {
            orte_errmgr.proc_aborted(&(jdata->aborted_proc->name), jdata->aborted_proc->exit_code);
        }
        goto CHECK_ALL_JOBS;
    } else if (jdata->num_terminated >= jdata->num_procs) {
        /* this job has terminated - now we need to check to see if ALL
         * the other jobs have also completed and wakeup if that is true
         */
        jdata->state = ORTE_JOB_STATE_TERMINATED;
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:check_job_completed declared job %s normally terminated - checking all jobs",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid)));

CHECK_ALL_JOBS:
        /* if this job is a continuously operating one, then don't do
         * anything further - just return here
         */
        if (NULL != jdata && ORTE_JOB_CONTROL_CONTINUOUS_OP & jdata->controls) {
            goto CHECK_ALIVE;
        }

        /* if the job that is being checked is the HNP, then we are
         * trying to terminate the orteds. In that situation, we
         * do -not- check all jobs - we simply notify the HNP
         * that the orteds are complete. Also check special case
         * if jdata is NULL - we want
         * to definitely declare the job done if the orteds
         * have completed, no matter what else may be happening.
         * This can happen if a ctrl-c hits in the "wrong" place
         * while launching
         */
        if (jdata == NULL || jdata->jobid == ORTE_PROC_MY_NAME->jobid) {
            jdata = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
            if (jdata->num_terminated >= jdata->num_procs) {
                /* orteds are done! */
                jdata->state = ORTE_JOB_STATE_TERMINATED;
                orte_trigger_event(&orteds_exit);
                return;
            }
        }

        /* Release the resources used by this job. Since some errmgrs may want
         * to continue using resources allocated to the job as part of their
         * fault recovery procedure, we only do this once the job is "complete".
         * Note that an aborted/killed job -is- flagged as complete and will
         * therefore have its resources released. We need to do this after
         * we call the errmgr so that any attempt to restart the job will
         * avoid doing so in the exact same place as the current job
         */
        if( NULL != jdata->map  && jdata->state == ORTE_JOB_STATE_TERMINATED) {
            map = jdata->map;
            for( index = 0; index < map->nodes->size; index++ ) {
                if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, index))) {
                    continue;
                }
                OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                     "%s releasing procs from node %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     node->name));
                for( i = 0; i < node->procs->size; i++ ) {
                    if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, i))) {
                        continue;
                    }
                    if (proc->name.jobid != jdata->jobid) {
                        /* skip procs from another job */
                        continue;
                    }
                    node->slots_inuse--;
                    node->num_procs--;
                    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                         "%s releasing proc %s from node %s",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         ORTE_NAME_PRINT(&proc->name), node->name));
                    /* set the entry in the node array to NULL */
                    opal_pointer_array_set_item(node->procs, i, NULL);
                    /* release the proc once for the map entry */
                    OBJ_RELEASE(proc);
                }
            }
            OBJ_RELEASE(map);
            jdata->map = NULL;
        }
        
CHECK_ALIVE:
        /* now check to see if all jobs are done - release this jdata
         * object when we find it
         */
        one_still_alive = false;
        for (j=1; j < orte_job_data->size; j++) {
            if (NULL == (job = (orte_job_t*)opal_pointer_array_get_item(orte_job_data, j))) {
                /* since we are releasing jdata objects as we
                 * go, we can no longer assume that the job_data
                 * array is left justified
                 */
                continue;
            }
            /* if this is the job we are checking AND it normally terminated,
             * then go ahead and release it. We cannot release it if it
             * abnormally terminated as mpirun needs the info so it can
             * report appropriately to the user
             */
            if (NULL != jdata && job->jobid == jdata->jobid &&
                (jdata->state == ORTE_JOB_STATE_TERMINATED ||
                 jdata->state == ORTE_JOB_STATE_KILLED_BY_CMD)) {
                /* release this object, ensuring that the
                 * pointer array internal accounting
                 * is maintained!
                 */
                opal_pointer_array_set_item(orte_job_data, j, NULL);  /* ensure the array has a NULL */
                OBJ_RELEASE(jdata);
                continue;
            }
            /* if the job is flagged to not be monitored, skip it */
            if (ORTE_JOB_CONTROL_DO_NOT_MONITOR & job->controls) {
                continue;
            }
            /* when checking for job termination, we must be sure to NOT check
             * our own job as it - rather obviously - has NOT terminated!
             */
            if (job->num_terminated < job->num_procs) {
                /* we have at least one job that is not done yet - we cannot
                 * just return, though, as we need to ensure we cleanout the
                 * job data for the job that just completed
                 */
                OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                     "%s plm:base:check_job_completed job %s is not terminated",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_JOBID_PRINT(job->jobid)));
                one_still_alive = true;
            }
        }
        /* if a job is still alive, we just return */
        if (one_still_alive) {
            OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                 "%s plm:base:check_job_completed at least one job is not terminated",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            return;
        }
        /* if we get here, then all jobs are done, so wakeup */
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:check_job_completed all jobs terminated - waking up",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        /* set the exit status to 0 - this will only happen if it
         * wasn't already set by an error condition
         */
        ORTE_UPDATE_EXIT_STATUS(0);
        orte_trigger_event(&orte_exit);
    }
    
}

void orte_plm_base_proc_state_notify(orte_proc_state_t state, orte_process_name_t *proc)
{
    if (NULL != proc) {
        switch(state) {
        case ORTE_PROC_STATE_TERMINATED:
        case ORTE_PROC_STATE_ABORTED:
        case ORTE_PROC_STATE_ABORTED_BY_SIG:
        case ORTE_PROC_STATE_TERM_WO_SYNC:
        case ORTE_PROC_STATE_KILLED_BY_CMD:
            orte_notifier.log(ORTE_NOTIFIER_ERROR, state, "%d: Process %s is dead.",
                              orte_process_info.pid, ORTE_JOBID_PRINT(proc->jobid));
            break;

        case ORTE_PROC_STATE_FAILED_TO_START:
            orte_notifier.log(ORTE_NOTIFIER_ERROR, state,
                              "%d: Process %s has called abort.",
                              orte_process_info.pid, ORTE_JOBID_PRINT(proc->jobid));
            break;
        default:
            break;
        }
    }
}

static char timestring[128];

static char *pretty_print_timing(int64_t secs, int64_t usecs)
{
    unsigned long minutes, seconds;
    float fsecs;
    
    seconds = secs + (usecs / 1000000l);
    minutes = seconds / 60l;
    seconds = seconds % 60l;
    if (0 == minutes && 0 == seconds) {
        fsecs = ((float)(secs)*1000000.0 + (float)usecs) / 1000.0;
        snprintf(timestring, 128, "%8.2f millisecs", fsecs);
    } else {
        snprintf(timestring, 128, "%3lu:%02lu min:sec", minutes, seconds);
    }
    
    return timestring;
}

