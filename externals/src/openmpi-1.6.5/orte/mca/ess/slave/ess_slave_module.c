/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
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
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <sys/types.h>
#include <stdio.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/event/event.h"
#include "opal/runtime/opal.h"
#include "opal/mca/paffinity/paffinity.h"

#include "orte/util/show_help.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"
#include "opal/util/malloc.h"

#include "orte/mca/rml/base/base.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/base/base.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/grpcomm/base/base.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/plm/base/base.h"

#include "orte/mca/rmaps/base/base.h"
#if OPAL_ENABLE_FT_CR == 1
#include "orte/mca/snapc/base/base.h"
#endif
#include "orte/mca/filem/base/base.h"
#include "orte/util/proc_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"

#include "orte/runtime/orte_cr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/slave/ess_slave.h"

static int slave_set_name(void);

static int rte_init(void);
static int rte_finalize(void);
static uint8_t proc_get_locality(orte_process_name_t *proc);
static orte_vpid_t proc_get_daemon(orte_process_name_t *proc);
static char* proc_get_hostname(orte_process_name_t *proc);
static orte_local_rank_t proc_get_local_rank(orte_process_name_t *proc);
static orte_node_rank_t proc_get_node_rank(orte_process_name_t *proc);
static int update_pidmap(opal_byte_object_t *bo);
static int update_nidmap(opal_byte_object_t *bo);

#if OPAL_ENABLE_FT_CR == 1
static int rte_ft_event(int state);
static int ess_slave_ft_event_update_process_info(orte_process_name_t proc, pid_t pid);
#endif

orte_ess_base_module_t orte_ess_slave_module = {
    rte_init,
    rte_finalize,
    orte_ess_base_app_abort,
    proc_get_locality,
    proc_get_daemon,
    proc_get_hostname,
    proc_get_local_rank,
    proc_get_node_rank,
    update_pidmap,
    update_nidmap,
#if OPAL_ENABLE_FT_CR == 1
    rte_ft_event
#else
    NULL
#endif
};

static int rte_init(void)
{
    int ret;
    char *error = NULL;
    
    /* run the prolog */
    if (ORTE_SUCCESS != (ret = orte_ess_base_std_prolog())) {
        error = "orte_ess_base_std_prolog";
        goto error;
    }
    
    /* Start by getting a unique name from the enviro */
    slave_set_name();
    
    /* use the default procedure to finish my setup */
    if (ORTE_SUCCESS != (ret = orte_ess_base_app_setup())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ess_base_app_setup";
        goto error;
    }
    
    /* init my nidmap arrays - no data can be available, but
     * we want to ensure that nobody else who looks at
     * those arrays will segfault
     */
    if (ORTE_SUCCESS != (ret = orte_util_nidmap_init(NULL))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_util_nidmap_init";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_util_setup_local_nidmap_entries())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    return ORTE_SUCCESS;

error:
    orte_show_help("help-orte-runtime.txt",
                   "orte_init:startup:internal-failure",
                   true, error, ORTE_ERROR_NAME(ret), ret);
    
    return ret;
}

static int rte_finalize(void)
{
    int ret;
    
    /* use the default procedure to finish */
    if (ORTE_SUCCESS != (ret = orte_ess_base_app_finalize())) {
        ORTE_ERROR_LOG(ret);
    }
    
    /* deconstruct the nidmap and jobmap arrays */
    orte_util_nidmap_finalize();
    
    return ret;    
}

static uint8_t proc_get_locality(orte_process_name_t *proc)
{
    /* no proc can be local */
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slave: proc %s is REMOTE",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    
    return OPAL_PROC_NON_LOCAL;
    
}

static orte_vpid_t proc_get_daemon(orte_process_name_t *proc)
{
    /* if it is me, the answer is my daemon's vpid */
    if (proc->jobid == ORTE_PROC_MY_NAME->jobid &&
        proc->vpid == ORTE_PROC_MY_NAME->vpid) {
        return ORTE_PROC_MY_DAEMON->vpid;
    }

    /* otherwise, no idea */
    return ORTE_VPID_INVALID;
}

static char* proc_get_hostname(orte_process_name_t *proc)
{
    /* if it is me, the answer is my nodename */
    if (proc->jobid == ORTE_PROC_MY_NAME->jobid &&
        proc->vpid == ORTE_PROC_MY_NAME->vpid) {
        return orte_process_info.nodename;
    }
    
    /* otherwise, no idea */
    return NULL;
}

static orte_local_rank_t proc_get_local_rank(orte_process_name_t *proc)
{
    /* if it is me, the local rank is zero */
    if (proc->jobid == ORTE_PROC_MY_NAME->jobid &&
        proc->vpid == ORTE_PROC_MY_NAME->vpid) {
        return 0;
    }
    
    /* otherwise, no idea */
    return ORTE_LOCAL_RANK_INVALID;
}

static orte_node_rank_t proc_get_node_rank(orte_process_name_t *proc)
{
    /* if it is me, the node rank is zero */
    if (proc->jobid == ORTE_PROC_MY_NAME->jobid &&
        proc->vpid == ORTE_PROC_MY_NAME->vpid) {
        return 0;
    }
    
    /* otherwise, no idea */
    return ORTE_NODE_RANK_INVALID;
}

static int update_pidmap(opal_byte_object_t *bo)
{
    return ORTE_SUCCESS;
}

static int update_nidmap(opal_byte_object_t *bo)
{
    return ORTE_SUCCESS;
}

static int slave_set_name(void)
{
    char *jobid_str, *procid_str;
    int id, rc;
    orte_jobid_t jobid;
    orte_vpid_t vpid;
    
    id = mca_base_param_register_string("orte", "ess", "jobid", NULL, NULL);
    mca_base_param_lookup_string(id, &jobid_str);
    if (NULL == jobid_str) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    if (ORTE_SUCCESS != (rc = orte_util_convert_string_to_jobid(&jobid, jobid_str))) {
        ORTE_ERROR_LOG(rc);
        return(rc);
    }
    free(jobid_str);
    
    id = mca_base_param_register_string("orte", "ess", "vpid", NULL, NULL);
    mca_base_param_lookup_string(id, &procid_str);
    if (NULL == procid_str) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    if (ORTE_SUCCESS != (rc = orte_util_convert_string_to_vpid(&vpid, procid_str))) {
        ORTE_ERROR_LOG(rc);
        return(rc);
    }
    free(procid_str);
    
    ORTE_PROC_MY_NAME->jobid = jobid;
    ORTE_PROC_MY_NAME->vpid = vpid;
    
    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                         "ess:slave set name to %s", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* get the non-name common environmental variables */
    if (ORTE_SUCCESS != (rc = orte_ess_env_get())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}

#if OPAL_ENABLE_FT_CR == 1
static int rte_ft_event(int state)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_proc_type_t svtype;

    /******** Checkpoint Prep ********/
    if(OPAL_CRS_CHECKPOINT == state) {
        /*
         * Notify SnapC
         */
        if( ORTE_SUCCESS != (ret = orte_snapc.ft_event(OPAL_CRS_CHECKPOINT))) {
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Notify Routed
         */
        if( ORTE_SUCCESS != (ret = orte_routed.ft_event(OPAL_CRS_CHECKPOINT))) {
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Notify RML -> OOB
         */
        if( ORTE_SUCCESS != (ret = orte_rml.ft_event(OPAL_CRS_CHECKPOINT))) {
            exit_status = ret;
            goto cleanup;
        }
    }
    /******** Continue Recovery ********/
    else if (OPAL_CRS_CONTINUE == state ) {
        /*
         * Notify RML -> OOB
         */
        if( ORTE_SUCCESS != (ret = orte_rml.ft_event(OPAL_CRS_CONTINUE))) {
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Notify Routed
         */
        if( ORTE_SUCCESS != (ret = orte_routed.ft_event(OPAL_CRS_CONTINUE))) {
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Notify SnapC
         */
        if( ORTE_SUCCESS != (ret = orte_snapc.ft_event(OPAL_CRS_CONTINUE))) {
            exit_status = ret;
            goto cleanup;
        }
    }
    /******** Restart Recovery ********/
    else if (OPAL_CRS_RESTART == state ) {
        /*
         * This should follow the ess init() function
         */

        /*
         * Clear nidmap and jmap
         */
        orte_util_nidmap_finalize();

        /*
         * - Reset Contact information
         */
        if( ORTE_SUCCESS != (ret = slave_set_name() ) ) {
            exit_status = ret;
        }

        /*
         * Notify RML -> OOB
         */
        if( ORTE_SUCCESS != (ret = orte_rml.ft_event(OPAL_CRS_RESTART))) {
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Restart the routed framework
         * JJH: Lie to the finalize function so it does not try to contact the daemon.
         */
        svtype = orte_process_info.proc_type;
        orte_process_info.proc_type = ORTE_PROC_TOOL;
        if (ORTE_SUCCESS != (ret = orte_routed.finalize()) ) {
            exit_status = ret;
            goto cleanup;
        }
        orte_process_info.proc_type = svtype;
        if (ORTE_SUCCESS != (ret = orte_routed.initialize()) ) {
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Group Comm - Clean out stale data
         */
        orte_grpcomm.finalize();
        if (ORTE_SUCCESS != (ret = orte_grpcomm.init())) {
            exit_status = ret;
            goto cleanup;
        }
        if (ORTE_SUCCESS != (ret = orte_grpcomm.purge_proc_attrs())) {
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Restart the PLM - Does nothing at the moment, but included for completeness
         */
        if (ORTE_SUCCESS != (ret = orte_plm.finalize())) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        if (ORTE_SUCCESS != (ret = orte_plm.init())) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        /*
         * RML - Enable communications
         */
        if (ORTE_SUCCESS != (ret = orte_rml.enable_comm())) {
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Session directory re-init
         */
        if (ORTE_SUCCESS != (ret = orte_session_dir(true,
                                                    orte_process_info.tmpdir_base,
                                                    orte_process_info.nodename,
                                                    NULL, /* Batch ID -- Not used */
                                                    ORTE_PROC_MY_NAME))) {
            exit_status = ret;
        }

        opal_output_set_output_file_info(orte_process_info.proc_session_dir,
                                         "output-", NULL, NULL);

        /*
         * Notify Routed
         */
        if( ORTE_SUCCESS != (ret = orte_routed.ft_event(OPAL_CRS_RESTART))) {
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Notify SnapC
         */
        if( ORTE_SUCCESS != (ret = orte_snapc.ft_event(OPAL_CRS_RESTART))) {
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Send new PID to HNP/daemon
         * The checkpointer could have used a proxy program to boot us
         * so the pid that the orted got from fork() may not be the
         * PID of this application.
         * - Note: BLCR does this because it tries to preseve the PID
         *         of the program across checkpointes
         */
        if( ORTE_SUCCESS != (ret = ess_slave_ft_event_update_process_info(orte_process_info.my_name, getpid())) ) {
            exit_status = ret;
            goto cleanup;
        }

        /* if one was provided, build my nidmap */
        if (ORTE_SUCCESS != (ret = orte_util_nidmap_init(orte_process_info.sync_buf))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }
    else if (OPAL_CRS_TERM == state ) {
        /* Nothing */
    }
    else {
        /* Error state = Nothing */
    }

 cleanup:

    return exit_status;
}

static int ess_slave_ft_event_update_process_info(orte_process_name_t proc, pid_t proc_pid)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_buffer_t buffer;
    orte_snapc_cmd_flag_t command = ORTE_SNAPC_LOCAL_UPDATE_CMD;

    OBJ_CONSTRUCT(&buffer, opal_buffer_t);

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &command, 1, ORTE_SNAPC_CMD )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &proc, 1, ORTE_NAME))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &proc_pid, 1, OPAL_PID))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_DAEMON, &buffer, ORTE_RML_TAG_SNAPC, 0))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    OBJ_DESTRUCT(&buffer);

    return exit_status;
}
#endif

