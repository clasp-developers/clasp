/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#include <ctype.h>
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_IFADDRS_H
#include <ifaddrs.h>
#endif


#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/argv.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/util/printf.h"

#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/nidmap.h"
#include "orte/util/pre_condition_transports.h"
#include "orte/util/regex.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/slurmd/ess_slurmd.h"

static int rte_init(void);
static int rte_finalize(void);
static void rte_abort(int error_code, bool report) __opal_attribute_noreturn__;
static uint8_t proc_get_locality(orte_process_name_t *proc);
static orte_vpid_t proc_get_daemon(orte_process_name_t *proc);
static char* proc_get_hostname(orte_process_name_t *proc);
static orte_local_rank_t proc_get_local_rank(orte_process_name_t *proc);
static orte_node_rank_t proc_get_node_rank(orte_process_name_t *proc);
static int update_pidmap(opal_byte_object_t *bo);
static int update_nidmap(opal_byte_object_t *bo);

orte_ess_base_module_t orte_ess_slurmd_module = {
    rte_init,
    rte_finalize,
    rte_abort,
    proc_get_locality,
    proc_get_daemon,
    proc_get_hostname,
    proc_get_local_rank,
    proc_get_node_rank,
    update_pidmap,
    update_nidmap,
    NULL /* ft_event */
};

/* Local globals */
static bool app_init_complete;
static bool slurm20;

/****    MODULE FUNCTIONS    ****/

static int rte_init(void)
{
    int ret;
    char *error = NULL;
    int32_t jobfam, stepid;
    char **nodes = NULL;
    char *envar;
    int i, j;
    orte_nid_t *node;
    orte_jmap_t *jmap;
    orte_pmap_t *pmap;
    orte_vpid_t vpid;
    int local_rank;
    int nodeid;
    int num_nodes;
    int cpus_per_task;
    char *regexp, *tasks_per_node;
    int *ppn;
    bool block=false, cyclic=false;
    uint64_t unique_key[2];
    char *cs_env, *string_key, *enviro_value;

    /* init flag */
    app_init_complete = false;
    slurm20 = false;
    
    /* run the prolog */
    if (ORTE_SUCCESS != (ret = orte_ess_base_std_prolog())) {
        error = "orte_ess_base_std_prolog";
        goto error;
    }
    
    
    /* Only application procs can use this module. Since we
     * were directly launched by srun, we need to bootstrap
     * our own global info so we can startup. Srun will have
     * provided that info in our environment, so get it from there
     */
    
    /* declare ourselves to be standalone - i.e., not launched by orted */
    orte_standalone_operation = true;
    
    /* get the slurm jobid - this will be our job family */
    envar = getenv("SLURM_JOBID");
    /* don't need to check this for NULL - if it was, we would
     * never have been selected anyway
     */
    jobfam = strtol(envar, NULL, 10);
    /* get the slurm stepid - this will be our local jobid */
    if (NULL == (envar = getenv("SLURM_STEPID"))) {
        error = "could not get SLURM_STEPID";
        goto error;
    }
    /* because the stepid could be zero, and we want the local
     * jobid to be unique, increment it by one so the system
     * doesn't think that we are a bunch of daemons!
     */
    stepid = strtol(envar, NULL, 10) + 1;
    /* now build the jobid */
    ORTE_PROC_MY_NAME->jobid = ORTE_CONSTRUCT_LOCAL_JOBID(jobfam << 16, stepid);
    
    /* setup transport keys in case the MPI layer needs them -
     * we can use the SLURM jobid and stepid as unique keys
     * because they are unique values assigned by the RM
     */
    unique_key[0] = (uint64_t)jobfam;
    unique_key[1] = (uint64_t)stepid;
    if (NULL == (string_key = orte_pre_condition_transports_print(unique_key))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (NULL == (cs_env = mca_base_param_environ_variable("orte_precondition_transports",NULL,NULL))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    asprintf(&enviro_value, "%s=%s", cs_env, string_key);
    putenv(enviro_value);
    free(cs_env);
    free(string_key);

    /* get the slurm procid - this will be our vpid */
    if (NULL == (envar = getenv("SLURM_PROCID"))) {
        error = "could not get SLURM_PROCID";
        goto error;
    }
    ORTE_PROC_MY_NAME->vpid = strtol(envar, NULL, 10);
    
    /* get our local rank */
    if (NULL == (envar = getenv("SLURM_LOCALID"))) {
        error = "could not get SLURM_LOCALID";
        goto error;
    }
    local_rank = strtol(envar, NULL, 10);

    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                         "%s local rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         local_rank));
    
    /* get the number of procs in this job */
    if (NULL == (envar = getenv("SLURM_STEP_NUM_TASKS"))) {
        error = "could not get SLURM_STEP_NUM_TASKS";
        goto error;
    }
    orte_process_info.num_procs = strtol(envar, NULL, 10);
    
    /* set the app_num so that MPI attributes get set correctly */
    orte_process_info.app_num = 1;
    
    /* if this is SLURM 2.0 or above, get our port
     * assignments for use in the OOB
     */
    if (NULL != (envar = getenv("SLURM_STEP_RESV_PORTS"))) {
        /* convert this to an MCA param that will be
         * picked up by the OOB
         */
        orte_oob_static_ports = strdup(envar);
        slurm20 = true;
        OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                             "%s using SLURM-reserved ports %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             envar));
    }
    
    /* get my local nodeid */
    if (NULL == (envar = getenv("SLURM_NODEID"))) {
        error = "could not get SLURM_NODEID";
        goto error;
    }
    nodeid = strtol(envar, NULL, 10);
    ORTE_PROC_MY_DAEMON->jobid = 0;
    ORTE_PROC_MY_DAEMON->vpid = nodeid;
    
    /* get the number of ppn */
    if (NULL == (tasks_per_node = getenv("SLURM_STEP_TASKS_PER_NODE"))) {
        error = "could not get SLURM_STEP_TASKS_PER_NODE";
        goto error;
    }
    
    /* get the number of CPUs per task that the user provided to slurm */
    if (NULL != (envar = getenv("SLURM_CPUS_PER_TASK"))) {
        cpus_per_task = strtol(envar, NULL, 10);
        if(0 >= cpus_per_task) {
            error = "got bad value from SLURM_CPUS_PER_TASK";
            goto error;
        }
    } else {
        cpus_per_task = 1;
    }
    
    /* get the node list */
    if (NULL == (regexp = getenv("SLURM_STEP_NODELIST"))) {
        error = "could not get SLURM_STEP_NODELIST";
        goto error;
    }
    /* break that down into a list of nodes */
    if (ORTE_SUCCESS != (ret = orte_regex_extract_node_names(regexp, &nodes))) {
        error = "could not parse node list";
        goto error;
    }
    num_nodes = opal_argv_count(nodes);
    orte_process_info.num_nodes = num_nodes;
    
    /* compute the ppn */
    if (ORTE_SUCCESS != (ret = orte_regex_extract_ppn(num_nodes, tasks_per_node, &ppn))) {
        error = "could not determine #procs on each node";
        goto error;
    }
    /* for slurm, we have to normalize the ppn by the cpus_per_task */
    for (i=0; i < num_nodes; i++) {
        ppn[i] /= cpus_per_task;
    }
    
    /* get the distribution (i.e., mapping) mode */
    if (NULL == (envar = getenv("SLURM_DISTRIBUTION")) ||
        0 == strcmp(envar, "block")) {
        /* assume byslot mapping */
        block = true;
    } else if (0 == strcmp(envar, "cyclic")) {
        /* bynode mapping */
        cyclic = true;
    } else {
        /* cannot currently support other mapping modes */
        error = "distribution/mapping mode not supported";
        goto error;
    }
#if 0
    SLURM_DIST_PLANESIZE=0
    SLURM_DIST_LLLP=
#endif
    
    /* setup the nidmap arrays */
    if (ORTE_SUCCESS != (ret = orte_util_nidmap_init(NULL))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_util_nidmap_init";
        goto error;
    }
    
    /* set the size of the nidmap storage so we minimize realloc's */
    if (ORTE_SUCCESS != (ret = opal_pointer_array_set_size(&orte_nidmap, orte_process_info.num_nodes))) {
        error = "could not set pointer array size for nidmap";
        goto error;
    }
    
    /* construct the nidmap */
    for (i=0; i < num_nodes; i++) {
        node = OBJ_NEW(orte_nid_t);
        node->name = strdup(nodes[i]);
        node->daemon = i;
        node->index = i;
        opal_pointer_array_set_item(&orte_nidmap, i, node);
    }
    opal_argv_free(nodes);
    
    /* create a job map for this job */
    jmap = OBJ_NEW(orte_jmap_t);
    jmap->job = ORTE_PROC_MY_NAME->jobid;
    opal_pointer_array_add(&orte_jobmap, jmap);
    /* update the num procs */
    jmap->num_procs = orte_process_info.num_procs;
    /* set the size of the pidmap storage so we minimize realloc's */
    if (ORTE_SUCCESS != (ret = opal_pointer_array_set_size(&jmap->pmap, jmap->num_procs))) {
        ORTE_ERROR_LOG(ret);
        error = "could not set value array size for pidmap";
        goto error;
    }
        
    /* construct the pidmap */
    if (block) {
        /* for each node, cycle through the ppn */
        vpid = 0;
        for (i=0; i < orte_nidmap.size; i++) {
            if (NULL == (node = (orte_nid_t*)opal_pointer_array_get_item(&orte_nidmap, i))) {
                continue;
            }
            /* compute the vpid for each proc on this node
             * and add a pmap entry for it
             */
            for (j=0; j < ppn[i]; j++) {
                pmap = OBJ_NEW(orte_pmap_t);
                pmap->node = node->index;
                pmap->local_rank = j;
                pmap->node_rank = j;
                if (ORTE_SUCCESS != (ret = opal_pointer_array_set_item(&jmap->pmap, vpid, pmap))) {
                    ORTE_ERROR_LOG(ret);
                    error = "could not set pmap values";
                    goto error;
                }
                OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                                     "%s node %d name %s rank %d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     (int) node->index, node->name, (int)vpid));
                vpid++;
            }
        }
    } else if (cyclic) {
        /* cycle across the nodes */
        vpid = 0;
        while (vpid < orte_process_info.num_procs) {
            for (i=0; i < num_nodes && vpid < orte_process_info.num_procs; i++) {
                if (0 < ppn[i]) {
                    if (NULL == (node = (orte_nid_t*)opal_pointer_array_get_item(&orte_nidmap, i))) {
                        /* this is an error */
                        error = "error initializing process map";
                        goto error;
                    }
                    pmap = OBJ_NEW(orte_pmap_t);
                    pmap->node = node->index;
                    pmap->local_rank = ppn[i]-1;
                    pmap->node_rank = ppn[i]-1;
                    if (ORTE_SUCCESS != (ret = opal_pointer_array_set_item(&jmap->pmap, vpid, pmap))) {
                        ORTE_ERROR_LOG(ret);
                        error = "could not set pmap values";
                        goto error;
                    }
                    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                                         "%s node %d name %s rank %d",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         (int) node->index, node->name, (int)vpid));
                    vpid++;
                    --ppn[i];
                }
            }
        }
    }
    free(ppn);

    /* ensure we pick the correct critical components */
    putenv("OMPI_MCA_grpcomm=hier");
    putenv("OMPI_MCA_routed=direct");
    
    /* now use the default procedure to finish my setup */
    if (ORTE_SUCCESS != (ret = orte_ess_base_app_setup())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ess_base_app_setup";
        goto error;
    }
    
    /* flag that we completed init */
    app_init_complete = true;
    
    return ORTE_SUCCESS;
    
error:
    orte_show_help("help-orte-runtime.txt",
                   "orte_init:startup:internal-failure",
                   true, error, ORTE_ERROR_NAME(ret), ret);
    return ret;
}

static int rte_finalize(void)
{
    int ret = ORTE_SUCCESS;
   
    if (app_init_complete) {
        /* use the default procedure to finish */
        if (ORTE_SUCCESS != (ret = orte_ess_base_app_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
    }
    
    unsetenv("OMPI_MCA_orte_precondition_transports");

    /* deconstruct my nidmap and jobmap arrays - this
     * function protects itself from being called
     * before things were initialized
     */
    orte_util_nidmap_finalize();
    
    return ret;    
}

static void rte_abort(int error_code, bool report)
{
    if (ORTE_ERR_SOCKET_NOT_AVAILABLE == error_code && slurm20) {
        /* exit silently with a special error code for slurm 2.0 */
        orte_ess_base_app_abort(108, false);
    } else {
        orte_ess_base_app_abort(error_code, report);
    }
}

static uint8_t proc_get_locality(orte_process_name_t *proc)
{
    orte_nid_t *nid;
    
    if (NULL == (nid = orte_util_lookup_nid(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return OPAL_PROC_NON_LOCAL;
    }
    
    if (nid->daemon == ORTE_PROC_MY_DAEMON->vpid) {
        OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                             "%s ess:slurmd: proc %s is LOCAL",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(proc)));
        return (OPAL_PROC_ON_NODE | OPAL_PROC_ON_CU | OPAL_PROC_ON_CLUSTER);
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurmd: proc %s is REMOTE",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    
    return OPAL_PROC_NON_LOCAL;
    
}

static orte_vpid_t proc_get_daemon(orte_process_name_t *proc)
{
    orte_nid_t *nid;
    
    if( ORTE_JOBID_IS_DAEMON(proc->jobid) ) {
        return proc->vpid;
    }

    if (NULL == (nid = orte_util_lookup_nid(proc))) {
        return ORTE_VPID_INVALID;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurmd: proc %s is hosted by daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         ORTE_VPID_PRINT(nid->daemon)));
    
    return nid->daemon;
}

static char* proc_get_hostname(orte_process_name_t *proc)
{
    orte_nid_t *nid;
    
    if (NULL == (nid = orte_util_lookup_nid(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return NULL;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurmd: proc %s is on host %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         nid->name));
    
    return nid->name;
}

static orte_local_rank_t proc_get_local_rank(orte_process_name_t *proc)
{
    orte_pmap_t *pmap;
    
    if (NULL == (pmap = orte_util_lookup_pmap(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_LOCAL_RANK_INVALID;
    }    
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurmd: proc %s has local rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)pmap->local_rank));
    
    return pmap->local_rank;
}

static orte_node_rank_t proc_get_node_rank(orte_process_name_t *proc)
{
    orte_pmap_t *pmap;
    
    if (NULL == (pmap = orte_util_lookup_pmap(proc))) {
        return ORTE_NODE_RANK_INVALID;
    }    
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurmd: proc %s has node rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)pmap->node_rank));
    
    return pmap->node_rank;
}

static int update_pidmap(opal_byte_object_t *bo)
{
    int ret;
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurmd: updating pidmap",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* build the pmap */
    if (ORTE_SUCCESS != (ret = orte_util_decode_pidmap(bo))) {
        ORTE_ERROR_LOG(ret);
    }
    
    return ret;
}

static int update_nidmap(opal_byte_object_t *bo)
{
    int rc;
    /* decode the nidmap - the util will know what to do */
    if (ORTE_SUCCESS != (rc = orte_util_decode_nodemap(bo))) {
        ORTE_ERROR_LOG(rc);
    }    
    return rc;
}



#if 0
/***   AVAILABLE SLURM ENVARS   ***/
SLURM_JOB_ID=38749
SLURM_JOB_NUM_NODES=1
SLURM_JOB_NODELIST=odin097
SLURM_JOB_CPUS_PER_NODE=4
SLURM_JOBID=38749
SLURM_NNODES=1
SLURM_NODELIST=odin097
SLURM_TASKS_PER_NODE=2
SLURM_PRIO_PROCESS=0
SLURM_UMASK=0022
SLURM_NPROCS=2
SLURM_CPUS_PER_TASK=1
SLURM_STEPID=1
SLURM_SRUN_COMM_PORT=33650
SLURM_STEP_ID=1
SLURM_STEP_NODELIST=odin097
SLURM_STEP_NUM_NODES=1
SLURM_STEP_NUM_TASKS=2
SLURM_STEP_TASKS_PER_NODE=2
SLURM_STEP_LAUNCHER_HOSTNAME=(null)
SLURM_STEP_LAUNCHER_PORT=33650
SLURM_SRUN_COMM_HOST=129.79.240.100
SLURM_TASK_PID=5528
SLURM_CPUS_ON_NODE=4
SLURM_NODEID=0
SLURM_PROCID=1
SLURM_LOCALID=1
SLURM_LAUNCH_NODE_IPADDR=129.79.240.100
SLURM_GTIDS=0,1
SLURM_CHECKPOINT_PATH=/nfs/rinfs/san/homedirs/rhc
SLURMD_NODENAME=odin097
#endif
