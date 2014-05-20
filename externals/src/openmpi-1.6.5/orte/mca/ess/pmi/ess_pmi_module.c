/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.
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

#include <pmi.h>

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
#include "orte/mca/ess/pmi/ess_pmi.h"

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

orte_ess_base_module_t orte_ess_pmi_module = {
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

static bool app_init_complete=false;
static int pmi_maxlen=0;

/****    MODULE FUNCTIONS    ****/

static int rte_init(void)
{
    int ret, i, j;
    char *error = NULL, *localj;
    int32_t jobfam, stepid;
    char *envar;
    uint64_t unique_key[2];
    char *cs_env, *string_key;
    char *pmi_id=NULL;
    orte_nid_t *nid;
    orte_jmap_t *jmap;
    orte_pmap_t *pmap;
    int *ranks;

    /* run the prolog */
    if (ORTE_SUCCESS != (ret = orte_ess_base_std_prolog())) {
        error = "orte_ess_base_std_prolog";
        goto error;
    }
    
    /* get our PMI id length */
    if (PMI_SUCCESS != (ret = PMI_Get_id_length_max(&pmi_maxlen))) {
        error = "PMI_Get_id_length_max";
        goto error;
    }
    pmi_id = malloc(pmi_maxlen);
    if (PMI_SUCCESS != (ret = PMI_Get_kvs_domain_id(pmi_id, pmi_maxlen))) {
        free(pmi_id);
        error = "PMI_Get_kvs_domain_id";
        goto error;
    }

    /* PMI is very nice to us - the domain id is an integer followed
     * by a '.', followed by essentially a stepid. The first integer
     * defines an overall job number. The second integer is the number of
     * individual jobs we have run within that allocation. So we translate
     * this as the overall job number equating to our job family, and
     * the individual number equating to our local jobid
     */
    jobfam = strtol(pmi_id, &localj, 10);
    if (NULL == localj) {
        /* hmmm - no '.', so let's just use zero */
        stepid = 0;
    } else {
        localj++; /* step over the '.' */
        stepid = strtol(localj, NULL, 10) + 1; /* add one to avoid looking like a daemon */
    }
    free(pmi_id);

    /* now build the jobid */
    ORTE_PROC_MY_NAME->jobid = ORTE_CONSTRUCT_LOCAL_JOBID(jobfam << 16, stepid);
    /* get our rank */
    if (PMI_SUCCESS != (ret = PMI_Get_rank(&i))) {
        ORTE_PMI_ERROR(ret, "PMI_Get_rank");
        error = "could not get PMI rank";
        goto error;
    }
    ORTE_PROC_MY_NAME->vpid = i;

    /* setup transport keys in case the MPI layer needs them -
     * we can use the jobfam and stepid as unique keys
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
    asprintf(&envar, "%s=%s", cs_env, string_key);
    putenv(envar);
    /* cannot free the envar as that messes up our environ */
    free(cs_env);
    free(string_key);

    /* get the number of procs */
    if (PMI_SUCCESS != (ret = PMI_Get_universe_size(&i))) {
        ORTE_PMI_ERROR(ret, "PMI_Get_universe_size");
        error = "could not get PMI universe size";
        goto error;
    }
    orte_process_info.num_procs = i;

    /* get our app_context number */
    if (PMI_SUCCESS != (ret = PMI_Get_appnum(&i))) {
        ORTE_PMI_ERROR(ret, "PMI_Get_appnum");
        error = "could not get PMI appnum";
        goto error;
    }
    orte_process_info.app_num = i;

    /* setup the nidmap arrays - they will be filled by the modex */
    if (ORTE_SUCCESS != (ret = orte_util_nidmap_init(NULL))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_util_nidmap_init";
        goto error;
    }
    /* initialize our entry */
    if (ORTE_SUCCESS != (ret = orte_util_setup_local_nidmap_entries())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_util_setup_local_nidmap_entries";
        goto error;
    }
    /* correct the daemon entry on our nidmap object - note that
     * each proc's nidmap will be different, but the only thing that
     * matters here (since we are not routing messages) is that
     * we know which procs are on the same nodes
     */
    nid = (orte_nid_t*)opal_pointer_array_get_item(&orte_nidmap, 0);
    nid->daemon = 0;
    /* setup my daemon's name - arbitrary, since we don't route
     * messages
     */
    ORTE_PROC_MY_DAEMON->jobid = 0;
    ORTE_PROC_MY_DAEMON->vpid = 0;

    /* get the job map for this job */
    jmap = (orte_jmap_t*)opal_pointer_array_get_item(&orte_jobmap, 0);
    /* update the num procs */
    jmap->num_procs = orte_process_info.num_procs;
    /* set the size of the pidmap storage so we minimize realloc's */
    if (ORTE_SUCCESS != (ret = opal_pointer_array_set_size(&jmap->pmap, jmap->num_procs))) {
        ORTE_ERROR_LOG(ret);
        error = "could not set array size for pidmap";
        goto error;
    }

    /* get our local proc info to find our local rank */
    if (PMI_SUCCESS != (ret = PMI_Get_clique_size(&i))) {
        ORTE_PMI_ERROR(ret, "PMI_Get_clique_size");
        error = "could not get PMI clique size";
        goto error;
    }
    ranks = (int*)malloc(i * sizeof(int));
    if (PMI_SUCCESS != (ret = PMI_Get_clique_ranks(ranks, i))) {
        ORTE_PMI_ERROR(ret, "PMI_Get_clique_ranks");
        error = "could not get clique ranks";
        goto error;
    }
    /* cycle thru the array until we find our rank */
    for (j=0; j < i; j++) {
        if (NULL == (pmap = (orte_pmap_t*)opal_pointer_array_get_item(&jmap->pmap, ranks[j]))) {
            /* need to create this entry */
            pmap = OBJ_NEW(orte_pmap_t);
            pmap->node = nid->index;
            opal_pointer_array_set_item(&jmap->pmap, ranks[j], pmap);
        }
        pmap->local_rank = j;
        pmap->node_rank = j;
    }
    free(ranks);

    /* ensure we pick the correct critical components */
    putenv("OMPI_MCA_grpcomm=pmi");
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
    
    /* remove the envars that we pushed into environ
     * so we leave that structure intact
     */
    unsetenv("OMPI_MCA_grpcomm");
    unsetenv("OMPI_MCA_routed");
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
    orte_ess_base_app_abort(error_code, report);
}

static uint8_t proc_get_locality(orte_process_name_t *proc)
{
    orte_nid_t *nid;
    
    if (NULL == (nid = orte_util_lookup_nid(proc))) {
        return OPAL_PROC_NON_LOCAL;
    }
    
    if (nid->daemon == ORTE_PROC_MY_DAEMON->vpid) {
        OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                             "%s ess:pmi: proc %s is LOCAL",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(proc)));
        return (OPAL_PROC_ON_NODE | OPAL_PROC_ON_CU | OPAL_PROC_ON_CLUSTER);
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:pmi: proc %s is REMOTE",
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
                         "%s ess:pmi: proc %s is hosted by daemon %s",
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
                         "%s ess:pmi: proc %s is on host %s",
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
                         "%s ess:pmi: proc %s has local rank %d",
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
                         "%s ess:pmi: proc %s has node rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)pmap->node_rank));
    
    return pmap->node_rank;
}

static int update_pidmap(opal_byte_object_t *bo)
{
    int ret;
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:pmi: updating pidmap",
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

