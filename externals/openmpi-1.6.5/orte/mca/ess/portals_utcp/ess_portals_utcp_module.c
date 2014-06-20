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
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <string.h>

#include "opal/util/argv.h"
#include "opal/mca/paffinity/paffinity.h"

#include "orte/mca/errmgr/base/base.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/grpcomm/base/base.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/portals_utcp/ess_portals_utcp.h"

static int rte_init(void);
static int rte_finalize(void);
static void rte_abort(int status, bool report) __opal_attribute_noreturn__;
static uint8_t proc_get_locality(orte_process_name_t *proc);
static char* proc_get_hostname(orte_process_name_t *proc);
static orte_local_rank_t proc_get_local_rank(orte_process_name_t *proc);
static orte_node_rank_t proc_get_node_rank(orte_process_name_t *proc);

orte_ess_base_module_t orte_ess_portals_utcp_module = {
    rte_init,
    rte_finalize,
    rte_abort,
    proc_get_locality,
    NULL,   /* proc_get_daemon is only used in ORTE */
    proc_get_hostname,
    proc_get_local_rank,
    proc_get_node_rank,
    NULL,   /* add_pidmap is only used in ORTE */
    NULL,   /* update_nidmap is only used in ORTE */
    NULL /* ft_event */
};

static char **nidmap=NULL;

static int rte_init(void)
{
    int rc;
    orte_vpid_t vpid;
    char *vpid_string;
    char *nidmap_string;

    vpid_string = getenv("PTL_MY_RID");
    nidmap_string = getenv("PTL_NIDMAP");
    if (NULL == vpid_string || NULL == nidmap_string ||
        NULL == getenv("PTL_PIDMAP") || NULL == getenv("PTL_IFACE")) {
        return ORTE_ERR_NOT_FOUND;
    }

    /* Get our process information */

    /* Procs in this environment are directly launched. Hence, there
     * was no mpirun to create a jobid for us, and each app proc is
     * going to have to fend for itself. For now, we assume that the
     * jobid is some arbitrary number (say, 1).
     */
    ORTE_PROC_MY_NAME->jobid = 1; /* not 0, since it has special meaning */
    
    /*  find our vpid assuming range starts at 0 */
    if (ORTE_SUCCESS != (rc = orte_util_convert_string_to_vpid(&vpid, vpid_string))) {
        ORTE_ERROR_LOG(rc);
        return(rc);
    }
    ORTE_PROC_MY_NAME->vpid = vpid;
    
    /*
     * Get the number of procs in the job.  We assume vpids start at 0.  We
     * assume that there are <num : + 1> procs, since the nidmap is a
     * : seperated list of nids, and the utcp reference implementation
     * assumes all will be present
     */
    /* split the nidmap string */
    nidmap = opal_argv_split(nidmap_string, ':');
    orte_process_info.num_procs = (orte_std_cntr_t) opal_argv_count(nidmap);

    /* MPI_Init needs the grpcomm framework, so we have to init it */
    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_open())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_select())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* that's all we need here */
    return ORTE_SUCCESS;
}


static int rte_finalize(void)
{
    /* destruct the nidmap */
    opal_argv_free(nidmap);
    
    /* just cleanup the things we used */
    orte_grpcomm_base_close();
    
    /* clean out the global structures */
    orte_proc_info_finalize();
    
    return ORTE_SUCCESS;
}

static void rte_abort(int status, bool report)
{
    exit(status);
}

static uint8_t proc_get_locality(orte_process_name_t *proc)
{
    if (NULL != nidmap[proc->vpid] &&
        NULL != nidmap[ORTE_PROC_MY_NAME->vpid] &&
        0 == strcmp(nidmap[proc->vpid],
                    nidmap[ORTE_PROC_MY_NAME->vpid])) {
        return (OPAL_PROC_ON_NODE | OPAL_PROC_ON_CU | OPAL_PROC_ON_CLUSTER);
    }
    
    return OPAL_PROC_NON_LOCAL;
}

static char* proc_get_hostname(orte_process_name_t *proc)
{
    return nidmap[proc->vpid];
}

static orte_local_rank_t proc_get_local_rank(orte_process_name_t *proc)
{
    /* RHC: someone more familiar with CNOS needs to
     * fix this to return the correct value
     */
    return 0;
}

static orte_node_rank_t proc_get_node_rank(orte_process_name_t *proc)
{
    /* RHC: someone more familiar with CNOS needs to
     * fix this to return the correct value
     */
    return 0;
}
