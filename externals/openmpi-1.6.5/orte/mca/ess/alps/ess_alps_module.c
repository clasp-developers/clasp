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
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Los Alamos National Security, LLC.
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

#if ORTE_MCA_ESS_ALPS_HAVE_CNOS == 1
#  if defined(HAVE_CNOS_MPI_OS_H)
#    include "cnos_mpi_os.h"
#  elif defined(HAVE_CATAMOUNT_CNOS_MPI_OS_H)
#    include "catamount/cnos_mpi_os.h"
#  endif
#endif
#include <errno.h>

#include "orte/util/show_help.h"
#include "opal/util/argv.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/util/regex.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/alps/ess_alps.h"

static int alps_set_name(void);
static int rte_init(void);
static int rte_finalize(void);
static int get_vpid(orte_vpid_t *outvp,
                    orte_vpid_t start_vpid);

orte_ess_base_module_t orte_ess_alps_module = {
    rte_init,
    rte_finalize,
    orte_ess_base_app_abort,
    orte_ess_base_proc_get_locality,
    orte_ess_base_proc_get_daemon,
    orte_ess_base_proc_get_hostname,
    orte_ess_base_proc_get_local_rank,
    orte_ess_base_proc_get_node_rank,
    orte_ess_base_proc_get_epoch,
    orte_ess_base_update_pidmap,
    orte_ess_base_update_nidmap,
    NULL /* ft_event */
};

/* Local variables */
static orte_vpid_t starting_vpid = 0;

static int
get_vpid(orte_vpid_t *outvp,
         orte_vpid_t start_vpid)
{
#if ORTE_MCA_ESS_ALPS_HAVE_CNOS == 1
    *outvp = (orte_vpid_t)cnos_get_rank() + start_vpid;
    return ORTE_SUCCESS;
#else
    /* Cray XE6 Notes:
     * using PMI_GNI_LOC_ADDR to set vpid.
     */
    int rank = 0;
    char *env;

    if (NULL == (env = getenv("PMI_GNI_LOC_ADDR"))) {
        OPAL_OUTPUT_VERBOSE((0, orte_ess_base_output,
                             "PMI_GNI_LOC_ADDR not found, cannot continue\n"));
        ORTE_ERROR_LOG(ORTE_ERROR);
        return ORTE_ERROR;
    }
    errno = 0;
    rank = (int)strtol(env, (char **)NULL, 10);
    if (0 != errno) {
        OPAL_OUTPUT_VERBOSE((0, orte_ess_base_output,
                             "strtol error detected at %s:%d\n", __FILE__,
                             __LINE__));
        ORTE_ERROR_LOG(ORTE_ERROR);
        return ORTE_ERROR;
    }
    *outvp = (orte_vpid_t)(rank + (int)start_vpid);
    return ORTE_SUCCESS;
#endif /* ORTE_MCA_ESS_ALPS_HAVE_CNOS == 1 */
}

static int rte_init(void)
{
    int ret, i;
    char *error = NULL;
    char **hosts = NULL;

    /* run the prolog */
    if (ORTE_SUCCESS != (ret = orte_ess_base_std_prolog())) {
        error = "orte_ess_base_std_prolog";
        goto error;
    }

    /* Start by getting a unique name */
    alps_set_name();

    /* if I am a daemon, complete my setup using the
     * default procedure
     */
    if (ORTE_PROC_IS_DAEMON) {
        if (NULL != orte_node_regex) {
            /* extract the nodes */
            if (ORTE_SUCCESS != (ret =
                orte_regex_extract_node_names(orte_node_regex, &hosts)) ||
                NULL == hosts) {
                error = "orte_regex_extract_node_names";
                goto error;
            }

            /* find our host in the list */
            for (i=0; NULL != hosts[i]; i++) {
                if (0 == strncmp(hosts[i], orte_process_info.nodename,
                                 strlen(hosts[i]))) {
                    /* correct our vpid */
                    ORTE_PROC_MY_NAME->vpid = starting_vpid + i;
                    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                                         "ess:alps reset name to %s",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                    break;
                }
            }
        }
        if (ORTE_SUCCESS != (ret = orte_ess_base_orted_setup(hosts))) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ess_base_orted_setup";
            goto error;
        }
        opal_argv_free(hosts);
        return ORTE_SUCCESS;
    }
    if (ORTE_PROC_IS_TOOL) {
        /* otherwise, if I am a tool proc, use that procedure */
        if (ORTE_SUCCESS != (ret = orte_ess_base_tool_setup())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ess_base_tool_setup";
            goto error;
        }
        /* as a tool, I don't need a nidmap - so just return now */
        return ORTE_SUCCESS;
    }
    /* otherwise, I must be an application process - use
     * the default procedure to finish my setup
     */
    if (ORTE_SUCCESS != (ret = orte_ess_base_app_setup())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ess_base_app_setup";
        goto error;
    }
    /* setup the nidmap arrays */
    if (ORTE_SUCCESS !=
        (ret = orte_util_nidmap_init(orte_process_info.sync_buf))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_util_nidmap_init";
        goto error;
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

    /* if I am a daemon, finalize using the default procedure */
    if (ORTE_PROC_IS_DAEMON) {
        if (ORTE_SUCCESS != (ret = orte_ess_base_orted_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
    } else if (ORTE_PROC_IS_TOOL) {
        /* otherwise, if I am a tool proc, use that procedure */
        if (ORTE_SUCCESS != (ret = orte_ess_base_tool_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
        /* as a tool, I didn't create a nidmap - so just return now */
        return ret;
    } else {
        /* otherwise, I must be an application process
         * use the default procedure to finish
         */
        if (ORTE_SUCCESS != (ret = orte_ess_base_app_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
    }

    /* deconstruct my nidmap and jobmap arrays */
    orte_util_nidmap_finalize();

    return ret;
}

static int alps_set_name(void)
{
    int rc;
    orte_jobid_t jobid;
    char *tmp;
    orte_vpid_t vpid;

    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                         "ess:alps setting name"));

    mca_base_param_reg_string_name("orte", "ess_jobid", "Process jobid",
                                   true, false, NULL, &tmp);
    if (NULL == tmp) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    if (ORTE_SUCCESS != (rc = orte_util_convert_string_to_jobid(&jobid, tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    free(tmp);

    mca_base_param_reg_string_name("orte", "ess_vpid", "Process vpid",
                                   true, false, NULL, &tmp);
    if (NULL == tmp) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    if (ORTE_SUCCESS != (rc = orte_util_convert_string_to_vpid(&starting_vpid,
                                                               tmp))) {
        ORTE_ERROR_LOG(rc);
        return(rc);
    }
    free(tmp);

    if (ORTE_SUCCESS != (rc = get_vpid(&vpid, starting_vpid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    ORTE_PROC_MY_NAME->jobid = jobid;
    ORTE_PROC_MY_NAME->vpid = vpid;
    ORTE_EPOCH_SET(ORTE_PROC_MY_NAME->epoch,ORTE_EPOCH_INVALID);
    ORTE_EPOCH_SET(ORTE_PROC_MY_NAME->epoch,
                   orte_ess.proc_get_epoch(ORTE_PROC_MY_NAME));

    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                         "ess:alps set name to %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* get the num procs as provided in the cmd line param */
    if (ORTE_SUCCESS != (rc = orte_ess_env_get())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (orte_process_info.max_procs < orte_process_info.num_procs) {
        orte_process_info.max_procs = orte_process_info.num_procs;
    }

    return ORTE_SUCCESS;
}
