/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Institut National de Recherche en Informatique
 *                         et Automatique. All rights reserved.
 * Copyright (c) 2011      Oracle and/or its affiliates. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
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
#include "orte/util/show_help.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_cr.h"
#include "opal/mca/pstat/base/base.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/mca/sysinfo/base/base.h"


#include "orte/mca/rml/base/base.h"
#include "orte/mca/routed/base/base.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/grpcomm/base/base.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/errmgr/errmgr.h"
#if OPAL_ENABLE_FT_CR == 1
#include "orte/mca/snapc/base/base.h"
#endif
#include "orte/mca/filem/base/base.h"
#include "orte/util/proc_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/util/regex.h"
#include "orte/util/show_help.h"
#include "orte/mca/notifier/base/base.h"

#include "orte/runtime/orte_cr.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/ess/base/base.h"

static bool plm_in_use;

int orte_ess_base_orted_setup(char **hosts)
{
    int ret;
    char *error = NULL;
    char *plm_to_use;

    /* initialize the global list of local children and job data */
    OBJ_CONSTRUCT(&orte_local_children, opal_list_t);
    OBJ_CONSTRUCT(&orte_local_jobdata, opal_list_t);
    
    /* open and setup the opal_pstat framework so we can provide
     * process stats if requested
     */
    if (ORTE_SUCCESS != (ret = opal_pstat_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "opal_pstat_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = opal_pstat_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_pstat_base_select";
        goto error;
    }
    
    /* open and setup the local resource discovery framework */
    if (ORTE_SUCCESS != (ret = opal_sysinfo_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "opal_sysinfo_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = opal_sysinfo_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "opal_sysinfo_base_select";
        goto error;
    }
    
    /* some environments allow remote launches - e.g., ssh - so
     * open the PLM and select something -only- if we are given
     * a specific module to use
     */
    mca_base_param_reg_string_name("plm", NULL,
                                   "Which plm component to use (empty = none)",
                                   false, false,
                                   NULL, &plm_to_use);
    
    if (NULL == plm_to_use) {
        plm_in_use = false;
    } else {
        plm_in_use = true;
        
        if (ORTE_SUCCESS != (ret = orte_plm_base_open())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_plm_base_open";
            goto error;
        }
        
        if (ORTE_SUCCESS != (ret = orte_plm_base_select())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_plm_base_select";
            goto error;
        }
    }

    /* Setup the communication infrastructure */
    
    /* Runtime Messaging Layer - this opens/selects the OOB as well */
    if (ORTE_SUCCESS != (ret = orte_rml_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_rml_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml_base_select";
        goto error;
    }

    /* Routed system */
    if (ORTE_SUCCESS != (ret = orte_routed_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_routed_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_routed_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_routed_base_select";
        goto error;
    }
    /*
     * Group communications
     */
    if (ORTE_SUCCESS != (ret = orte_grpcomm_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_grpcomm_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_grpcomm_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_grpcomm_base_select";
        goto error;
    }
    
    /* Open/select the odls */
    if (ORTE_SUCCESS != (ret = orte_odls_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_odls_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_odls_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_odls_base_select";
        goto error;
    }
    
    /* enable communication with the rml */
    if (ORTE_SUCCESS != (ret = orte_rml.enable_comm())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml.enable_comm";
        goto error;
    }
    
    /* initialize the nidmaps */
    if (ORTE_SUCCESS != (ret = orte_util_nidmap_init(NULL))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_util_nidmap_init";
        goto error;
    }
    /* if we are using static ports, then we need to setup
     * the daemon info so the RML can function properly
     * without requiring a wireup stage. This must be done
     * after we enable_comm as that function determines our
     * own port, which we need in order to construct the nidmap
     */
    if (orte_static_ports) {
        if (NULL != orted_launch_cmd) {
            /* the launch cmd was given via regexp on the cmd line - parse
             * it to get the contact info
             */
            if (ORTE_SUCCESS != (ret = orte_regex_decode_maps(orted_launch_cmd, NULL))) {
                ORTE_ERROR_LOG(ret);
                error = "orte_regex_decode_maps";
                goto error;
            }
        } else {
            if (ORTE_SUCCESS != (ret = orte_util_setup_local_nidmap_entries())) {
                ORTE_ERROR_LOG(ret);
                error = "orte_util_nidmap_init";
                goto error;
            }
            /* extract the node info from the environment and
             * build a nidmap from it
             */
            if (ORTE_SUCCESS != (ret = orte_util_build_daemon_nidmap(hosts))) {
                ORTE_ERROR_LOG(ret);
                error = "construct daemon map from static ports";
                goto error;
            }
        }
    }
    /* be sure to update the routing tree so the initial "phone home"
     * to mpirun goes through the tree!
     */
    if (ORTE_SUCCESS != (ret = orte_routed.update_routing_tree())) {
        ORTE_ERROR_LOG(ret);
        error = "failed to update routing tree";
        goto error;
    }

    /* Now provide a chance for the PLM
     * to perform any module-specific init functions. This
     * needs to occur AFTER the communications are setup
     * as it may involve starting a non-blocking recv
     * Do this only if a specific PLM was given to us - the
     * orted has no need of the proxy PLM at all
     */
    if (plm_in_use) {
        if (ORTE_SUCCESS != (ret = orte_plm.init())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_plm_init";
            goto error;
        }
    }
    
    /* setup my session directory */
    OPAL_OUTPUT_VERBOSE((2, orte_debug_output,
                         "%s setting up session dir with\n\ttmpdir: %s\n\thost %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == orte_process_info.tmpdir_base) ? "UNDEF" : orte_process_info.tmpdir_base,
                         orte_process_info.nodename));
    
    if (ORTE_SUCCESS != (ret = orte_session_dir(true,
                                                orte_process_info.tmpdir_base,
                                                orte_process_info.nodename, NULL,
                                                ORTE_PROC_MY_NAME))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_session_dir";
        goto error;
    }
    
    /* setup the routed info - the selected routed component
     * will know what to do. 
     */
    if (ORTE_SUCCESS != (ret = orte_routed.init_routes(ORTE_PROC_MY_NAME->jobid, NULL))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_routed.init_routes";
        goto error;
    }
    
    /* setup I/O forwarding system - must come after we init routes */
    if (ORTE_SUCCESS != (ret = orte_iof_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_iof_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_iof_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_iof_base_select";
        goto error;
    }
    
    /* setup the FileM */
    if (ORTE_SUCCESS != (ret = orte_filem_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_filem_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_filem_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_filem_base_select";
        goto error;
    }
    
#if OPAL_ENABLE_FT_CR == 1
    /*
     * Setup the SnapC
     */
    if (ORTE_SUCCESS != (ret = orte_snapc_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_snapc_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_snapc_base_select(ORTE_PROC_IS_HNP, !ORTE_PROC_IS_DAEMON))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_snapc_base_select";
        goto error;
    }
    
    /* For daemons, ORTE doesn't need the OPAL CR stuff */
    opal_cr_set_enabled(false);
#else
    opal_cr_set_enabled(false);
#endif
    
    /*
     * Initalize the CR setup
     * Note: Always do this, even in non-FT builds.
     * If we don't some user level tools may hang.
     */
    if (ORTE_SUCCESS != (ret = orte_cr_init())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_cr_init";
        goto error;
    }
    
    /* setup the notifier system */
    if (ORTE_SUCCESS != (ret = orte_notifier_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_notifer_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_notifier_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_notifer_select";
        goto error;
    }
    
    return ORTE_SUCCESS;
    
error:
    orte_show_help("help-orte-runtime.txt",
                   "orte_init:startup:internal-failure",
                   true, error, ORTE_ERROR_NAME(ret), ret);
    
    /* cleanup the global list of local children and job data */
    OBJ_DESTRUCT(&orte_local_children);
    OBJ_DESTRUCT(&orte_local_jobdata);
    
    return ret;
}

int orte_ess_base_orted_finalize(void)
{
    opal_list_item_t *item;
    
    /* ensure all the orteds depart together */
    if (!orte_abnormal_term_ordered) {
        /* if we are abnormally terminating, don't attempt
         * to do a barrier as nobody else will be entering
         * that call
         */
        orte_grpcomm.onesided_barrier();
    }
    
    orte_notifier_base_close();
    
    orte_cr_finalize();
    
#if OPAL_ENABLE_FT_CR == 1
    orte_snapc_base_close();
#endif
    orte_filem_base_close();
    
    orte_odls_base_close();
    
    orte_wait_finalize();
    orte_iof_base_close();

    /* finalize selected modules */
    if (plm_in_use) {
        orte_plm_base_close();
    }
    
    /* now can close the rml and its friendly group comm */
    orte_grpcomm_base_close();
    orte_routed_base_close();
    orte_rml_base_close();
        
    /* cleanup the global list of local children and job data */
    while (NULL != (item = opal_list_remove_first(&orte_local_children))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&orte_local_children);
    while (NULL != (item = opal_list_remove_first(&orte_local_jobdata))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&orte_local_jobdata);
    
    /* cleanup any lingering session directories */
    orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);
    
    /* handle the orted-specific OPAL stuff */
    opal_sysinfo_base_close();
    opal_pstat_base_close();
    
    return ORTE_SUCCESS;    
}
