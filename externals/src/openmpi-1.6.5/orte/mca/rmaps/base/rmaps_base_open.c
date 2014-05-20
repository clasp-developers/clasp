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
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#if !ORTE_DISABLE_FULL_SUPPORT

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/paffinity/paffinity.h"

#include "orte/runtime/orte_globals.h"
#include "orte/util/show_help.h"

#include "orte/mca/rmaps/base/rmaps_private.h"

#endif

#include "orte/mca/rmaps/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/rmaps/base/static-components.h"

#if ORTE_DISABLE_FULL_SUPPORT
/* have to include a bogus function here so that
 * the build system sees at least one function
 * in the library
 */
int orte_rmaps_base_open(void)
{
    return ORTE_SUCCESS;
}

#else

/*
 * Global variables
 */
orte_rmaps_base_t orte_rmaps_base;

/*
 * Declare the RMAPS module to hold the API function pointers
 */
orte_rmaps_t orte_rmaps = {
    orte_rmaps_base_map_job,
    orte_rmaps_base_get_job_map
};


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_rmaps_base_open(void)
{
    int param, value;
    char *policy;
    bool btmp;

    /* init the globals */
    orte_rmaps_base.active_module = NULL;
    
    /* Debugging / verbose output.  Always have stream open, with
        verbose set by the mca open system... */
    orte_rmaps_base.rmaps_output = opal_output_open(NULL);

    /* Are we scheduling by node or by slot? */
    param = mca_base_param_reg_string_name("rmaps", "base_schedule_policy",
                                           "Scheduling Policy for RMAPS. [slot (alias:core) | socket | board | node]",
                                           false, false, "slot", &policy);
    
    /* if something is specified, do not override what may already
     * be present - could have been given on cmd line
     */
    if (0 == strcasecmp(policy, "slot") ||
        0 == strcasecmp(policy, "core")) {
        ORTE_XSET_MAPPING_POLICY(ORTE_MAPPING_BYSLOT);
    } else if (0 == strcasecmp(policy, "socket")) {
        ORTE_XSET_MAPPING_POLICY(ORTE_MAPPING_BYSOCKET);
    } else if (0 == strcasecmp(policy, "board")) {
       ORTE_XSET_MAPPING_POLICY(ORTE_MAPPING_BYBOARD);
    } else if (0 == strcasecmp(policy, "node")) {
        ORTE_XSET_MAPPING_POLICY(ORTE_MAPPING_BYNODE);
    }

    /* check for procs/xxx directives */
    param = mca_base_param_reg_int_name("rmaps", "base_pernode",
                                        "Launch one ppn as directed",
                                        false, false, (int)false, &value);
    if (value) {
        orte_rmaps_base.npernode = 1;
    }
    
    /* #procs/node */
    param = mca_base_param_reg_int_name("rmaps", "base_n_pernode",
                                        "Launch n procs/node",
                                        false, false, -1, &value);
    if (0 < value) {
        orte_rmaps_base.npernode = value;
    }
    
    /* #procs/board */
    param = mca_base_param_reg_int_name("rmaps", "base_n_perboard",
                                        "Launch n procs/board",
                                        false, false, -1, &orte_rmaps_base.nperboard);
    if (0 < orte_rmaps_base.nperboard) {
        ORTE_ADD_MAPPING_POLICY(ORTE_MAPPING_NPERXXX);
    }

    /* #procs/socket */
    param = mca_base_param_reg_int_name("rmaps", "base_n_persocket",
                                        "Launch n procs/socket",
                                        false, false, -1, &orte_rmaps_base.npersocket);
    if (0 < orte_rmaps_base.npersocket) {
        ORTE_ADD_MAPPING_POLICY(ORTE_MAPPING_NPERXXX);
        /* force bind to socket if not overridden by user */
        ORTE_XSET_BINDING_POLICY(ORTE_BIND_TO_SOCKET);
    }
    
    /* Do we want to loadbalance the job */
    param = mca_base_param_reg_int_name("rmaps", "base_loadbalance",
                                        "Balance total number of procs across all allocated nodes",
                                        false, false, (int)false, &value);
    orte_rmaps_base.loadbalance = OPAL_INT_TO_BOOL(value);
    
    /* #cpus/rank to use */
    param = mca_base_param_reg_int_name("rmaps", "base_cpus_per_proc",
                                        "Number of cpus to use for each rank [1-2**15 (default=1)]",
                                        false, false, 1, NULL);
    mca_base_param_reg_syn_name(param, "rmaps", "base_cpus_per_rank", false);
    mca_base_param_lookup_int(param, &value);
    orte_rmaps_base.cpus_per_rank = value;
    /* if the #cpus/rank > #cpus/socket, politely tell the user and abort
     *
     * NOTE: have to check that the default_num_cores_per_socket was set
     * as ompi_info doesn't call the ess init function, and thus might
     * leave this value at its default of zero
     */
    if (0 < orte_default_num_cores_per_socket &&
        orte_rmaps_base.cpus_per_rank > orte_default_num_cores_per_socket) {
        orte_show_help("help-orte-rmaps-base.txt", "too-many-cpus-per-rank",
                       true, orte_rmaps_base.cpus_per_rank,
                       orte_default_num_cores_per_socket);
        return ORTE_ERR_SILENT;
    }
    /* if the cpus/rank > 1, then we have to bind to cores UNLESS the binding has
     * already been set to something else
     */
    if (1 < orte_rmaps_base.cpus_per_rank) {
        ORTE_XSET_BINDING_POLICY(ORTE_BIND_TO_CORE);
    }
    
    /* stride to use */
    param = mca_base_param_reg_int_name("rmaps", "base_stride",
                                        "When binding multiple cores to a rank, the step size to use between cores [1-2**15 (default: 1)]",
                                        false, false, 1, &value);
    orte_rmaps_base.stride = value;
    
    /* did the user provide a slot list? */
    param = mca_base_param_reg_string_name("rmaps", "base_slot_list",
                                           "List of processor IDs to bind MPI processes to (e.g., used in conjunction with rank files) [default=NULL]",
                                           false, false, NULL, &orte_rmaps_base.slot_list);
    /* ensure we flag mapping by user */
    if (NULL != orte_rmaps_base.slot_list ||
        NULL != orte_rankfile) {
        ORTE_ADD_MAPPING_POLICY(ORTE_MAPPING_BYUSER);
    }
    
    /* Should we schedule on the local node or not? */
    mca_base_param_reg_int_name("rmaps", "base_no_schedule_local",
                                "If false, allow scheduling MPI applications on the same node as mpirun (default).  If true, do not schedule any MPI applications on the same node as mpirun",
                                false, false, (int)false, &value);
    if (value) {
        orte_default_mapping_policy |= ORTE_MAPPING_NO_USE_LOCAL;
    }

    /* Should we oversubscribe or not? */
    /** default condition that allows oversubscription */
    mca_base_param_reg_int_name("rmaps", "base_no_oversubscribe",
                                "If true, then do not allow oversubscription of nodes - mpirun will return an error if there aren't enough nodes to launch all processes without oversubscribing",
                                false, false, (int)false, &value);
    if (value) {
        orte_rmaps_base.oversubscribe = false;
    } else {
        orte_rmaps_base.oversubscribe = true;
    }
    
    /* should we display the map after determining it? */
    mca_base_param_reg_int_name("rmaps", "base_display_map",
                                "Whether to display the process map after it is computed",
                                false, false, (int)false, &value);
    orte_rmaps_base.display_map = OPAL_INT_TO_BOOL(value);
    
    /* should we display a detailed (developer-quality) version of the map after determining it? */
    mca_base_param_reg_int_name("rmaps", "base_display_devel_map",
                                "Whether to display a developer-detail process map after it is computed",
                                false, false, (int)false, &value);
    btmp = OPAL_INT_TO_BOOL(value);
    if (btmp) {
        orte_rmaps_base.display_map = true;
        orte_devel_level_output = true;
    }
    
    /* Open up all the components that we can find */
    if (ORTE_SUCCESS != 
        mca_base_components_open("rmaps", orte_rmaps_base.rmaps_output,
                                 mca_rmaps_base_static_components, 
                                 &orte_rmaps_base.available_components, true)) {
       return ORTE_ERROR;
    }

    /* All done */

    return ORTE_SUCCESS;
}

#endif /* ORTE_DISABLE_FULL_SUPPORT */
