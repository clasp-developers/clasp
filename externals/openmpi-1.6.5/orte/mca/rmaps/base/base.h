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
/** @file:
 * rmaps framework base functionality.
 */

#ifndef ORTE_MCA_RMAPS_BASE_H
#define ORTE_MCA_RMAPS_BASE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/types.h"

#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"

#include "orte/runtime/orte_globals.h"

#include "orte/mca/rmaps/rmaps.h"

BEGIN_C_DECLS

/**
 * Open the rmaps framework
 */
ORTE_DECLSPEC int orte_rmaps_base_open(void);

#if !ORTE_DISABLE_FULL_SUPPORT

/*
 * Global functions for MCA overall collective open and close
 */

/**
 * Struct to hold data global to the rmaps framework
 */
typedef struct {
    /** Verbose/debug output stream */
    int rmaps_output;
    /** List of available components */
    opal_list_t available_components;
    /** selected module */
    orte_rmaps_base_module_t *active_module;
    /** whether or not we allow oversubscription of nodes */
    bool oversubscribe;
    /** number of ppn for n_per_node mode */
    int npernode;
    /* number of procs/board */
    int nperboard;
    /* number of procs/socket */
    int npersocket;
    /* cpus per rank */
    int cpus_per_rank;
    /* stride */
    int stride;
    /* do not allow use of the localhost */
    bool no_use_local;
    /* display the map after it is computed */
    bool display_map;
    /* balance load across nodes */
    bool loadbalance;
    /* slot list, if provided by user */
    char *slot_list;
} orte_rmaps_base_t;

/**
 * Global instance of rmaps-wide framework data
 */
ORTE_DECLSPEC extern orte_rmaps_base_t orte_rmaps_base;

/**
 * Select an rmaps component / module
 */
ORTE_DECLSPEC int orte_rmaps_base_select(void);

/**
 * Utility routines to get/set vpid mapping for the job
 */ 
                                                                  
ORTE_DECLSPEC int orte_rmaps_base_get_vpid_range(orte_jobid_t jobid, 
    orte_vpid_t *start, orte_vpid_t *range);
ORTE_DECLSPEC int orte_rmaps_base_set_vpid_range(orte_jobid_t jobid, 
    orte_vpid_t start, orte_vpid_t range);

/**
 * Close down the rmaps framework
 */
ORTE_DECLSPEC int orte_rmaps_base_close(void);

#endif /* ORTE_DISABLE_FULL_SUPPORT */

END_C_DECLS

#endif
