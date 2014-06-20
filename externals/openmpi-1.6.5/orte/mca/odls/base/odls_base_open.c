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
 * Copyright (c) 2011      Oracle and/or its affiliates.  All rights reserved.
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
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/mca/sysinfo/sysinfo.h"
#include "opal/util/output.h"
#include "opal/util/path.h"
#include "opal/util/argv.h"

#include "orte/mca/plm/plm_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/show_help.h"
#include "orte/util/parse_options.h"

#include "orte/mca/odls/base/odls_private.h"

#endif

#include "orte/mca/odls/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/odls/base/static-components.h"

#if ORTE_DISABLE_FULL_SUPPORT
/* have to include a bogus function here so that
 * the build system sees at least one function
 * in the library
 */
int orte_odls_base_open(void)
{
    return ORTE_SUCCESS;
}

#else

/*
 * Instantiate globals
 */
orte_odls_base_module_t orte_odls;

/* instance the child list object */
static void orte_odls_child_constructor(orte_odls_child_t *ptr)
{
    ptr->name = NULL;
    ptr->restarts = 0;
    ptr->pid = 0;
    ptr->app_idx = -1;
    ptr->alive = false;
    ptr->coll_recvd = false;
    /* set the default state to "failed to start" so
     * we can correctly report should something
     * go wrong during launch
     */
    ptr->state = ORTE_PROC_STATE_FAILED_TO_START;
    ptr->exit_code = 0;
    ptr->init_recvd = false;
    ptr->fini_recvd = false;
    ptr->rml_uri = NULL;
    ptr->slot_list = NULL;
    ptr->waitpid_recvd = false;
    ptr->iof_complete = false;
    ptr->do_not_barrier = false;
}
static void orte_odls_child_destructor(orte_odls_child_t *ptr)
{
    if (NULL != ptr->name) free(ptr->name);
    if (NULL != ptr->rml_uri) free(ptr->rml_uri);
    if (NULL != ptr->slot_list) free(ptr->slot_list);
}
OBJ_CLASS_INSTANCE(orte_odls_child_t,
                   opal_list_item_t,
                   orte_odls_child_constructor,
                   orte_odls_child_destructor);

static void orte_odls_job_constructor(orte_odls_job_t *ptr)
{
    ptr->jobid = ORTE_JOBID_INVALID;
    ptr->state = ORTE_JOB_STATE_UNDEF;
    ptr->launch_msg_processed = false;
    ptr->apps = NULL;
    ptr->num_apps = 0;
    ptr->policy = 0;
    ptr->cpus_per_rank = 1;
    ptr->stride = 1;
    ptr->controls = 0;
    ptr->stdin_target = ORTE_VPID_INVALID;
    ptr->total_slots_alloc = 0;
    ptr->num_procs = 0;
    ptr->num_local_procs = 0;
    ptr->regexp = NULL;
    ptr->pmap = NULL;
    OBJ_CONSTRUCT(&ptr->collection_bucket, opal_buffer_t);
    OBJ_CONSTRUCT(&ptr->local_collection, opal_buffer_t);
    ptr->collective_type = ORTE_GRPCOMM_COLL_NONE;
    ptr->num_contributors = 0;
    ptr->num_participating = -1;
    ptr->num_collected = 0;
}
static void orte_odls_job_destructor(orte_odls_job_t *ptr)
{
    int32_t i;
    
    if (NULL != ptr->apps) {
        for (i=0; i < ptr->num_apps; i++) {
            OBJ_RELEASE(ptr->apps[i]);
        }
        if (NULL != ptr->apps) {
            free(ptr->apps);
        }
    }
    
    if (NULL != ptr->regexp) {
        free(ptr->regexp);
    }
    
    if (NULL != ptr->pmap && NULL != ptr->pmap->bytes) {
        free(ptr->pmap->bytes);
        free(ptr->pmap);
    }
    
    OBJ_DESTRUCT(&ptr->collection_bucket);
    OBJ_DESTRUCT(&ptr->local_collection);
}
OBJ_CLASS_INSTANCE(orte_odls_job_t,
                   opal_list_item_t,
                   orte_odls_job_constructor,
                   orte_odls_job_destructor);

/*
 * Framework global variables
 */
orte_odls_base_t orte_odls_base;
orte_odls_globals_t orte_odls_globals;

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_odls_base_open(void)
{
    char **ranks=NULL, *tmp;
    int i, rank, sock, core, rc;
    orte_namelist_t *nm;
    bool xterm_hold;
    
    /* Debugging / verbose output.  Always have stream open, with
        verbose set by the mca open system... */
    orte_odls_globals.output = opal_output_open(NULL);

    mca_base_param_reg_int_name("odls", "base_sigkill_timeout",
                                "Time to wait for a process to die after issuing a kill signal to it",
                                false, false, 1, &orte_odls_globals.timeout_before_sigkill);

    /* initialize ODLS globals */
    OBJ_CONSTRUCT(&orte_odls_globals.mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&orte_odls_globals.cond, opal_condition_t);
    OBJ_CONSTRUCT(&orte_odls_globals.xterm_ranks, opal_list_t);
    orte_odls_globals.xtermcmd = NULL;
    orte_odls_globals.dmap = NULL;
    orte_odls_globals.debugger = NULL;
    orte_odls_globals.debugger_launched = false;
    OBJ_CONSTRUCT(&orte_odls_globals.sysinfo, opal_list_t);

#if OPAL_HAVE_HWLOC
    /* ensure we have the local topology */
    if (NULL == opal_hwloc_topology) {
        if (0 != hwloc_topology_init(&opal_hwloc_topology) ||
            0 != hwloc_topology_load(opal_hwloc_topology)) {
            return ORTE_ERR_NOT_SUPPORTED;
        }
    }
#endif
    
    /* init globals */
    OPAL_PAFFINITY_CPU_ZERO(orte_odls_globals.my_cores);
    orte_odls_globals.bound = false;
    orte_odls_globals.num_processors = 0;
    orte_odls_globals.num_sockets = orte_default_num_sockets_per_board;
    orte_odls_globals.num_cores_per_socket = orte_default_num_cores_per_socket;
    OBJ_CONSTRUCT(&orte_odls_globals.sockets, opal_bitmap_t);
    opal_bitmap_init(&orte_odls_globals.sockets, 16);
    
    /* see if paffinity is supported */
    if (ORTE_SUCCESS == (rc = opal_paffinity_base_get(&orte_odls_globals.my_cores))) {
        /* get the number of local sockets unless we were given a number */
        if (0 == orte_default_num_sockets_per_board) {
            opal_paffinity_base_get_socket_info(&orte_odls_globals.num_sockets);
            /* on some really old kernels or unusual systems, we may not
             * see any sockets - so default to a value of 1 to avoid
             * the segfault
             */
            if (orte_odls_globals.num_sockets <= 0) {
                orte_odls_globals.num_sockets = 1;
            }
        }
        /* get the number of local processors */
        opal_paffinity_base_get_processor_info(&orte_odls_globals.num_processors);
        /* compute the base number of cores/socket, if not given */
        if (0 == orte_default_num_cores_per_socket) {
            orte_odls_globals.num_cores_per_socket = orte_odls_globals.num_processors / orte_odls_globals.num_sockets;
        }
        OPAL_OUTPUT_VERBOSE((1, orte_odls_globals.output,
                             "%s FOUND %d SOCKETS WITH %d CORES-PER-SOCKET",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             orte_odls_globals.num_sockets,
                             orte_odls_globals.num_cores_per_socket));
        /* determine if we are bound */
        OPAL_PAFFINITY_PROCESS_IS_BOUND(orte_odls_globals.my_cores, &orte_odls_globals.bound);
        /* if we are bound, determine the number of sockets - and which ones - that are available to us */
        if (orte_odls_globals.bound) {
            for (i=0; i < orte_odls_globals.num_processors; i++) {
                if (OPAL_PAFFINITY_CPU_ISSET(i, orte_odls_globals.my_cores)) {
                    opal_paffinity_base_get_map_to_socket_core(i, &sock, &core);
                    opal_bitmap_set_bit(&orte_odls_globals.sockets, sock);
                }
            }
            /* determine how many sockets we have available to us */
            orte_odls_globals.num_sockets = 0;
            for (i=0; i < opal_bitmap_size(&orte_odls_globals.sockets); i++) {
                if (opal_bitmap_is_set_bit(&orte_odls_globals.sockets, i)) {
                    orte_odls_globals.num_sockets++;
                }
            }
            if (ORTE_PROC_IS_HNP && orte_report_bindings) {
                opal_output(0, "System has detected external process binding to cores %04lx",
                            orte_odls_globals.my_cores.bitmask[0]);
            }
        }
    } else {
        OPAL_OUTPUT_VERBOSE((1, orte_odls_globals.output,
                             "%s AFFINITY NOT SUPPORTED: %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_ERROR_NAME(rc)));
    }
    
    /* check if the user requested that we display output in xterms */
    if (NULL != orte_xterm) {
        /* construct a list of ranks to be displayed */
        xterm_hold = false;
        orte_util_parse_range_options(orte_xterm, &ranks);
        for (i=0; i < opal_argv_count(ranks); i++) {
            if (0 == strcmp(ranks[i], "BANG")) {
                xterm_hold = true;
                continue;
            }
            nm = OBJ_NEW(orte_namelist_t);
            rank = strtol(ranks[i], NULL, 10);
            if (-1 == rank) {
                /* wildcard */
                nm->name.vpid = ORTE_VPID_WILDCARD;
            } else if (rank < 0) {
                /* error out on bozo case */
                orte_show_help("help-odls-base.txt",
                               "orte-odls-base:xterm-neg-rank",
                               true, rank);
                return ORTE_ERROR;
            } else {
                /* we can't check here if the rank is out of
                 * range as we don't yet know how many ranks
                 * will be in the job - we'll check later
                 */
                nm->name.vpid = rank;
            }
            opal_list_append(&orte_odls_globals.xterm_ranks, &nm->item);
        }
        opal_argv_free(ranks);
        /* construct the xtermcmd */
        orte_odls_globals.xtermcmd = NULL;
        tmp = opal_find_absolute_path("xterm");
        if (NULL == tmp) {
            return ORTE_ERROR;
        }
        opal_argv_append_nosize(&orte_odls_globals.xtermcmd, tmp);
        free(tmp);
        opal_argv_append_nosize(&orte_odls_globals.xtermcmd, "-T");
        opal_argv_append_nosize(&orte_odls_globals.xtermcmd, "save");
        if (xterm_hold) {
            opal_argv_append_nosize(&orte_odls_globals.xtermcmd, "-hold");
        }
        opal_argv_append_nosize(&orte_odls_globals.xtermcmd, "-e");
    }
    
    /* collect the system info */
    if (NULL != opal_sysinfo.query) {
        char *keys[] = {
            OPAL_SYSINFO_CPU_TYPE,
            OPAL_SYSINFO_CPU_MODEL,
            OPAL_SYSINFO_NUM_CPUS,
            OPAL_SYSINFO_MEM_SIZE,
            NULL
        };
        opal_list_item_t *item;
        opal_sysinfo_value_t *info;

        /* get and store our local resources */
        opal_sysinfo.query(keys, &orte_odls_globals.sysinfo);
        /* find our cpu type and model, save it for later */
        for (item = opal_list_get_first(&orte_odls_globals.sysinfo);
             item != opal_list_get_end(&orte_odls_globals.sysinfo) &&
                (NULL == orte_local_cpu_type || NULL == orte_local_cpu_model);
             item = opal_list_get_next(item)) {
            info = (opal_sysinfo_value_t*)item;

            if (0 == strcmp(info->key, OPAL_SYSINFO_CPU_TYPE)) {
                orte_local_cpu_type = strdup(info->data.str);
            }
            if (0 == strcmp(info->key, OPAL_SYSINFO_CPU_MODEL)) {
                orte_local_cpu_model = strdup(info->data.str);
            }
        }
    }

    /* Open up all available components */

    if (ORTE_SUCCESS != 
        mca_base_components_open("odls", orte_odls_globals.output,
                                    mca_odls_base_static_components, 
                                    &orte_odls_base.available_components, true)) {
        return ORTE_ERROR;
    }

    /* are there components available for use ?  - 
     * orte_odls_base.available_components is always initialized */
    if(0 < opal_list_get_size(&(orte_odls_base.available_components))) {
        orte_odls_base.components_available = true;
    } else {
        orte_odls_base.components_available = false;
    }
    
    /* All done */
    
    return ORTE_SUCCESS;
}
#endif
