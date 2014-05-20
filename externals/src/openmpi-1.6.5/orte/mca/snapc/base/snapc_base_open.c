/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2008 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Evergrid, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/opal_environ.h"
#include "opal/util/output.h"

#include "orte/constants.h"
#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"

#include "orte/mca/snapc/base/static-components.h"

#if ORTE_DISABLE_FULL_SUPPORT
/* have to include a bogus function here so that
 * the build system sees at least one function
 * in the library
 */
int orte_snapc_base_open(void)
{
    return ORTE_SUCCESS;
}

#else

/*
 * Globals
 */
int  orte_snapc_base_output  = -1;
bool orte_snapc_base_is_tool = false;
orte_snapc_base_module_t orte_snapc = {
    NULL, /* snapc_init      */
    NULL, /* snapc_finalize  */
    NULL, /* setup_job       */
    NULL  /* release_job     */
};

opal_list_t orte_snapc_base_components_available;
orte_snapc_base_component_t orte_snapc_base_selected_component;
orte_snapc_coord_type_t orte_snapc_coord_type = ORTE_SNAPC_UNASSIGN_TYPE;

char * orte_snapc_base_global_snapshot_dir = NULL;
char * orte_snapc_base_global_snapshot_loc = NULL;
char * orte_snapc_base_global_snapshot_ref = NULL;
bool orte_snapc_base_store_in_place = true;
bool orte_snapc_base_store_only_one_seq = false;
bool orte_snapc_base_establish_global_snapshot_dir = false;
bool orte_snapc_base_is_global_dir_shared = false;

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
int orte_snapc_base_open(void)
{
    int value = 0;
    char * str_value = NULL;

    OPAL_OUTPUT_VERBOSE((10, orte_snapc_base_output,
                         "snapc:base: open()"));

    orte_snapc_base_output = opal_output_open(NULL);

    /* Global Snapshot directory */
    mca_base_param_reg_string_name("snapc",
                                   "base_global_snapshot_dir",
                                   "The base directory to use when storing global snapshots",
                                   false, false,
                                   opal_home_directory(),
                                   &orte_snapc_base_global_snapshot_dir);

    mca_base_param_reg_int_name("snapc",
                                "base_global_shared",
                                "If the global_snapshot_dir is on a shared file system all nodes can access, "
                                "then the checkpoint files can be copied more efficiently when FileM is used."
                                " [Default = disabled]",
                                false, false,
                                0,
                                &value);
    orte_snapc_base_is_global_dir_shared = OPAL_INT_TO_BOOL(value);

    OPAL_OUTPUT_VERBOSE((20, orte_snapc_base_output,
                         "snapc:base: open: base_global_snapshot_dir    = %s (%s)",
                         orte_snapc_base_global_snapshot_dir,
                         (orte_snapc_base_is_global_dir_shared ? "Shared" : "Local") ));

    /*
     * Store the checkpoint files in their final location.
     * This assumes that the storage place is on a shared file 
     * system that all nodes can access uniformly.
     * Default = enabled
     */
    mca_base_param_reg_int_name("snapc",
                                "base_store_in_place",
                                "If global_snapshot_dir is on a shared file system all nodes can access, "
                                "then the checkpoint files can be stored in place instead of incurring a "
                                "remote copy. [Default = enabled]",
                                false, false,
                                1,
                                &value);
    orte_snapc_base_store_in_place = OPAL_INT_TO_BOOL(value);
    OPAL_OUTPUT_VERBOSE((20, orte_snapc_base_output,
                         "snapc:base: open: base_store_in_place    = %d",
                         orte_snapc_base_store_in_place));

    /*
     * Reuse sequence numbers
     * This will create a directory and always use seq 0 for all checkpoints
     * This *should* also enforce a 2-phase commit protocol
     */
    mca_base_param_reg_int_name("snapc_base",
                                "only_one_seq",
                                "Only store the most recent checkpoint sequence. [Default = disabled]",
                                false, false,
                                0,
                                &value);
    orte_snapc_base_store_only_one_seq = OPAL_INT_TO_BOOL(value);

    OPAL_OUTPUT_VERBOSE((20, orte_snapc_base_output,
                         "snapc:base: open: base_only_one_seq    = %d",
                         orte_snapc_base_store_only_one_seq));

    /*
     * Pre-establish the global snapshot directory upon job registration
     */
    mca_base_param_reg_int_name("snapc_base",
                                "establish_global_snapshot_dir",
                                "Establish the global snapshot directory on job startup. [Default = disabled]",
                                false, false,
                                0,
                                &value);
    orte_snapc_base_establish_global_snapshot_dir = OPAL_INT_TO_BOOL(value);

    OPAL_OUTPUT_VERBOSE((20, orte_snapc_base_output,
                         "snapc:base: open: base_establish_global_snapshot_dir    = %d",
                         orte_snapc_base_establish_global_snapshot_dir));

    /*
     * User defined global snapshot directory name for this job
     */
    mca_base_param_reg_string_name("snapc_base",
                                   "global_snapshot_ref",
                                   "The global snapshot reference to be used for this job. "
                                   " [Default = ompi_global_snapshot_MPIRUNPID.ckpt]",
                                   false, false,
                                   NULL,
                                   &orte_snapc_base_global_snapshot_ref);

    OPAL_OUTPUT_VERBOSE((20, orte_snapc_base_output,
                         "snapc:base: open: base_global_snapshot_ref    = %s",
                         orte_snapc_base_global_snapshot_ref));

    /* Init the sequence (interval) number */
    orte_snapc_base_snapshot_seq_number = 0;

    if( NULL == orte_snapc_base_global_snapshot_loc ) {
        char *t1 = NULL;
        char *t2 = NULL;
        orte_snapc_base_unique_global_snapshot_name(&t1, getpid() );
        orte_snapc_base_get_global_snapshot_directory(&t2, t1 );
        orte_snapc_base_global_snapshot_loc = strdup(t2);
        free(t1);
        free(t2);
    }


    /* 
     * Which SnapC component to open
     *  - NULL or "" = auto-select
     *  - "none" = Empty component
     *  - ow. select that specific component
     * Note: Set the default to NULL here so ompi_info will work correctly,
     *       The 'real' default is set in base_select.c
     */
    mca_base_param_reg_string_name("snapc", NULL,
                                   "Which SNAPC component to use (empty = auto-select)",
                                   false, false,
                                   NULL, &str_value);
    if( NULL != str_value ) {
        free(str_value);
    }

    /* Open up all available components */
    if (OPAL_SUCCESS !=
        mca_base_components_open("snapc", 
                                 orte_snapc_base_output, 
                                 mca_snapc_base_static_components,
                                 &orte_snapc_base_components_available,
                                 true)) {
        return ORTE_ERROR;
    }
    
    return ORTE_SUCCESS;
}

#endif /* ORTE_DISABLE_FULL_SUPPORT */
