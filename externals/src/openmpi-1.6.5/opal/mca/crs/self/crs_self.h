/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

/**
 * @file
 * 
 * SELF CRS component
 *
 * Simple, braindead implementation.
 */

#ifndef MCA_CRS_SELF_EXPORT_H
#define MCA_CRS_SELF_EXPORT_H

#include "opal_config.h"


#include "opal/mca/mca.h"
#include "opal/mca/crs/crs.h"

BEGIN_C_DECLS

#define PREFIX_DEFAULT    ("opal_crs_self_user")
#define SUFFIX_CHECKPOINT ("checkpoint")
#define SUFFIX_CONTINUE   ("continue")
#define SUFFIX_RESTART    ("restart")

    typedef int (*opal_crs_self_checkpoint_callback_fn_t)(char **restart_cmd);
    typedef int (*opal_crs_self_continue_callback_fn_t)(void);
    typedef int (*opal_crs_self_restart_callback_fn_t)(void);

    /*
     * Local Component structures
     */
    struct opal_crs_self_component_t {
        opal_crs_base_component_t super;  /** Base CRS component */

        char *prefix;    /** Prefix for user callbacks */
        bool do_restart; /** Start by calling user restart routine in opal_init? */
        bool can_checkpoint; /** If checkpointing is enabled */

        /** User defined functions */
        opal_crs_self_checkpoint_callback_fn_t ucb_checkpoint_fn;
        opal_crs_self_continue_callback_fn_t   ucb_continue_fn;
        opal_crs_self_restart_callback_fn_t    ucb_restart_fn;
    };
    typedef struct opal_crs_self_component_t opal_crs_self_component_t;
    OPAL_MODULE_DECLSPEC extern opal_crs_self_component_t mca_crs_self_component;

    int opal_crs_self_component_query(mca_base_module_t **module, int *priority);

    /*
     * Module functions
     */
    int opal_crs_self_module_init(void);
    int opal_crs_self_module_finalize(void);

    /*
     * Actual funcationality
     */
    int opal_crs_self_checkpoint( pid_t pid,
                                  opal_crs_base_snapshot_t *snapshot,
                                  opal_crs_base_ckpt_options_t *options,
                                  opal_crs_state_type_t *state);

    int opal_crs_self_restart(    opal_crs_base_snapshot_t *snapshot, bool spawn_child, pid_t *child_pid);

    int opal_crs_self_disable_checkpoint(void);
    int opal_crs_self_enable_checkpoint(void);

    int opal_crs_self_prelaunch(int32_t rank,
                                char *base_snapshot_dir,
                                char **app,
                                char **cwd,
                                char ***argv,
                                char ***env);

    int opal_crs_self_reg_thread(void);


END_C_DECLS

#endif /* MCA_CRS_SELF_EXPORT_H */
