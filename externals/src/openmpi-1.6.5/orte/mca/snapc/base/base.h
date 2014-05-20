/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
#ifndef ORTE_SNAPC_BASE_H
#define ORTE_SNAPC_BASE_H

#include "orte_config.h"
#include "orte/types.h"


#include "orte/mca/snapc/snapc.h"

/*
 * Global functions for MCA overall SNAPC
 */

BEGIN_C_DECLS

/**
 * Initialize the SNAPC MCA framework
 *
 * @retval ORTE_SUCCESS Upon success
 * @retval ORTE_ERROR   Upon failures
 * 
 * This function is invoked during orte_init();
 */
ORTE_DECLSPEC int orte_snapc_base_open(void);

#if !ORTE_DISABLE_FULL_SUPPORT

/*
 * Commands for command line tool and SnapC interaction
 */
typedef uint8_t orte_snapc_cmd_flag_t;
#define ORTE_SNAPC_CMD  OPAL_UINT8
#define ORTE_SNAPC_GLOBAL_INIT_CMD    1
#define ORTE_SNAPC_GLOBAL_TERM_CMD    2
#define ORTE_SNAPC_GLOBAL_UPDATE_CMD  3
#define ORTE_SNAPC_LOCAL_UPDATE_CMD   4

/**
 * There are 3 types of Coordinators, and any process may be once or more type.
 * e.g., orterun is can often be both a Global and Local coordinator if it
 *       launches processes locally.
 */
typedef uint32_t orte_snapc_coord_type_t;
#define ORTE_SNAPC_UNASSIGN_TYPE     0
#define ORTE_SNAPC_GLOBAL_COORD_TYPE 1
#define ORTE_SNAPC_LOCAL_COORD_TYPE  2
#define ORTE_SNAPC_APP_COORD_TYPE    4
ORTE_DECLSPEC extern orte_snapc_coord_type_t orte_snapc_coord_type;

#define ORTE_SNAPC_COORD_NAME_PRINT(ct) ( (ct == (ORTE_SNAPC_GLOBAL_COORD_TYPE | ORTE_SNAPC_LOCAL_COORD_TYPE) ) ? "Global-Local" : \
                                          (ct ==  ORTE_SNAPC_GLOBAL_COORD_TYPE) ? "Global" : \
                                          (ct ==  ORTE_SNAPC_LOCAL_COORD_TYPE)  ? "Local"  : \
                                          (ct ==  ORTE_SNAPC_APP_COORD_TYPE)    ? "App"    : \
                                          "Unknown")

    /**
     * Global Snapshot Object Maintenance functions
     */
    void orte_snapc_base_local_snapshot_construct(orte_snapc_base_local_snapshot_t *obj);
    void orte_snapc_base_local_snapshot_destruct( orte_snapc_base_local_snapshot_t *obj);

    void orte_snapc_base_global_snapshot_construct(orte_snapc_base_global_snapshot_t *obj);
    void orte_snapc_base_global_snapshot_destruct( orte_snapc_base_global_snapshot_t *obj);

    /**
     * Select an available component.
     *
     * @retval ORTE_SUCCESS Upon Success
     * @retval ORTE_NOT_FOUND If no component can be selected
     * @retval ORTE_ERROR Upon other failure
     *
     */
    ORTE_DECLSPEC int orte_snapc_base_select(bool seed, bool app);
    
    /**
     * Finalize the SNAPC MCA framework
     *
     * @retval ORTE_SUCCESS Upon success
     * @retval ORTE_ERROR   Upon failures
     * 
     * This function is invoked during orte_finalize();
     */
    ORTE_DECLSPEC int orte_snapc_base_close(void);

    void orte_snapc_base_quiesce_construct(orte_snapc_base_quiesce_t *obj);
    void orte_snapc_base_quiesce_destruct( orte_snapc_base_quiesce_t *obj);

    /**
     * 'None' component functions
     * These are to be used when no component is selected.
     * They just return success, and empty strings as necessary.
     */
    ORTE_DECLSPEC int orte_snapc_base_none_open(void);
    ORTE_DECLSPEC int orte_snapc_base_none_close(void);
    ORTE_DECLSPEC int orte_snapc_base_none_query(mca_base_module_t **module, int *priority);

    ORTE_DECLSPEC int orte_snapc_base_module_init(bool seed, bool app);
    ORTE_DECLSPEC int orte_snapc_base_module_finalize(void);
    ORTE_DECLSPEC int orte_snapc_base_none_setup_job(orte_jobid_t jobid);
    ORTE_DECLSPEC int orte_snapc_base_none_release_job(orte_jobid_t jobid);
    ORTE_DECLSPEC int orte_snapc_base_none_ft_event(int state);
    ORTE_DECLSPEC int orte_snapc_base_none_start_ckpt(orte_snapc_base_quiesce_t *datum);
    ORTE_DECLSPEC int orte_snapc_base_none_end_ckpt(orte_snapc_base_quiesce_t *datum);

    ORTE_DECLSPEC extern int  orte_snapc_base_output;
    ORTE_DECLSPEC extern opal_list_t orte_snapc_base_components_available;
    ORTE_DECLSPEC extern orte_snapc_base_component_t orte_snapc_base_selected_component;
    ORTE_DECLSPEC extern orte_snapc_base_module_t orte_snapc;

    /**
     * Globals
     */
#define orte_snapc_base_metadata_filename (strdup("global_snapshot_meta.data"))

    ORTE_DECLSPEC extern char * orte_snapc_base_global_snapshot_dir;
    ORTE_DECLSPEC extern char * orte_snapc_base_global_snapshot_ref;
    ORTE_DECLSPEC extern char * orte_snapc_base_global_snapshot_loc;
    ORTE_DECLSPEC extern bool   orte_snapc_base_store_in_place;
    ORTE_DECLSPEC extern bool   orte_snapc_base_store_only_one_seq;
    ORTE_DECLSPEC extern bool   orte_snapc_base_establish_global_snapshot_dir;
    ORTE_DECLSPEC extern bool   orte_snapc_base_is_global_dir_shared;
    ORTE_DECLSPEC extern size_t orte_snapc_base_snapshot_seq_number;


    /**
     * Some utility functions
     */
    ORTE_DECLSPEC void orte_snapc_ckpt_state_notify(int state);
    ORTE_DECLSPEC int orte_snapc_ckpt_state_str(char ** state_str, int state);

    ORTE_DECLSPEC int orte_snapc_base_unique_global_snapshot_name(char **name_str, pid_t pid);
    ORTE_DECLSPEC int orte_snapc_base_get_global_snapshot_metadata_file(char **file_name, char *uniq_snapshot_name);
    ORTE_DECLSPEC int orte_snapc_base_get_global_snapshot_directory(char **dir_name, char *uniq_global_snapshot_name);
    ORTE_DECLSPEC int orte_snapc_base_init_global_snapshot_directory(char *uniq_global_snapshot_name,
                                                                        bool empty_metadata);
    ORTE_DECLSPEC int orte_snapc_base_add_timestamp(char * global_snapshot_ref);
    ORTE_DECLSPEC int orte_snapc_base_add_vpid_metadata(orte_process_name_t *proc,
                                                        char * global_snapshot_ref,
                                                        char *snapshot_ref,
                                                        char *snapshot_location,
                                                        char *crs_agent);
    ORTE_DECLSPEC int orte_snapc_base_finalize_metadata(char * global_snapshot_ref);
    ORTE_DECLSPEC int orte_snapc_base_extract_metadata(orte_snapc_base_global_snapshot_t *snapshot);

    ORTE_DECLSPEC int orte_snapc_base_get_all_snapshot_refs(char *base_dir, int *num_refs, char ***snapshot_refs);
    ORTE_DECLSPEC int orte_snapc_base_get_all_snapshot_ref_seqs(char *base_dir, char *snapshot_name, int *num_seqs, int **snapshot_ref_seqs);

    /*******************************
     * Global Coordinator functions
     *******************************/
    /* Initial handshake with the orte_checkpoint command */
    ORTE_DECLSPEC int orte_snapc_base_global_coord_ckpt_init_cmd(orte_process_name_t* peer,
                                                                 opal_buffer_t* buffer,
                                                                 opal_crs_base_ckpt_options_t *options,
                                                                 orte_jobid_t *jobid);
    ORTE_DECLSPEC int orte_snapc_base_global_coord_ckpt_update_cmd(orte_process_name_t* peer,
                                                                   char *global_snapshot_handle,
                                                                   int seq_num,
                                                                   int ckpt_status);

    ORTE_DECLSPEC int orte_snapc_base_unpack_options(opal_buffer_t* buffer,
                                                     opal_crs_base_ckpt_options_t *options);
    ORTE_DECLSPEC int orte_snapc_base_pack_options(opal_buffer_t* buffer,
                                                   opal_crs_base_ckpt_options_t *options);

#endif /* ORTE_DISABLE_FULL_SUPPORT */

END_C_DECLS

#endif /* ORTE_SNAPC_BASE_H */
