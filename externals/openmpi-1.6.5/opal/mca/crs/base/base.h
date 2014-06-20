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
#ifndef OPAL_CRS_BASE_H
#define OPAL_CRS_BASE_H

#include "opal_config.h"
#include "opal/mca/crs/crs.h"
#include "opal/util/opal_environ.h"

/*
 * Global functions for MCA overall CRS
 */

BEGIN_C_DECLS

/* Some local strings to use genericly with the local metadata file */
#define CRS_METADATA_BASE       ("# ")
#define CRS_METADATA_COMP       ("# Component: ")
#define CRS_METADATA_PID        ("# PID: ")
#define CRS_METADATA_CONTEXT    ("# CONTEXT: ")
#define CRS_METADATA_MKDIR      ("# MKDIR: ")
#define CRS_METADATA_TOUCH      ("# TOUCH: ")

    /**
     * Initialize the CRS MCA framework
     *
     * @retval OPAL_SUCCESS Upon success
     * @retval OPAL_ERROR   Upon failures
     * 
     * This function is invoked during opal_init();
     */
    OPAL_DECLSPEC int opal_crs_base_open(void);
    
    /**
     * Select an available component.
     *
     * @retval OPAL_SUCCESS Upon Success
     * @retval OPAL_NOT_FOUND If no component can be selected
     * @retval OPAL_ERROR Upon other failure
     *
     */
    OPAL_DECLSPEC int opal_crs_base_select(void);
    
    /**
     * Finalize the CRS MCA framework
     *
     * @retval OPAL_SUCCESS Upon success
     * @retval OPAL_ERROR   Upon failures
     * 
     * This function is invoked during opal_finalize();
     */
    OPAL_DECLSPEC int opal_crs_base_close(void);

    /**
     * Globals
     */
#define opal_crs_base_metadata_filename (strdup("snapshot_meta.data"))

    OPAL_DECLSPEC extern int  opal_crs_base_output;
    OPAL_DECLSPEC extern opal_list_t opal_crs_base_components_available;
    OPAL_DECLSPEC extern opal_crs_base_component_t opal_crs_base_selected_component;
    OPAL_DECLSPEC extern opal_crs_base_module_t opal_crs;
    OPAL_DECLSPEC extern char * opal_crs_base_snapshot_dir;

    /**
     * Some utility functions
     */
    OPAL_DECLSPEC char * opal_crs_base_state_str(opal_crs_state_type_t state);

    OPAL_DECLSPEC char * opal_crs_base_unique_snapshot_name(pid_t pid);
    OPAL_DECLSPEC int    opal_crs_base_extract_expected_component(char *snapshot_loc, char ** component_name, int *prev_pid);
    OPAL_DECLSPEC int    opal_crs_base_init_snapshot_directory(opal_crs_base_snapshot_t *snapshot);
    OPAL_DECLSPEC char * opal_crs_base_get_snapshot_directory(char *uniq_snapshot_name);

    /*
     * Read a token to the metadata file
     * NULL can be passed for snapshot_loc if nit_snapshot_directory has been called.
     */
    OPAL_DECLSPEC int opal_crs_base_metadata_read_token(char *snapshot_loc, char * token, char ***value);

    /*
     * Write a token to the metadata file
     * NULL can be passed for snapshot_loc if nit_snapshot_directory has been called.
     */
    OPAL_DECLSPEC int opal_crs_base_metadata_write_token(char *snapshot_loc, char * token, char *value);

    /*
     * Register a file for cleanup.
     * Useful in C/R when files only need to temporarily exist for restart
     */
    OPAL_DECLSPEC int opal_crs_base_cleanup_append(char* filename, bool is_dir);

    /*
     * Flush the cleanup of all registered files.
     */
    OPAL_DECLSPEC int opal_crs_base_cleanup_flush(void);

    /*
     * Copy the options structure
     */
    OPAL_DECLSPEC int opal_crs_base_copy_options(opal_crs_base_ckpt_options_t *from,
                                                 opal_crs_base_ckpt_options_t *to);
    /*
     * Clear the options structure
     */
    OPAL_DECLSPEC int opal_crs_base_clear_options(opal_crs_base_ckpt_options_t *target);

END_C_DECLS

#endif /* OPAL_CRS_BASE_H */
