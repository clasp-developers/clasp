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

#include "orte_config.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "opal/mca/base/mca_base_param.h"

#include "opal/util/output.h"
#include "opal/util/opal_environ.h"

#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"

#include "snapc_full.h"

/*
 * Full module
 */
static orte_snapc_base_module_t loc_module = {
    /** Initialization Function */
    orte_snapc_full_module_init,
    /** Finalization Function */
    orte_snapc_full_module_finalize,
    orte_snapc_full_setup_job,
    orte_snapc_full_release_job,
    orte_snapc_full_ft_event,
    orte_snapc_full_start_ckpt,
    orte_snapc_full_end_ckpt
};

/*
 * Global Snapshot structure
 */
void orte_snapc_full_orted_construct(orte_snapc_full_orted_snapshot_t *obj);
void orte_snapc_full_orted_destruct( orte_snapc_full_orted_snapshot_t *obj);

OBJ_CLASS_INSTANCE(orte_snapc_full_orted_snapshot_t,
                   orte_snapc_base_global_snapshot_t,
                   orte_snapc_full_orted_construct,
                   orte_snapc_full_orted_destruct);

/*
 * Local Snapshot structure
 */
void orte_snapc_full_app_construct(orte_snapc_full_app_snapshot_t *obj);
void orte_snapc_full_app_destruct( orte_snapc_full_app_snapshot_t *obj);

OBJ_CLASS_INSTANCE(orte_snapc_full_app_snapshot_t,
                   orte_snapc_base_local_snapshot_t,
                   orte_snapc_full_app_construct,
                   orte_snapc_full_app_destruct);

/************************************
 * Locally Global vars & functions :)
 ************************************/


/************************
 * Function Definitions
 ************************/
void orte_snapc_full_orted_construct(orte_snapc_full_orted_snapshot_t *snapshot) {
    snapshot->process_name.jobid  = 0;
    snapshot->process_name.vpid   = 0;

    snapshot->state = ORTE_SNAPC_CKPT_STATE_NONE;

    snapshot->opal_crs = NULL;

    snapshot->options = OBJ_NEW(opal_crs_base_ckpt_options_t);

    snapshot->filem_request = NULL;
}

void orte_snapc_full_orted_destruct( orte_snapc_full_orted_snapshot_t *snapshot) {
    snapshot->process_name.jobid  = 0;
    snapshot->process_name.vpid   = 0;

    snapshot->state = ORTE_SNAPC_CKPT_STATE_NONE;

    if( NULL != snapshot->opal_crs ) {
        free( snapshot->opal_crs );
        snapshot->opal_crs = NULL;
    }

    if( NULL != snapshot->options ) {
        OBJ_RELEASE(snapshot->options);
        snapshot->options = NULL;
    }

    if( NULL != snapshot->filem_request ) {
        OBJ_RELEASE(snapshot->filem_request);
        snapshot->filem_request = NULL;
    }
}

void orte_snapc_full_app_construct(orte_snapc_full_app_snapshot_t *app_snapshot) {
    app_snapshot->comm_pipe_r = NULL;
    app_snapshot->comm_pipe_w = NULL;

    app_snapshot->comm_pipe_r_fd = -1;
    app_snapshot->comm_pipe_w_fd = -1;

    app_snapshot->is_eh_active = false;

    app_snapshot->process_pid  = 0;

    app_snapshot->options = OBJ_NEW(opal_crs_base_ckpt_options_t);
}

void orte_snapc_full_app_destruct( orte_snapc_full_app_snapshot_t *app_snapshot) {
    if( NULL != app_snapshot->comm_pipe_r ) {
        free(app_snapshot->comm_pipe_r);
        app_snapshot->comm_pipe_r = NULL;
    }

    if( NULL != app_snapshot->comm_pipe_w ) {
        free(app_snapshot->comm_pipe_w);
        app_snapshot->comm_pipe_w = NULL;
    }

    app_snapshot->comm_pipe_r_fd = -1;
    app_snapshot->comm_pipe_w_fd = -1;

    app_snapshot->is_eh_active = false;

    app_snapshot->process_pid  = 0;

    if( NULL != app_snapshot->options ) {
        OBJ_RELEASE(app_snapshot->options);
        app_snapshot->options = NULL;
    }
}

/*
 * MCA Functions
 */
int orte_snapc_full_component_query(mca_base_module_t **module, int *priority)
{
    opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                        "snapc:full: component_query()");

    *priority = mca_snapc_full_component.super.priority;
    *module = (mca_base_module_t *)&loc_module;

    return ORTE_SUCCESS;
}

int orte_snapc_full_module_init(bool seed, bool app)
{
    int ret, exit_status = ORTE_SUCCESS;

    opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                        "snapc:full: module_init(%d, %d)", seed, app);

    /* 
     * Global Snapshot Coordinator 
     */
    if(seed) {
        opal_output_verbose(5, mca_snapc_full_component.super.output_handle,
                            "snapc:full: module_init: Global Snapshot Coordinator");

        orte_snapc_coord_type |= ORTE_SNAPC_GLOBAL_COORD_TYPE;

        if( ORTE_SUCCESS != (ret = global_coord_init()) ) {
            exit_status = ret;
            goto cleanup;
        }
    }
    /* 
     * Local Snapshot Coordinator -- orted 
     */
    else if(!seed && !app) {
        /* 
         * JJH Currently we are not guarenteed a bootproxy, and we have no way
         * JJH (that I know of) to tell if we were generated from the bootproxy
         * JJH or from the HNP inside the application.
         * JJH so for this component we assume that there exists a local coordinator
         */
        opal_output_verbose(5, mca_snapc_full_component.super.output_handle,
                            "snapc:full: module_init: Local Snapshot Coordinator");

        orte_snapc_coord_type |= ORTE_SNAPC_LOCAL_COORD_TYPE;

        if( ORTE_SUCCESS != (ret = local_coord_init()) ) {
            exit_status = ret;
            goto cleanup;
        }
    }
    /* 
     * Application Snapshot Coordinator
     */
    else if(app) {
        opal_output_verbose(5, mca_snapc_full_component.super.output_handle,
                            "snapc:full: module_init: Application Snapshot Coordinator");

        orte_snapc_coord_type |= ORTE_SNAPC_APP_COORD_TYPE;

        if( ORTE_SUCCESS != (ret = app_coord_init()) ) {
            exit_status = ret;
            goto cleanup;
        }
    }
    else {
        /* 
         * Logically this should not happen
         */
        opal_output_verbose(5, mca_snapc_full_component.super.output_handle,
                            "snapc:full: module_init: Unknown Snapshot Coordinator");

        orte_snapc_coord_type = ORTE_SNAPC_UNASSIGN_TYPE;

        exit_status = ORTE_ERROR;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

int orte_snapc_full_module_finalize(void)
{
    opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                        "snapc:full: module_finalize()");

    switch(orte_snapc_coord_type) 
        {
        case ORTE_SNAPC_GLOBAL_COORD_TYPE:
            global_coord_finalize();
            break;
        case ORTE_SNAPC_LOCAL_COORD_TYPE:
            local_coord_finalize();
            break;
        case ORTE_SNAPC_APP_COORD_TYPE:
            app_coord_finalize();
            break;
        default:
            break;
        }

    orte_snapc_coord_type = ORTE_SNAPC_UNASSIGN_TYPE;

    return ORTE_SUCCESS;
}

int orte_snapc_full_setup_job(orte_jobid_t jobid) {
    int ret, exit_status = ORTE_SUCCESS;

    if( ORTE_SNAPC_GLOBAL_COORD_TYPE == (orte_snapc_coord_type & ORTE_SNAPC_GLOBAL_COORD_TYPE)) {
        if(ORTE_SUCCESS != (ret = global_coord_setup_job(jobid) ) ) {
            exit_status = ret;
        }
    }
    else if( ORTE_SNAPC_LOCAL_COORD_TYPE == (orte_snapc_coord_type & ORTE_SNAPC_LOCAL_COORD_TYPE)) {
        if(ORTE_SUCCESS != (ret = local_coord_setup_job(jobid) ) ) {
            exit_status = ret;
        }
    }

    return exit_status;
}

int orte_snapc_full_release_job(orte_jobid_t jobid) {
    int ret, exit_status = ORTE_SUCCESS;

    if( ORTE_SNAPC_GLOBAL_COORD_TYPE == (orte_snapc_coord_type & ORTE_SNAPC_GLOBAL_COORD_TYPE)) {
        if(ORTE_SUCCESS != (ret = global_coord_release_job(jobid) ) ) {
            exit_status = ret;
        }
    }
    else if( ORTE_SNAPC_LOCAL_COORD_TYPE == (orte_snapc_coord_type & ORTE_SNAPC_LOCAL_COORD_TYPE )) {
        if(ORTE_SUCCESS != (ret = local_coord_release_job(jobid) ) ) {
            exit_status = ret;
        }
    }

    return exit_status;
}

int orte_snapc_full_ft_event(int state) {
    switch(orte_snapc_coord_type) 
        {
        case ORTE_SNAPC_GLOBAL_COORD_TYPE:
        case ORTE_SNAPC_LOCAL_COORD_TYPE:
            ; /* Do nothing */
            break;
        case ORTE_SNAPC_APP_COORD_TYPE:
            return app_coord_ft_event(state);
            break;
        default:
            break;
        }

    return ORTE_SUCCESS;
}

int orte_snapc_full_start_ckpt(orte_snapc_base_quiesce_t *datum)
{
    switch(orte_snapc_coord_type) 
        {
        case ORTE_SNAPC_GLOBAL_COORD_TYPE:
            return global_coord_start_ckpt(datum);
            break;
        case ORTE_SNAPC_LOCAL_COORD_TYPE:
            ; /* Do nothing */
            break;
        case ORTE_SNAPC_APP_COORD_TYPE:
            return app_coord_start_ckpt(datum);
            break;
        default:
            break;
        }

    return ORTE_SUCCESS;
}

int orte_snapc_full_end_ckpt(orte_snapc_base_quiesce_t *datum)
{
    switch(orte_snapc_coord_type) 
        {
        case ORTE_SNAPC_GLOBAL_COORD_TYPE:
            return global_coord_end_ckpt(datum);
            break;
        case ORTE_SNAPC_LOCAL_COORD_TYPE:
            ; /* Do nothing */
            break;
        case ORTE_SNAPC_APP_COORD_TYPE:
            return app_coord_end_ckpt(datum);
            break;
        default:
            break;
        }

    return ORTE_SUCCESS;
}

/******************
 * Local functions
 ******************/
