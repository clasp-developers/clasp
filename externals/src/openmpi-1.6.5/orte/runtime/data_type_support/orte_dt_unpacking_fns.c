/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
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

#include "orte_config.h"
#include "orte/types.h"

#include <sys/types.h>

#include "orte/mca/errmgr/errmgr.h"
#include "opal/dss/dss.h"
#include "opal/dss/dss_internal.h"
#include "orte/runtime/data_type_support/orte_dt_support.h"

/*
 * ORTE_STD_CNTR
 */
int orte_dt_unpack_std_cntr(opal_buffer_t *buffer, void *dest,
                             int32_t *num_vals, opal_data_type_t type)
{
    int ret;
    
    /* Turn around and unpack the real type */
    if (ORTE_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, dest, num_vals, ORTE_STD_CNTR_T))) {
        ORTE_ERROR_LOG(ret);
    }
    
    return ret;
}

/*
 * NAME
 */
int orte_dt_unpack_name(opal_buffer_t *buffer, void *dest,
                             int32_t *num_vals, opal_data_type_t type)
{
    int rc;
    int32_t i, num;
    orte_process_name_t* proc;
    orte_jobid_t *jobid;
    orte_vpid_t *vpid;
    
    num = *num_vals;
    
    /* allocate space for all the jobids in a contiguous array */
    jobid = (orte_jobid_t*)malloc(num * sizeof(orte_jobid_t));
    if (NULL == jobid) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        *num_vals = 0;
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    /* now unpack them in one shot */
    if (ORTE_SUCCESS != (rc =
                         orte_dt_unpack_jobid(buffer, jobid, num_vals, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        *num_vals = 0;
        free(jobid);
        return rc;
    }
    
    /* collect all the vpids in a contiguous array */
    vpid = (orte_vpid_t*)malloc(num * sizeof(orte_vpid_t));
    if (NULL == vpid) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        *num_vals = 0;
        free(jobid);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    /* now unpack them in one shot */
    if (ORTE_SUCCESS != (rc =
                         orte_dt_unpack_vpid(buffer, vpid, num_vals, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        *num_vals = 0;
        free(vpid);
        free(jobid);
        return rc;
    }
    
    /* build the names from the jobid/vpid arrays */
    proc = (orte_process_name_t*)dest;
    for (i=0; i < num; i++) {
        proc->jobid = jobid[i];
        proc->vpid = vpid[i];
        proc++;
    }
    
    /* cleanup */
    free(vpid);
    free(jobid);
    
    return ORTE_SUCCESS;
}

/*
 * JOBID
 */
int orte_dt_unpack_jobid(opal_buffer_t *buffer, void *dest,
                              int32_t *num_vals, opal_data_type_t type)
{
    int ret;
    
    /* Turn around and unpack the real type */
    if (ORTE_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, dest, num_vals, ORTE_JOBID_T))) {
        ORTE_ERROR_LOG(ret);
    }
    
    return ret;
}

/*
 * VPID
 */
int orte_dt_unpack_vpid(opal_buffer_t *buffer, void *dest,
                             int32_t *num_vals, opal_data_type_t type)
{
    int ret;
    
    /* Turn around and unpack the real type */
    if (ORTE_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, dest, num_vals, ORTE_VPID_T))) {
        ORTE_ERROR_LOG(ret);
    }
    
    return ret;
}

#if !ORTE_DISABLE_FULL_SUPPORT
/*
 * JOB
 * NOTE: We do not pack all of the job object's fields as many of them have no
 * value in sending them to another location. The only purpose in packing and
 * sending a job object is to communicate the data required to dynamically
 * spawn another job - so we only pack that limited set of required data.
 * Therefore, only unpack what was packed
 */
int orte_dt_unpack_job(opal_buffer_t *buffer, void *dest,
                       int32_t *num_vals, opal_data_type_t type)
{
    int rc;
    int32_t i, j, n, np, nprocs;
    orte_job_t **jobs;
    orte_proc_t *proc;
    
    /* unpack into array of orte_job_t objects */
    jobs = (orte_job_t**) dest;
    for (i=0; i < *num_vals; i++) {

        /* create the orte_job_t object */
        jobs[i] = OBJ_NEW(orte_job_t);
        if (NULL == jobs[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        /* unpack the jobid */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                                &(jobs[i]->jobid), &n, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the num apps */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                                (&(jobs[i]->num_apps)), &n, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* if there are apps, unpack them */
        if (0 < jobs[i]->num_apps) {
            orte_app_context_t *app;
            for (j=0; j < jobs[i]->num_apps; j++) {
                n = 1;
                if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                               &app, &n, ORTE_APP_CONTEXT))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                opal_pointer_array_add(jobs[i]->apps, app);
            }
        }
        
        /* unpack control flags */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                            (&(jobs[i]->controls)), &n, ORTE_JOB_CONTROL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
                
        /* unpack stdin target */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                            (&(jobs[i]->stdin_target)), &n, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the total slots allocated to the job */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (&(jobs[i]->total_slots_alloc)), &n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the number of procs */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (void*)(&(jobs[i]->num_procs)), &n, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the actual number of proc entries in the message */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, (void*)&nprocs, &n, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (0 < nprocs) {
            for (np=0; np < nprocs; np++) {
                n = 1;
                if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                                 (void**)&proc, &n, ORTE_PROC))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                opal_pointer_array_set_item(jobs[i]->procs, proc->name.vpid, proc);
            }
        }
        
        /* if the map is NULL, then we din't pack it as there was
         * nothing to pack. Instead, we packed a flag to indicate whether or not
         * the map is included         */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                                            &j, &n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (0 < j) {
            /* unpack the map */
            n = 1;
            if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                                            (&(jobs[i]->map)), &n, ORTE_JOB_MAP))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        
        /* no bookmark of oversubscribe_override flags to unpack */
        
        /* unpack the job state */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (&(jobs[i]->state)), &n, ORTE_JOB_STATE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the number launched */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (&(jobs[i]->num_launched)), &n, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the number reported */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (&(jobs[i]->num_reported)), &n, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the number terminated */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (&(jobs[i]->num_terminated)), &n, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the abort flag */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (&(jobs[i]->abort)), &n, OPAL_BOOL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

#if OPAL_ENABLE_FT_CR == 1
        /* unpack the ckpt state */
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (&(jobs[i]->ckpt_state)), &n, OPAL_SIZE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the ckpt snapshot ref */
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (&(jobs[i]->ckpt_snapshot_ref)), &n, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the ckpt snapshot loc */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (&(jobs[i]->ckpt_snapshot_loc)), &n, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
#endif
    }

    return ORTE_SUCCESS;
}

/*
 * NODE
 */
int orte_dt_unpack_node(opal_buffer_t *buffer, void *dest,
                        int32_t *num_vals, opal_data_type_t type)
{
    int rc;
    int32_t i, n;
    orte_node_t **nodes;
    
    /* unpack into array of orte_node_t objects */
    nodes = (orte_node_t**) dest;
    for (i=0; i < *num_vals; i++) {
        
        /* create the node object */
        nodes[i] = OBJ_NEW(orte_node_t);
        if (NULL == nodes[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        
        /* do not unpack the index - meaningless here */
        
        /* unpack the node name */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         &(nodes[i]->name), &n, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* do not unpack the daemon name or launch id */
        
        /* unpack the number of procs on the node */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (&(nodes[i]->num_procs)), &n, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* do not unpack the proc info */
        
        /* unpack whether we are oversubscribed or not */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (&(nodes[i]->oversubscribed)), &n, OPAL_BOOL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the state */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (&(nodes[i]->state)), &n, ORTE_NODE_STATE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the number of slots */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (&(nodes[i]->slots)), &n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the number of slots in use */
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (&(nodes[i]->slots_inuse)), &n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the number of slots allocated */
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (&(nodes[i]->slots_alloc)), &n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the max number of slots */
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (&(nodes[i]->slots_max)), &n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* do not unpack the board, socket, and core info */
        
        /* do not unpack the username */
    }
    return ORTE_SUCCESS;
}

/*
 * PROC
 */
int orte_dt_unpack_proc(opal_buffer_t *buffer, void *dest,
                        int32_t *num_vals, opal_data_type_t type)
{
    int rc;
    int32_t i, n;
    orte_proc_t **procs;
    
    /* unpack into array of orte_proc_t objects */
    procs = (orte_proc_t**) dest;
    for (i=0; i < *num_vals; i++) {
        
        /* create the orte_proc_t object */
        procs[i] = OBJ_NEW(orte_proc_t);
        if (NULL == procs[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        
        /* unpack the name */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         &(procs[i]->name), &n, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the pid */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (&(procs[i]->pid)), &n, OPAL_PID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the local rank */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (&(procs[i]->local_rank)), &n, ORTE_LOCAL_RANK))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the local rank */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                          (&(procs[i]->node_rank)), &n, ORTE_NODE_RANK))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the state */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (&(procs[i]->state)), &n, ORTE_PROC_STATE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the app context index */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (&(procs[i]->app_idx)), &n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the name of the node where this proc is executing */
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                          (void*)(&(procs[i]->nodename)), &n, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the number of restarts */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                          (&(procs[i]->restarts)), &n, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
#if OPAL_ENABLE_FT_CR == 1
        /* unpack the ckpt state */
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (&(procs[i]->ckpt_state)), &n, OPAL_SIZE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the ckpt snapshot ref */
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (&(procs[i]->ckpt_snapshot_ref)), &n, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the ckpt snapshot loc */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                         (&(procs[i]->ckpt_snapshot_loc)), &n, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
#endif
    }
    return ORTE_SUCCESS;
}

/*
 * APP_CONTEXT
 */
int orte_dt_unpack_app_context(opal_buffer_t *buffer, void *dest,
                                 int32_t *num_vals, opal_data_type_t type)
{
    int rc;
    orte_app_context_t **app_context;
    int32_t i, max_n=1, count;
    int8_t have_prefix, user_specified, have_preload_files, have_preload_files_dest_dir;

    /* unpack into array of app_context objects */
    app_context = (orte_app_context_t**) dest;
    for (i=0; i < *num_vals; i++) {

        /* create the app_context object */
        app_context[i] = OBJ_NEW(orte_app_context_t);
        if (NULL == app_context[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        /* get the app index number */
        max_n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, &(app_context[i]->idx),
                    &max_n, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the application name */
        max_n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, &(app_context[i]->app),
                    &max_n, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* get the number of processes */
        max_n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, &(app_context[i]->num_procs),
                    &max_n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* get the number of argv strings that were packed */
        max_n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, &count, &max_n, ORTE_STD_CNTR))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }

        /* if there are argv strings, allocate the required space for the char * pointers */
        if (0 < count) {
            app_context[i]->argv = (char **)malloc((count+1) * sizeof(char*));
            if (NULL == app_context[i]->argv) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            app_context[i]->argv[count] = NULL;

            /* and unpack them */
            max_n = count;
            if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, app_context[i]->argv, &max_n, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }

        /* get the number of env strings */
        max_n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, &count, &max_n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if there are env strings, allocate the required space for the char * pointers */
        if (0 < count) {
            app_context[i]->env = (char **)malloc((count+1) * sizeof(char*));
            if (NULL == app_context[i]->env) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            app_context[i]->env[count] = NULL;

            /* and unpack them */
            max_n = count;
            if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, app_context[i]->env, &max_n, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }

        /* unpack the cwd */
        max_n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, &app_context[i]->cwd,
                    &max_n, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the user-specified cwd flag */
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, &user_specified,
                    &max_n, OPAL_INT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (user_specified) {
            app_context[i]->user_specified_cwd = true;
        } else {
            app_context[i]->user_specified_cwd = false;
        }

        /* unpack the hostfile name */
        max_n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, &(app_context[i]->hostfile),
                                                         &max_n, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the add_hostfile name */
        max_n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, &(app_context[i]->add_hostfile),
                                                         &max_n, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* get the number of add_host strings that were packed */
        max_n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, &count, &max_n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* if there are dash_host strings, allocate the required space for the char * pointers */
        if (0 < count) {
            app_context[i]->add_host = (char **)malloc((count+1) * sizeof(char*));
            if (NULL == app_context[i]->add_host) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            app_context[i]->add_host[count] = NULL;
            
            /* and unpack them */
            max_n = count;
            if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, app_context[i]->add_host, &max_n, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        
        /* get the number of dash_host strings that were packed */
        max_n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, &count, &max_n, ORTE_STD_CNTR))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }

        /* if there are dash_host strings, allocate the required space for the char * pointers */
        if (0 < count) {
            app_context[i]->dash_host = (char **)malloc((count+1) * sizeof(char*));
            if (NULL == app_context[i]->dash_host) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            app_context[i]->dash_host[count] = NULL;

            /* and unpack them */
            max_n = count;
            if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, app_context[i]->dash_host, &max_n, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }

        /* unpack the prefix dir if there is one */
        max_n=1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, &have_prefix,
                   &max_n, OPAL_INT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (have_prefix) {
            if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, &app_context[i]->prefix_dir,
                                                             &max_n, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        } else {
            app_context[i]->prefix_dir = NULL;
        }

        /* Unpack the preload_binary flag */
        max_n=1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, &(app_context[i]->preload_binary),
                   &max_n, OPAL_BOOL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* Unpack the preload_libs flag */
        max_n=1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, &(app_context[i]->preload_libs),
                                                         &max_n, OPAL_BOOL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* Unpack the preload_files set */
        max_n=1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, &have_preload_files,
                                                         &max_n, OPAL_INT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (have_preload_files) {
            if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, &app_context[i]->preload_files,
                                                             &max_n, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        } else {
            app_context[i]->preload_files = NULL;
        }

        /* Unpack the preload_files_dest_dir set */
        max_n=1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, &have_preload_files_dest_dir,
                                                         &max_n, OPAL_INT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (have_preload_files_dest_dir) {
            if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, &app_context[i]->preload_files_dest_dir,
                                                             &max_n, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        } else {
            app_context[i]->preload_files_dest_dir = NULL;
        }

        /* Unpack the preload_files_src_dir set */
        max_n=1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, &have_preload_files_dest_dir,
                                                         &max_n, OPAL_INT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (have_preload_files_dest_dir) {
            if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, &app_context[i]->preload_files_src_dir,
                                                             &max_n, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        } else {
            app_context[i]->preload_files_src_dir = NULL;
        }
        
    }

    return ORTE_SUCCESS;
}

/*
 * EXIT CODE
 */
int orte_dt_unpack_exit_code(opal_buffer_t *buffer, void *dest,
                                   int32_t *num_vals, opal_data_type_t type)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, dest, num_vals, ORTE_EXIT_CODE_T))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * NODE STATE
 */
int orte_dt_unpack_node_state(opal_buffer_t *buffer, void *dest,
                                    int32_t *num_vals, opal_data_type_t type)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, dest, num_vals, ORTE_NODE_STATE_T))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * PROC STATE
 */
int orte_dt_unpack_proc_state(opal_buffer_t *buffer, void *dest,
                                    int32_t *num_vals, opal_data_type_t type)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, dest, num_vals, ORTE_PROC_STATE_T))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * JOB STATE
 */
int orte_dt_unpack_job_state(opal_buffer_t *buffer, void *dest,
                                   int32_t *num_vals, opal_data_type_t type)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, dest, num_vals, ORTE_JOB_STATE_T))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * JOB_MAP
 * NOTE: There is no obvious reason to include all the node information when
 * sending a map - hence, we do not pack that field, so don't unpack it here
 */
int orte_dt_unpack_map(opal_buffer_t *buffer, void *dest,
                               int32_t *num_vals, opal_data_type_t type)
{
    int rc;
    int32_t i, n;
    orte_job_map_t **maps;
    
    /* unpack into array of orte_job_map_t objects */
    maps = (orte_job_map_t**) dest;
    for (i=0; i < *num_vals; i++) {
        
        /* create the orte_rmaps_base_map_t object */
        maps[i] = OBJ_NEW(orte_job_map_t);
        if (NULL == maps[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        
        /* unpack the policy */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                                                         &(maps[i]->policy), &n, ORTE_MAPPING_POLICY))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the #procs/node */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                                                         &(maps[i]->npernode), &n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the oversubscribe flag */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                                                         &(maps[i]->oversubscribe), &n, OPAL_BOOL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the display map flag */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                                                         &(maps[i]->display_map), &n, OPAL_BOOL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the number of daemons to be created */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, &(maps[i]->num_new_daemons), &n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the starting vpid of the new daemons */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, &(maps[i]->daemon_vpid_start), &n, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the number of nodes */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss_unpack_buffer(buffer,
                                                         &(maps[i]->num_nodes), &n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    return ORTE_SUCCESS;
}

/*
 * RML_TAG
 */
int orte_dt_unpack_tag(opal_buffer_t *buffer, void *dest,
                             int32_t *num_vals, opal_data_type_t type)
{
    int ret;
    
    /* Turn around and unpack the real type */
    if (ORTE_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, dest, num_vals, ORTE_RML_TAG_T))) {
        ORTE_ERROR_LOG(ret);
    }
    
    return ret;
}

/*
 * ORTE_DAEMON_CMD
 */
int orte_dt_unpack_daemon_cmd(opal_buffer_t *buffer, void *dest, int32_t *num_vals,
                                opal_data_type_t type)
{
    int ret;
    
    /* turn around and unpack the real type */
    ret = opal_dss_unpack_buffer(buffer, dest, num_vals, ORTE_DAEMON_CMD_T);
    
    return ret;
}

/*
 * ORTE_GRPCOMM_MODE
 */
int orte_dt_unpack_grpcomm_mode(opal_buffer_t *buffer, void *dest, int32_t *num_vals,
                                opal_data_type_t type)
{
    int ret;
    
    /* turn around and unpack the real type */
    ret = opal_dss_unpack_buffer(buffer, dest, num_vals, ORTE_GRPCOMM_MODE_T);
    
    return ret;
}

/*
 * ORTE_IOF_TAG
 */
int orte_dt_unpack_iof_tag(opal_buffer_t *buffer, void *dest, int32_t *num_vals,
                                opal_data_type_t type)
{
    int ret;
    
    /* turn around and unpack the real type */
    ret = opal_dss_unpack_buffer(buffer, dest, num_vals, ORTE_IOF_TAG_T);
    
    return ret;
}

#endif
