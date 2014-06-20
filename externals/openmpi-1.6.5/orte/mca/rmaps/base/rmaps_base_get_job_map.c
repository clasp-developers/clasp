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

#include "orte_config.h"
#include "orte/constants.h"

#include <string.h>

#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/rmaps/base/rmaps_private.h"


/*
 * Function to return a pointer to a job map
 */
orte_job_map_t* orte_rmaps_base_get_job_map(orte_jobid_t job)
{
    orte_job_t *jdata;
    orte_job_map_t *map;
    orte_job_t *daemons;
    orte_proc_t *proc;
    orte_std_cntr_t i;
    orte_node_t *node;
    
    /* lookup the job's data object */
    if (NULL == (jdata = orte_get_job_data_object(job))) {
        /* bad jobid */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return NULL;
    }
    
    /* locate the map */
    map = jdata->map;
    
    /* lookup the daemon's job data struct */
    if (NULL == (daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        /* bad jobid */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return NULL;
    }
    
    /* check to see if daemons have been launched since we
     * last updated the map
     */
    for (i=0; i < map->nodes->size; i++) {
        if (NULL != map->nodes->addr[i]) {
            node = (orte_node_t*)map->nodes->addr[i];
            if (NULL != node->daemon) {
                if (daemons->procs->size < (orte_std_cntr_t)node->daemon->name.vpid) {
                    /* well that is bad */
                    ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
                    return NULL;                    
                }
                /* find the daemon's info */
                proc = (orte_proc_t*)daemons->procs->addr[node->daemon->name.vpid];
                if (NULL == proc) {
                    /* well that is bad too */
                    ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
                    return NULL;
                }
                if (NULL != proc->rml_uri) {
                    node->daemon_launched = true;
                } else {
                    node->daemon_launched = false;
                }
            }
        }
    }
    
    return map;
}
