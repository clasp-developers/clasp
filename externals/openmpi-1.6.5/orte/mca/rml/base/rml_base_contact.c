/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
/** @file */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include "opal/util/argv.h"
#include "opal/util/output.h"

#include "opal/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/rml/base/base.h"


int orte_rml_base_get_contact_info(orte_jobid_t job, opal_buffer_t *data)
{
    orte_vpid_t i;
    orte_job_t *jdata;
    orte_proc_t **procs;
    int rc;
    
    /* lookup the job */
    if (NULL == (jdata = orte_get_job_data_object(job))) {
        /* bad jobid */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* cycle through all procs in the job, adding their contact info to the buffer */
    procs = (orte_proc_t**)jdata->procs->addr;
    for (i=0; i < jdata->num_procs; i++) {
        /* if this proc doesn't have any contact info, ignore it */
        if (NULL == procs[i]->rml_uri) {
            continue;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &procs[i]->rml_uri, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    return ORTE_SUCCESS;
}

int orte_rml_base_update_contact_info(opal_buffer_t* data)
{
    orte_std_cntr_t cnt;
    orte_vpid_t num_procs;
    char *rml_uri;
    orte_process_name_t name;
    bool got_name;
    int rc;

    /* unpack the data for each entry */
    num_procs = 0;
    name.jobid = ORTE_JOBID_INVALID;
    got_name = false;
    cnt = 1;
    while (ORTE_SUCCESS == (rc = opal_dss.unpack(data, &rml_uri, &cnt, OPAL_STRING))) {
        
        OPAL_OUTPUT_VERBOSE((5, orte_rml_base_output,
                             "%s rml:base:update:contact:info got uri %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             NULL == rml_uri ? "NULL" : rml_uri));
        
        if (NULL != rml_uri) {
            /* set the contact info into the hash table */
            if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(rml_uri))) {
                ORTE_ERROR_LOG(rc);
                free(rml_uri);
                return(rc);
            }
            if (!got_name) {
                /* we only get an update from a single jobid - the command
                 * that creates these doesn't cross jobid boundaries - so
                 * record it here
                 */
                if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(rml_uri, &name, NULL))) {
                    ORTE_ERROR_LOG(rc);
                    free(rml_uri);
                    return rc;
                }
                got_name = true;
                /* if this is for a different job family, update the route to this proc */
                if (ORTE_JOB_FAMILY(name.jobid) != ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
                    if (ORTE_SUCCESS != (rc = orte_routed.update_route(&name, &name))) {
                        ORTE_ERROR_LOG(rc);
                        free(rml_uri);
                        return rc;
                    }
                }
            }
            free(rml_uri);
        }
        
        /* track how many procs were in the message */
        ++num_procs;
    }
    if (ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }    
    
    /* if we are a daemon and this was info about our jobid, this update would
     * include updated contact info
     * for all daemons in the system - indicating that the number of daemons
     * changed since we were initially launched. Thus, update the num_procs
     * in our process_info struct so we can correctly route any messages
     */
    if (ORTE_PROC_MY_NAME->jobid == name.jobid &&
        ORTE_PROC_IS_DAEMON &&
        orte_process_info.num_procs < num_procs) {
        orte_process_info.num_procs = num_procs;
        /* if we changed it, then we better update the routed
         * tree so daemon collectives work correctly
         */
        if (ORTE_SUCCESS != (rc = orte_routed.update_routing_tree())) {
            ORTE_ERROR_LOG(rc);
        }
    }
    
    return ORTE_SUCCESS;
}

int
orte_rml_base_parse_uris(const char* uri,
                         orte_process_name_t* peer, 
                         char*** uris)
{
    int rc;

    /* parse the process name */
    char* cinfo = strdup(uri);
    char* ptr = strchr(cinfo, ';');
    if(NULL == ptr) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        free(cinfo);
        return ORTE_ERR_BAD_PARAM;
    }
    *ptr = '\0';
    ptr++;
    if (ORTE_SUCCESS != (rc = orte_util_convert_string_to_process_name(peer, cinfo))) {
        ORTE_ERROR_LOG(rc);
        free(cinfo);
        return rc;
    }

    if (NULL != uris) {
	/* parse the remainder of the string into an array of uris */
	*uris = opal_argv_split(ptr, ';');
    }
    free(cinfo);
    return ORTE_SUCCESS;
}
