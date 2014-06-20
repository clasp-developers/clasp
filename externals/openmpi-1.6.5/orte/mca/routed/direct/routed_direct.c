/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/threads/condition.h"
#include "opal/dss/dss_types.h"
#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/rml/base/rml_contact.h"

#include "orte/mca/routed/base/base.h"
#include "routed_direct.h"

static int init(void);
static int finalize(void);
static int delete_route(orte_process_name_t *proc);
static int update_route(orte_process_name_t *target,
                        orte_process_name_t *route);
static orte_process_name_t get_route(orte_process_name_t *target);
static int init_routes(orte_jobid_t job, opal_buffer_t *ndat);
static int route_lost(const orte_process_name_t *route);
static bool route_is_defined(const orte_process_name_t *target);
static int update_routing_tree(void);
static orte_vpid_t get_routing_tree(opal_list_t *children);
static int get_wireup_info(opal_buffer_t *buf);
static int set_lifeline(orte_process_name_t *proc);

#if OPAL_ENABLE_FT_CR == 1
static int direct_ft_event(int state);
#endif

orte_routed_module_t orte_routed_direct_module = {
    init,
    finalize,
    delete_route,
    update_route,
    get_route,
    init_routes,
    route_lost,
    route_is_defined,
    set_lifeline,
    update_routing_tree,
    get_routing_tree,
    get_wireup_info,
#if OPAL_ENABLE_FT_CR == 1
    direct_ft_event
#else
    NULL
#endif
};

/* local globals */
static opal_condition_t         cond;
static opal_mutex_t             lock;


static int init(void)
{
    /* setup the global condition and lock */
    OBJ_CONSTRUCT(&cond, opal_condition_t);
    OBJ_CONSTRUCT(&lock, opal_mutex_t);

    return ORTE_SUCCESS;
}

static int finalize(void)
{
    int rc;
    
    /* if I am the HNP, I need to stop the comm recv */
    if (ORTE_PROC_IS_HNP) {
        orte_routed_base_comm_stop();
    }
    
    if (ORTE_PROC_IS_MPI && NULL != orte_process_info.my_daemon_uri) {
        /* if a daemon launched me, register that I am leaving */
        if (ORTE_SUCCESS != (rc = orte_routed_base_register_sync(false))) {
            ORTE_ERROR_LOG(rc);
        }
    }
    
    /* destruct the global condition and lock */
    OBJ_DESTRUCT(&cond);
    OBJ_DESTRUCT(&lock);

    return ORTE_SUCCESS;
}

static int delete_route(orte_process_name_t *proc)
{
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_direct_delete_route for %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    
    /*There is nothing to do here */
    
    return ORTE_SUCCESS;
}

static int update_route(orte_process_name_t *target,
                        orte_process_name_t *route)
{ 
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_direct_update: %s --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(route)));

    /*There is nothing to do here */
    
    return ORTE_SUCCESS;
}


static orte_process_name_t get_route(orte_process_name_t *target)
{
    orte_process_name_t *ret;

    if (target->jobid == ORTE_JOBID_INVALID ||
        target->vpid == ORTE_VPID_INVALID) {
        ret = ORTE_NAME_INVALID;
    } else {
        /* all routes are direct */
        ret = target;
    }

    OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                         "%s routed_direct_get(%s) --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(ret)));
    
    return *ret;
}


static int init_routes(orte_jobid_t job, opal_buffer_t *ndat)
{
    int rc;
    
    /* if I am a tool, then I stand alone - there is nothing to do */
    if (ORTE_PROC_IS_TOOL) {
        return ORTE_SUCCESS;
    }
    
    /* if I am a daemon or HNP, then I have to extract the routing info for this job
     * from the data sent to me for launch and update the routing tables to
     * point at the daemon for each proc
     */
    if (ORTE_PROC_IS_DAEMON) {
        
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                             "%s direct: init routes for daemon job %s\n\thnp_uri %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(job),
                             (NULL == orte_process_info.my_hnp_uri) ? "NULL" : orte_process_info.my_hnp_uri));
        
        if (NULL == ndat) {
            /* indicates this is being called during orte_init.
             * Get the HNP's name for possible later use
             */
            if (NULL == orte_process_info.my_hnp_uri) {
                /* fatal error */
                ORTE_ERROR_LOG(ORTE_ERR_FATAL);
                return ORTE_ERR_FATAL;
            }
            /* set the contact info into the hash table */
            if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(orte_process_info.my_hnp_uri))) {
                ORTE_ERROR_LOG(rc);
                return(rc);
            }
            
            /* extract the hnp name and store it */
            if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(orte_process_info.my_hnp_uri,
                                                               ORTE_PROC_MY_HNP, NULL))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            
            /* daemons will send their contact info back to the HNP as
             * part of the message confirming they are read to go. HNP's
             * load their contact info during orte_init
             */
        } else {
            /* ndat != NULL means we are getting an update of RML info
             * for the daemons - so update our contact info and routes
             */
            if (ORTE_SUCCESS != (rc = orte_rml_base_update_contact_info(ndat))) {
                ORTE_ERROR_LOG(rc);
            }
            return rc;
        }
        
        OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                             "%s routed_direct: completed init routes",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        return ORTE_SUCCESS;
    }
    
    
    if (ORTE_PROC_IS_HNP) {
        
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                             "%s routed_direct: init routes for HNP job %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(job)));
        
        if (NULL == ndat) {
            /* if ndat is NULL, then this is being called during init, so just
             * make myself available to catch any reported contact info
             */
            if (ORTE_SUCCESS != (rc = orte_routed_base_comm_start())) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        } else {
            /* if this is for my own jobid, then I am getting an update of RML info
             * for the daemons - so update our contact info and routes
             */
            if (ORTE_PROC_MY_NAME->jobid == job) {
                if (ORTE_SUCCESS != (rc = orte_rml_base_update_contact_info(ndat))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
        }
        
        return ORTE_SUCCESS;
    }

    /***   MUST BE A PROC   ***/
    
    /* if ndat=NULL, then we are being called during orte_init */
    if (NULL == ndat) {
        if (NULL != orte_process_info.my_daemon_uri) {
            /* we are being launched by a daemon, so we need to
             * register a sync with it to get our nidmap back
             */
            /* Set the contact info in the RML - this won't actually establish
             * the connection, but just tells the RML how to reach the daemon
             * if/when we attempt to send to it
             */
            if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(orte_process_info.my_daemon_uri))) {
                ORTE_ERROR_LOG(rc);
                return(rc);
            }
            /* extract the daemon's name so we can update the routing table */
            if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(orte_process_info.my_daemon_uri,
                                                               ORTE_PROC_MY_DAEMON, NULL))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* register ourselves -this sends a message to the daemon (warming up that connection)
             * and sends our contact info to the HNP when all local procs have reported
             */
            if (ORTE_SUCCESS != (rc = orte_routed_base_register_sync(true))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* no answer is expected or coming */
        }
        return ORTE_SUCCESS;
    }
    
    /* if ndat != NULL, then this is being invoked by the proc to
     * init a route to a specified process that is outside of our
     * job family. It really doesn't matter as everything must
     * go direct
     */
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_direct: init routes w/non-NULL data",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    if (ORTE_SUCCESS != (rc = orte_rml_base_update_contact_info(ndat))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}

static int route_lost(const orte_process_name_t *route)
{
    /* there is no lifeline, so we don't care */
    return ORTE_SUCCESS;
}


static bool route_is_defined(const orte_process_name_t *target)
{
    /* all routes are defined */
    return true;
}

static int set_lifeline(orte_process_name_t *proc)
{
    /* there is no lifeline */
    return ORTE_SUCCESS;
}

static int update_routing_tree(void)
{
    /* nothing to do here */
    return ORTE_SUCCESS;
}

static orte_vpid_t get_routing_tree(opal_list_t *children)
{
    orte_vpid_t i;
    orte_routed_tree_t *nm;
    
    if (!ORTE_PROC_IS_HNP) {
        /* if I am not the HNP, there is nothing to do */
        return ORTE_VPID_INVALID;
    }
    
    /* if I am the HNP, then I need to construct a list containing all
     * daemons so I can relay messages to them
     */
    for (i=0; i < orte_process_info.num_procs; i++) {
        nm = OBJ_NEW(orte_routed_tree_t);
        nm->vpid = i;
        opal_list_append(children, &nm->super);
    }
    return ORTE_VPID_INVALID;
}

static int get_wireup_info(opal_buffer_t *buf)
{
    /* this is a meaningless command for a direct as I am not allowed to route */
    return ORTE_SUCCESS;
}


#if OPAL_ENABLE_FT_CR == 1
static int direct_ft_event(int state)
{
    int ret, exit_status = ORTE_SUCCESS;

    /******** Checkpoint Prep ********/
    if(OPAL_CRS_CHECKPOINT == state) {
    }
    /******** Continue Recovery ********/
    else if (OPAL_CRS_CONTINUE == state ) {
    }
    /******** Restart Recovery ********/
    else if (OPAL_CRS_RESTART == state ) {
        /*
         * Re-exchange the routes
         */
        if (ORTE_SUCCESS != (ret = orte_routed.init_routes(ORTE_PROC_MY_NAME->jobid, NULL))) {
            exit_status = ret;
            goto cleanup;
        }
    }
    else if (OPAL_CRS_TERM == state ) {
        /* Nothing */
    }
    else {
        /* Error state = Nothing */
    }

 cleanup:
    return exit_status;
}
#endif

