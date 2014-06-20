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
#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/runtime.h"

#include "orte/mca/rml/base/rml_contact.h"

#include "orte/mca/routed/base/base.h"
#include "routed_slave.h"

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
static int slave_ft_event(int state);
#endif

orte_routed_module_t orte_routed_slave_module = {
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
    slave_ft_event
#else
    NULL
#endif
};

/* local globals */
static opal_condition_t         cond;
static opal_mutex_t             lock;
static orte_process_name_t      *lifeline=NULL;
static orte_process_name_t      local_lifeline;


static int init(void)
{
    /* setup the global condition and lock */
    OBJ_CONSTRUCT(&cond, opal_condition_t);
    OBJ_CONSTRUCT(&lock, opal_mutex_t);

    lifeline = NULL;

    return ORTE_SUCCESS;
}

static int finalize(void)
{
    /* destruct the global condition and lock */
    OBJ_DESTRUCT(&cond);
    OBJ_DESTRUCT(&lock);

    lifeline = NULL;

    return ORTE_SUCCESS;
}

static int delete_route(orte_process_name_t *proc)
{
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_slave_delete_route for %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    
    /*There is nothing to do here */
    
    return ORTE_SUCCESS;
}

static int update_route(orte_process_name_t *target,
                        orte_process_name_t *route)
{ 
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_slave_update: %s --> %s",
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
        /* a slave must always route via its parent daemon */
        ret = ORTE_PROC_MY_DAEMON;
    }

    OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                         "%s routed_slave_get(%s) --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(ret)));
    
    return *ret;
}


static int init_routes(orte_jobid_t job, opal_buffer_t *ndat)
{
    int rc;
    opal_buffer_t buf;
    
    if (NULL != ndat) {
        /* if ndat != NULL, then this is being invoked by the proc to
         * init a route to a specified process that is outside of our
         * job family. It really doesn't matter to a slave, though, as
         * everything has to go through our parent daemon, who must
         * already know how to reach the specified proc since the
         * inbound message had to go through it!
         */
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                             "%s routed_slave: init routes w/non-NULL data",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        return ORTE_SUCCESS;
    }
    
    /* if ndat=NULL, then we are being called during orte_init. In this
     * case, we need to setup a few critical pieces of info
     */
    
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_slave: init routes for proc job %s\n\thnp_uri %s\n\tdaemon uri %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_JOBID_PRINT(job),
                         (NULL == orte_process_info.my_hnp_uri) ? "NULL" : orte_process_info.my_hnp_uri,
                         (NULL == orte_process_info.my_daemon_uri) ? "NULL" : orte_process_info.my_daemon_uri));
    
    if (NULL == orte_process_info.my_daemon_uri) {
        /* in this module, we absolutely MUST have this information - if
         * we didn't get it, then error out
         */
        opal_output(0, "%s ERROR: Failed to identify the local daemon's URI",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        opal_output(0, "%s ERROR: This is a fatal condition when the slave router",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        opal_output(0, "%s ERROR: has been selected - either select the unity router",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        opal_output(0, "%s ERROR: or ensure that the local daemon info is provided",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        return ORTE_ERR_FATAL;
    }
    
    /* we have to set the HNP's name, even though we won't route messages directly
     * to it. This is required to ensure that we -do- send messages to the correct
     * HNP name
     */
    if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(orte_process_info.my_hnp_uri,
                                                       ORTE_PROC_MY_HNP, NULL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
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
    
    /* set our lifeline to the local daemon - we will abort if this connection is lost */
    lifeline = ORTE_PROC_MY_DAEMON;
    
    /* send a message back to our daemon letting it know we are alive. This allows the
     * daemon to "block" in spawn until we are running
     */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    orte_rml.send_buffer(ORTE_PROC_MY_DAEMON, &buf, ORTE_RML_TAG_REPORT_REMOTE_LAUNCH, 0);
    OBJ_DESTRUCT(&buf);
    
    /* no answer is expected or coming */
    
    return ORTE_SUCCESS;
}

static int route_lost(const orte_process_name_t *route)
{
    /* if we lose the connection to the lifeline and we are NOT already,
     * in finalize, tell the OOB to abort.
     * NOTE: we cannot call abort from here as the OOB needs to first
     * release a thread-lock - otherwise, we will hang!!
     */
    if (!orte_finalizing &&
        NULL != lifeline &&
        OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, route, lifeline)) {
        return ORTE_ERR_FATAL;
    }

    /* we don't care about this one, so return success */
    return ORTE_SUCCESS;
}


static bool route_is_defined(const orte_process_name_t *target)
{
    /* only the route to my daemon is defined */
    if (target->jobid != ORTE_PROC_MY_DAEMON->jobid ||
        target->vpid != ORTE_PROC_MY_DAEMON->vpid) {
        return false;
    }
    
    return true;
}

static int set_lifeline(orte_process_name_t *proc)
{
    /* we have to copy the proc data because there is no
     * guarantee that it will be preserved
     */
    local_lifeline.jobid = proc->jobid;
    local_lifeline.vpid = proc->vpid;
    lifeline = &local_lifeline;
    
    return ORTE_SUCCESS;
}

static int update_routing_tree(void)
{
    /* this is a meaningless command for a slave as I am not allowed to route */
    return ORTE_ERR_NOT_SUPPORTED;
}

static orte_vpid_t get_routing_tree(opal_list_t *children)
{
    /* this is a meaningless command for a slave as I am not allowed to route */
    return ORTE_VPID_INVALID;
}

static int get_wireup_info(opal_buffer_t *buf)
{
    /* this is a meaningless command for a slave as I am not allowed to route */
    return ORTE_ERR_NOT_SUPPORTED;
}


#if OPAL_ENABLE_FT_CR == 1
static int slave_ft_event(int state)
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

