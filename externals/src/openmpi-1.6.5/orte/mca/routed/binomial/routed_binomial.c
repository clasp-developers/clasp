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
#include "opal/dss/dss.h"
#include "opal/class/opal_hash_table.h"
#include "opal/class/opal_bitmap.h"
#include "opal/util/bit_ops.h"
#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/runtime.h"

#include "orte/mca/rml/base/rml_contact.h"

#include "orte/mca/routed/base/base.h"
#include "routed_binomial.h"

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
static int binomial_ft_event(int state);
#endif

orte_routed_module_t orte_routed_binomial_module = {
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
    binomial_ft_event
#else
    NULL
#endif
};

/* local globals */
static opal_hash_table_t        jobfam_list;
static opal_condition_t         cond;
static opal_mutex_t             lock;
static orte_process_name_t      *lifeline=NULL;
static orte_process_name_t      local_lifeline;
static orte_process_name_t      my_parent;
static int                      num_children;
static opal_list_t              my_children;
static bool                     ack_recvd;


static int init(void)
{
    OBJ_CONSTRUCT(&jobfam_list, opal_hash_table_t);
    opal_hash_table_init(&jobfam_list, 128);
    
    /* setup the global condition and lock */
    OBJ_CONSTRUCT(&cond, opal_condition_t);
    OBJ_CONSTRUCT(&lock, opal_mutex_t);

    lifeline = NULL;
    
    /* setup the list of children */
    OBJ_CONSTRUCT(&my_children, opal_list_t);
    num_children = 0;
    my_parent.jobid = ORTE_PROC_MY_NAME->jobid;
    
    return ORTE_SUCCESS;
}

static int finalize(void)
{
    int rc;
    opal_list_item_t *item;
    
    /* if I am an application process, indicate that I am
        * truly finalizing prior to departure
        */
    if (!ORTE_PROC_IS_HNP &&
        !ORTE_PROC_IS_DAEMON &&
        !ORTE_PROC_IS_TOOL) {
        if (ORTE_SUCCESS != (rc = orte_routed_base_register_sync(false))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    /* if I am the HNP, I need to stop the comm recv */
    if (ORTE_PROC_IS_HNP) {
        orte_routed_base_comm_stop();
    }
    
    OBJ_DESTRUCT(&jobfam_list);
    /* destruct the global condition and lock */
    OBJ_DESTRUCT(&cond);
    OBJ_DESTRUCT(&lock);

    lifeline = NULL;

    /* deconstruct the list of children */
    while (NULL != (item = opal_list_remove_first(&my_children))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&my_children);
    num_children = 0;
    
    return ORTE_SUCCESS;
}

static int delete_route(orte_process_name_t *proc)
{
    int rc;
    orte_process_name_t *route_copy;
    
    if (proc->jobid == ORTE_JOBID_INVALID ||
        proc->vpid == ORTE_VPID_INVALID) {
        return ORTE_ERR_BAD_PARAM;
    }
    
    /* if I am an application process, I don't have any routes
     * so there is nothing for me to do
     */
    if (!ORTE_PROC_IS_HNP && !ORTE_PROC_IS_DAEMON &&
        !ORTE_PROC_IS_TOOL) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_binomial_delete_route for %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    
    
    /* if this is from a different job family, then I need to
     * look it up appropriately
     */
    if (ORTE_JOB_FAMILY(proc->jobid) != ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
        
        /* if I am a daemon, then I will automatically route
         * anything to this job family via my HNP - so I have nothing
         * in my routing table and thus have nothing to do
         * here, just return
         */
        if (ORTE_PROC_IS_DAEMON) {
            return ORTE_SUCCESS;
        }
        
        /* see if this proc is present - it will have a wildcard vpid,
         * so we have to look for it with that condition
         */
        rc = opal_hash_table_get_value_uint32(&jobfam_list,
                                              ORTE_JOB_FAMILY(proc->jobid),
                                              (void**)&route_copy);
        if (ORTE_SUCCESS == rc && NULL != route_copy) {
            /* proc is present - remove the data */
            free(route_copy);
            rc = opal_hash_table_remove_value_uint32(&jobfam_list,
                                                     ORTE_JOB_FAMILY(proc->jobid));
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
            }            
            return rc;
        }
        
        /* not present - nothing to do */
        return ORTE_SUCCESS;
    }
    
    /* THIS CAME FROM OUR OWN JOB FAMILY...there is nothing
     * to do here. The routes will be redefined when we update
     * the routing tree
     */
    
    return ORTE_SUCCESS;
}

static int update_route(orte_process_name_t *target,
                        orte_process_name_t *route)
{ 
    int rc;
    orte_process_name_t *route_copy;
    
    if (target->jobid == ORTE_JOBID_INVALID ||
        target->vpid == ORTE_VPID_INVALID) {
        return ORTE_ERR_BAD_PARAM;
    }

    /* if I am an application process, we don't update the route since
     * we automatically route everything through the local daemon
     */
    if (ORTE_PROC_IS_APP) {
        return ORTE_SUCCESS;
    }

    /* if the job family is zero, then this is going to a local slave,
     * so the path is direct and there is nothing to do here
     */
    if (0 == ORTE_JOB_FAMILY(target->jobid)) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_binomial_update: %s --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(route)));


    /* if this is from a different job family, then I need to
     * track how to send messages to it
     */
    if (ORTE_JOB_FAMILY(target->jobid) != ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
        
        /* if I am a daemon, then I will automatically route
         * anything to this job family via my HNP - so nothing to do
         * here, just return
         */
        if (ORTE_PROC_IS_DAEMON) {
            return ORTE_SUCCESS;
        }
        
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                             "%s routed_binomial_update: diff job family routing job %s --> %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(target->jobid), 
                             ORTE_NAME_PRINT(route)));
        
        /* see if this target is already present - it will have a wildcard vpid,
         * so we have to look for it with that condition
         */
        rc = opal_hash_table_get_value_uint32(&jobfam_list,
                                              ORTE_JOB_FAMILY(target->jobid),
                                              (void**)&route_copy);
        if (ORTE_SUCCESS == rc && NULL != route_copy) {
            /* target already present - update the route info
             * in case it has changed
             */
            *route_copy = *route;
            rc = opal_hash_table_set_value_uint32(&jobfam_list,
                                                  ORTE_JOB_FAMILY(target->jobid), route_copy);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
            }            
            return rc;
        }
        
        /* not there, so add the route FOR THE JOB FAMILY*/
        route_copy = (orte_process_name_t *) malloc(sizeof(orte_process_name_t));
        *route_copy = *route;
        rc = opal_hash_table_set_value_uint32(&jobfam_list,
                                              ORTE_JOB_FAMILY(target->jobid), route_copy);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }
    
    /* THIS CAME FROM OUR OWN JOB FAMILY... */
    
    opal_output(0, "%s CALL TO UPDATE ROUTE FOR OWN JOB FAMILY", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
    
    return ORTE_ERR_NOT_SUPPORTED;
}


static orte_process_name_t get_route(orte_process_name_t *target)
{
    orte_process_name_t *ret, daemon;
    opal_list_item_t *item;
    orte_routed_tree_t *child;
    int rc;

    if (target->jobid == ORTE_JOBID_INVALID ||
        target->vpid == ORTE_VPID_INVALID) {
        ret = ORTE_NAME_INVALID;
        goto found;
    }
    
    /* if it is me, then the route is just direct */
    if (OPAL_EQUAL == opal_dss.compare(ORTE_PROC_MY_NAME, target, ORTE_NAME)) {
        ret = target;
        goto found;
    }
    
    /* if I am an application process, always route via my local daemon */
    if (ORTE_PROC_IS_APP) {
        ret = ORTE_PROC_MY_DAEMON;
        goto found;
    }

    /* if I am a tool, the route is direct if target is in
     * my own job family, and to the target's HNP if not
     */
    if (ORTE_PROC_IS_TOOL) {
        if (ORTE_JOB_FAMILY(target->jobid) == ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
            ret = target;
            goto found;
        } else {
            ORTE_HNP_NAME_FROM_JOB(&daemon, target->jobid);
            ret = &daemon;
            goto found;
        }
    }
    
    /******     HNP AND DAEMONS ONLY     ******/
    
    /* if the job family is zero, then this is going to a local slave,
     * so the path is direct
     */
    if (0 == ORTE_JOB_FAMILY(target->jobid)) {
        ret = target;
        goto found;
    }
    
    /* IF THIS IS FOR A DIFFERENT JOB FAMILY... */
    if (ORTE_JOB_FAMILY(target->jobid) != ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
        /* if I am a daemon, route this via the HNP */
        if (ORTE_PROC_IS_DAEMON) {
            ret = ORTE_PROC_MY_HNP;
            goto found;
        }
        
        /* if I am the HNP or a tool, then I stored a route to
         * this job family, so look it up
         */
        rc = opal_hash_table_get_value_uint32(&jobfam_list,
                                              ORTE_JOB_FAMILY(target->jobid), (void**)&ret);
        if (ORTE_SUCCESS == rc) {
            /* got a good result - return it */
            goto found;
        }
        /* not found - so we have no route */
        ret = ORTE_NAME_INVALID;
        goto found;
    }
     
    /* THIS CAME FROM OUR OWN JOB FAMILY... */

    /* if we are not using static ports and this is going to the HNP, send direct */
    if (!orte_static_ports &&
        ORTE_PROC_MY_HNP->jobid == target->jobid &&
        ORTE_PROC_MY_HNP->vpid == target->vpid) {
        OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                             "%s routing not enabled - going direct",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        ret = target;
        goto found;
    }
    
    daemon.jobid = ORTE_PROC_MY_NAME->jobid;
    /* find out what daemon hosts this proc */
    if (ORTE_VPID_INVALID == (daemon.vpid = orte_ess.proc_get_daemon(target))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        ret = ORTE_NAME_INVALID;
        goto found;
    }
  
    /* if the daemon is me, then send direct to the target! */
    if (ORTE_PROC_MY_NAME->vpid == daemon.vpid) {
        ret = target;
        goto found;
    } else {
        /* search routing tree for next step to that daemon */
        for (item = opal_list_get_first(&my_children);
             item != opal_list_get_end(&my_children);
             item = opal_list_get_next(item)) {
            child = (orte_routed_tree_t*)item;
            if (child->vpid == daemon.vpid) {
                /* the child is hosting the proc - just send it there */
                ret = &daemon;
                goto found;
            }
            /* otherwise, see if the daemon we need is below the child */
            if (opal_bitmap_is_set_bit(&child->relatives, daemon.vpid)) {
                /* yep - we need to step through this child */
                daemon.vpid = child->vpid;
                ret = &daemon;
                goto found;
            }
        }
    }

    /* if we get here, then the target daemon is not beneath
     * any of our children, so we have to step up through our parent
     */
    daemon.vpid = my_parent.vpid;
    ret = &daemon;

 found:
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_binomial_get(%s) --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(ret)));
    
    return *ret;
}

/* HANDLE ACK MESSAGES FROM AN HNP */
static void release_ack(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    ack_recvd = true;
    OBJ_RELEASE(mev);
}

static void recv_ack(int status, orte_process_name_t* sender,
                     opal_buffer_t* buffer, orte_rml_tag_t tag,
                     void* cbdata)
{
    /* don't process this right away - we need to get out of the recv before
     * we process the message as it may ask us to do something that involves
     * more messaging! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release above - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, release_ack);    
}


static int init_routes(orte_jobid_t job, opal_buffer_t *ndat)
{
    /* the binomial module routes all proc communications through
     * the local daemon. Daemons must identify which of their
     * daemon-peers is "hosting" the specified recipient and
     * route the message to that daemon. Daemon contact info
     * is handled elsewhere, so all we need to do here is
     * ensure that the procs are told to route through their
     * local daemon, and that daemons are told how to route
     * for each proc
     */
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
                             "%s routed_binomial: init routes for daemon job %s\n\thnp_uri %s",
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

            /* if we are using static ports, set my lifeline to point at my parent */
            if (orte_static_ports) {
                lifeline = &my_parent;
            } else {
                /* set our lifeline to the HNP - we will abort if that connection is lost */
                lifeline = ORTE_PROC_MY_HNP;
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
                             "%s routed_binomial: completed init routes",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        return ORTE_SUCCESS;
    }
    

    if (ORTE_PROC_IS_HNP) {
        
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                             "%s routed_binomial: init routes for HNP job %s",
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
            /* the HNP has no lifeline */
            lifeline = NULL;
        } else {
            /* if this is for my own jobid, then I am getting an update of RML info
             * for the daemons - so update our contact info and routes
             */
            if (ORTE_PROC_MY_NAME->jobid == job) {
                if (ORTE_SUCCESS != (rc = orte_rml_base_update_contact_info(ndat))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            } else {
                /* if not, then I need to process the callback */
                if (ORTE_SUCCESS != (rc = orte_routed_base_process_callback(job, ndat))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
        }

        return ORTE_SUCCESS;
    }

    {  /* MUST BE A PROC */
        /* if ndat != NULL, then this is being invoked by the proc to
         * init a route to a specified process that is outside of our
         * job family. We want that route to go through our HNP, routed via
         * out local daemon - however, we cannot know for
         * certain that the HNP already knows how to talk to the specified
         * procs. For example, in OMPI's publish/subscribe procedures, the
         * DPM framework looks for an mca param containing the global ompi-server's
         * uri. This info will come here so the proc can setup a route to
         * the server - we need to pass the routing info to our HNP
         */
        if (NULL != ndat) {
            int rc;
            
            OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                                 "%s routed_binomial: init routes w/non-NULL data",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            
            /* if this is for a job family of zero, then we know that the enclosed
             * procs are local slaves to our daemon. In that case, we can just ignore this
             * as our daemon - given that it had to spawn the local slave - already
             * knows how to talk to them
             */
            if (0 == ORTE_JOB_FAMILY(job)) {
                return ORTE_SUCCESS;
            }
            
            if (ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid) != ORTE_JOB_FAMILY(job)) {
                /* if this is for a different job family, then we route via our HNP
                 * to minimize connection counts to entities such as ompi-server, so
                 * start by sending the contact info to the HNP for update
                 */
                OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                                     "%s routed_binomial_init_routes: diff job family - sending update to %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_HNP)));
                
                if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, ndat,
                                                   ORTE_RML_TAG_RML_INFO_UPDATE, 0))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                
                /* wait right here until the HNP acks the update to ensure that
                 * any subsequent messaging can succeed
                 */
                ack_recvd = false;
                rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_UPDATE_ROUTE_ACK,
                                             ORTE_RML_NON_PERSISTENT, recv_ack, NULL);
                
                ORTE_PROGRESSED_WAIT(ack_recvd, 0, 1);
                
                OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                                     "%s routed_binomial_init_routes: ack recvd",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                
                /* our get_route function automatically routes all messages for
                 * other job families via the HNP, so nothing more to do here
                 */
            }
            return ORTE_SUCCESS;
        }
        
        /* if ndat=NULL, then we are being called during orte_init. In this
         * case, we need to setup a few critical pieces of info
         */
        
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                             "%s routed_binomial: init routes for proc job %s\n\thnp_uri %s\n\tdaemon uri %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_JOBID_PRINT(job),
                             (NULL == orte_process_info.my_hnp_uri) ? "NULL" : orte_process_info.my_hnp_uri,
                             (NULL == orte_process_info.my_daemon_uri) ? "NULL" : orte_process_info.my_daemon_uri));
                
        if (NULL == orte_process_info.my_daemon_uri) {
            /* in this module, we absolutely MUST have this information - if
             * we didn't get it, then error out
             */
            opal_output(0, "%s ERROR: Failed to identify the local daemon's URI",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            opal_output(0, "%s ERROR: This is a fatal condition when the binomial router",
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
        
        /* register ourselves -this sends a message to the daemon (warming up that connection)
         * and sends our contact info to the HNP when all local procs have reported
         *
         * NOTE: it may seem odd that we send our contact info to the HNP - after all,
         * the HNP doesn't really need to know how to talk to us directly if we are
         * using this routing method. However, this is good for two reasons:
         *
         * (1) some debuggers and/or tools may need RML contact
         *     info to set themselves up
         *
         * (2) doing so allows the HNP to "block" in a dynamic launch
         *     until all procs are reported running, thus ensuring that no communication
         *     is attempted until the overall ORTE system knows how to talk to everyone -
         *     otherwise, the system can just hang.
         */
        if (ORTE_SUCCESS != (rc = orte_routed_base_register_sync(true))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* no answer is expected or coming */
        
        return ORTE_SUCCESS;
    }
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
        opal_output_verbose(10, orte_routed_base_output,
                            "%s routed:binomial: Connection to lifeline %s lost",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(lifeline));
        return ORTE_ERR_FATAL;
    }

    /* we don't care about this one, so return success */
    return ORTE_SUCCESS;
}


static bool route_is_defined(const orte_process_name_t *target)
{
    /* find out what daemon hosts this proc */
    if (ORTE_VPID_INVALID == orte_ess.proc_get_daemon((orte_process_name_t*)target)) {
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

static int binomial_tree(int rank, int parent, int me, int num_procs,
                         int *nchildren, opal_list_t *childrn, opal_bitmap_t *relatives)
{
    int i, bitmap, peer, hibit, mask, found;
    orte_routed_tree_t *child;
    opal_bitmap_t *relations;
    
    /* is this me? */
    if (me == rank) {
        bitmap = opal_cube_dim(num_procs);
        
        hibit = opal_hibit(rank, bitmap);
        --bitmap;
        
        for (i = hibit + 1, mask = 1 << i; i <= bitmap; ++i, mask <<= 1) {
            peer = rank | mask;
            if (peer < num_procs) {
                child = OBJ_NEW(orte_routed_tree_t);
                child->vpid = peer;
                OPAL_OUTPUT_VERBOSE((3, orte_routed_base_output,
                                     "%s routed:binomial found child %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_VPID_PRINT(child->vpid)));
                if (NULL != childrn) {
                    /* this is a direct child - add it to my list */
                    opal_list_append(childrn, &child->super);
                    (*nchildren)++;
                    /* setup the relatives bitmap */
                    opal_bitmap_init(&child->relatives, num_procs);
                    /* point to the relatives */
                    relations = &child->relatives;
                } else {
                    /* we are recording someone's relatives - set the bit */
                    opal_bitmap_set_bit(relatives, peer);
                    /* point to this relations */
                    relations = relatives;
                }
                /* search for this child's relatives */
                binomial_tree(0, 0, peer, num_procs, NULL, NULL, relations);
            }
        }
        return parent;
    }
    
    /* find the children of this rank */
    bitmap = opal_cube_dim(num_procs);
    
    hibit = opal_hibit(rank, bitmap);
    --bitmap;
    
    for (i = hibit + 1, mask = 1 << i; i <= bitmap; ++i, mask <<= 1) {
        peer = rank | mask;
        if (peer < num_procs) {
            /* execute compute on this child */
            if (0 <= (found = binomial_tree(peer, rank, me, num_procs, nchildren, childrn, relatives))) {
                return found;
            }
        }
    }
    return -1;
}

static int update_routing_tree(void)
{
    orte_routed_tree_t *child;
    int j;
    opal_list_item_t *item;
    
    /* if I am anything other than a daemon or the HNP, this
     * is a meaningless command as I am not allowed to route
     */
    if (!ORTE_PROC_IS_DAEMON && !ORTE_PROC_IS_HNP) {
        return ORTE_ERR_NOT_SUPPORTED;
    }
    
    /* clear the list of children if any are already present */
    while (NULL != (item = opal_list_remove_first(&my_children))) {
        OBJ_RELEASE(item);
    }
    num_children = 0;
    
    /* compute my direct children and the bitmap that shows which vpids
     * lie underneath their branch
     */
    my_parent.vpid = binomial_tree(0, 0, ORTE_PROC_MY_NAME->vpid,
                                   orte_process_info.num_procs,
                                   &num_children, &my_children, NULL);
    
    if (0 < opal_output_get_verbosity(orte_routed_base_output)) {
        opal_output(0, "%s: parent %d num_children %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), my_parent.vpid, num_children);
        for (item = opal_list_get_first(&my_children);
             item != opal_list_get_end(&my_children);
             item = opal_list_get_next(item)) {
            child = (orte_routed_tree_t*)item;
            opal_output(0, "%s: \tchild %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), child->vpid);
            for (j=0; j < (int)orte_process_info.num_procs; j++) {
                if (opal_bitmap_is_set_bit(&child->relatives, j)) {
                    opal_output(0, "%s: \t\trelation %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), j);
                }
            }
        }
    }

    return ORTE_SUCCESS;
}

static orte_vpid_t get_routing_tree(opal_list_t *children)
{
    opal_list_item_t *item;
    orte_routed_tree_t *child, *nm;
    
    /* if I am anything other than a daemon or the HNP, this
     * is a meaningless command as I am not allowed to route
     */
    if (!ORTE_PROC_IS_DAEMON && !ORTE_PROC_IS_HNP) {
        return ORTE_VPID_INVALID;
    }
    
    /* the binomial routing tree always goes to our children,
     * for any job
     */
    if (NULL != children) {
        for (item = opal_list_get_first(&my_children);
             item != opal_list_get_end(&my_children);
             item = opal_list_get_next(item)) {
            child = (orte_routed_tree_t*)item;
            nm = OBJ_NEW(orte_routed_tree_t);
            nm->vpid = child->vpid;
            opal_bitmap_copy(&nm->relatives, &child->relatives);
            opal_list_append(children, &nm->super);
        }
    }
    
    /* return my parent's vpid */
    return my_parent.vpid;
}

static int get_wireup_info(opal_buffer_t *buf)
{
    int rc;
    
    /* if I am anything other than the HNP, this
     * is a meaningless command as I cannot get
     * the requested info
     */
    if (!ORTE_PROC_IS_HNP) {
        return ORTE_ERR_NOT_SUPPORTED;
    }
    
    /* if we are not using static ports, then we need to share the
     * comm info - otherwise, just return
     */
    if (orte_static_ports) {
        return ORTE_SUCCESS;
    }
    
    if (ORTE_SUCCESS != (rc = orte_rml_base_get_contact_info(ORTE_PROC_MY_NAME->jobid, buf))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        return rc;
    }

    return ORTE_SUCCESS;
}


#if OPAL_ENABLE_FT_CR == 1
static int binomial_ft_event(int state)
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

