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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <string.h>
#include <fcntl.h>

#include "opal/dss/dss.h"
#include "opal/runtime/opal.h"
#include "opal/mca/paffinity/paffinity.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/util/nidmap.h"
#include "orte/orted/orted.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/grpcomm/base/base.h"
#include "grpcomm_hier.h"


/* Static API's */
static int init(void);
static void finalize(void);
static int xcast(orte_jobid_t job,
                 opal_buffer_t *buffer,
                 orte_rml_tag_t tag);
static int allgather(opal_buffer_t *sbuf, opal_buffer_t *rbuf);
static int barrier(void);
static int modex(opal_list_t *procs);

/* Module def */
orte_grpcomm_base_module_t orte_grpcomm_hier_module = {
    init,
    finalize,
    xcast,
    allgather,
    orte_grpcomm_base_allgather_list,
    barrier,
    NULL,  /* onesided barrier only used by daemons */
    orte_grpcomm_base_set_proc_attr,
    orte_grpcomm_base_get_proc_attr,
    modex,
    orte_grpcomm_base_purge_proc_attrs
};


/* Local data */
static orte_local_rank_t my_local_rank;
static opal_list_t my_local_peers;
static orte_process_name_t my_local_rank_zero_proc;
static int num_local_peers;
static bool coll_initialized = false;
static orte_vpid_t *my_coll_peers=NULL;
static int cpeers=0;

/**
 * Initialize the module
 */
static int init(void)
{
    int rc;
    
    OBJ_CONSTRUCT(&my_local_peers, opal_list_t);

    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_modex_init())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return rc;
}

/**
 * Finalize the module
 */
static void finalize(void)
{
    opal_list_item_t *item;
    
    orte_grpcomm_base_modex_finalize();
    
    while (NULL != (item = opal_list_remove_first(&my_local_peers))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&my_local_peers);
    
    if (NULL != my_coll_peers) {
        free(my_coll_peers);
    }
}

/**
 *  A "broadcast-like" function to a job's processes.
 *  @param  jobid   The job whose processes are to receive the message
 *  @param  buffer  The data to broadcast
 */

static int xcast(orte_jobid_t job,
                 opal_buffer_t *buffer,
                 orte_rml_tag_t tag)
{
    int rc = ORTE_SUCCESS;
    opal_buffer_t buf;
    orte_daemon_cmd_flag_t command;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:xcast sent to job %s tag %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job), (long)tag));
    
    /* if there is no message to send, then just return ok */
    if (NULL == buffer) {
        return ORTE_SUCCESS;
    }
    
    /* setup a buffer to handle the xcast command */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    /* all we need to do is send this to the HNP - the relay logic
     * will ensure everyone else gets it! So tell the HNP to
     * process and relay it. The HNP will use the routed.get_routing_tree
     * to find out who it should relay the message to.
     */
    command = ORTE_DAEMON_PROCESS_AND_RELAY_CMD;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    /* pack the target jobid and tag for use in relay */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &job, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &tag, 1, ORTE_RML_TAG))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* if this isn't intended for the daemon command tag, then we better
     * tell the daemon to deliver it to the procs, and what job is supposed
     * to get it - this occurs when a caller just wants to send something
     * to all the procs in a job. In that use-case, the caller doesn't know
     * anything about inserting daemon commands or what routing algo might
     * be used, so we have to help them out a little. Functions that are
     * sending commands to the daemons themselves are smart enough to know
     * what they need to do.
     */
    if (ORTE_RML_TAG_DAEMON != tag) {
        command = ORTE_DAEMON_MESSAGE_LOCAL_PROCS;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &job, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &tag, 1, ORTE_RML_TAG))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    }
    
    /* copy the payload into the new buffer - this is non-destructive, so our
     * caller is still responsible for releasing any memory in the buffer they
     * gave to us
     */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(&buf, buffer))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* if I am the HNP, just set things up so the cmd processor gets called.
     * We don't want to message ourselves as this can create circular logic
     * in the RML. Instead, this macro will set a zero-time event which will
     * cause the buffer to be processed by the cmd processor - probably will
     * fire right away, but that's okay
     * The macro makes a copy of the buffer, so it's okay to release it here
     */
    if (ORTE_PROC_IS_HNP) {
        ORTE_MESSAGE_EVENT(ORTE_PROC_MY_NAME, &buf, ORTE_RML_TAG_DAEMON, orte_daemon_cmd_processor);
    } else {
        /* otherwise, send it to the HNP for relay */
        if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buf, ORTE_RML_TAG_DAEMON, 0))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        rc = ORTE_SUCCESS;
    }
    
CLEANUP:
    OBJ_DESTRUCT(&buf);
    return rc;
}


/* the barrier is executed as an allgather with data length of zero */
static int barrier(void)
{
    opal_buffer_t buf1, buf2;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:hier entering barrier",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    OBJ_CONSTRUCT(&buf1, opal_buffer_t);
    OBJ_CONSTRUCT(&buf2, opal_buffer_t);
    
    if (ORTE_SUCCESS != (rc = allgather(&buf1, &buf2))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_DESTRUCT(&buf1);
    OBJ_DESTRUCT(&buf2);
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:hier barrier complete",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return rc;
}

static opal_buffer_t allgather_buf;
static int allgather_num_recvd;

static void process_msg(int fd, short event, void *data)
{
    int rc;
    orte_message_event_t *mev = (orte_message_event_t*)data;
    
    /* xfer the data */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(&allgather_buf, mev->buffer))) {
        ORTE_ERROR_LOG(rc);
    }
    allgather_num_recvd++;
    /* release the message */
    OBJ_RELEASE(mev);
}

static void allgather_recv(int status, orte_process_name_t* sender,
                            opal_buffer_t *buffer,
                            orte_rml_tag_t tag, void *cbdata)
{
    int rc;
    
    /* don't process this right away - we need to get out of the recv before
     * we process the message as it may ask us to do something that involves
     * more messaging! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release above - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, process_msg);
    
    /* reissue the recv */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ALLGATHER,
                                                      ORTE_RML_NON_PERSISTENT, allgather_recv, NULL))) {
        ORTE_ERROR_LOG(rc);
    }
}

static int allgather(opal_buffer_t *sbuf, opal_buffer_t *rbuf)
{
    int rc=ORTE_SUCCESS;
    opal_list_item_t *item;
    orte_namelist_t *nm;
    opal_buffer_t final_buf;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:hier entering allgather",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* have I initialized my local info? */
    if (!coll_initialized) {
        orte_process_name_t proc;
        orte_vpid_t v;
        
        /* get my local rank so I can locally cache it */
        my_local_rank = orte_ess.get_local_rank(ORTE_PROC_MY_NAME);
        
        /* if I am local_rank=0 for this node and job, then setup
         * my array of local_rank=0 peers
         */
        if (0 == my_local_rank) {
            /* we need one entry/node in this job */
            my_coll_peers = (orte_vpid_t*)malloc(orte_process_info.num_nodes * sizeof(orte_vpid_t));
            cpeers = 0;
        }
        
        /* cycle through the procs to create a list of those that are local to me */
        proc.jobid = ORTE_PROC_MY_NAME->jobid;
        for (v=0; v < orte_process_info.num_procs; v++) {
            proc.vpid = v;
            /* is this proc local_rank=0 on its node? */
            if (0 == my_local_rank && 0 == orte_ess.get_local_rank(&proc)) {
                my_coll_peers[cpeers++] = v;
            }
            /* if this is me, or this proc isn't on our node, ignore it */
            if (v == ORTE_PROC_MY_NAME->vpid ||
                !OPAL_PROC_ON_LOCAL_NODE(orte_ess.proc_get_locality(&proc))) {
                continue;
            }
            /* add this proc to our list of local peers */
            nm = OBJ_NEW(orte_namelist_t);
            nm->name.jobid = proc.jobid;
            nm->name.vpid = proc.vpid;
            opal_list_append(&my_local_peers, &nm->item);
            /* if I am not local_rank=0, is this one? */
            if (0 != my_local_rank &&
                0 == orte_ess.get_local_rank(&proc)) {
                my_local_rank_zero_proc.jobid = proc.jobid;
                my_local_rank_zero_proc.vpid = proc.vpid;
            }
        }

        /* compute the number of local peers */
        num_local_peers = opal_list_get_size(&my_local_peers);
        
        /* flag that I have initialized things */
        coll_initialized = true;
    }
    
    /* if I am not local rank = 0 */
    if (0 != my_local_rank) {
        /* send our data to the local_rank=0 proc on this node */
        if (0 > (rc = orte_rml.send_buffer(&my_local_rank_zero_proc, sbuf, ORTE_RML_TAG_ALLGATHER, 0))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* setup to get return buffer */
        OBJ_CONSTRUCT(&allgather_buf, opal_buffer_t);

        /* now receive the final result. Be sure to do this in
         * a manner that allows us to return without being in a recv!
         */
        allgather_num_recvd = 0;
        rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ALLGATHER,
                                     ORTE_RML_NON_PERSISTENT, allgather_recv, NULL);
        if (rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        ORTE_PROGRESSED_WAIT(false, allgather_num_recvd, 1);

        /* cancel the lingering recv */
        orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ALLGATHER);

        /* copy payload to the caller's buffer */
        if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(rbuf, &allgather_buf))) {
            ORTE_ERROR_LOG(rc);
        }
        OBJ_DESTRUCT(&allgather_buf);
        
    } else {
        /* I am local_rank = 0 on this node! */
        
        /* setup to recv data from the procs that share this node with me */
        OBJ_CONSTRUCT(&allgather_buf, opal_buffer_t);
        
        /* seed it with my own data */
        opal_dss.copy_payload(&allgather_buf, sbuf);
        
        /* wait to receive their data. Be sure to do this in
         * a manner that allows us to return without being in a recv!
         */
        allgather_num_recvd = 0;
        rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ALLGATHER,
                                     ORTE_RML_NON_PERSISTENT, allgather_recv, NULL);
        if (rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        ORTE_PROGRESSED_WAIT(false, allgather_num_recvd, num_local_peers);
        
        /* cancel the lingering recv */
        orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ALLGATHER);
        
        /* take the recv'd data and use one of the base collectives
         * to exchange it with all other local_rank=0 procs in a scalable
         * manner - the exact collective will depend upon the number of
         * nodes in the job
         */
        OBJ_CONSTRUCT(&final_buf, opal_buffer_t);
        if (ORTE_SUCCESS != (rc = orte_grpcomm_base_allgather(&allgather_buf, rbuf, num_local_peers + 1,
                                                              ORTE_PROC_MY_NAME->jobid,
                                                              cpeers, my_coll_peers))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&allgather_buf);
            OBJ_DESTRUCT(&final_buf);
            return rc;
        }
        OBJ_DESTRUCT(&allgather_buf);  /* done with this */

        /* distribute the results to our local peers */
        for (item = opal_list_get_first(&my_local_peers);
             item != opal_list_get_end(&my_local_peers);
             item = opal_list_get_next(item)) {
            nm = (orte_namelist_t*)item;
            if (0 > (rc = orte_rml.send_buffer(&nm->name, rbuf, ORTE_RML_TAG_ALLGATHER, 0))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:hier allgather completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return ORTE_SUCCESS;
}

/***   MODEX SECTION ***/

static int modex(opal_list_t *procs)
{
    int rc=ORTE_SUCCESS;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:hier: modex entered",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if we were given a list of procs to modex with, then this is happening
     * as part of a connect/accept operation. In this case, we -must- do the
     * modex for two reasons:
     *
     * (a) the modex could involve procs from different mpiruns. In this case,
     *     there is no way for the two sets of procs to know which node the
     *     other procs are on, so we cannot use the profile_file to determine
     *     their contact info
     *
     * (b) in a comm_spawn, the parent job does not have a pidmap for the
     *     child job. Thus, it cannot know where the child procs are located,
     *     and cannot use the profile_file to determine their contact info
     *
     */
    if (NULL != procs) {
        if (ORTE_SUCCESS != (rc = orte_grpcomm_base_full_modex(procs, true))) {
            ORTE_ERROR_LOG(rc);
        }
    } else {
    
        /* otherwise, we are doing this across our peers */
        if (ORTE_SUCCESS != (rc = orte_grpcomm_base_peer_modex(true))) {
            ORTE_ERROR_LOG(rc);
        }
    }
    return rc;
}
