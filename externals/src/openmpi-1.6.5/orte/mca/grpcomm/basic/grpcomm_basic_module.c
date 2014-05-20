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

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/util/nidmap.h"
#include "orte/orted/orted.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/grpcomm/base/base.h"
#include "grpcomm_basic.h"


/* Static API's */
static int init(void);
static void finalize(void);
static int xcast(orte_jobid_t job,
                 opal_buffer_t *buffer,
                 orte_rml_tag_t tag);
static int allgather(opal_buffer_t *sbuf, opal_buffer_t *rbuf);
static int barrier(void);
static int onesided_barrier(void);
static int modex(opal_list_t *procs);
static int set_proc_attr(const char *attr_name, const void *data, size_t size);
static int get_proc_attr(const orte_process_name_t proc,
                         const char * attribute_name, void **val, 
                         size_t *size);

/* Module def */
orte_grpcomm_base_module_t orte_grpcomm_basic_module = {
    init,
    finalize,
    xcast,
    allgather,
    orte_grpcomm_base_allgather_list,
    barrier,
    onesided_barrier,
    set_proc_attr,
    get_proc_attr,
    modex,
    orte_grpcomm_base_purge_proc_attrs
};

static bool recv_on;
static opal_buffer_t *profile_buf=NULL;
static int profile_fd = -1;
static void profile_recv(int status, orte_process_name_t* sender,
                         opal_buffer_t* buffer, orte_rml_tag_t tag,
                         void* cbdata);
static void daemon_coll_recv(int status, orte_process_name_t* sender,
                             opal_buffer_t* buffer, orte_rml_tag_t tag,
                             void* cbdata);

/**
 * Initialize the module
 */
static int init(void)
{
    int rc;
    int value;
    
    mca_base_param_reg_int_name("orte", "grpcomm_recv_on",
                                "Turn on grpcomm recv for profile purposes",
                                true, false,
                                (int) false, &value);
    recv_on = OPAL_INT_TO_BOOL(value);

    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_modex_init())) {
        ORTE_ERROR_LOG(rc);
    }
    
    if (opal_profile && ORTE_PROC_IS_MPI) {
        /* if I am an MPI application proc, then create a buffer
         * to pack all my attributes in */
        profile_buf = OBJ_NEW(opal_buffer_t);
        /* seed it with the node name */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(profile_buf, &orte_process_info.nodename, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
        }
    }
    
    if (ORTE_PROC_IS_HNP && recv_on) {
        /* open the profile file for writing */
        if (NULL == opal_profile_file) {
            /* no file specified - we will just ignore any incoming data */
            profile_fd = -1;
        } else {
            profile_fd = open(opal_profile_file, O_CREAT|O_RDWR|O_TRUNC, 0644);
            if (profile_fd < 0) {
                /* couldn't be opened */
                ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
                return ORTE_ERR_FILE_OPEN_FAILURE;
            }
        }
        
        if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                          ORTE_RML_TAG_GRPCOMM_PROFILE,
                                                          ORTE_RML_NON_PERSISTENT,
                                                          profile_recv,
                                                          NULL))) {
            ORTE_ERROR_LOG(rc);
        }
    }
    
    /* if we are a daemon or the hnp, we need to post a
     * recv to catch any collective operations
     */
    if (ORTE_PROC_IS_DAEMON || ORTE_PROC_IS_HNP) {
        if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                          ORTE_RML_TAG_DAEMON_COLLECTIVE,
                                                          ORTE_RML_NON_PERSISTENT,
                                                          daemon_coll_recv,
                                                          NULL))) {
            ORTE_ERROR_LOG(rc);
        }
    }
    
    return rc;
}

/**
 * Finalize the module
 */
static void finalize(void)
{
    opal_byte_object_t bo, *boptr;
    opal_buffer_t profile;
    
    orte_grpcomm_base_modex_finalize();
    
    if (opal_profile && ORTE_PROC_IS_MPI) {
        /* if I am an MPI proc, send my buffer to the collector */
        boptr = &bo;
        opal_dss.unload(profile_buf, (void**)&boptr->bytes, &boptr->size);
        OBJ_RELEASE(profile_buf);
        /* store it as a single object */
        OBJ_CONSTRUCT(&profile, opal_buffer_t);
        opal_dss.pack(&profile, &boptr, 1, OPAL_BYTE_OBJECT);
        /* send the buffer */
        orte_rml.send_buffer(ORTE_PROC_MY_HNP, &profile, ORTE_RML_TAG_GRPCOMM_PROFILE, 0);
        /* done with buffer */
        OBJ_DESTRUCT(&profile);
    }
    
    if (ORTE_PROC_IS_HNP && recv_on) {
        /* if we are profiling and I am the HNP, then stop the
         * profiling receive
         */
        orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_GRPCOMM_PROFILE);
        if (0 <= profile_fd) {
            close(profile_fd);
            profile_fd = -1;
        }
    } 

    /* if we are a daemon or the hnp, we need to cancel the
     * recv we posted
     */
    if (ORTE_PROC_IS_DAEMON || ORTE_PROC_IS_HNP) {
        orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DAEMON_COLLECTIVE);
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


static bool barrier_recvd;

static void barrier_recv(int status, orte_process_name_t* sender,
                         opal_buffer_t *buffer,
                         orte_rml_tag_t tag, void *cbdata)
{
    /* flag as recvd */
    barrier_recvd = true;
}

static int barrier(void)
{
    opal_buffer_t buf;
    orte_grpcomm_coll_t coll_type=ORTE_GRPCOMM_BARRIER;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic entering barrier",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* everyone sends barrier to local daemon */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    /* tell the daemon we are doing a barrier */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &coll_type, 1, ORTE_GRPCOMM_COLL_T))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    /* send to local daemon */
    if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_DAEMON, &buf, ORTE_RML_TAG_DAEMON_COLLECTIVE, 0))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    OBJ_DESTRUCT(&buf);
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s grpcomm:basic barrier sent",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* now receive the release. Be sure to do this in
     * a manner that allows us to return without being in a recv!
     */
    barrier_recvd = false;
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_BARRIER,
                                 ORTE_RML_NON_PERSISTENT, barrier_recv, NULL);
    if (rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    ORTE_PROGRESSED_WAIT(barrier_recvd, 0, 1);
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s grpcomm:basic received barrier release",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    return ORTE_SUCCESS;
}

static int num_onesided_barrier_recvd;

static void process_onesided_barrier(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    /* release the message */
    OBJ_RELEASE(mev);
    /* flag as recvd */
    num_onesided_barrier_recvd++;
}

static void onesided_barrier_recv(int status, orte_process_name_t* sender,
                                  opal_buffer_t* buffer, orte_rml_tag_t tag,
                                  void* cbdata)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                         "%s grpcomm:bad:receive got message from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));
    
    /* don't process this right away - we need to get out of the recv before
     * we process the message as it may ask us to do something that involves
     * more messaging! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release above - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, process_onesided_barrier);
    
    /* reissue the recv */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_ONESIDED_BARRIER,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      onesided_barrier_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    return;
}
/* quick timeout loop */
static bool timer_fired;

static void quicktime_cb(int fd, short event, void *cbdata)
{
    /* declare it fired */
    timer_fired = true;
}

static int onesided_barrier(void)
{
    int num_participating;
    opal_list_t daemon_tree;
    opal_buffer_t buf;
    orte_process_name_t my_parent;
    opal_event_t *quicktime=NULL;
    struct timeval quicktimeval;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                         "%s grpcomm:basic: onesided barrier called",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if we are not to use the barrier, then just return */
    if (!orte_orted_exit_with_barrier) {
        if (ORTE_PROC_IS_HNP) {
            /* if we are the HNP, we need to do a little delay to give
             * the orteds a chance to exit before we leave
             */
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                                 "%s grpcomm:basic: onesided barrier adding delay timer",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            quicktimeval.tv_sec = 0;
            quicktimeval.tv_usec = 100;
            timer_fired = false;
            ORTE_DETECT_TIMEOUT(&quicktime, orte_process_info.num_procs, 1000, 10000, quicktime_cb);
            ORTE_PROGRESSED_WAIT(timer_fired, 0, 1);
        }
        return ORTE_SUCCESS;
    }
    
    /* initialize things */
    num_onesided_barrier_recvd = 0;
    num_participating = 0;
    
    /* figure out how many participants we should be expecting */
    OBJ_CONSTRUCT(&daemon_tree, opal_list_t);
    my_parent.jobid = ORTE_PROC_MY_NAME->jobid;
    my_parent.vpid = orte_routed.get_routing_tree(&daemon_tree);
    num_participating = opal_list_get_size(&daemon_tree);
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                         "%s grpcomm:basic: onesided barrier num_participating %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), num_participating));
    
    /* set the recv */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_ONESIDED_BARRIER,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      onesided_barrier_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    
    /* wait to recv them */
    ORTE_PROGRESSED_WAIT(false, num_onesided_barrier_recvd, num_participating);
    
    /* cancel the recv */
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ONESIDED_BARRIER);
    
    /* if I am the HNP, then we are done */
    if (ORTE_PROC_IS_HNP) {
        return ORTE_SUCCESS;
    }
    
    /* send a zero-byte msg to my parent */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    /* send it */
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                         "%s grpcomm:basic:onsided:barrier not the HNP - sending to parent %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&my_parent)));
    if (0 > (rc = orte_rml.send_buffer(&my_parent, &buf, ORTE_RML_TAG_ONESIDED_BARRIER, 0))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    OBJ_DESTRUCT(&buf);
    
    return ORTE_SUCCESS;
}


static opal_buffer_t *allgather_buf;
static orte_std_cntr_t allgather_complete;

static void allgather_recv(int status, orte_process_name_t* sender,
                            opal_buffer_t *buffer,
                            orte_rml_tag_t tag, void *cbdata)
{
    int rc;
    
    /* xfer the data */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(allgather_buf, buffer))) {
        ORTE_ERROR_LOG(rc);
    }
    allgather_complete = true;
}

static int allgather(opal_buffer_t *sbuf, opal_buffer_t *rbuf)
{
    int rc;
    opal_buffer_t coll;
    orte_grpcomm_coll_t coll_type=ORTE_GRPCOMM_ALLGATHER;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic entering allgather",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* everyone sends data to their local daemon */
    OBJ_CONSTRUCT(&coll, opal_buffer_t);
    /* tell the daemon we are doing an allgather */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&coll, &coll_type, 1, ORTE_GRPCOMM_COLL_T))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&coll);
        return rc;
    }    
    /* add our data to it */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(&coll, sbuf))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&coll);
        return rc;
    }
    /* send to local daemon */
    if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_DAEMON, &coll, ORTE_RML_TAG_DAEMON_COLLECTIVE, 0))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&coll);
        return rc;
    }
    OBJ_DESTRUCT(&coll);
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s grpcomm:basic allgather buffer sent",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* setup the buffer that will recv the results */
    allgather_buf = OBJ_NEW(opal_buffer_t);
    
    /* now receive the final result. Be sure to do this in
     * a manner that allows us to return without being in a recv!
     */
    allgather_complete = false;
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ALLGATHER,
                                 ORTE_RML_NON_PERSISTENT, allgather_recv, NULL);
    if (rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    ORTE_PROGRESSED_WAIT(allgather_complete, 0, 1);
    
    /* copy payload to the caller's buffer */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(rbuf, allgather_buf))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(allgather_buf);
        return rc;
    }
    OBJ_RELEASE(allgather_buf);
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic allgather completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    return ORTE_SUCCESS;
}

/***   MODEX SECTION ***/
static int modex(opal_list_t *procs)
{
    int rc=ORTE_SUCCESS;
    int fd;
    opal_byte_object_t bo, *boptr;
    int32_t i, n;
    char *nodename, *attr;
    orte_nid_t **nd, *ndptr;
    orte_attr_t *attrdata;
    opal_buffer_t bobuf;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic: modex entered",
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
        if (ORTE_SUCCESS != (rc = orte_grpcomm_base_full_modex(procs, false))) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }
    
    /* Do a modex across our peers if we are doing an opal_profile so that the
     * HNP can collect our modex info
     */
    
    if (opal_profile) {
        if (ORTE_SUCCESS != (rc = orte_grpcomm_base_peer_modex(false))) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }
    
    if (OPAL_ENABLE_HETEROGENEOUS_SUPPORT) {
        /* decide if we need to add the architecture to the modex. Check
         * first to see if hetero is enabled - if not, then we clearly
         * don't need to exchange arch's as they are all identical
         */
        /* Case 1: If different apps in this job were built differently - e.g., some
         * are built 32-bit while others are built 64-bit - then we need to modex
         * regardless of any other consideration. The user is reqd to tell us via a
         * cmd line option if this situation exists, which will result in an mca param
         * being set for us, so all we need to do is check for the global boolean
         * that corresponds to that param
         *
         * Case 2: the nodes are hetero, but the app binaries were built
         * the same - i.e., either they are both 32-bit, or they are both 64-bit, but
         * no mixing of the two. In this case, we include the info in the modex
         */
        if (orte_hetero_apps || !orte_homogeneous_nodes) {
            OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                                 "%s grpcomm:basic: modex is required",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            
            if (ORTE_SUCCESS != (rc = orte_grpcomm_base_peer_modex(false))) {
                ORTE_ERROR_LOG(rc);
            }
            return rc;
        }
    }
    
    /* no modex is required - see if the data was included in the launch message */
    if (orte_send_profile) {
        /* the info was provided in the nidmap - there is nothing more we have to do */
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                             "%s grpcomm:basic:modex using nidmap",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        return ORTE_SUCCESS;
    }
    
    /* see if a profile file was given to us */
    if (NULL == opal_profile_file) {
        /* if we don't have any other way to do this, then let's default to doing the
         * modex so we at least can function, even if it isn't as fast as we might like
         */
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                             "%s grpcomm:basic: modex is required",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        if (ORTE_SUCCESS != (rc = orte_grpcomm_base_peer_modex(false))) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }
    
    fd = open(opal_profile_file, O_RDONLY);
    if (fd < 0) {
        orte_show_help("help-orte-runtime.txt", "grpcomm-basic:file-cant-open", true, opal_profile_file);
        return ORTE_ERR_NOT_FOUND;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic:modex reading %s file",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),  opal_profile_file));

    /* loop through file until end */
    boptr = &bo;
    nd = (orte_nid_t**)orte_nidmap.addr;
    while (0 < read(fd, &bo.size, sizeof(bo.size))) {
        /* this is the number of bytes in the byte object */
        bo.bytes = (uint8_t *) malloc(bo.size);
        if (0 > read(fd, bo.bytes, bo.size)) {
            orte_show_help("help-orte-runtime.txt", "orte_nidmap:unable-read-file", true, opal_profile_file);
            close(fd);
            return ORTE_ERR_FILE_READ_FAILURE;
        }
        /* load the byte object into a buffer for unpacking */
        OBJ_CONSTRUCT(&bobuf, opal_buffer_t);
        opal_dss.load(&bobuf, boptr->bytes, boptr->size);
        /* unpack the nodename */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&bobuf, &nodename, &n, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* find this node in nidmap */
        for (i=0, ndptr=NULL; i < orte_nidmap.size && NULL != nd[i]; i++) {
            /* since we may not have kept fqdn hostnames, we can only check
             * for equality to the length of the name in the nid
             */
            if (0 == strncmp(nd[i]->name, nodename, strlen(nd[i]->name))) {
                ndptr = nd[i];
                break;
            }
        }
        free(nodename);  /* done with this */
        if (NULL == ndptr) {
            /* didn't find it! */
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        
        /* loop through the rest of the object to unpack the attr's themselves */
        n = 1;
        while (ORTE_SUCCESS == opal_dss.unpack(&bobuf, &attr, &n, OPAL_STRING)) {
            attrdata = OBJ_NEW(orte_attr_t);
            attrdata->name = strdup(attr);
            /* read the number of bytes in the blob */
            n = 1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&bobuf, &attrdata->size, &n, OPAL_INT32))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* unpack the bytes */
            attrdata->bytes = (uint8_t *) malloc(attrdata->size);
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(&bobuf, attrdata->bytes, &attrdata->size, OPAL_BYTE))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* add to our list for this node */
            opal_list_append(&ndptr->attrs, &attrdata->super);
        }
        OBJ_DESTRUCT(&bobuf);
    }

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic: modex completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return rc;
}

/* the HNP will -never- execute the following as it is NOT an MPI process */
static int set_proc_attr(const char *attr_name, const void *data, size_t size)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:basic:set_proc_attr for attribute %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), attr_name));

    /* if we are doing a profile, pack this up */
    if (opal_profile) {
        int32_t isize;
        
        if (ORTE_SUCCESS != (rc = opal_dss.pack(profile_buf, &attr_name, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        isize = size;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(profile_buf, &isize, 1, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(profile_buf, data, isize, OPAL_BYTE))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        /* let it fall through so that the job doesn't hang! */
        return orte_grpcomm_base_set_proc_attr(attr_name, data, size);
    }
    
    /* we always have to set our own attributes in case they are needed for
     * a connect/accept at some later time
     */
cleanup:
    return orte_grpcomm_base_set_proc_attr(attr_name, data, size);
}

static int get_proc_attr(const orte_process_name_t proc,
                         const char * attribute_name, void **val, 
                         size_t *size)
{
    orte_nid_t *nid;
    opal_list_item_t *item;
    orte_attr_t *attr;
    
    /* find this proc's node in the nidmap */
    if (NULL == (nid = orte_util_lookup_nid((orte_process_name_t*)&proc))) {
        /* proc wasn't found - return error */
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                             "%s grpcomm:basic:get_proc_attr: no modex entry for proc %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc)));
        return ORTE_ERR_NOT_FOUND;
        
    }
    
    /* look for this attribute */
    for (item = opal_list_get_first(&nid->attrs);
         item != opal_list_get_end(&nid->attrs);
         item = opal_list_get_next(item)) {
        attr = (orte_attr_t*)item;
        if (0 == strcmp(attr->name, attribute_name)) {
            /* copy the data to the caller */
            void *copy = malloc(attr->size);
            
            if (copy == NULL) {
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            memcpy(copy, attr->bytes, attr->size);
            *val = copy;
            *size = attr->size;
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                                 "%s grpcomm:basic:get_proc_attr: found %d bytes for attr %s on proc %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)attr->size,
                                 attribute_name, ORTE_NAME_PRINT(&proc)));
            return ORTE_SUCCESS;
        }
    }
    
    /* get here if attribute isn't found */
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                         "%s grpcomm:basic:get_proc_attr: no attr avail or zero byte size for proc %s attribute %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&proc), attribute_name));
    *val = NULL;
    *size = 0;
    
    return ORTE_SUCCESS;
}


/* process incoming messages in order of receipt */
static void process_msg(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    int32_t rc, count;
    opal_byte_object_t *bo;
    
    /* save the info in the file */
    if (0 <= profile_fd) {
        /* extract the byte object holding the node's modex info */
        count=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(mev->buffer, &bo, &count, OPAL_BYTE_OBJECT))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                             "%s grpcomm:basic:receive:profile writing %d bytes of data from proc %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             bo->size, ORTE_NAME_PRINT(&mev->sender)));
        
        write(profile_fd, &bo->size, sizeof(bo->size));
        write(profile_fd, bo->bytes, bo->size);
        free(bo->bytes);
        free(bo);
    }
    
CLEANUP:
    /* release the message */
    OBJ_RELEASE(mev);
}

/*
 * NOTE: The incoming buffer "buffer" is OBJ_RELEASED by the calling program.
 * DO NOT RELEASE THIS BUFFER IN THIS CODE
 */

static void profile_recv(int status, orte_process_name_t* sender,
                         opal_buffer_t* buffer, orte_rml_tag_t tag,
                         void* cbdata)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                         "%s grpcomm:basic:receive got message from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));
    
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
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_GRPCOMM_PROFILE,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      profile_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    return;
}

/***************  COLLECTIVES FOR DAEMONS  **************/

static bool all_children_participated(orte_jobid_t job)
{
    opal_list_item_t *item;
    orte_odls_child_t *child;
    
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        /* is this child part of the specified job? */
        if (child->name->jobid == job && !child->coll_recvd) {
            /* if this child has *not* participated yet, return false */
            return false;
        }
    }
    
    /* if we get here, then everyone in the job has participated */
    return true;
    
}

static int daemon_collective(orte_process_name_t *sender, opal_buffer_t *data)
{
    orte_jobid_t jobid;
    orte_odls_job_t *jobdat;
    orte_routed_tree_t *child;
    orte_std_cntr_t n;
    opal_list_t daemon_tree;
    opal_list_item_t *item, *next;
    int32_t num_contributors;
    opal_buffer_t buf;
    orte_process_name_t my_parent, proc;
    orte_vpid_t daemonvpid;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                         "%s odls: daemon collective called",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* unpack the jobid using this collective */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &jobid, &n, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* lookup the job record for it */
    jobdat = NULL;
    for (item = opal_list_get_first(&orte_local_jobdata);
         item != opal_list_get_end(&orte_local_jobdata);
         item = opal_list_get_next(item)) {
        jobdat = (orte_odls_job_t*)item;
        
        /* is this the specified job? */
        if (jobdat->jobid == jobid) {
            break;
        }
    }
    if (NULL == jobdat) {
        /* race condition - someone sent us a collective before we could
         * parse the add_local_procs cmd. Just add the jobdat object
         * and continue
         */
        jobdat = OBJ_NEW(orte_odls_job_t);
        jobdat->jobid = jobid;
        opal_list_append(&orte_local_jobdata, &jobdat->super);
    }
    
    /* it may be possible to get here prior to having actually finished processing our
     * local launch msg due to the race condition between different nodes and when
     * they start their individual procs. Hence, we have to first ensure that we
     * -have- finished processing the launch msg, or else we won't know whether
     * or not to wait before sending this on
     */
    ORTE_PROGRESSED_WAIT(jobdat->launch_msg_processed, 0, 1);
    
    /* unpack the collective type */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &jobdat->collective_type, &n, ORTE_GRPCOMM_COLL_T))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* unpack the number of contributors in this data bucket */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &num_contributors, &n, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    jobdat->num_contributors += num_contributors;
    
    /* xfer the data */
    opal_dss.copy_payload(&jobdat->collection_bucket, data);
    
    /* count the number of participants collected */
    jobdat->num_collected++;
    
    /* if we haven't already done so, figure out how many participants we
     * should be expecting
     */
    if (jobdat->num_participating < 0) {
        if (0 < jobdat->num_local_procs) {
            /* we have children, so account for our own participation */
            jobdat->num_participating = 1;
        } else {
            jobdat->num_participating = 0;
        }
        /* now see if anyone else will be sending us something */
        OBJ_CONSTRUCT(&daemon_tree, opal_list_t);
        orte_routed.get_routing_tree(&daemon_tree);
        /* unfortunately, there is no simple way to determine which of our "child"
         * daemons in the routing tree will be sending us something. All we can do
         * is brute force a search, though we attempt to keep it as short as possible
         */
        proc.jobid = jobid;
        proc.vpid = 0;
        while (proc.vpid < jobdat->num_procs && 0 < opal_list_get_size(&daemon_tree)) {
            /* get the daemon that hosts this proc */
            daemonvpid = orte_ess.proc_get_daemon(&proc);
            /* is this daemon one of our children, or at least its contribution
             * will pass through one of our children
             */
            item = opal_list_get_first(&daemon_tree);
            while (item != opal_list_get_end(&daemon_tree)) {
                next = opal_list_get_next(item);
                child = (orte_routed_tree_t*)item;
                if (child->vpid == daemonvpid || opal_bitmap_is_set_bit(&child->relatives, daemonvpid)) {
                    /* it does - add to num_participating */
                    jobdat->num_participating++;
                    /* remove this from the list so we don't double count it */
                    opal_list_remove_item(&daemon_tree, item);
                    /* done with search */
                    break;
                }
                item = next;
            }
            proc.vpid++;
        }
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                         "%s grpcomm:bad: daemon collective for job %s from %s type %ld num_collected %d num_participating %d num_contributors %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_JOBID_PRINT(jobid),
                         ORTE_NAME_PRINT(sender),
                         (long)jobdat->collective_type, jobdat->num_collected,
                         jobdat->num_participating, jobdat->num_contributors));
    
    if (jobdat->num_collected == jobdat->num_participating) {
        /* if I am the HNP, go process the results */
        if (ORTE_PROC_IS_HNP) {
            goto hnp_process;
        }
        
        /* if I am not the HNP, send to my parent */
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        /* pack the jobid */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &jobid, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* pack the collective type */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &jobdat->collective_type, 1, ORTE_GRPCOMM_COLL_T))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* pack the number of contributors */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &jobdat->num_contributors, 1, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* xfer the payload*/
        opal_dss.copy_payload(&buf, &jobdat->collection_bucket);
        /* reset everything for next collective */
        jobdat->num_contributors = 0;
        jobdat->num_collected = 0;
        OBJ_DESTRUCT(&jobdat->collection_bucket);
        OBJ_CONSTRUCT(&jobdat->collection_bucket, opal_buffer_t);
        /* send it */
        my_parent.jobid = ORTE_PROC_MY_NAME->jobid;
        my_parent.vpid = orte_routed.get_routing_tree(NULL);
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                             "%s grpcomm:bad: daemon collective not the HNP - sending to parent %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&my_parent)));
        if (0 > (rc = orte_rml.send_buffer(&my_parent, &buf, ORTE_RML_TAG_DAEMON_COLLECTIVE, 0))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        OBJ_DESTRUCT(&buf);
    }
    return ORTE_SUCCESS;
    
hnp_process:
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                         "%s grpcomm:bad: daemon collective HNP - xcasting to job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jobid)));
    /* setup a buffer to send the results back to the job members */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    
    if (ORTE_GRPCOMM_BARRIER == jobdat->collective_type) {
        /* reset everything for next collective */
        jobdat->num_contributors = 0;
        jobdat->num_collected = 0;
        OBJ_DESTRUCT(&jobdat->collection_bucket);
        OBJ_CONSTRUCT(&jobdat->collection_bucket, opal_buffer_t);
        /* don't need anything in this for a barrier */
        if (ORTE_SUCCESS != (rc = orte_grpcomm.xcast(jobid, &buf, ORTE_RML_TAG_BARRIER))) {
            ORTE_ERROR_LOG(rc);
        }
    } else if (ORTE_GRPCOMM_ALLGATHER == jobdat->collective_type) {
        int32_t numc;
        /* add the data */
        numc = jobdat->num_contributors;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &numc, 1, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(&buf, &jobdat->collection_bucket))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        /* reset everything for next collective */
        jobdat->num_contributors = 0;
        jobdat->num_collected = 0;
        OBJ_DESTRUCT(&jobdat->collection_bucket);
        OBJ_CONSTRUCT(&jobdat->collection_bucket, opal_buffer_t);
        /* send the buffer */
        if (ORTE_SUCCESS != (rc = orte_grpcomm.xcast(jobid, &buf, ORTE_RML_TAG_ALLGATHER))) {
            ORTE_ERROR_LOG(rc);
        }
    } else {
        /* no other collectives currently supported! */
        ORTE_ERROR_LOG(ORTE_ERR_NOT_IMPLEMENTED);
        rc = ORTE_ERR_NOT_IMPLEMENTED;
    }
    
cleanup:
    OBJ_DESTRUCT(&buf);
    
    return ORTE_SUCCESS;    
}


static void reset_child_participation(orte_jobid_t job)
{
    opal_list_item_t *item;
    orte_odls_child_t *child;
    
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        /* is this child part of the specified job? */
        if (child->name->jobid == job) {
            /* clear flag */
            child->coll_recvd = false;
        }
    }    
}

static void process_coll_msg(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    orte_process_name_t *proc;
    opal_buffer_t *buf, relay;
    int32_t rc, n;
    opal_list_item_t *item;
    orte_odls_child_t *child;
    bool found = false;
    orte_odls_job_t *jobdat;
    
    proc = &mev->sender;
    buf = mev->buffer;
    
    /* is the sender a local proc, or a daemon relaying the collective? */
    if (ORTE_PROC_MY_NAME->jobid == proc->jobid) {
        /* this is a relay - call that code */
        if (ORTE_SUCCESS != (rc = daemon_collective(proc, buf))) {
            ORTE_ERROR_LOG(rc);
        }
        goto CLEANUP;
    }
    
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        /* find this child */
        if (OPAL_EQUAL == opal_dss.compare(proc, child->name, ORTE_NAME)) {
            
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                                 "%s grpcomm:bad: collecting data from child %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(child->name)));
            
            found = true;
            break;
        }
    }
    
    /* if it wasn't found on the list, then we need to add it - must have
     * come from a singleton
     */
    if (!found) {
        child = OBJ_NEW(orte_odls_child_t);
        if (ORTE_SUCCESS != (rc = opal_dss.copy((void**)&child->name, proc, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        opal_list_append(&orte_local_children, &child->super);
        /* we don't know any other info about the child, so just indicate it's
         * alive
         */
        child->alive = true;
        /* setup a jobdat for it */
        orte_odls_base_setup_singleton_jobdat(proc->jobid);
    }
    
    /* this was one of our local procs - find the jobdat for this job */
    jobdat = NULL;
    for (item = opal_list_get_first(&orte_local_jobdata);
         item != opal_list_get_end(&orte_local_jobdata);
         item = opal_list_get_next(item)) {
        jobdat = (orte_odls_job_t*)item;
        
        /* is this the specified job? */
        if (jobdat->jobid == proc->jobid) {
            break;
        }
    }
    if (NULL == jobdat) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        rc = ORTE_ERR_NOT_FOUND;
        goto CLEANUP;
    }
    
    /* unpack the collective type */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, &jobdat->collective_type, &n, ORTE_GRPCOMM_COLL_T))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* collect the provided data */
    opal_dss.copy_payload(&jobdat->local_collection, buf);
    
    /* flag this proc as having participated */
    child->coll_recvd = true;
    
    /* now check to see if all local procs in this job have participated */
    if (all_children_participated(proc->jobid)) {
        
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                             "%s grpcomm:bad: executing collective",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        /* prep a buffer to pass it all along */
        OBJ_CONSTRUCT(&relay, opal_buffer_t);
        /* pack the jobid */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&relay, &proc->jobid, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* pack the collective type */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&relay, &jobdat->collective_type, 1, ORTE_GRPCOMM_COLL_T))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* pack the number of contributors */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&relay, &jobdat->num_local_procs, 1, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* xfer the payload*/
        opal_dss.copy_payload(&relay, &jobdat->local_collection);
        /* refresh the collection bucket for reuse */
        OBJ_DESTRUCT(&jobdat->local_collection);
        OBJ_CONSTRUCT(&jobdat->local_collection, opal_buffer_t);
        reset_child_participation(proc->jobid);
        /* pass this to the daemon collective operation */
        daemon_collective(ORTE_PROC_MY_NAME, &relay);
        
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                             "%s grpcomm:bad: collective completed",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    }
    
CLEANUP:
    /* release the message */
    OBJ_RELEASE(mev);
}

static void daemon_coll_recv(int status, orte_process_name_t* sender,
                             opal_buffer_t* buffer, orte_rml_tag_t tag,
                             void* cbdata)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                         "%s grpcomm:bad:receive got message from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));
    
    /* don't process this right away - we need to get out of the recv before
     * we process the message as it may ask us to do something that involves
     * more messaging! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release above - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, process_coll_msg);
    
    /* reissue the recv */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_DAEMON_COLLECTIVE,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      daemon_coll_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    return;
}
