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
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC. 
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"
#include "orte/types.h"

#include "orte/runtime/orte_globals.h"

#include "orte/mca/oob/tcp/oob_tcp.h"


/**
 * Process a matched posted receive
 *
 * Note that the match lock must be held prior to the call.
 */

static void mca_oob_tcp_msg_matched(mca_oob_tcp_msg_t* msg, mca_oob_tcp_msg_t* match)
{
    int i,rc;
    if(match->msg_rc < 0)  {
        rc = match->msg_rc; 

    } else {

        if (msg->msg_flags & ORTE_RML_ALLOC) match->msg_flags |= ORTE_RML_ALLOC;
        /* if we are just doing peek, return bytes without dequeing message */
        rc = mca_oob_tcp_msg_copy(match, msg->msg_uiov, msg->msg_ucnt);
        if(rc >= 0 && ORTE_RML_TRUNC & msg->msg_flags) {
            rc = 0;
            for(i=1; i<match->msg_rwcnt+1; i++)
                rc += match->msg_rwiov[i].iov_len;
        }
        if(ORTE_RML_PEEK & msg->msg_flags) {
            OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
            msg->msg_cbfunc(rc, 
                &match->msg_peer, 
                msg->msg_uiov, 
                msg->msg_ucnt,  
                match->msg_hdr.msg_tag, 
                msg->msg_cbdata);
            OPAL_THREAD_LOCK(&mca_oob_tcp_component.tcp_match_lock);
            return;
        }
    }

    /* otherwise remove the match */
    opal_list_remove_item(&mca_oob_tcp_component.tcp_msg_recv, (opal_list_item_t *) match);

    /* invoke callback */
    OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
    msg->msg_cbfunc(rc, 
        &match->msg_peer, 
        msg->msg_uiov, 
        msg->msg_ucnt, 
        match->msg_hdr.msg_tag, 
        msg->msg_cbdata);
    OPAL_THREAD_LOCK(&mca_oob_tcp_component.tcp_match_lock);

    /* return match to free list */
    MCA_OOB_TCP_MSG_RETURN(match);
}

/*
 * Non-blocking version of mca_oob_recv().
 *
 * @param peer (IN)    Opaque name of peer process or ORTE_NAME_WILDCARD for wildcard receive.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param count (IN)   Number of elements in iovec array.
 * @param tag (IN)     User supplied tag for matching send/recv.
 * @param flags (IN)   May be MCA_OOB_PEEK to return up to size bytes of msg w/out removing it from the queue,
 * @param cbfunc (IN)  Callback function on recv completion.
 * @param cbdata (IN)  User data that is passed to callback function.
 * @return             OMPI error code (<0) on error.
 */
int mca_oob_tcp_recv_nb(
    orte_process_name_t* peer, 
    struct iovec* iov, 
    int count,
    int tag,
    int flags, 
    orte_rml_callback_fn_t cbfunc, 
    void* cbdata)
{
    mca_oob_tcp_msg_t *msg;
    mca_oob_tcp_msg_t *match;
    int i, rc, size = 0;

    /* validate params */
    if(NULL == iov || 0 == count) {
        return ORTE_ERR_BAD_PARAM;
    }

    /* allocate/initialize the posted receive */
    MCA_OOB_TCP_MSG_ALLOC(msg, rc);
    if(NULL == msg) {
        OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
        return rc;
    }

    /* determine overall size of user supplied buffer */
    for(i = 0; i < count; i++) {
        size += iov[i].iov_len;
    }

    /* fill in the header */
    msg->msg_hdr.msg_origin = *peer;
    msg->msg_hdr.msg_src = *ORTE_PROC_MY_NAME;
    msg->msg_hdr.msg_dst = *peer;
    msg->msg_hdr.msg_size = size;
    msg->msg_hdr.msg_tag = tag;
    msg->msg_type = MCA_OOB_TCP_POSTED;
    msg->msg_rc = 0;
    msg->msg_flags = flags;
    msg->msg_uiov = iov;
    msg->msg_ucnt = count;
    msg->msg_cbfunc = cbfunc;
    msg->msg_cbdata = cbdata;
    msg->msg_complete = false;
    msg->msg_peer = *peer;
    msg->msg_rwbuf = NULL;
    msg->msg_rwiov = NULL;

    /* acquire the match lock */
    OPAL_THREAD_LOCK(&mca_oob_tcp_component.tcp_match_lock);
    if(flags & ORTE_RML_PERSISTENT) {

        opal_list_append(&mca_oob_tcp_component.tcp_msg_post, (opal_list_item_t *) msg);
        while(NULL != (match = mca_oob_tcp_msg_match_recv(peer,tag))) {
            mca_oob_tcp_msg_matched(msg, match);
        }

    } else {

        /* check to see if a matching receive is on the list */
        match = mca_oob_tcp_msg_match_recv(peer, tag);
        if(NULL != match) {
            mca_oob_tcp_msg_matched(msg, match);
            MCA_OOB_TCP_MSG_RETURN(msg);
        } else {
            opal_list_append(&mca_oob_tcp_component.tcp_msg_post, (opal_list_item_t *) msg);
        }
    }
    OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
    return 0;
}


/*
 * Cancel non-blocking recv.
 *
 * @param peer (IN)    Opaque name of peer process or ORTE_NAME_WILDCARD for wildcard receive.
 * @param tag (IN)     User supplied tag for matching send/recv.
 * @return             OMPI error code (<0) on error or number of bytes actually received.
 */

int mca_oob_tcp_recv_cancel(
    orte_process_name_t* name, 
    int tag)
{
    int matched = 0;
    opal_list_item_t *item, *next;

    /* wait for any previously matched messages to be processed */
    OPAL_THREAD_LOCK(&mca_oob_tcp_component.tcp_match_lock);
#if OPAL_ENABLE_PROGRESS_THREADS
    if(opal_event_progress_thread() == false) {
        while(mca_oob_tcp_component.tcp_match_count) {
            opal_condition_wait(
                &mca_oob_tcp_component.tcp_match_cond, 
                &mca_oob_tcp_component.tcp_match_lock);
        }
    }
#endif

    /* remove any matching posted receives */
    for(item =  opal_list_get_first(&mca_oob_tcp_component.tcp_msg_post);
        item != opal_list_get_end(&mca_oob_tcp_component.tcp_msg_post);
        item =  next) {
        mca_oob_tcp_msg_t* msg = (mca_oob_tcp_msg_t*)item;
        next = opal_list_get_next(item);

        if (OPAL_EQUAL == opal_dss.compare(name, &msg->msg_peer, ORTE_NAME)) {
            if (msg->msg_hdr.msg_tag == tag) {
                opal_list_remove_item(&mca_oob_tcp_component.tcp_msg_post, &msg->super.super);
                MCA_OOB_TCP_MSG_RETURN(msg);
                matched++;
            }
        }
    }
    OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_match_lock);
    return (matched > 0) ? ORTE_SUCCESS : ORTE_ERR_NOT_FOUND;
}

