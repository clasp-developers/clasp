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
#include "opal/types.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/oob/tcp/oob_tcp.h"

static int mca_oob_tcp_send_self(
    mca_oob_tcp_peer_t* peer,
    mca_oob_tcp_msg_t* msg,
    struct iovec* iov, 
    int count)
{
    unsigned char *ptr;
    int size = 0;
    int rc;

    for(rc = 0; rc < count; rc++) {
        size += iov[rc].iov_len;
    }
    msg->msg_rwbuf = malloc(size);
    if(NULL == msg->msg_rwbuf) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    ptr = (unsigned char *)msg->msg_rwbuf;
    for(rc = 0; rc < count; rc++) {
        memcpy(ptr, iov[rc].iov_base, iov[rc].iov_len);
        ptr += iov[rc].iov_len;
    }
    msg->msg_hdr.msg_size = size;

    /*
     * Copied original buffer - so local send completion.
    */
                                                                                                     
    opal_mutex_lock(&msg->msg_lock);
    msg->msg_complete = true;
    if(NULL != msg->msg_cbfunc) {
        msg->msg_cbfunc(
            ORTE_SUCCESS, 
            &peer->peer_name, 
            msg->msg_uiov, 
            msg->msg_ucnt, 
            msg->msg_hdr.msg_tag, 
            msg->msg_cbdata);
    } else {
        opal_condition_broadcast(&msg->msg_condition);
    }
    opal_mutex_unlock(&msg->msg_lock);

    /*
     * Attempt to match against posted receive
     */
    mca_oob_tcp_msg_recv_complete(msg, peer);
    return size;
}


/*
 * Non-blocking version of mca_oob_send().
 *
 * @param peer (IN)    Opaque name of peer process.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param count (IN)   Number of elements in iovec array.
 * @param flags (IN)   Currently unused.
 * @param cbfunc (IN)  Callback function on send completion.
 * @param cbdata (IN)  User data that is passed to callback function.
 * @return             OMPI error code (<0) on error number of bytes actually sent.
 *
 */
int mca_oob_tcp_send_nb(
    orte_process_name_t* target, 
    orte_process_name_t* origin, 
    struct iovec* iov, 
    int count,
    int tag,
    int flags, 
    orte_rml_callback_fn_t cbfunc, 
    void* cbdata)
{
    mca_oob_tcp_peer_t* peer = mca_oob_tcp_peer_lookup(target);
    mca_oob_tcp_msg_t* msg;
    int size;
    int rc;

    if(NULL == peer)
        return ORTE_ERR_UNREACH;

    MCA_OOB_TCP_MSG_ALLOC(msg, rc);
    if(NULL == msg) {
        return rc;
    }

    /* calculate the size of the message */
    size = 0;
    for(rc = 0; rc < count; rc++) {
        size += iov[rc].iov_len;
    }

    if(mca_oob_tcp_component.tcp_debug >= OOB_TCP_DEBUG_ALL) {
        opal_output(0, "%s-%s mca_oob_tcp_send_nb: tag %d size %lu\n",
            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
            ORTE_NAME_PRINT(&(peer->peer_name)),
            tag, (unsigned long)size );
    }

    /* turn the size to network byte order so there will be no problems */
    msg->msg_hdr.msg_type = MCA_OOB_TCP_DATA;
    msg->msg_hdr.msg_size = size;
    msg->msg_hdr.msg_tag = tag;
    msg->msg_hdr.msg_origin = *origin;
    msg->msg_hdr.msg_src = *ORTE_PROC_MY_NAME;
    msg->msg_hdr.msg_dst = *target;

    /* create one additional iovect that will hold the size of the message */
    msg->msg_type = MCA_OOB_TCP_POSTED;
    msg->msg_rc = 0;
    msg->msg_flags = flags;
    msg->msg_uiov = iov;
    msg->msg_ucnt = count;
    msg->msg_rwiov = mca_oob_tcp_msg_iov_alloc(msg,count+1);
    msg->msg_rwiov[0].iov_base = (ompi_iov_base_ptr_t)(&msg->msg_hdr);
    msg->msg_rwiov[0].iov_len = sizeof(msg->msg_hdr);
    msg->msg_rwptr = msg->msg_rwiov;
    msg->msg_rwcnt = msg->msg_rwnum = count + 1;
    memcpy(msg->msg_rwiov+1, msg->msg_uiov, sizeof(struct iovec)*msg->msg_ucnt);
    msg->msg_rwbuf = NULL;
    msg->msg_cbfunc = cbfunc;
    msg->msg_cbdata = cbdata;
    msg->msg_complete = false;
    msg->msg_peer = peer->peer_name;
    
    if (OPAL_EQUAL == mca_oob_tcp_process_name_compare(target, ORTE_PROC_MY_NAME)) {  /* local delivery */
        rc = mca_oob_tcp_send_self(peer,msg,iov,count);
        if (rc < 0 ) {
            return rc;
        } else if (size == rc) {
            return ORTE_SUCCESS;
        } else {
            return ORTE_ERROR;
        }
    }

    MCA_OOB_TCP_HDR_HTON(&msg->msg_hdr);
    rc = mca_oob_tcp_peer_send(peer, msg);
    if(rc != ORTE_SUCCESS) {
        if (rc != ORTE_ERR_ADDRESSEE_UNKNOWN) {
            MCA_OOB_TCP_MSG_RETURN(msg);
        }
        return rc;
    }

    return ORTE_SUCCESS;
}

