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
/** @file:
 * 
 * contains the data structure we will use to describe a message
 */

#ifndef _MCA_OOB_TCP_MESSAGE_H_
#define _MCA_OOB_TCP_MESSAGE_H_

#include "orte_config.h"
#include "orte/types.h"

#include <errno.h>


#include "orte/mca/oob/oob.h"
#include "oob_tcp_hdr.h"

BEGIN_C_DECLS

struct mca_oob_tcp_peer_t;

#define MCA_OOB_TCP_IOV_MAX  16

typedef enum { MCA_OOB_TCP_POSTED, MCA_OOB_TCP_UNEXPECTED } mca_oob_tcp_type_t;


/**
 * describes each message being progressed.
 */
struct mca_oob_tcp_msg_t {
    opal_free_list_item_t      super;    /**< allow this item to be put on a list */
    mca_oob_tcp_type_t    msg_type;      /**< posted receive or unexpected */
    int                   msg_flags;     /**< flags to send/recv */
    int                   msg_rc;        /**< the return code for the send/recv (amount sent/recvd or errno) */
    mca_oob_tcp_hdr_t     msg_hdr;       /**< header used to convey message properties to peer */
    struct iovec*         msg_uiov;      /**< the user supplied iovec array */
    int                   msg_ucnt;      /**< the number of items in the user iovec array */
    struct iovec *        msg_rwiov;     /**< copy of iovec array - not data */
    struct iovec *        msg_rwptr;     /**< current read/write pointer into msg_iov */
    int                   msg_rwnum;     /**< number of iovecs left for read/write */
    int                   msg_rwcnt;     /**< total number of iovecs for read/write */
    void*                 msg_rwbuf;     /**< optional buffer for send/recv */
    orte_rml_callback_fn_t msg_cbfunc;    /**< the callback function for the send/receive */    
    void *                msg_cbdata;    /**< the data for the callback fnuction */
    bool                  msg_complete;  /**< whether the message is done sending or not */
    orte_process_name_t   msg_peer;      /**< the name of the peer */
    opal_mutex_t          msg_lock;      /**< lock for the condition variable */
    opal_condition_t      msg_condition; /**< condition variable for completion */
    struct iovec          msg_iov[MCA_OOB_TCP_IOV_MAX];  /** preallocate space for iovec array */
};

/**
 * Convenience typedef
 */
typedef struct mca_oob_tcp_msg_t mca_oob_tcp_msg_t;

OBJ_CLASS_DECLARATION(mca_oob_tcp_msg_t);

/**
 * Get a new structure for use with a message
 */
#define MCA_OOB_TCP_MSG_ALLOC(msg, rc)                             \
{                                                                  \
    opal_free_list_item_t* item;                                   \
    OPAL_FREE_LIST_GET(&mca_oob_tcp_component.tcp_msgs, item, rc); \
    msg = (mca_oob_tcp_msg_t*)item;                                \
}

/**
 * return a message structure that is no longer needed
 */
#define MCA_OOB_TCP_MSG_RETURN(msg)                                     \
{                                                                       \
    /* frees the iovec allocated during the send/receive */             \
    if(NULL != msg->msg_rwiov)                                          \
        mca_oob_tcp_msg_iov_return(msg,msg->msg_rwiov);                 \
    if(NULL != msg->msg_rwbuf)                                          \
        free(msg->msg_rwbuf);                                           \
    OPAL_FREE_LIST_RETURN(&mca_oob_tcp_component.tcp_msgs,              \
                          &msg->super);                                 \
}

/**
 *  Wait for a msg to complete.
 *  @param  msg (IN)     Message to wait on.
 *  @param  size (OUT)   Number of bytes delivered.
 *  @retval ORTE_SUCCESS or error code on failure.
 */
int mca_oob_tcp_msg_wait(mca_oob_tcp_msg_t* msg, int* size);

/**
 *  Wait - up to a timeout - for a msg to complete.
 *  @param  msg (IN)     Message to wait on.
 *  @param  size (OUT)   Number of bytes delivered.
 *  @retval ORTE_SUCCESS or error code on failure.
 */
int mca_oob_tcp_msg_timedwait(mca_oob_tcp_msg_t* msg, int* size, struct timespec* ts);

/**
 *  Signal that a message has completed. Wakes up any pending threads (for blocking send)
 *  or invokes callbacks for non-blocking case.
 *  @param  msg (IN)   Message send/recv that has completed.
 *  @param  peer (IN)  The peer the send/receive was from
 *  @retval ORTE_SUCCESS or error code on failure.
 */
int mca_oob_tcp_msg_complete(mca_oob_tcp_msg_t* msg, orte_process_name_t * peer);

/**
 *  Called to copy the results of a message into user supplied iovec array.
 *  @param  msg (IN)   Message send that is in progress. 
 *  @param  iov (IN)   Iovec array of user supplied buffers.
 *  @retval count      Number of elements in iovec array.
 */

int mca_oob_tcp_msg_copy(mca_oob_tcp_msg_t* msg, struct iovec* iov, int count);

/**
 *  Called asynchronously to progress sending a message from the event library thread.
 *  @param  msg (IN)   Message send that is in progress. 
 *  @param  peer (IN)  Peer we are sending to.
 *  @retval            Number of bytes copied.
 */
bool mca_oob_tcp_msg_send_handler(mca_oob_tcp_msg_t* msg, struct mca_oob_tcp_peer_t * peer);

/**
 *  Called asynchronously to progress sending a message from the event library thread.
 *  @param  msg (IN)   Message send that is in progress. 
 *  @param  peer (IN)  Peer theat we are recieving from.
 *  @retval bool       Bool flag indicating wether operation has completed.
 */

bool mca_oob_tcp_msg_recv_handler(mca_oob_tcp_msg_t* msg, struct mca_oob_tcp_peer_t * peer);

/**
 * The message has been completely received - so attempt to match
 * against posted recvs.
 */

void mca_oob_tcp_msg_recv_complete(mca_oob_tcp_msg_t* msg, struct mca_oob_tcp_peer_t* peer);

/**
 *  Match name to a message that has been received asynchronously (unexpected).
 *
 *  @param  name (IN)  Name associated with peer or wildcard to match first posted recv.
 *  @param  tag (IN)   Message tag. 
 *  @return msg        Matched message or NULL.
 *
 *  Note - this routine requires the caller to be holding the module lock.
 */

mca_oob_tcp_msg_t* mca_oob_tcp_msg_match_recv(orte_process_name_t* name, int tag);

/**
 *  Match name to a posted recv request.
 *
 *  @param  name (IN)  Name associated with peer or wildcard to match first posted recv.
 *  @param  tag (IN)   Message tag. 
 *  @return msg        Matched message or NULL.
 *
 *  Note - this routine requires the caller to be holding the module lock.
 */

mca_oob_tcp_msg_t* mca_oob_tcp_msg_match_post(orte_process_name_t* name, int tag);

/**
 *  Allocate space for iovec array - if the request number of elements is less than
 *  MCA_OOB_TCP_IOV_MAX then use the array allocated along w/ the message - otherwise
 *  allocate count elements.
 *
 *  @param  msg (IN)  Message to allocate array.
 *  @param  count (IN) the number of iovecs
 *  @return           Array of iovec elements.
 *
 */
static inline struct iovec* mca_oob_tcp_msg_iov_alloc(mca_oob_tcp_msg_t* msg, int count)
{
    if(count <= MCA_OOB_TCP_IOV_MAX) 
        return msg->msg_iov;
    return (struct iovec *)malloc(sizeof(struct iovec) * count);
}


/**
 *  Release resource held by iovec array if this is not part of the message.
 *
 *  @param  msg (IN)  Message to allocate array.
 *  @param  iov (IN)  Iovec array to return.
 *
 */

static inline void mca_oob_tcp_msg_iov_return(mca_oob_tcp_msg_t* msg, struct iovec* iov)
{
    if(iov != msg->msg_iov)
        free(iov);
}

END_C_DECLS

#endif /* _MCA_OOB_TCP_MESSAGE_H_ */

