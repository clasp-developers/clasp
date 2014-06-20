/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2010 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_PML_OB1_SEND_REQUEST_H
#define OMPI_PML_OB1_SEND_REQUEST_H

#include "ompi/mca/btl/btl.h"
#include "ompi/mca/pml/base/pml_base_sendreq.h"
#include "ompi/mca/mpool/base/base.h"
#include "pml_ob1_comm.h"
#include "pml_ob1_hdr.h"
#include "pml_ob1_rdma.h"
#include "pml_ob1_rdmafrag.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/mca/bml/bml.h" 

BEGIN_C_DECLS

typedef enum {
    MCA_PML_OB1_SEND_PENDING_NONE,
    MCA_PML_OB1_SEND_PENDING_SCHEDULE,
    MCA_PML_OB1_SEND_PENDING_START
} mca_pml_ob1_send_pending_t;

struct mca_pml_ob1_send_request_t {
    mca_pml_base_send_request_t req_send;
    mca_bml_base_endpoint_t* req_endpoint;
    ompi_ptr_t req_recv;
    int32_t req_state;
    int32_t req_lock;
    bool req_throttle_sends;
    size_t req_pipeline_depth;
    size_t req_bytes_delivered;
    uint32_t req_rdma_cnt; 
    mca_pml_ob1_send_pending_t req_pending;
    opal_mutex_t req_send_range_lock; 
    opal_list_t req_send_ranges;
    mca_pml_ob1_com_btl_t req_rdma[1]; 
};
typedef struct mca_pml_ob1_send_request_t mca_pml_ob1_send_request_t;

OBJ_CLASS_DECLARATION(mca_pml_ob1_send_request_t);

struct mca_pml_ob1_send_range_t {
    ompi_free_list_item_t base;
    uint64_t range_send_offset;
    uint64_t range_send_length;
    int range_btl_idx;
    int range_btl_cnt;
    mca_pml_ob1_com_btl_t range_btls[1];
};
typedef struct mca_pml_ob1_send_range_t mca_pml_ob1_send_range_t;
OBJ_CLASS_DECLARATION(mca_pml_ob1_send_range_t);

static inline bool lock_send_request(mca_pml_ob1_send_request_t *sendreq)
{
    return OPAL_THREAD_ADD32(&sendreq->req_lock,  1) == 1;
}

static inline bool unlock_send_request(mca_pml_ob1_send_request_t *sendreq)
{
    return OPAL_THREAD_ADD32(&sendreq->req_lock, -1) == 0;
}

static inline void
add_request_to_send_pending(mca_pml_ob1_send_request_t* sendreq,
                            const mca_pml_ob1_send_pending_t type,
                            const bool append)
{
    opal_list_item_t *item = (opal_list_item_t*)sendreq;

    OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
    sendreq->req_pending = type;
    if(append)
        opal_list_append(&mca_pml_ob1.send_pending, item);
    else
        opal_list_prepend(&mca_pml_ob1.send_pending, item);

    OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
}

static inline mca_pml_ob1_send_request_t*
get_request_from_send_pending(mca_pml_ob1_send_pending_t *type)
{
    mca_pml_ob1_send_request_t *sendreq;

    OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
    sendreq = (mca_pml_ob1_send_request_t*)
        opal_list_remove_first(&mca_pml_ob1.send_pending);
    if(sendreq) {
        *type = sendreq->req_pending;
        sendreq->req_pending = MCA_PML_OB1_SEND_PENDING_NONE;
    }
    OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);

    return sendreq;
}

#define MCA_PML_OB1_SEND_REQUEST_ALLOC( comm,                           \
                                        dst,                            \
                                        sendreq,                        \
                                        rc)                             \
    {                                                                   \
        ompi_proc_t *proc = ompi_comm_peer_lookup( comm, dst );         \
        ompi_free_list_item_t* item;                                    \
                                                                        \
        rc = OMPI_ERR_OUT_OF_RESOURCE;                                  \
        if( OPAL_LIKELY(NULL != proc) ) {                               \
            rc = OMPI_SUCCESS;                                          \
            OMPI_FREE_LIST_WAIT(&mca_pml_base_send_requests, item, rc); \
            sendreq = (mca_pml_ob1_send_request_t*)item;                \
            sendreq->req_send.req_base.req_proc = proc;                 \
        }                                                               \
    }


#define MCA_PML_OB1_SEND_REQUEST_INIT( sendreq,                         \
                                       buf,                             \
                                       count,                           \
                                       datatype,                        \
                                       dst,                             \
                                       tag,                             \
                                       comm,                            \
                                       sendmode,                        \
                                       persistent)                      \
    {                                                                   \
        MCA_PML_BASE_SEND_REQUEST_INIT(&sendreq->req_send,              \
                                       buf,                             \
                                       count,                           \
                                       datatype,                        \
                                       dst,                             \
                                       tag,                             \
                                       comm,                            \
                                       sendmode,                        \
                                       persistent,                      \
                                       0); /* convertor_flags */        \
        (sendreq)->req_recv.pval = NULL;                                \
    }

#define MCA_PML_OB1_SEND_REQUEST_RESET(sendreq)                         \
    if (sendreq->req_send.req_bytes_packed > 0) {                       \
        size_t _position = 0;                                           \
        opal_convertor_set_position(&sendreq->req_send.req_base.req_convertor, \
                                    &_position);                        \
        assert( 0 == _position );                                       \
    }

static inline void mca_pml_ob1_free_rdma_resources(mca_pml_ob1_send_request_t* sendreq)
{
    size_t r;

    /* return mpool resources */
    for(r = 0; r < sendreq->req_rdma_cnt; r++) {
        mca_mpool_base_registration_t* reg = sendreq->req_rdma[r].btl_reg;
        if( NULL != reg && reg->mpool != NULL ) {
            reg->mpool->mpool_deregister(reg->mpool, reg);
        }
    }
    sendreq->req_rdma_cnt = 0;
}


/**
 * Start a send request. 
 */

#define MCA_PML_OB1_SEND_REQUEST_START(sendreq, rc)       \
    do {                                                  \
        rc = mca_pml_ob1_send_request_start(sendreq);     \
    } while (0)


/*
 * Mark a send request as completed at the MPI level.
 */

#define MCA_PML_OB1_SEND_REQUEST_MPI_COMPLETE(sendreq, with_signal)                  \
do {                                                                                 \
   (sendreq)->req_send.req_base.req_ompi.req_status.MPI_SOURCE =                     \
       (sendreq)->req_send.req_base.req_comm->c_my_rank;                             \
   (sendreq)->req_send.req_base.req_ompi.req_status.MPI_TAG =                        \
        (sendreq)->req_send.req_base.req_tag;                                        \
   (sendreq)->req_send.req_base.req_ompi.req_status.MPI_ERROR = OMPI_SUCCESS;        \
   (sendreq)->req_send.req_base.req_ompi.req_status._ucount =                        \
        (sendreq)->req_send.req_bytes_packed;                                        \
   ompi_request_complete( &((sendreq)->req_send.req_base.req_ompi), (with_signal) ); \
                                                                                     \
   PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_COMPLETE,                                \
                            &(sendreq->req_send.req_base), PERUSE_SEND);             \
} while(0)

/*
 * Release resources associated with a request
 */

#define MCA_PML_OB1_SEND_REQUEST_RETURN(sendreq)                        \
    do {                                                                \
    /*  Let the base handle the reference counts */                     \
    MCA_PML_BASE_SEND_REQUEST_FINI((&(sendreq)->req_send));             \
    OMPI_FREE_LIST_RETURN( &mca_pml_base_send_requests,                 \
                           (ompi_free_list_item_t*)sendreq);            \
    } while(0)


/*
 * The PML has completed a send request. Note that this request
 * may have been orphaned by the user or have already completed
 * at the MPI level.
 * This function will never be called directly from the upper level, as it
 * should only be an internal call to the PML.
 *
 */
static inline void
send_request_pml_complete(mca_pml_ob1_send_request_t *sendreq)
{
    assert(false == sendreq->req_send.req_base.req_pml_complete);

    if(sendreq->req_send.req_bytes_packed > 0) {
        PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_XFER_END,
                                 &(sendreq->req_send.req_base), PERUSE_SEND);
    }

    /* return mpool resources */
    mca_pml_ob1_free_rdma_resources(sendreq);

    if (sendreq->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED &&
        sendreq->req_send.req_addr != sendreq->req_send.req_base.req_addr) {
        mca_pml_base_bsend_request_fini((ompi_request_t*)sendreq);
    }

    OPAL_THREAD_LOCK(&ompi_request_lock);
    if(false == sendreq->req_send.req_base.req_ompi.req_complete) {
        /* Should only be called for long messages (maybe synchronous) */
        MCA_PML_OB1_SEND_REQUEST_MPI_COMPLETE(sendreq, true);
    }
    sendreq->req_send.req_base.req_pml_complete = true;

    if(sendreq->req_send.req_base.req_free_called) {
        MCA_PML_OB1_SEND_REQUEST_RETURN(sendreq);
    }
    OPAL_THREAD_UNLOCK(&ompi_request_lock);
}

/* returns true if request was completed on PML level */
static inline bool
send_request_pml_complete_check(mca_pml_ob1_send_request_t *sendreq)
{
#if OPAL_HAVE_THREAD_SUPPORT
    opal_atomic_rmb();
#endif
    /* if no more events are expected for the request and the whole message is
     * already sent and send fragment scheduling isn't running in another
     * thread then complete the request on PML level. From now on, if user
     * called free on this request, the request structure can be reused for
     * another request or if the request is persistent it can be restarted */
    if(sendreq->req_state == 0 &&
            sendreq->req_bytes_delivered >= sendreq->req_send.req_bytes_packed
            && lock_send_request(sendreq)) {
        send_request_pml_complete(sendreq);
        return true;
    }

    return false;
}

/**
 *  Schedule additional fragments 
 */
int
mca_pml_ob1_send_request_schedule_once(mca_pml_ob1_send_request_t*);

static inline int 
mca_pml_ob1_send_request_schedule_exclusive(mca_pml_ob1_send_request_t* sendreq)
{
    int rc;
    do {
        rc = mca_pml_ob1_send_request_schedule_once(sendreq);
        if(rc == OMPI_ERR_OUT_OF_RESOURCE)
            break;
    } while(!unlock_send_request(sendreq));

    if(OMPI_SUCCESS == rc)
        send_request_pml_complete_check(sendreq);

    return rc;
}

static inline void
mca_pml_ob1_send_request_schedule(mca_pml_ob1_send_request_t* sendreq)
{
    /*
     * Only allow one thread in this routine for a given request.
     * However, we cannot block callers on a mutex, so simply keep track
     * of the number of times the routine has been called and run through
     * the scheduling logic once for every call.
     */

    if(!lock_send_request(sendreq))
        return;

    mca_pml_ob1_send_request_schedule_exclusive(sendreq);
}

/**
 *  Start the specified request
 */

int mca_pml_ob1_send_request_start_buffered(
    mca_pml_ob1_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size);

int mca_pml_ob1_send_request_start_copy(
    mca_pml_ob1_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size);

int mca_pml_ob1_send_request_start_prepare(
    mca_pml_ob1_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size);

int mca_pml_ob1_send_request_start_rdma(
    mca_pml_ob1_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size);

int mca_pml_ob1_send_request_start_rndv(
    mca_pml_ob1_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size,
    int flags);

static inline int
mca_pml_ob1_send_request_start_btl( mca_pml_ob1_send_request_t* sendreq,
                                    mca_bml_base_btl_t* bml_btl )
{
    size_t size = sendreq->req_send.req_bytes_packed;
    mca_btl_base_module_t* btl = bml_btl->btl;
    size_t eager_limit = btl->btl_eager_limit - sizeof(mca_pml_ob1_hdr_t);
    int rc;

    if( OPAL_LIKELY(size <= eager_limit) ) {
        switch(sendreq->req_send.req_send_mode) {
        case MCA_PML_BASE_SEND_SYNCHRONOUS:
            rc = mca_pml_ob1_send_request_start_rndv(sendreq, bml_btl, size, 0);
            break;
        case MCA_PML_BASE_SEND_BUFFERED:
            rc = mca_pml_ob1_send_request_start_copy(sendreq, bml_btl, size);
            break;
        case MCA_PML_BASE_SEND_COMPLETE:
            rc = mca_pml_ob1_send_request_start_prepare(sendreq, bml_btl, size);
            break;
        default:
            if (size != 0 && bml_btl->btl_flags & MCA_BTL_FLAGS_SEND_INPLACE) {
                rc = mca_pml_ob1_send_request_start_prepare(sendreq, bml_btl, size);
            } else {
                rc = mca_pml_ob1_send_request_start_copy(sendreq, bml_btl, size);
            }
            break;
        }
    } else {
        size = eager_limit;
        if(OPAL_UNLIKELY(btl->btl_rndv_eager_limit < eager_limit))
            size = btl->btl_rndv_eager_limit;
        if(sendreq->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED) {
            rc = mca_pml_ob1_send_request_start_buffered(sendreq, bml_btl, size);
        } else if
                (opal_convertor_need_buffers(&sendreq->req_send.req_base.req_convertor) == false) {
            unsigned char *base;
            opal_convertor_get_current_pointer( &sendreq->req_send.req_base.req_convertor, (void**)&base );
            
            if( 0 != (sendreq->req_rdma_cnt = (uint32_t)mca_pml_ob1_rdma_btls(
                                                                              sendreq->req_endpoint,
                                                                              base,
                                                                              sendreq->req_send.req_bytes_packed,
                                                                              sendreq->req_rdma))) {
                rc = mca_pml_ob1_send_request_start_rdma(sendreq, bml_btl,
                                                         sendreq->req_send.req_bytes_packed);
                if( OPAL_UNLIKELY(OMPI_SUCCESS != rc) ) {
                    mca_pml_ob1_free_rdma_resources(sendreq);
                }
            } else {
                rc = mca_pml_ob1_send_request_start_rndv(sendreq, bml_btl, size,
                                                         MCA_PML_OB1_HDR_FLAGS_CONTIG);
            }
        } else {
            rc = mca_pml_ob1_send_request_start_rndv(sendreq, bml_btl, size, 0);
        }
    }

    return rc;
}

static inline int
mca_pml_ob1_send_request_start( mca_pml_ob1_send_request_t* sendreq )
{   
    mca_pml_ob1_comm_t* comm = sendreq->req_send.req_base.req_comm->c_pml_comm;
    mca_bml_base_endpoint_t* endpoint = (mca_bml_base_endpoint_t*)
                                        sendreq->req_send.req_base.req_proc->proc_bml;
    size_t i;

    if( OPAL_UNLIKELY(endpoint == NULL) ) {
        return OMPI_ERR_UNREACH;
    }

    sendreq->req_endpoint = endpoint;
    sendreq->req_state = 0;
    sendreq->req_lock = 0;
    sendreq->req_pipeline_depth = 0;
    sendreq->req_bytes_delivered = 0;
    sendreq->req_pending = MCA_PML_OB1_SEND_PENDING_NONE;
    sendreq->req_send.req_base.req_sequence = OPAL_THREAD_ADD32(
        &comm->procs[sendreq->req_send.req_base.req_peer].send_sequence,1);

    MCA_PML_BASE_SEND_START( &sendreq->req_send.req_base );

    for(i = 0; i < mca_bml_base_btl_array_get_size(&endpoint->btl_eager); i++) {
        mca_bml_base_btl_t* bml_btl;
        int rc;

        /* select a btl */
        bml_btl = mca_bml_base_btl_array_get_next(&endpoint->btl_eager);
        rc = mca_pml_ob1_send_request_start_btl(sendreq, bml_btl);
        if( OPAL_LIKELY(OMPI_ERR_OUT_OF_RESOURCE != rc) )
            return rc;
    }
    add_request_to_send_pending(sendreq, MCA_PML_OB1_SEND_PENDING_START, true);

    return OMPI_SUCCESS;
}

/**
 *  Initiate a put scheduled by the receiver.
 */

void mca_pml_ob1_send_request_put( mca_pml_ob1_send_request_t* sendreq,
                                   mca_btl_base_module_t* btl,
                                   mca_pml_ob1_rdma_hdr_t* hdr );

int mca_pml_ob1_send_request_put_frag(mca_pml_ob1_rdma_frag_t* frag);

/* This function tries to continue sendreq that was stuck because of resource
 * unavailability. A sendreq may be added to send_pending list if there is no
 * resource to send initial packet or there is not resource to schedule data
 * for sending. The reason the sendreq was added to the list is stored inside
 * sendreq struct and appropriate operation is retried when resource became
 * available. bml_btl passed to the function doesn't represents sendreq
 * destination, it represents BTL on which resource was freed, so only this BTL
 * should be considered for sending packets */
void mca_pml_ob1_send_request_process_pending(mca_bml_base_btl_t *bml_btl);

void mca_pml_ob1_send_request_copy_in_out(mca_pml_ob1_send_request_t *sendreq,
                uint64_t send_offset, uint64_t send_length);

END_C_DECLS

#endif  /* OMPI_PML_OB1_SEND_REQUEST_H */
