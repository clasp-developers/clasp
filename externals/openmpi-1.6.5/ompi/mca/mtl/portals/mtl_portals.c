/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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

#include "ompi_config.h"

#include "ompi/mca/mtl/mtl.h"
#include "opal/class/opal_list.h"

#include "mtl_portals.h"
#include "mtl_portals_endpoint.h"
#include "mtl_portals_request.h"
#include "mtl_portals_recv.h"
#include "mtl_portals_recv_short.h"
#include "mtl_portals_send_short.h"

static ptl_handle_md_t send_catchall_md_h;
static ptl_handle_md_t ack_catchall_md_h;
static ptl_handle_md_t read_catchall_md_h;
static ptl_handle_md_t unex_long_md_h;

static ompi_mtl_portals_request_t catchall_send_request;
static ompi_mtl_portals_request_t catchall_ack_request;
static ompi_mtl_portals_request_t catchall_read_request;

mca_mtl_portals_module_t ompi_mtl_portals = {
    {
        8191,        /* max cid - 2^13 - 1 */
        (1UL << 30), /* max tag value - must allow negatives */
        0,           /* request reserve space */
        0,           /* flags */

        ompi_mtl_portals_add_procs,
        ompi_mtl_portals_del_procs,
        ompi_mtl_portals_finalize,

        ompi_mtl_portals_send,
        ompi_mtl_portals_isend,
        ompi_mtl_portals_irecv,
        ompi_mtl_portals_iprobe,

        NULL,       /* cancel */
        NULL,       /* add_comm */
        NULL        /* del_comm */
    }
};


/* BWB - fix me - this should be an ompi_free_list_item_t */
OBJ_CLASS_INSTANCE(ompi_mtl_portals_event_t, opal_list_item_t,
                   NULL, NULL);


/* catchall callback */
static int
ompi_mtl_portals_catchall_callback(ptl_event_t *ev, 
                                   ompi_mtl_portals_request_t *ptl_request)
{
    if (ptl_request == &catchall_send_request) {
        opal_output(fileno(stderr), "ERROR - received catchall event on send queue"); 
    } else if (ptl_request == &catchall_ack_request) {
        opal_output(fileno(stderr), "ERROR - received catchall event on ack queue");  
    } else if (ptl_request == &catchall_read_request) {
        opal_output(fileno(stderr), "ERROR - received catchall event on read queue");  
    } else {
        opal_output(fileno(stderr), "ERROR - received catchall event of unknown origin");  
    }

    abort();
    return OMPI_ERROR;
} 


int
ompi_mtl_portals_add_procs(struct mca_mtl_base_module_t *mtl,
                           size_t nprocs,
                           struct ompi_proc_t** procs, 
                           struct mca_mtl_base_endpoint_t **mtl_peer_data)
{
    int ret = OMPI_SUCCESS;
    ptl_process_id_t *portals_procs = NULL;
    ptl_md_t md;
    size_t i;
    ptl_match_bits_t match_bits;
    ptl_match_bits_t ignore_bits;
    ptl_process_id_t anyid = { PTL_NID_ANY, PTL_PID_ANY };
    bool accel;
    
    assert(mtl == &ompi_mtl_portals.base);

    /* if we haven't already initialized the network, do so now.  We
       delay until add_procs because if we want the automatic runtime
       environment setup the common code does for the utcp
       implementation, we can't do it until modex information can be
       received. */
    
    if (PTL_INVALID_HANDLE == ompi_mtl_portals.ptl_ni_h) {
        
        ret = ompi_common_portals_ni_initialize(&(ompi_mtl_portals.ptl_ni_h), &accel);
        if (OMPI_SUCCESS != ret) goto cleanup;
    }   
    
    /* event queue for expected events */
    ret = PtlEQAlloc(ompi_mtl_portals.ptl_ni_h,
                     ompi_mtl_portals.ptl_expected_queue_size,
                     PTL_EQ_HANDLER_NONE,
                     &(ompi_mtl_portals.ptl_eq_h));
    assert(ret == PTL_OK);

    /* event queue for unexpected receives */
    ret = PtlEQAlloc(ompi_mtl_portals.ptl_ni_h,
                     ompi_mtl_portals.ptl_unexpected_queue_size,
                     PTL_EQ_HANDLER_NONE,
                     &(ompi_mtl_portals.ptl_unex_eq_h));
    assert(ret == PTL_OK);

    /* empty event queue for PtlMEMDPost() */
    ret = PtlEQAlloc(ompi_mtl_portals.ptl_ni_h,
                     1,
                     PTL_EQ_HANDLER_NONE,
                     &(ompi_mtl_portals.ptl_empty_eq_h));
    assert(ret == PTL_OK);

    /* attach the long unex msg buffer */
    match_bits  = PTL_LONG_MSG;
    ignore_bits = ~(PTL_LONG_MSG);

    ret = PtlMEAttach(ompi_mtl_portals.ptl_ni_h,
                      OMPI_MTL_PORTALS_SEND_TABLE_ID,
                      anyid,
                      match_bits,
                      ignore_bits,
                      PTL_RETAIN,
                      PTL_INS_AFTER,
                      &(ompi_mtl_portals.ptl_unex_long_me_h));
    assert(ret == PTL_OK);

    md.start     = NULL;
    md.length    = 0;
    md.threshold = PTL_MD_THRESH_INF;
    md.max_size  = 0;
    md.options   = PTL_MD_OP_PUT | PTL_MD_TRUNCATE | PTL_MD_ACK_DISABLE;
    md.eq_handle = ompi_mtl_portals.ptl_unex_eq_h;
    md.user_ptr  = NULL;

    ret = PtlMDAttach(ompi_mtl_portals.ptl_unex_long_me_h,
                      md,
                      PTL_RETAIN,
                      &unex_long_md_h);
    assert(ret == PTL_OK);

    /* attach catchalls to the send, ack, and read portals */
    md.eq_handle = ompi_mtl_portals.ptl_eq_h;

    /* catchall for the send portal */
    catchall_send_request.event_callback = ompi_mtl_portals_catchall_callback;
    md.user_ptr = &catchall_send_request;
    ret = PtlMEMDPost(ompi_mtl_portals.ptl_ni_h,
                      ompi_mtl_portals.ptl_unex_long_me_h,
                      anyid,
                      0,
                      ~0,
                      PTL_RETAIN,
                      PTL_INS_AFTER,
                      md,
                      PTL_UNLINK,
                      &(ompi_mtl_portals.ptl_send_catchall_me_h),
                      &send_catchall_md_h,
                      ompi_mtl_portals.ptl_empty_eq_h);
    assert(ret == PTL_OK);

    /* catchall for ack portal */
    catchall_ack_request.event_callback = ompi_mtl_portals_catchall_callback;
    md.user_ptr = &catchall_ack_request;
    ret = PtlMEAttach(ompi_mtl_portals.ptl_ni_h,
                      OMPI_MTL_PORTALS_ACK_TABLE_ID,
                      anyid,
                      0,
                      ~0,
                      PTL_RETAIN,
                      PTL_INS_AFTER,
                      &(ompi_mtl_portals.ptl_ack_catchall_me_h));
    assert(ret == PTL_OK);

    ret = PtlMDAttach(ompi_mtl_portals.ptl_ack_catchall_me_h,
                      md,
                      PTL_UNLINK,
                      &ack_catchall_md_h);
    assert(ret == PTL_OK);

    /* catchall for read portal */
    catchall_read_request.event_callback = ompi_mtl_portals_catchall_callback;
    md.user_ptr = &catchall_read_request;
    ret = PtlMEAttach(ompi_mtl_portals.ptl_ni_h,
                      OMPI_MTL_PORTALS_READ_TABLE_ID,
                      anyid,
                      0,
                      ~0,
                      PTL_RETAIN,
                      PTL_INS_AFTER,
                      &(ompi_mtl_portals.ptl_read_catchall_me_h));
    assert(ret == PTL_OK);

    ret = PtlMDAttach(ompi_mtl_portals.ptl_read_catchall_me_h,
                      md,
                      PTL_RETAIN,
                      &read_catchall_md_h);
    assert(ret == PTL_OK);

    /* attach short unex recv blocks */
    ret = ompi_mtl_portals_recv_short_enable((mca_mtl_portals_module_t*) mtl);

    opal_progress_register(ompi_mtl_portals_progress);

    /* bind zero-length md for sending zero-length msgs and acks */
    md.start     = NULL;
    md.length    = 0;
    md.threshold = PTL_MD_THRESH_INF;
    md.max_size  = 0;
    md.options   = PTL_MD_EVENT_START_DISABLE | PTL_MD_EVENT_END_DISABLE;
    md.user_ptr  = NULL;
    md.eq_handle = PTL_EQ_NONE;

    ret = PtlMDBind(ompi_mtl_portals.ptl_ni_h,
                    md,
                    PTL_RETAIN,
                    &ompi_mtl_portals.ptl_zero_md_h ); 
    assert(ret == PTL_OK);

    /* set up the short copy blocks */
    ompi_mtl_portals_short_setup();

    

    /* get the list of ptl_process_id_t structures for the given proc
       structures.  If the Portals runtime environment supports
       comm_spawn, we'll be able to support it as well. */
    portals_procs = malloc(sizeof(ptl_process_id_t) * nprocs);
    if (NULL == portals_procs) goto cleanup;
    ret = ompi_common_portals_get_procs(nprocs, procs, portals_procs);
    if (OMPI_SUCCESS != ret) goto cleanup;

    /* copy the ptl_process_id_t information into our per-proc data
       store */
    for (i = 0 ; i < nprocs ; ++i) {
        mtl_peer_data[i] = malloc(sizeof(struct mca_mtl_base_endpoint_t));
        if (NULL == mtl_peer_data[i]) goto cleanup;
 
        mtl_peer_data[i]->ptl_proc.nid = portals_procs[i].nid;
        mtl_peer_data[i]->ptl_proc.pid = portals_procs[i].pid;
    }
    
 cleanup:
    if (NULL != portals_procs) free(portals_procs);
    return ret;
}


int
ompi_mtl_portals_del_procs(struct mca_mtl_base_module_t *mtl,
                           size_t nprocs,
                           struct ompi_proc_t** procs, 
                           struct mca_mtl_base_endpoint_t **mtl_peer_data)
{
    size_t i;

    assert(mtl == &ompi_mtl_portals.base);

    for (i = 0 ; i < nprocs ; ++i) {
        if (NULL != mtl_peer_data[i]) {
            free(mtl_peer_data[i]);
        }
    }

    ompi_mtl_portals_recv_short_disable((mca_mtl_portals_module_t *) mtl);

    ompi_mtl_portals_short_cleanup();

    (void)PtlMDUnlink(ompi_mtl_portals.ptl_zero_md_h);

    (void)PtlMDUnlink(send_catchall_md_h);

    (void)PtlMDUnlink(ack_catchall_md_h);

    (void)PtlMDUnlink(read_catchall_md_h);

    (void)PtlMDUnlink(unex_long_md_h);

    (void)PtlMEUnlink(ompi_mtl_portals.ptl_unex_long_me_h);
	
    (void)PtlMEUnlink(ompi_mtl_portals.ptl_send_catchall_me_h);

    (void)PtlMEUnlink(ompi_mtl_portals.ptl_ack_catchall_me_h);

    (void)PtlMEUnlink(ompi_mtl_portals.ptl_read_catchall_me_h);

    return OMPI_SUCCESS;
}


int
ompi_mtl_portals_finalize(struct mca_mtl_base_module_t *mtl)
{
    assert(mtl == &ompi_mtl_portals.base);

    /* Don't try to wait for things to finish if we've never initialized */
    if (PTL_INVALID_HANDLE != ompi_mtl_portals.ptl_ni_h) {
        ptl_event_t ev;
        int ret;

        opal_progress_unregister(ompi_mtl_portals_progress);

        /* Before progressing remaining events, check whether we don't get PTL_EQ_INVALID */
        ret = PtlEQPeek(ompi_mtl_portals.ptl_eq_h, &ev);

        if (PTL_EQ_INVALID != ret) {
            while (0 != ompi_mtl_portals_progress()) { }
        }
    }

    ompi_common_portals_ni_finalize();
    ompi_common_portals_finalize();

    return OMPI_SUCCESS;
}


int
ompi_mtl_portals_progress(void)
{
    int count = 0, ret;
    ptl_event_t ev;
    ompi_mtl_portals_request_t *ptl_request;

    while (true) {

	ret = PtlEQGet(ompi_mtl_portals.ptl_eq_h, &ev);

        if (PTL_OK == ret) {
            if (ev.type == PTL_EVENT_UNLINK) continue;

            if (NULL != ev.md.user_ptr) {
                ptl_request = ev.md.user_ptr;
                ret = ptl_request->event_callback(&ev, ptl_request);

                if (OMPI_SUCCESS != ret) {
                    opal_output(0, " Error returned from the event callback.  Error code - %d \n",ret);
                    abort();
                }
            }
        } else if (PTL_EQ_EMPTY == ret) {
            break;
        } else {
            opal_output(0, " Error returned from PtlEQGet.  Error code - %d \n",ret);
            abort();
        }
    }

    /* clean out the unexpected event queue too */
    if (ompi_mtl_portals.ptl_aggressive_polling == true) {
	while ( NULL != ompi_mtl_portals_search_unex_events(0, ~0, true) );
    }

    return count;
}
