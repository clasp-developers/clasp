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

#include "ompi/communicator/communicator.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/mca/mtl/base/base.h"
#include "ompi/mca/mtl/base/mtl_base_datatype.h"

#include "mtl_portals.h"
#include "mtl_portals_request.h"
#include "mtl_portals_endpoint.h"
#include "mtl_portals_send_short.h"

/* send event callback functions */

/* called when no ack is necessary */
static int
ompi_mtl_portals_medium_callback(ptl_event_t *ev, ompi_mtl_portals_request_t *ptl_request)
{

    switch (ev->type) {

	case PTL_EVENT_SEND_END:

	    if (ptl_request->free_after) {
		free(ev->md.start);
	    }
	    
	    OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
				 "send complete: 0x%016llx\n",
				 ev->match_bits));
	    
	    ptl_request->is_complete = true;
            if ( NULL != ptl_request->super.ompi_req ) {
		ptl_request->super.ompi_req->req_status.MPI_ERROR = OMPI_SUCCESS;
		ptl_request->super.completion_callback(&ptl_request->super);
	    }

	    
	    break;

	default:
	    opal_output(fileno(stderr)," Unexpected event type %d in ompi_mtl_portals_medium_callback()\n",ev->type); 
	    /* abort(); */
    }

    return OMPI_SUCCESS;
}

/* called when send should wait for an ack or get */
static int
ompi_mtl_portals_long_callback(ptl_event_t *ev, struct ompi_mtl_portals_request_t* ptl_request)
{

    switch (ev->type) {

	case PTL_EVENT_SEND_END:
	case PTL_EVENT_ACK:
	case PTL_EVENT_GET_END:
        /* we only receive an ack if the message was received into an
           expected message.  Otherwise, we don't get an ack, but mark
           completion when the message was pulled (long message). */
	    if ( ++(ptl_request->event_count) == 2 ) {

		if (ptl_request->free_after) {
		    free(ev->md.start);
		}

		OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
				     "send complete: 0x%016llx\n", 
				     ev->match_bits));

		ptl_request->is_complete = true;
		if ( NULL != ptl_request->super.ompi_req ) {
		    ptl_request->super.ompi_req->req_status.MPI_ERROR = OMPI_SUCCESS;
		    ptl_request->super.completion_callback(&ptl_request->super);
		}
	    }

	    break;

	default:
	    opal_output(fileno(stderr)," Unexpected event type %d in ompi_mtl_portals_long_callback()\n",ev->type); 
	    abort();
    }
    
    return OMPI_SUCCESS;
}

/* called for a rendezvous long send */ 
static int
ompi_mtl_portals_long_rendezvous_callback(ptl_event_t *ev, struct ompi_mtl_portals_request_t* ptl_request)
{

    switch (ev->type) {

	case PTL_EVENT_GET_END:

		if (ptl_request->free_after) {
		    free(ev->md.start);
		}

		OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
				     "send complete: 0x%016llx\n", 
				     ev->match_bits));

		ptl_request->is_complete = true;
		if ( NULL != ptl_request->super.ompi_req ) {
		    ptl_request->super.ompi_req->req_status.MPI_ERROR = OMPI_SUCCESS;
		    ptl_request->super.completion_callback(&ptl_request->super);
		}

	    break;

	default:
	    opal_output(fileno(stderr)," Unexpected event type %d in ompi_mtl_portals_long_callback()\n",ev->type); 
	    abort();
    }
    
    return OMPI_SUCCESS;
}

/* called when sync send should wait for an ack or put */
static int
ompi_mtl_portals_sync_callback(ptl_event_t *ev, struct ompi_mtl_portals_request_t* ptl_request)
{

    switch (ev->type) {

	case PTL_EVENT_SEND_END:
	case PTL_EVENT_ACK:
	case PTL_EVENT_PUT_END:
        /* we only receive an ack if the message was received into an
           expected message.  Otherwise, we don't get an ack, but mark
           completion when a zero-length put arrrives. */
	    if ( ++(ptl_request->event_count) == 2 ) {

		if (ptl_request->free_after) {
		    free(ev->md.start);
		}

		OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
				     "send complete: 0x%016llx\n", 
				     ev->match_bits));

		ptl_request->is_complete = true;
		if ( NULL != ptl_request->super.ompi_req ) {
		    ptl_request->super.ompi_req->req_status.MPI_ERROR = OMPI_SUCCESS;
		    ptl_request->super.completion_callback(&ptl_request->super);
		}
	    }

	    break;

	default:
	    opal_output(fileno(stderr)," Unexpected event type %d in ompi_mtl_portals_sync_callback()\n",ev->type); 
	    abort();
    }
    
    return OMPI_SUCCESS;
}

/* internal send functions */

static int
ompi_mtl_portals_zero_send(mca_pml_base_send_mode_t mode, int contextid, int localrank,
			   int tag, ptl_process_id_t dest)
{
    int ret;
    ptl_match_bits_t match_bits;
    ptl_match_bits_t mode_bits;

    mode_bits = (MCA_PML_BASE_SEND_READY != mode) ? PTL_SHORT_MSG : PTL_READY_MSG;

    PTL_SET_SEND_BITS(match_bits, contextid, localrank, tag, mode_bits); 

    ret = PtlPut(ompi_mtl_portals.ptl_zero_md_h,
		 PTL_NO_ACK_REQ,
		 dest,
		 OMPI_MTL_PORTALS_SEND_TABLE_ID,
		 0,
		 match_bits,
		 0,
		 0 );
    if (PTL_OK != ret) {
	return ompi_common_portals_error_ptl_to_ompi(ret);
    }

    return OMPI_SUCCESS;

}

static int
ompi_mtl_portals_short_send(mca_pml_base_send_mode_t mode, void *start, int length,
			    int contextid, int localrank, int tag, ptl_process_id_t dest)
{
    int ret;
    ptl_match_bits_t match_bits;
    ptl_size_t offset;
    void *copyblock;
    ptl_match_bits_t mode_bits;

    mode_bits = (MCA_PML_BASE_SEND_READY != mode) ? PTL_SHORT_MSG : PTL_READY_MSG;

    PTL_SET_SEND_BITS(match_bits, contextid, localrank, tag, mode_bits); 

    offset = ompi_mtl_portals_alloc_short_buf() * ompi_mtl_portals.ptl_copy_block_len;

    copyblock = (char *)ompi_mtl_portals.ptl_short_md.start + offset;
	
    memcpy(copyblock, start, length);

    ret = PtlPutRegion(ompi_mtl_portals.ptl_short_md_h,
		       offset,
		       length,
		       PTL_NO_ACK_REQ,
		       dest,
		       OMPI_MTL_PORTALS_SEND_TABLE_ID,
		       0,
		       match_bits,
		       0,
		       0);
    if (PTL_OK != ret ) {
	return ompi_common_portals_error_ptl_to_ompi(ret);
    }

    return OMPI_SUCCESS;
	
}

static int
ompi_mtl_portals_medium_isend( mca_pml_base_send_mode_t mode, void *start, int length,
			       int contextid, int localrank, int tag, ptl_process_id_t dest,
			       ompi_mtl_portals_request_t *ptl_request )
{
    int ret;
    ptl_match_bits_t match_bits;
    ptl_md_t md;
    ptl_handle_md_t md_h;
    ptl_match_bits_t mode_bits;

    mode_bits = (MCA_PML_BASE_SEND_READY != mode) ? PTL_SHORT_MSG : PTL_READY_MSG;

    PTL_SET_SEND_BITS(match_bits, contextid, localrank, tag, mode_bits); 
    
    md.start = start;
    md.length = length;
    md.threshold = 1; /* send end */
    md.options = PTL_MD_EVENT_START_DISABLE;
    md.user_ptr = ptl_request;
    md.eq_handle = ompi_mtl_portals.ptl_eq_h;
    
    ret = PtlMDBind(ompi_mtl_portals.ptl_ni_h,
		    md,
		    PTL_UNLINK,
		    &md_h);
    if (PTL_OK != ret) {
	if (ptl_request->free_after) free(start);
	return ompi_common_portals_error_ptl_to_ompi(ret);
    }
    
    ret = PtlPut(md_h,
		 PTL_NO_ACK_REQ,
		 dest,
		 OMPI_MTL_PORTALS_SEND_TABLE_ID,
		 0,
		 match_bits,
		 0,
		 0);
    if (PTL_OK != ret) {
	PtlMDUnlink(md_h);
	if (ptl_request->free_after) free(start);
	return ompi_common_portals_error_ptl_to_ompi(ret);
    }

    ptl_request->is_complete = false;
    ptl_request->event_callback = ompi_mtl_portals_medium_callback;
    ptl_request->event_count = 0;
    
    return OMPI_SUCCESS;
    
}

static int
ompi_mtl_portals_long_isend( void *start, int length, int contextid, int localrank, int tag,
			     ptl_process_id_t dest, ompi_mtl_portals_request_t *ptl_request )
{
    int ret;
    ptl_match_bits_t match_bits;
    ptl_md_t md;
    ptl_handle_md_t md_h;
    ptl_handle_me_t me_h;

    PTL_SET_SEND_BITS(match_bits, contextid, localrank, tag, PTL_LONG_MSG);

    md.start = start;
    md.length = length;

    if (ompi_mtl_portals.ptl_use_rendezvous == true) {
        md.threshold = 1; /* get event */
    } else {
        md.threshold = 2; /* sent event, ack or get event */
    }

    md.options = PTL_MD_OP_GET | PTL_MD_EVENT_START_DISABLE;
    md.user_ptr = ptl_request;
    md.eq_handle = ompi_mtl_portals.ptl_eq_h;

    ret = PtlMEMDPost(ompi_mtl_portals.ptl_ni_h,
		      ompi_mtl_portals.ptl_read_catchall_me_h,
		      dest,
		      (ptl_match_bits_t)(uintptr_t)ptl_request,
		      0,
		      PTL_UNLINK,
		      PTL_INS_BEFORE,
		      md,
		      PTL_UNLINK,
		      &me_h,
		      &md_h,
		      ompi_mtl_portals.ptl_empty_eq_h);
    if (PTL_OK != ret) {
	if (ptl_request->free_after) free(start);
	return ompi_common_portals_error_ptl_to_ompi(ret);
    }

    if (ompi_mtl_portals.ptl_use_rendezvous == false) {

        ret = PtlPut(md_h,
		     PTL_ACK_REQ,
		     dest,
		     OMPI_MTL_PORTALS_SEND_TABLE_ID,
		     0,
		     match_bits,
		     0,
		     (ptl_hdr_data_t)(uintptr_t)ptl_request);

	ptl_request->event_callback = ompi_mtl_portals_long_callback;

    } else {

        /* just send a zero-length message */
	ret = PtlPut(ompi_mtl_portals.ptl_zero_md_h,
		     PTL_NO_ACK_REQ,
		     dest,
		     OMPI_MTL_PORTALS_SEND_TABLE_ID,
                     0,
                     match_bits,
                     0,
                     (ptl_hdr_data_t)(uintptr_t)ptl_request);

	ptl_request->event_callback = ompi_mtl_portals_long_rendezvous_callback;

    }

    if (PTL_OK != ret) {
	PtlMEUnlink(me_h);
	if (ptl_request->free_after) free(start);
	return ompi_common_portals_error_ptl_to_ompi(ret);
    }

    ptl_request->is_complete = false;
    ptl_request->event_count = 0;

    return OMPI_SUCCESS;
}

static int
ompi_mtl_portals_sync_isend( void *start, int length, int contextid, int localrank, int tag,
			     ptl_process_id_t dest, ompi_mtl_portals_request_t *ptl_request )
{
    int ret;
    ptl_match_bits_t match_bits;
    ptl_md_t md;
    ptl_handle_md_t md_h;
    ptl_handle_me_t me_h;

    PTL_SET_SEND_BITS(match_bits, contextid, localrank, tag, PTL_SHORT_MSG);

    md.start = start;
    md.length = length;
    md.threshold = 2; /* send, {ack, get} */
    md.options = PTL_MD_OP_PUT | PTL_MD_EVENT_START_DISABLE;
    md.user_ptr = ptl_request;
    md.eq_handle = ompi_mtl_portals.ptl_eq_h;

    ret = PtlMEMDPost(ompi_mtl_portals.ptl_ni_h,
		      ompi_mtl_portals.ptl_ack_catchall_me_h,
		      dest,
		      (ptl_match_bits_t)(uintptr_t)ptl_request,
		      0,
		      PTL_UNLINK,
		      PTL_INS_BEFORE,
		      md,
		      PTL_UNLINK,
		      &me_h,
		      &md_h,
		      ompi_mtl_portals.ptl_empty_eq_h);
    if (PTL_OK != ret) {
	if (ptl_request->free_after) free(start);
	return ompi_common_portals_error_ptl_to_ompi(ret);
    }

    ret = PtlPut(md_h,
		 PTL_ACK_REQ,
		 dest,
		 OMPI_MTL_PORTALS_SEND_TABLE_ID,
		 0,
		 match_bits,
		 0,
		 (ptl_hdr_data_t)(uintptr_t)ptl_request);
    if (PTL_OK != ret) {
	PtlMEUnlink(me_h);
	if (ptl_request->free_after) free(start);
	return ompi_common_portals_error_ptl_to_ompi(ret);
    }

    ptl_request->is_complete = false;
    ptl_request->event_callback = ompi_mtl_portals_sync_callback;
    ptl_request->event_count = 0;

    return OMPI_SUCCESS;
    
}

/* We use a macro for this since the send code is identical for blocking and non-blocking
   sends, but we want both to be as fast as possible. We define it here since it doesn't
   get used anywhere else. */
#define PTL_SEND_CODE									\
{											\
    switch (mode) {									\
	case MCA_PML_BASE_SEND_STANDARD:						\
	case MCA_PML_BASE_SEND_READY:							\
	case MCA_PML_BASE_SEND_BUFFERED:					     	\
											\
	    if (0 == length) {								\
											\
		ret =  ompi_mtl_portals_zero_send(mode,					\
						  comm->c_contextid,			\
						  comm->c_my_rank,			\
						  tag,					\
						  endpoint->ptl_proc);			\
		if (OMPI_SUCCESS != ret) return ret;			        	\
											\
                ptl_request->is_complete = true;                                        \
											\
                break;                                                                  \
											\
	    } else if (length <= ompi_mtl_portals.ptl_copy_block_len) {			\
											\
		ret =  ompi_mtl_portals_short_send(mode,				\
						   start,				\
						   length,				\
						   comm->c_contextid,			\
						   comm->c_my_rank,			\
						   tag,					\
						   endpoint->ptl_proc);			\
		if (OMPI_SUCCESS != ret) return ret;			        	\
											\
                ptl_request->is_complete = true;                                        \
											\
                break;                                                                  \
											\
	    } else if ((length <= ompi_mtl_portals.eager_limit) ||			\
		       (MCA_PML_BASE_SEND_READY == mode)) {				\
											\
		ret = ompi_mtl_portals_medium_isend(mode,				\
						    start,				\
						    length,				\
						    comm->c_contextid,			\
						    comm->c_my_rank,			\
						    tag,				\
						    endpoint->ptl_proc,			\
						    ptl_request );			\
		if (OMPI_SUCCESS != ret) return ret;			        	\
											\
		break;									\
											\
	    }										\
											\
	    /* long standard send case falls through */					\
											\
	case MCA_PML_BASE_SEND_SYNCHRONOUS:						\
											\
	    if (length <= ompi_mtl_portals.eager_limit) {				\
											\
		ret = ompi_mtl_portals_sync_isend(start,				\
						  length,				\
						  comm->c_contextid,			\
						  comm->c_my_rank,			\
						  tag,					\
						  endpoint->ptl_proc,			\
						  ptl_request );			\
		if (OMPI_SUCCESS != ret) return ret;					\
											\
	    } else {									\
	    /* if we got this far, we're either a standard or synchronous long send */	\
		ret = ompi_mtl_portals_long_isend(start,				\
						  length,				\
						  comm->c_contextid,			\
						  comm->c_my_rank,			\
						  tag,					\
						  endpoint->ptl_proc,			\
						  ptl_request );			\
		if (OMPI_SUCCESS != ret) return ret;					\
											\
	    }										\
											\
	    break;									\
											\
	default:									\
	    opal_output(fileno(stderr),"Unexpected msg type %d\n", mode);                        \
            abort();                                                                    \
											\
    }											\
}

/* external send functions */
int
ompi_mtl_portals_send(struct mca_mtl_base_module_t* mtl,
                      struct ompi_communicator_t* comm,
                      int dest,
                      int tag,
                      struct opal_convertor_t *convertor,
                      mca_pml_base_send_mode_t mode)
{
    int ret;
    ompi_proc_t *ompi_proc = ompi_comm_peer_lookup( comm, dest );
    mca_mtl_base_endpoint_t *endpoint = (mca_mtl_base_endpoint_t*) ompi_proc->proc_pml;
    void *start;
    size_t length;
    ompi_mtl_portals_request_t mtl_request;
    ompi_mtl_portals_request_t *ptl_request = (ompi_mtl_portals_request_t*)&mtl_request;

    assert(mtl == &ompi_mtl_portals.base);

    ret = ompi_mtl_datatype_pack(convertor, &start, &length,
				 &(ptl_request->free_after));
    if (OMPI_SUCCESS != ret) return ret;

    ptl_request->is_complete = false;
    ptl_request->super.ompi_req = NULL;

    PTL_SEND_CODE;

    /* wait for send to complete */
    while (ptl_request->is_complete == false) {
	ompi_mtl_portals_progress();
    }

    return OMPI_SUCCESS;
    
}

int
ompi_mtl_portals_isend(struct mca_mtl_base_module_t* mtl,
                       struct ompi_communicator_t* comm,
                       int dest,
                       int tag,
                       struct opal_convertor_t *convertor,
                       mca_pml_base_send_mode_t mode,
                       bool blocking,
                       mca_mtl_request_t *mtl_request)
{
    int ret;
    ompi_proc_t *ompi_proc = ompi_comm_peer_lookup( comm, dest );
    mca_mtl_base_endpoint_t *endpoint = (mca_mtl_base_endpoint_t*)ompi_proc->proc_pml;
    ompi_mtl_portals_request_t *ptl_request = (ompi_mtl_portals_request_t*)mtl_request;
    void *start;
    size_t length;

    assert(mtl == &ompi_mtl_portals.base);

    ret = ompi_mtl_datatype_pack(convertor, &start, &length,
				 &(ptl_request->free_after));
    if (OMPI_SUCCESS != ret) return ret;

    ptl_request->is_complete = false;

    PTL_SEND_CODE;

    if (ptl_request->is_complete == true ) {
        ptl_request->super.ompi_req->req_status.MPI_ERROR = OMPI_SUCCESS;
        ptl_request->super.completion_callback(&ptl_request->super);
    }

    return OMPI_SUCCESS;

}

