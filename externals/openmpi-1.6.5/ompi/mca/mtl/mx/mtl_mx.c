/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2010 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/prefetch.h"

#include "ompi/mca/mtl/mtl.h"
#include "ompi/runtime/ompi_module_exchange.h"
#include "ompi/mca/mtl/base/mtl_base_datatype.h"
#include "ompi/mca/common/mx/common_mx.h"
#include "mtl_mx.h"
#include "mtl_mx_types.h"
#include "mtl_mx_endpoint.h"
#include "mtl_mx_request.h"

static int
ompi_mtl_mx_add_procs(struct mca_mtl_base_module_t *mtl,
                      size_t nprocs,
                      struct ompi_proc_t** procs, 
                      struct mca_mtl_base_endpoint_t **mtl_peer_data);
static int
ompi_mtl_mx_del_procs(struct mca_mtl_base_module_t *mtl,
                      size_t nprocs,
                      struct ompi_proc_t** procs, 
                      struct mca_mtl_base_endpoint_t **mtl_peer_data);

mca_mtl_mx_module_t ompi_mtl_mx = {
    {
        8191,        /* max cid - 2^13 - 1 */
        (1UL << 30), /* max tag value - must allow negatives */
        0,           /* request reserve space */
        0,           /* flags */
        

        ompi_mtl_mx_add_procs,
        ompi_mtl_mx_del_procs,
        ompi_mtl_mx_finalize,
        
        ompi_mtl_mx_send,
        ompi_mtl_mx_isend,
        
        ompi_mtl_mx_irecv,
        ompi_mtl_mx_iprobe,
        
        ompi_mtl_mx_cancel,
        NULL,
        NULL
    }    
};

int ompi_mtl_mx_progress( void ); 

int ompi_mtl_mx_module_init(){ 
    mx_param_t mx_param;
    mx_return_t mx_return;
    int32_t nic, ep;
    
    /* setup params */
    mx_param.key = MX_PARAM_UNEXP_QUEUE_MAX;
    mx_param.val.unexp_queue_max = ompi_mtl_mx.mx_unexp_queue_max;
    
    /* get a local endpoint */
    nic = ompi_mtl_mx.mx_board_num;
    if (nic < 0) {
      nic = MX_ANY_NIC;
    }
    ep = ompi_mtl_mx.mx_endpoint_num;
    if (ep < 0) {
      ep = MX_ANY_ENDPOINT;
    }
    mx_return = mx_open_endpoint(nic,
                                 ep,
                                 ompi_mtl_mx.mx_filter, 
                                 NULL, 
                                 0,
                                 &ompi_mtl_mx.mx_endpoint);
    
    if(mx_return != MX_SUCCESS) { 
        opal_output(ompi_mtl_base_output, "Error in mx_open_endpoint (error %s)\n", mx_strerror(mx_return));
        return OMPI_ERROR;
    }
    
    /* get the endpoint address */
    mx_return = mx_get_endpoint_addr( ompi_mtl_mx.mx_endpoint, 
                                      &ompi_mtl_mx.mx_endpoint_addr); 
    
    if(mx_return != MX_SUCCESS) { 
        opal_output(ompi_mtl_base_output, "Error in mx_get_endpoint_addr (error %s)\n", mx_strerror(mx_return));
        return OMPI_ERROR;
    }
    
    mx_return = mx_decompose_endpoint_addr( ompi_mtl_mx.mx_endpoint_addr, &(ompi_mtl_mx.mx_addr.nic_id),
                                            &(ompi_mtl_mx.mx_addr.endpoint_id) );
    
    if(mx_return != MX_SUCCESS) { 
        opal_output(ompi_mtl_base_output, "Error in mx_decompose_endpoint_addr (error %s)\n", mx_strerror(mx_return));
        return OMPI_ERROR;
    }
    opal_output_verbose(10, ompi_mtl_base_output, 
			"mtl:mx: local nic %d, endpoint %d, got nic %d, ep %d\n", nic, ep, 
			ompi_mtl_mx.mx_addr.nic_id,
			ompi_mtl_mx.mx_addr.endpoint_id);

    ompi_modex_send( &mca_mtl_mx_component.super.mtl_version, 
                             &ompi_mtl_mx.mx_addr, 
                             sizeof(mca_mtl_mx_addr_t));
    
    /* register the mtl mx progress function */
    opal_progress_register(ompi_mtl_mx_progress);
    
    return OMPI_SUCCESS; 
}

int
ompi_mtl_mx_finalize(struct mca_mtl_base_module_t* mtl) { 
    mx_return_t mx_return;
    
    opal_progress_unregister(ompi_mtl_mx_progress);
    
    /* free resources */
    mx_return = mx_close_endpoint(ompi_mtl_mx.mx_endpoint);
    if(mx_return != MX_SUCCESS){ 
        opal_output(ompi_mtl_base_output, "Error in mx_close_endpoint (error %s)\n", mx_strerror(mx_return));
        return OMPI_ERROR;
    }
    
    return OMPI_SUCCESS;
    
}

int
ompi_mtl_mx_add_procs(struct mca_mtl_base_module_t *mtl,
                      size_t nprocs,
                      struct ompi_proc_t** procs, 
                      struct mca_mtl_base_endpoint_t **mtl_peer_data)
{
    int i; 
    assert(mtl == &ompi_mtl_mx.super);
            
    for( i = 0; i < (int) nprocs; i++ ){ 
        mca_mtl_mx_endpoint_t* mtl_mx_endpoint = 
            mca_mtl_mx_endpoint_create(procs[i]); 
        if(NULL == mtl_mx_endpoint) { 
            return OMPI_ERROR; 
        }
        mtl_peer_data[i] =  (struct mca_mtl_base_endpoint_t*) 
            mtl_mx_endpoint;
    }

    /* because mx_connect isn't an interupting function, need to
       progress MX as often as possible during the stage gate 2.  This
       would have happened after the stage gate anyway, so we're just
       speeding things up a bit. */
#if OPAL_ENABLE_PROGRESS_THREADS == 0
    /* switch from letting us sit in the event library for a bit each
       time through opal_progress() to completely non-blocking */
    opal_progress_set_event_flag(OPAL_EVLOOP_NONBLOCK);
#endif
    
    return OMPI_SUCCESS;
}


int
ompi_mtl_mx_del_procs(struct mca_mtl_base_module_t *mtl,
                      size_t nprocs,
                      struct ompi_proc_t** procs, 
                      struct mca_mtl_base_endpoint_t **mtl_peer_data)
{
    return OMPI_SUCCESS;
}



int ompi_mtl_mx_progress( void ) { 
    mx_return_t mx_return;
    mx_request_t mx_request;
    mx_status_t mx_status;
    uint32_t result;
    mca_mtl_mx_request_t* mtl_mx_request;
    int completed = 0;
    
    while(1){
        mx_return = mx_ipeek(ompi_mtl_mx.mx_endpoint, 
                             &mx_request, 
                             &result);
        
        if( OPAL_UNLIKELY(mx_return != MX_SUCCESS) ) { 
            opal_output(ompi_mtl_base_output, "Error in mx_ipeek (error %s)\n", mx_strerror(mx_return));
        }
        if(result) { 
            completed++;
            mx_return = mx_test(ompi_mtl_mx.mx_endpoint, 
                                &mx_request, 
                                &mx_status,
                                &result);
            if( OPAL_UNLIKELY(mx_return != MX_SUCCESS) ) { 
                opal_output(ompi_mtl_base_output, "Error in mx_test (error %s)\n", mx_strerror(mx_return));
                abort();
            }
            if( OPAL_UNLIKELY(0 == result) ) { 
                opal_output(ompi_mtl_base_output, "Error in ompi_mtl_mx_progress, mx_ipeek returned a request, mx_test on the request resulted failure.\n");
                abort();
            }
            mtl_mx_request = (mca_mtl_mx_request_t*) mx_status.context;
            if(OMPI_MTL_MX_ISEND == mtl_mx_request->type) { 
                if(mtl_mx_request->free_after) { 
                    free(mtl_mx_request->mx_segment[0].segment_ptr);
                }
            } else {
                assert( OMPI_MTL_MX_IRECV == mtl_mx_request->type );
                
                ompi_mtl_datatype_unpack(mtl_mx_request->convertor, 
                                         mtl_mx_request->mx_segment[0].segment_ptr, 
                                         mx_status.xfer_length);
                /* set the status */
                MX_GET_SRC(mx_status.match_info,
                           mtl_mx_request->super.ompi_req->req_status.MPI_SOURCE);
                MX_GET_TAG(mx_status.match_info,
                           mtl_mx_request->super.ompi_req->req_status.MPI_TAG); 
                mtl_mx_request->super.ompi_req->req_status._ucount = 
                    mx_status.xfer_length;
            }
            /* suppose everything went just fine ... */
            mtl_mx_request->super.ompi_req->req_status.MPI_ERROR = OMPI_SUCCESS;
            if( OPAL_UNLIKELY(MX_STATUS_SUCCESS != mx_status.code) ) {
                if( MX_STATUS_TRUNCATED == mx_status.code ) {
                    mtl_mx_request->super.ompi_req->req_status.MPI_ERROR = MPI_ERR_TRUNCATE;
                } else {
                    mtl_mx_request->super.ompi_req->req_status.MPI_ERROR = MPI_ERR_INTERN;
                }
                return completed;
            }
            mtl_mx_request->super.completion_callback(&mtl_mx_request->super);
            return completed;
        } else { 
            return completed;
        }
    }
}

