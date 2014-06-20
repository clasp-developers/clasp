/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/prefetch.h"

#include "ompi/communicator/communicator.h"
#include "opal/datatype/opal_convertor.h"

#include "mtl_mx.h"
#include "mtl_mx_types.h"
#include "mtl_mx_request.h"
#include "ompi/mca/mtl/base/mtl_base_datatype.h"

int
ompi_mtl_mx_send(struct mca_mtl_base_module_t* mtl, 
                 struct ompi_communicator_t* comm,
                 int dest,
                 int tag,
                 struct opal_convertor_t *convertor,
                 mca_pml_base_send_mode_t mode)
{
    mx_return_t mx_return;
    uint64_t match_bits;
    mca_mtl_mx_request_t mtl_mx_request;
    size_t length;
    mx_status_t mx_status;
    uint32_t result;
    ompi_proc_t* ompi_proc = ompi_comm_peer_lookup( comm, dest );
    mca_mtl_mx_endpoint_t* mx_endpoint = (mca_mtl_mx_endpoint_t*) ompi_proc->proc_pml;
    char* where;

    assert(mtl == &ompi_mtl_mx.super);

    MX_SET_SEND_BITS(match_bits, comm->c_contextid, comm->c_my_rank, tag); 
    
    ompi_mtl_datatype_pack(convertor, 
                           &mtl_mx_request.mx_segment[0].segment_ptr, 
                           &length,
                           &mtl_mx_request.free_after);

    mtl_mx_request.mx_segment[0].segment_length = length;
    mtl_mx_request.convertor = convertor;
    mtl_mx_request.type = OMPI_MTL_MX_ISEND;

    OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
                         "issend bits: 0x%016" PRIu64 "\n", 
                         match_bits));

    if(mode == MCA_PML_BASE_SEND_SYNCHRONOUS) { 
        mx_return = mx_issend( ompi_mtl_mx.mx_endpoint, 
                               mtl_mx_request.mx_segment, 
                               1,
                               mx_endpoint->mx_peer_addr, 
                               match_bits, 
                               &mtl_mx_request, 
                               &mtl_mx_request.mx_request
                               );
        where = "mx_issend";
    } else { 
        mx_return = mx_isend( ompi_mtl_mx.mx_endpoint, 
                              mtl_mx_request.mx_segment,
                              1,
                              mx_endpoint->mx_peer_addr,
                              match_bits,
                              &mtl_mx_request,
                              &mtl_mx_request.mx_request
                              );
        where = "mx_isend";
    }
    if( OPAL_UNLIKELY(mx_return != MX_SUCCESS) ) { 
        char peer_name[MX_MAX_HOSTNAME_LEN];
        if(MX_SUCCESS != mx_nic_id_to_hostname( mx_endpoint->mx_peer->nic_id, peer_name)) { 
            sprintf( peer_name, "unknown %lx nic_id", (long)mx_endpoint->mx_peer->nic_id ); 
        }
        opal_output(ompi_mtl_base_output, "Error in %s (error %s) sending to %s\n",
                    where, mx_strerror(mx_return), peer_name);

        /* Free buffer if needed */
        if(mtl_mx_request.free_after) { 
            free(mtl_mx_request.mx_segment[0].segment_ptr);
        }
        return OMPI_ERROR;
    }
    
    do { 
        mx_return = mx_test(ompi_mtl_mx.mx_endpoint, 
                            &mtl_mx_request.mx_request,
                            &mx_status,
                            &result);
        if( OPAL_UNLIKELY(mx_return != MX_SUCCESS) ) { 
            opal_output(ompi_mtl_base_output, "Error in mx_wait (error %s)\n", mx_strerror(mx_return));
            abort();
        }
        if( OPAL_UNLIKELY(result && mx_status.code != MX_STATUS_SUCCESS) ) { 
            opal_output(ompi_mtl_base_output, 
                        "Error in ompi_mtl_mx_send, mx_wait returned something other than MX_STATUS_SUCCESS: mx_status.code = %d.\n", 
                        mx_status.code);
            abort();
        }
    } while(!result);

    /* Free buffer if needed */
    if(mtl_mx_request.free_after) { 
        free(mtl_mx_request.mx_segment[0].segment_ptr);
    }
    
    return OMPI_SUCCESS;
}

int
ompi_mtl_mx_isend(struct mca_mtl_base_module_t* mtl, 
                  struct ompi_communicator_t* comm,
                  int dest,
                  int tag,
                  struct opal_convertor_t *convertor,
                  mca_pml_base_send_mode_t mode,
                  bool blocking,
                  mca_mtl_request_t * mtl_request)
{
    mx_return_t mx_return;
    uint64_t match_bits;
    mca_mtl_mx_request_t * mtl_mx_request = (mca_mtl_mx_request_t*) mtl_request;
    size_t length;
    ompi_proc_t* ompi_proc = ompi_comm_peer_lookup( comm, dest );
    mca_mtl_mx_endpoint_t* mx_endpoint = (mca_mtl_mx_endpoint_t*) ompi_proc->proc_pml;
    char* where;

    assert(mtl == &ompi_mtl_mx.super);

    MX_SET_SEND_BITS(match_bits, comm->c_contextid, comm->c_my_rank, tag); 
    
    ompi_mtl_datatype_pack(convertor, 
                           &mtl_mx_request->mx_segment[0].segment_ptr, 
                           &length,
                           &mtl_mx_request->free_after);
    mtl_mx_request->mx_segment[0].segment_length = length;
    mtl_mx_request->convertor = convertor;
    mtl_mx_request->type = OMPI_MTL_MX_ISEND;

    OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
                         "issend bits: 0x%016" PRIu64 "\n", match_bits));

    if(mode == MCA_PML_BASE_SEND_SYNCHRONOUS) { 
        mx_return = mx_issend( ompi_mtl_mx.mx_endpoint, 
                               mtl_mx_request->mx_segment, 
                               1,
                               mx_endpoint->mx_peer_addr, 
                               match_bits, 
                               mtl_mx_request, 
                               &mtl_mx_request->mx_request
                               );
        where = "mx_issend";
    } else { 
        mx_return = mx_isend( ompi_mtl_mx.mx_endpoint, 
                              mtl_mx_request->mx_segment,
                              1,
                              mx_endpoint->mx_peer_addr,
                              match_bits,
                              mtl_mx_request,
                              &mtl_mx_request->mx_request
                              );
        where = "mx_isend";
    }
    if( OPAL_UNLIKELY(mx_return != MX_SUCCESS) ) { 
        char peer_name[MX_MAX_HOSTNAME_LEN];
        if(MX_SUCCESS != mx_nic_id_to_hostname( mx_endpoint->mx_peer->nic_id, peer_name)) { 
            sprintf( peer_name, "unknown %lx nic_id", (long)mx_endpoint->mx_peer->nic_id ); 
        }
        opal_output(ompi_mtl_base_output, "Error in %s (error %s) sending to %s\n",
                    where, mx_strerror(mx_return), peer_name);
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}
