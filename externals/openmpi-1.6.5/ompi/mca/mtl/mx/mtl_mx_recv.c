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
#include "ompi/communicator/communicator.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/mca/mtl/base/mtl_base_datatype.h"

#include "mtl_mx.h"
#include "mtl_mx_types.h"
#include "mtl_mx_request.h"

int
ompi_mtl_mx_irecv(struct mca_mtl_base_module_t* mtl,
                  struct ompi_communicator_t *comm,
                  int src,
                  int tag,
                  struct opal_convertor_t *convertor,
                  struct mca_mtl_request_t *mtl_request)
{
    int ret;    
    mx_return_t mx_return;
    mca_mtl_mx_request_t * mtl_mx_request = (mca_mtl_mx_request_t*) mtl_request;
    uint64_t match_bits;
    uint64_t mask_bits;
    size_t length;
    
    ret = ompi_mtl_datatype_recv_buf(convertor,
                                     &mtl_mx_request->mx_segment[0].segment_ptr,
                                     &length, 
                                     &mtl_mx_request->free_after);
    
    mtl_mx_request->mx_segment[0].segment_length = length;
    mtl_mx_request->convertor = convertor;
    mtl_mx_request->type = OMPI_MTL_MX_IRECV;

    if(OMPI_SUCCESS != ret) return ret;

    
    MX_SET_RECV_BITS(match_bits, 
                     mask_bits,
                     comm->c_contextid,
                     src,
                     tag);
    
    OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
                         "recv bits:   0x%016" PRIu64 " 0x%016" PRIu64 "\n", 
                         match_bits, mask_bits));

    mx_return = mx_irecv( ompi_mtl_mx.mx_endpoint, 
                          mtl_mx_request->mx_segment, 
                          1,
                          match_bits,
                          mask_bits,
                          mtl_mx_request, 
                          &mtl_mx_request->mx_request);
    if(mx_return != MX_SUCCESS) {
        opal_output(ompi_mtl_base_output, "Error in mx_irecv (error %s)\n", mx_strerror(mx_return));
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

