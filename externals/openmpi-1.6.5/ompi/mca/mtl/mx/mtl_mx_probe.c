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
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/communicator/communicator.h"
#include "ompi/mca/mtl/base/base.h"

#include "mtl_mx.h"
#include "mtl_mx_types.h"
#include "mtl_mx_request.h"


int
ompi_mtl_mx_iprobe(struct mca_mtl_base_module_t* mtl, 
                   struct ompi_communicator_t *comm,
                   int src,
                   int tag,
                   int *flag,
                   struct ompi_status_public_t *status)
{
    uint32_t result;
    mx_return_t ret;
    mx_status_t mx_status;
    uint64_t match_bits;
    uint64_t mask_bits;

    MX_SET_RECV_BITS(match_bits, 
                     mask_bits,
                     comm->c_contextid,
                     src,
                     tag);

    ret = mx_iprobe(ompi_mtl_mx.mx_endpoint,
                    match_bits,
                    mask_bits,
                    &mx_status,
                    &result);
    if (MX_SUCCESS != ret) {
        opal_output(ompi_mtl_base_output, "Error in mx_iprobe (error %s)\n", mx_strerror(ret));
        return OMPI_ERROR;
    }

    if (result) {
        if(MPI_STATUS_IGNORE != status) { 
            MX_GET_SRC(mx_status.match_info, status->MPI_SOURCE);
            MX_GET_TAG(mx_status.match_info, status->MPI_TAG); 
            OMPI_STATUS_SET_COUNT(&status->_ucount, &mx_status.msg_length);
        }
        *flag = 1;
    } else {
        *flag = 0;
    }

    return OMPI_SUCCESS;
}
