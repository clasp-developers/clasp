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
 * Copyright (c) 2009-2010 Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/request/request.h"
#include "pml_csum_recvreq.h"


int mca_pml_csum_iprobe(int src,
                       int tag,
                       struct ompi_communicator_t *comm,
                       int *matched, ompi_status_public_t * status)
{
    int rc = OMPI_SUCCESS;
    mca_pml_csum_recv_request_t recvreq;

    OBJ_CONSTRUCT( &recvreq, mca_pml_csum_recv_request_t );
    recvreq.req_recv.req_base.req_ompi.req_type = OMPI_REQUEST_PML;
    recvreq.req_recv.req_base.req_type = MCA_PML_REQUEST_IPROBE;

    MCA_PML_CSUM_RECV_REQUEST_INIT(&recvreq, NULL, 0, &ompi_mpi_char.dt, src, tag, comm, true);
    MCA_PML_CSUM_RECV_REQUEST_START(&recvreq);

    if( recvreq.req_recv.req_base.req_ompi.req_complete == true ) {
        if( NULL != status ) {
            OMPI_STATUS_SET(status, &recvreq.req_recv.req_base.req_ompi.req_status);
        }
        *matched = 1;
    } else {
        *matched = 0;
        opal_progress();
    }
    MCA_PML_BASE_RECV_REQUEST_FINI( &recvreq.req_recv );
    return rc;
}


int mca_pml_csum_probe(int src,
                      int tag,
                      struct ompi_communicator_t *comm,
                      ompi_status_public_t * status)
{
    mca_pml_csum_recv_request_t recvreq;

    OBJ_CONSTRUCT( &recvreq, mca_pml_csum_recv_request_t );
    recvreq.req_recv.req_base.req_ompi.req_type = OMPI_REQUEST_PML;
    recvreq.req_recv.req_base.req_type = MCA_PML_REQUEST_PROBE;

    MCA_PML_CSUM_RECV_REQUEST_INIT(&recvreq, NULL, 0, &ompi_mpi_char.dt, src, tag, comm, true);
    MCA_PML_CSUM_RECV_REQUEST_START(&recvreq);

    ompi_request_wait_completion(&recvreq.req_recv.req_base.req_ompi);

    if (NULL != status) {
        OMPI_STATUS_SET(status, &recvreq.req_recv.req_base.req_ompi.req_status);
    }
    MCA_PML_BASE_RECV_REQUEST_FINI( &recvreq.req_recv );
    return OMPI_SUCCESS;
}
