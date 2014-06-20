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
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/request/request.h"
#include "pml_csum_recvreq.h"
#include "ompi/peruse/peruse-internal.h"

int mca_pml_csum_irecv_init(void *addr,
                           size_t count,
                           ompi_datatype_t * datatype,
                           int src,
                           int tag,
                           struct ompi_communicator_t *comm,
                           struct ompi_request_t **request)
{
    int rc;
    mca_pml_csum_recv_request_t *recvreq;
    MCA_PML_CSUM_RECV_REQUEST_ALLOC(recvreq, rc);
    if (NULL == recvreq)
        return rc;

    MCA_PML_CSUM_RECV_REQUEST_INIT(recvreq,
                                   addr,
                                   count, datatype, src, tag, comm, true);
    
    PERUSE_TRACE_COMM_EVENT (PERUSE_COMM_REQ_ACTIVATE,
                             &((recvreq)->req_recv.req_base),
                             PERUSE_RECV);                              

    *request = (ompi_request_t *) recvreq;
    return OMPI_SUCCESS;
}

int mca_pml_csum_irecv(void *addr,
                      size_t count,
                      ompi_datatype_t * datatype,
                      int src,
                      int tag,
                      struct ompi_communicator_t *comm,
                      struct ompi_request_t **request)
{
    int rc;

    mca_pml_csum_recv_request_t *recvreq;
    MCA_PML_CSUM_RECV_REQUEST_ALLOC(recvreq, rc);
    if (NULL == recvreq)
        return rc;

    MCA_PML_CSUM_RECV_REQUEST_INIT(recvreq,
                                   addr,
                                   count, datatype, src, tag, comm, false);

    PERUSE_TRACE_COMM_EVENT (PERUSE_COMM_REQ_ACTIVATE,
                             &((recvreq)->req_recv.req_base),
                             PERUSE_RECV);

    MCA_PML_CSUM_RECV_REQUEST_START(recvreq);
    *request = (ompi_request_t *) recvreq;
    return OMPI_SUCCESS;
}


int mca_pml_csum_recv(void *addr,
                     size_t count,
                     ompi_datatype_t * datatype,
                     int src,
                     int tag,
                     struct ompi_communicator_t *comm,
                     ompi_status_public_t * status)
{
    int rc;
    mca_pml_csum_recv_request_t *recvreq;
    MCA_PML_CSUM_RECV_REQUEST_ALLOC(recvreq, rc);
    if (NULL == recvreq)
        return rc;

    MCA_PML_CSUM_RECV_REQUEST_INIT(recvreq,
                                   addr,
                                   count, datatype, src, tag, comm, false);

    PERUSE_TRACE_COMM_EVENT (PERUSE_COMM_REQ_ACTIVATE,
                             &((recvreq)->req_recv.req_base),
                             PERUSE_RECV);

    MCA_PML_CSUM_RECV_REQUEST_START(recvreq);
    ompi_request_wait_completion(&recvreq->req_recv.req_base.req_ompi);

    if (NULL != status) {  /* return status */
        OMPI_STATUS_SET(status, &recvreq->req_recv.req_base.req_ompi.req_status);
    }
    rc = recvreq->req_recv.req_base.req_ompi.req_status.MPI_ERROR;
    ompi_request_free( (ompi_request_t**)&recvreq );
    return rc;
}
