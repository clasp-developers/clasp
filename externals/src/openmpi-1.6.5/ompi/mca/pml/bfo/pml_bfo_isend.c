/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
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

#include "pml_bfo.h"
#include "pml_bfo_sendreq.h"
#include "pml_bfo_recvreq.h"
#include "ompi/peruse/peruse-internal.h"

int mca_pml_bfo_isend_init(void *buf,
                           size_t count,
                           ompi_datatype_t * datatype,
                           int dst,
                           int tag,
                           mca_pml_base_send_mode_t sendmode,
                           ompi_communicator_t * comm,
                           ompi_request_t ** request)
{
    int rc;
    
    mca_pml_bfo_send_request_t *sendreq = NULL;
    MCA_PML_BFO_SEND_REQUEST_ALLOC(comm, dst, sendreq, rc);
    if (rc != OMPI_SUCCESS)
        return rc;

    MCA_PML_BFO_SEND_REQUEST_INIT(sendreq,
                                  buf,
                                  count,
                                  datatype,
                                  dst, tag,
                                  comm, sendmode, true);
    
    PERUSE_TRACE_COMM_EVENT (PERUSE_COMM_REQ_ACTIVATE,
                             &(sendreq)->req_send.req_base,
                             PERUSE_SEND);

    *request = (ompi_request_t *) sendreq;
    return OMPI_SUCCESS;
}


int mca_pml_bfo_isend(void *buf,
                      size_t count,
                      ompi_datatype_t * datatype,
                      int dst,
                      int tag,
                      mca_pml_base_send_mode_t sendmode,
                      ompi_communicator_t * comm,
                      ompi_request_t ** request)
{
    int rc;
    mca_pml_bfo_send_request_t *sendreq = NULL;
    
    MCA_PML_BFO_SEND_REQUEST_ALLOC(comm, dst, sendreq, rc);
    if (rc != OMPI_SUCCESS)
        return rc;
    
    MCA_PML_BFO_SEND_REQUEST_INIT(sendreq,
                                  buf,
                                  count,
                                  datatype,
                                  dst, tag,
                                  comm, sendmode, false);

    PERUSE_TRACE_COMM_EVENT (PERUSE_COMM_REQ_ACTIVATE,
                             &(sendreq)->req_send.req_base,
                             PERUSE_SEND);

    MCA_PML_BFO_SEND_REQUEST_START(sendreq, rc);
    *request = (ompi_request_t *) sendreq;
    return rc;
}


int mca_pml_bfo_send(void *buf,
                     size_t count,
                     ompi_datatype_t * datatype,
                     int dst,
                     int tag,
                     mca_pml_base_send_mode_t sendmode,
                     ompi_communicator_t * comm)
{
    int rc;
    mca_pml_bfo_send_request_t *sendreq;

    MCA_PML_BFO_SEND_REQUEST_ALLOC(comm, dst, sendreq, rc);
    if (rc != OMPI_SUCCESS)
        return rc;
    
    MCA_PML_BFO_SEND_REQUEST_INIT(sendreq,
                                  buf,
                                  count,
                                  datatype,
                                  dst, tag,
                                  comm, sendmode, false);

    PERUSE_TRACE_COMM_EVENT (PERUSE_COMM_REQ_ACTIVATE,
                             &(sendreq)->req_send.req_base,
                             PERUSE_SEND);
    
    MCA_PML_BFO_SEND_REQUEST_START(sendreq, rc);
    if (rc != OMPI_SUCCESS) {
        MCA_PML_BFO_SEND_REQUEST_RETURN( sendreq );
        return rc;
    }

    ompi_request_wait_completion(&sendreq->req_send.req_base.req_ompi);

    rc = sendreq->req_send.req_base.req_ompi.req_status.MPI_ERROR;
    ompi_request_free( (ompi_request_t**)&sendreq );
    return rc;
}
