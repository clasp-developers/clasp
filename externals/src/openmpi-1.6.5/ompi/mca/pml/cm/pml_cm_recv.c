/*
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"

#include "opal/prefetch.h"

#include "ompi/request/request.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"

#include "pml_cm.h"
#include "pml_cm_recvreq.h"

int
mca_pml_cm_irecv_init(void *addr,
                      size_t count,
                      ompi_datatype_t * datatype,
                      int src,
                      int tag,
                      struct ompi_communicator_t *comm,
                      struct ompi_request_t **request)
{
    int ret;
    mca_pml_cm_hvy_recv_request_t *recvreq;
    ompi_proc_t* ompi_proc;
    
    MCA_PML_CM_HVY_RECV_REQUEST_ALLOC(recvreq, ret);
    if( OPAL_UNLIKELY(OMPI_SUCCESS != ret) ) return ret;
    
    MCA_PML_CM_HVY_RECV_REQUEST_INIT(recvreq, ompi_proc, comm, tag, src, 
                                     datatype, addr, count, true); 
    
    *request = (ompi_request_t*) recvreq;

    return OMPI_SUCCESS;
}


int
mca_pml_cm_irecv(void *addr,
                 size_t count,
                 ompi_datatype_t * datatype,
                 int src,
                 int tag,
                 struct ompi_communicator_t *comm,
                 struct ompi_request_t **request)
{
    int ret;
    mca_pml_cm_thin_recv_request_t *recvreq;
    ompi_proc_t* ompi_proc;
    
    MCA_PML_CM_THIN_RECV_REQUEST_ALLOC(recvreq, ret);
    if( OPAL_UNLIKELY(OMPI_SUCCESS != ret) ) return ret;
    
    MCA_PML_CM_THIN_RECV_REQUEST_INIT(recvreq,
                                      ompi_proc,
                                      comm,
                                      tag,
                                      src,
                                      datatype,
                                      addr,
                                      count);
    
    MCA_PML_CM_THIN_RECV_REQUEST_START(recvreq, comm, tag, src, ret);

    if( OPAL_LIKELY(OMPI_SUCCESS == ret) ) *request = (ompi_request_t*) recvreq;

    return ret;
}


int
mca_pml_cm_recv(void *addr,
                size_t count,
                ompi_datatype_t * datatype,
                int src,
                int tag,
                struct ompi_communicator_t *comm,
                ompi_status_public_t * status)
{
    int ret;
    mca_pml_cm_thin_recv_request_t *recvreq;
    ompi_proc_t* ompi_proc;
    
    MCA_PML_CM_THIN_RECV_REQUEST_ALLOC(recvreq, ret);
    if( OPAL_LIKELY(OMPI_SUCCESS != ret) ) return ret;

    MCA_PML_CM_THIN_RECV_REQUEST_INIT(recvreq,
                                      ompi_proc,
                                      comm, 
                                      tag,
                                      src,
                                      datatype,
                                      addr,
                                      count);
    
    
    MCA_PML_CM_THIN_RECV_REQUEST_START(recvreq, comm, tag, src, ret);
    if( OPAL_LIKELY(OMPI_SUCCESS != ret) ) {
        /* BWB - XXX - need cleanup of request here */
        MCA_PML_CM_THIN_RECV_REQUEST_RETURN(recvreq);
        return ret;
    }

    ompi_request_wait_completion(&recvreq->req_base.req_ompi);

    if (NULL != status) {  /* return status */
        OMPI_STATUS_SET(status, &recvreq->req_base.req_ompi.req_status);
    }
    ret = recvreq->req_base.req_ompi.req_status.MPI_ERROR;
    ompi_request_free( (ompi_request_t**)&recvreq );

    return ret;
}

