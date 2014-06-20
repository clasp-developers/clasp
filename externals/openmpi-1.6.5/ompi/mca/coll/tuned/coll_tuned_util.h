/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_COLL_TUNED_UTIL_EXPORT_H
#define MCA_COLL_TUNED_UTIL_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "opal/mca/mca.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/request/request.h"
#include "ompi/mca/pml/pml.h"

BEGIN_C_DECLS

/* prototypes */
int ompi_coll_tuned_sendrecv_actual( void* sendbuf, int scount, 
                                     ompi_datatype_t* sdatatype,
                                     int dest, int stag,
                                     void* recvbuf, int rcount, 
                                     ompi_datatype_t* rdatatype,
                                     int source, int rtag,
                                     struct ompi_communicator_t* comm,
                                     ompi_status_public_t* status );


/* inline functions */

static inline int
ompi_coll_tuned_sendrecv( void* sendbuf, int scount, ompi_datatype_t* sdatatype,
                          int dest, int stag,
                          void* recvbuf, int rcount, ompi_datatype_t* rdatatype,
                          int source, int rtag, 
                          struct ompi_communicator_t* comm,
                          ompi_status_public_t* status, int myid )
{
    if ((dest == myid) && (source == myid)) {
        return (int) ompi_datatype_sndrcv(sendbuf, (int32_t) scount, sdatatype, 
                                     recvbuf, (int32_t) rcount, rdatatype);
    }
    return ompi_coll_tuned_sendrecv_actual (sendbuf, scount, sdatatype, 
                                            dest, stag, 
                                            recvbuf, rcount, rdatatype,
                                            source, rtag, comm, status);
}

int 
ompi_coll_tuned_sendrecv_actual_localcompleted( void* sendbuf, int scount, 
                                                ompi_datatype_t* sdatatype,
                                                int dest, int stag,
                                                void* recvbuf, int rcount, 
                                                ompi_datatype_t* rdatatype,
                                                int source, int rtag,
                                                struct ompi_communicator_t* comm,
                                                ompi_status_public_t* status );


/* inline functions */

static inline int 
ompi_coll_tuned_sendrecv_localcompleted( void* sendbuf, int scount, 
                                         ompi_datatype_t* sdatatype,
                                         int dest, int stag,
                                         void* recvbuf, int rcount, 
                                         ompi_datatype_t* rdatatype,
                                         int source, int rtag,
                                         struct ompi_communicator_t* comm,
                                         ompi_status_public_t* status, int myid )
{
    if ((dest == myid) && (source == myid)) {
        return (int) ompi_datatype_sndrcv(sendbuf, (int32_t) scount, sdatatype, 
                                     recvbuf, (int32_t) rcount, rdatatype);
    }
    return ompi_coll_tuned_sendrecv_actual_localcompleted (sendbuf, scount, 
                                                           sdatatype, dest, 
                                                           stag,
                                                           recvbuf, rcount, 
                                                           rdatatype,
                                                           source, rtag, comm, 
                                                           status);
}

/* inline functions */
static inline int
ompi_coll_tuned_isendrecv( void* sendbuf, int scount, ompi_datatype_t* sdtype,
                           int dest, int stag, ompi_request_t** sreq,
                           void* recvbuf, int rcount, ompi_datatype_t* rdtype,
                           int source, int rtag, ompi_request_t** rreq,
                           struct ompi_communicator_t* comm ) {
   int ret, line;
   
   ret = MCA_PML_CALL(irecv(recvbuf, rcount, rdtype, source, rtag, comm, rreq));
   if (MPI_SUCCESS != ret) { line = __LINE__; goto error_handler; }

   ret = MCA_PML_CALL(isend(sendbuf, scount, sdtype, dest, stag, 
                            MCA_PML_BASE_SEND_STANDARD, comm, sreq));
   if (MPI_SUCCESS != ret) { line = __LINE__; goto error_handler; }
   
   return MPI_SUCCESS;
 error_handler:
   OPAL_OUTPUT((ompi_coll_tuned_stream, "%s:%d\tError occurred %d\n",
                __FILE__, line, ret));
   return ret;
}

END_C_DECLS
#endif /* MCA_COLL_TUNED_UTIL_EXPORT_H */


