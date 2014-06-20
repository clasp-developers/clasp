/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2010 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/datatype/ompi_datatype.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/proc/proc.h"
#include "ompi/memchecker.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Sendrecv_replace = PMPI_Sendrecv_replace
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Sendrecv_replace";


int MPI_Sendrecv_replace(void * buf, int count, MPI_Datatype datatype,
                         int dest, int sendtag, int source, int recvtag,
                         MPI_Comm comm, MPI_Status *status)

{
    int rc = MPI_SUCCESS;

    MEMCHECKER(
        memchecker_datatype(datatype);
        memchecker_call(&opal_memchecker_base_isdefined, buf, count, datatype);
        memchecker_comm(comm);
    );

    if ( MPI_PARAM_CHECK ) {
        rc = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        OMPI_CHECK_DATATYPE_FOR_RECV(rc, datatype, count);

        if (ompi_comm_invalid(comm)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, FUNC_NAME);
        } else if (dest != MPI_PROC_NULL && ompi_comm_peer_invalid(comm, dest)) {
            rc = MPI_ERR_RANK;
        } else if (sendtag < 0 || sendtag > mca_pml.pml_max_tag) {
            rc = MPI_ERR_TAG;
        } else if (source != MPI_PROC_NULL && source != MPI_ANY_SOURCE && ompi_comm_peer_invalid(comm, source)) {
            rc = MPI_ERR_RANK;
        } else if (((recvtag < 0) && (recvtag !=  MPI_ANY_TAG)) || (recvtag > mca_pml.pml_max_tag)) {
            rc = MPI_ERR_TAG;
        }
        OMPI_ERRHANDLER_CHECK(rc, comm, rc, FUNC_NAME);
    }

    OPAL_CR_ENTER_LIBRARY();

    /* simple case */
    if ( source == MPI_PROC_NULL || dest == MPI_PROC_NULL || count == 0 ) {
        rc = MPI_Sendrecv(buf,count,datatype,dest,sendtag,buf,count,datatype,source,recvtag,comm,status);

        OPAL_CR_EXIT_LIBRARY();
        return rc;
    } else {

        opal_convertor_t convertor;
        struct iovec iov;
        unsigned char recv_data[2048];
        size_t packed_size, max_data;
        uint32_t iov_count;
        ompi_status_public_t recv_status;
        ompi_proc_t* proc = ompi_comm_peer_lookup(comm,dest);
        if(proc == NULL) {
            rc = MPI_ERR_RANK;
            OMPI_ERRHANDLER_RETURN(rc, comm, rc, FUNC_NAME);
        }

        /* initialize convertor to unpack recv buffer */
        OBJ_CONSTRUCT(&convertor, opal_convertor_t);
        opal_convertor_copy_and_prepare_for_recv( proc->proc_convertor, &(datatype->super),
                                                  count, buf, 0, &convertor );

        /* setup a buffer for recv */
        opal_convertor_get_packed_size( &convertor, &packed_size );
        if( packed_size > sizeof(recv_data) ) {
            rc = MPI_Alloc_mem(packed_size, MPI_INFO_NULL, &iov.iov_base);
            if(OMPI_SUCCESS != rc) {
                OMPI_ERRHANDLER_RETURN(OMPI_ERR_OUT_OF_RESOURCE, comm, MPI_ERR_BUFFER, FUNC_NAME);
            }
        } else {
            iov.iov_base = (caddr_t)recv_data;
        }

        /* recv into temporary buffer */
        rc = MPI_Sendrecv( buf, count, datatype, dest, sendtag, iov.iov_base, packed_size, 
                           MPI_BYTE, source, recvtag, comm, &recv_status );
        if (rc != MPI_SUCCESS) {
            if(packed_size > sizeof(recv_data))
                MPI_Free_mem(iov.iov_base);
            OBJ_DESTRUCT(&convertor);
            OMPI_ERRHANDLER_RETURN(rc, comm, rc, FUNC_NAME);
        }

        /* unpack into users buffer */
        iov.iov_len = recv_status._ucount;
        iov_count = 1;
        max_data = recv_status._ucount;
        opal_convertor_unpack(&convertor, &iov, &iov_count, &max_data );

        /* return status to user */
        if(status != MPI_STATUS_IGNORE) {
            OMPI_STATUS_SET(status, &recv_status);
        }

        /* release resources */
        if(packed_size > sizeof(recv_data)) {
            MPI_Free_mem(iov.iov_base);
        }
        OBJ_DESTRUCT(&convertor);

        OPAL_CR_EXIT_LIBRARY();
        return MPI_SUCCESS;
    }
}
