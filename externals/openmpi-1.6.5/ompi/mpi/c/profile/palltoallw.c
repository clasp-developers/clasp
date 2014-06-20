/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2012 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/memchecker.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Alltoallw = PMPI_Alltoallw
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Alltoallw";


int MPI_Alltoallw(void *sendbuf, int *sendcounts, int *sdispls, 
                  MPI_Datatype *sendtypes,
                  void *recvbuf, int *recvcounts, int *rdispls, 
                  MPI_Datatype *recvtypes, MPI_Comm comm) 
{
    int i, size, err;   
    
    MEMCHECKER(
        ptrdiff_t recv_ext;
        ptrdiff_t send_ext;

        size = ompi_comm_remote_size(comm);

        memchecker_comm(comm);
        for ( i = 0; i < size; i++ ) {
            memchecker_datatype(sendtypes[i]);
            memchecker_datatype(recvtypes[i]);
            
            ompi_datatype_type_extent(sendtypes[i], &send_ext);
            ompi_datatype_type_extent(recvtypes[i], &recv_ext);

            memchecker_call(&opal_memchecker_base_isdefined,
                            (char *)(sendbuf)+sdispls[i]*send_ext,
                            sendcounts[i], sendtypes[i]);
            memchecker_call(&opal_memchecker_base_isaddressable,
                            (char *)(recvbuf)+sdispls[i]*recv_ext,
                            recvcounts[i], recvtypes[i]);
        }
    );

    if (MPI_PARAM_CHECK) {

        /* Unrooted operation -- same checks for all ranks */

        err = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_comm_invalid(comm)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                          FUNC_NAME);
        }

        if ((NULL == sendcounts) || (NULL == sdispls) || (NULL == sendtypes) ||
            (NULL == recvcounts) || (NULL == rdispls) || (NULL == recvtypes) ||
            MPI_IN_PLACE == sendbuf || MPI_IN_PLACE == recvbuf) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, FUNC_NAME);
        }

        /* We always define the remote group to be the same as the local
           group in the case of an intracommunicator, so it's safe to
           get the size of the remote group here for both intra- and
           intercommunicators */

        size = ompi_comm_remote_size(comm);
        for (i = 0; i < size; ++i) {
            if (recvcounts[i] < 0) {
                err = MPI_ERR_COUNT;
            } else if (MPI_DATATYPE_NULL == recvtypes[i] || NULL == recvtypes[i]) {
                err = MPI_ERR_TYPE;
            } else {
                OMPI_CHECK_DATATYPE_FOR_SEND(err, sendtypes[i], sendcounts[i]);
            }
            OMPI_ERRHANDLER_CHECK(err, comm, err, FUNC_NAME);
        }
    }

    OPAL_CR_ENTER_LIBRARY();

    /* Invoke the coll component to perform the back-end operation */

    err = comm->c_coll.coll_alltoallw(sendbuf, sendcounts, sdispls, sendtypes, 
                                      recvbuf, recvcounts, rdispls, recvtypes,
                                      comm, comm->c_coll.coll_alltoallw_module);
    OMPI_ERRHANDLER_RETURN(err, comm, err, FUNC_NAME);
}

