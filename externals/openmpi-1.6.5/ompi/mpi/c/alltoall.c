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
#pragma weak MPI_Alltoall = PMPI_Alltoall
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Alltoall";


int MPI_Alltoall(void *sendbuf, int sendcount, MPI_Datatype sendtype,
                 void *recvbuf, int recvcount, MPI_Datatype recvtype, 
                 MPI_Comm comm) 
{
    int err;

    MEMCHECKER(
        memchecker_datatype(sendtype);
        memchecker_datatype(recvtype);
        memchecker_call(&opal_memchecker_base_isdefined, sendbuf, sendcount, sendtype);
        memchecker_call(&opal_memchecker_base_isaddressable, recvbuf, recvcount, recvtype);
        memchecker_comm(comm);
    );

    if (MPI_PARAM_CHECK) {

        /* Unrooted operation -- same checks for all ranks on both
           intracommunicators and intercommunicators */

        err = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_comm_invalid(comm)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                          FUNC_NAME);
        } else if (MPI_DATATYPE_NULL == recvtype || NULL == recvtype) {
            err = MPI_ERR_TYPE;
        } else if (recvcount < 0) {
            err = MPI_ERR_COUNT;
        } else if (MPI_IN_PLACE == sendbuf || MPI_IN_PLACE == recvbuf) {
            err = MPI_ERR_ARG;
        } else {
            OMPI_CHECK_DATATYPE_FOR_SEND(err, sendtype, sendcount);
        }
        OMPI_ERRHANDLER_CHECK(err, comm, err, FUNC_NAME);
    }

    /* Do we need to do anything? */

    if (0 == sendcount && 0 == recvcount) {
        return MPI_SUCCESS;
    }

    OPAL_CR_ENTER_LIBRARY();

    /* Invoke the coll component to perform the back-end operation */

    err = comm->c_coll.coll_alltoall(sendbuf, sendcount, sendtype, 
                                     recvbuf, recvcount, recvtype,
                                     comm, comm->c_coll.coll_alltoall_module);
    OMPI_ERRHANDLER_RETURN(err, comm, err, FUNC_NAME);
}

