/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
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
#include "ompi/op/op.h"
#include "ompi/memchecker.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Reduce_local = PMPI_Reduce_local
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Reduce_local";


int MPI_Reduce_local(void *inbuf, void *inoutbuf, int count, 
                     MPI_Datatype datatype, MPI_Op op)
{
    MEMCHECKER(
        memchecker_datatype(datatype);
    );

    if (MPI_PARAM_CHECK) {
        char *msg;
        int err = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if (MPI_OP_NULL == op || NULL == op) {
            err = MPI_ERR_OP;
        } else if (!ompi_op_is_valid(op, datatype, &msg, FUNC_NAME)) {
            int ret = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_OP, msg);
            free(msg);
            return ret;
        } else {
            OMPI_CHECK_DATATYPE_FOR_SEND(err, datatype, count);
        }
        OMPI_ERRHANDLER_CHECK(err, MPI_COMM_WORLD, err, FUNC_NAME);
    }

    /* If the count is 0, just return */
    if (0 == count) {
        return MPI_SUCCESS;
    }

    OPAL_CR_ENTER_LIBRARY();

    /* Invoke the op component to perform the back-end operation */
    OBJ_RETAIN(op);
    ompi_op_reduce(op, inbuf, inoutbuf, count, datatype);
    OBJ_RELEASE(op);

    return MPI_SUCCESS;
}
