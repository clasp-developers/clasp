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
#include "ompi/memchecker.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_hvector = PMPI_Type_hvector
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Type_hvector";


int MPI_Type_hvector(int count,
                     int blocklength,
                     MPI_Aint stride,
                     MPI_Datatype oldtype,
                     MPI_Datatype *newtype)
{
    MEMCHECKER(
        memchecker_datatype(oldtype);
    );

   if ( MPI_PARAM_CHECK ) {
      OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
      if (NULL == oldtype || MPI_DATATYPE_NULL == oldtype ||
          NULL == newtype) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_TYPE,
                                      FUNC_NAME );
      } else if (count < 0) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COUNT,
                                      FUNC_NAME );
      } else if (blocklength < 0) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                      FUNC_NAME );
      }
   }

   return MPI_Type_create_hvector(count,
                                  blocklength,
                                  stride,
                                  oldtype,
                                  newtype);
}
