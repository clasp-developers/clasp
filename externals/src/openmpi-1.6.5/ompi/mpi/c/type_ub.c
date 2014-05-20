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
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/memchecker.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_ub = PMPI_Type_ub
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Type_ub";


int MPI_Type_ub(MPI_Datatype mtype, MPI_Aint *ub)
{
  MPI_Aint lb;
  MPI_Aint extent;
  int status;

  MEMCHECKER(
      memchecker_datatype(mtype);
  );

  if (MPI_PARAM_CHECK) {
    OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    if (NULL == mtype || MPI_DATATYPE_NULL == mtype) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_TYPE, FUNC_NAME);
    } else if (NULL == ub) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
    }
  }

  OPAL_CR_ENTER_LIBRARY();

  status = ompi_datatype_get_extent( mtype, &lb, &extent );
  if (MPI_SUCCESS == status) {
    *ub = lb + extent;
  }
  OMPI_ERRHANDLER_RETURN(status, MPI_COMM_WORLD, status, FUNC_NAME);
}
