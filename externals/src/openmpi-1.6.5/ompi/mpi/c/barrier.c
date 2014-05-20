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
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/memchecker.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Barrier = PMPI_Barrier
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Barrier";


int MPI_Barrier(MPI_Comm comm) 
{
  int err = MPI_SUCCESS;
    
  MEMCHECKER(
    memchecker_comm(comm);
  );
  
  /* Error checking */

  if (MPI_PARAM_CHECK) {
    OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    if (ompi_comm_invalid(comm)) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, FUNC_NAME);
    }
  }

  OPAL_CR_ENTER_LIBRARY();

  /* Intracommunicators: Only invoke the back-end coll module barrier
     function if there's more than one process in the communicator */

  if (OMPI_COMM_IS_INTRA(comm)) {
    if (ompi_comm_size(comm) > 1) {
      err = comm->c_coll.coll_barrier(comm, comm->c_coll.coll_barrier_module);
    }
  } 

  /* Intercommunicators -- always invoke, because, by definition,
     there's always at least 2 processes in an intercommunicator. */

  else {
      err = comm->c_coll.coll_barrier(comm, comm->c_coll.coll_barrier_module);
  }

  /* All done */

  OMPI_ERRHANDLER_RETURN(err, comm, err, FUNC_NAME);
}
