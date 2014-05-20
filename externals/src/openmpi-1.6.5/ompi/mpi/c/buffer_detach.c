/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
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
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Buffer_detach = PMPI_Buffer_detach
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Buffer_detach";


int MPI_Buffer_detach(void *buffer, int *size) 
{
    int ret = OMPI_SUCCESS;

  if (MPI_PARAM_CHECK) {
    OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    if (NULL == buffer || NULL == size) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
    }
  }

  OPAL_CR_ENTER_LIBRARY();
  ret = mca_pml_base_bsend_detach(buffer, size);

  OPAL_CR_EXIT_LIBRARY();
  return ret;
}
