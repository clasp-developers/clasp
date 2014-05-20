/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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

#include "ompi/mpi/f77/bindings.h"
#include "ompi/group/group.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GROUP_SIZE = mpi_group_size_f
#pragma weak pmpi_group_size = mpi_group_size_f
#pragma weak pmpi_group_size_ = mpi_group_size_f
#pragma weak pmpi_group_size__ = mpi_group_size_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GROUP_SIZE,
                           pmpi_group_size,
                           pmpi_group_size_,
                           pmpi_group_size__,
                           pmpi_group_size_f,
                           (MPI_Fint *group, MPI_Fint *size, MPI_Fint *ierr),
                           (group, size, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GROUP_SIZE = mpi_group_size_f
#pragma weak mpi_group_size = mpi_group_size_f
#pragma weak mpi_group_size_ = mpi_group_size_f
#pragma weak mpi_group_size__ = mpi_group_size_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GROUP_SIZE,
                           mpi_group_size,
                           mpi_group_size_,
                           mpi_group_size__,
                           mpi_group_size_f,
                           (MPI_Fint *group, MPI_Fint *size, MPI_Fint *ierr),
                           (group, size, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_group_size_f(MPI_Fint *group, MPI_Fint *size, MPI_Fint *ierr)
{
  ompi_group_t *c_group;
  OMPI_SINGLE_NAME_DECL(size);

  /* Make the fortran to c representation conversion */
  c_group = MPI_Group_f2c(*group);
  
  *ierr = OMPI_INT_2_FINT(MPI_Group_size(c_group, 
					 OMPI_SINGLE_NAME_CONVERT(size))); 
  if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
      OMPI_SINGLE_INT_2_FINT(size);
  }
}
