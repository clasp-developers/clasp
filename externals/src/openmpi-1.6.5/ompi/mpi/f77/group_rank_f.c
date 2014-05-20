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
#pragma weak PMPI_GROUP_RANK = mpi_group_rank_f
#pragma weak pmpi_group_rank = mpi_group_rank_f
#pragma weak pmpi_group_rank_ = mpi_group_rank_f
#pragma weak pmpi_group_rank__ = mpi_group_rank_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GROUP_RANK,
                           pmpi_group_rank,
                           pmpi_group_rank_,
                           pmpi_group_rank__,
                           pmpi_group_rank_f,
                           (MPI_Fint *group, MPI_Fint *rank, MPI_Fint *ierr),
                           (group, rank, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GROUP_RANK = mpi_group_rank_f
#pragma weak mpi_group_rank = mpi_group_rank_f
#pragma weak mpi_group_rank_ = mpi_group_rank_f
#pragma weak mpi_group_rank__ = mpi_group_rank_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GROUP_RANK,
                           mpi_group_rank,
                           mpi_group_rank_,
                           mpi_group_rank__,
                           mpi_group_rank_f,
                           (MPI_Fint *group, MPI_Fint *rank, MPI_Fint *ierr),
                           (group, rank, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_group_rank_f(MPI_Fint *group, MPI_Fint *rank, MPI_Fint *ierr)
{
  ompi_group_t *c_group;
  OMPI_SINGLE_NAME_DECL(rank);

  /* Make the fortran to c representation conversion */
  c_group = MPI_Group_f2c(*group);
  
  *ierr = OMPI_INT_2_FINT(MPI_Group_rank(c_group, 
					 OMPI_SINGLE_NAME_CONVERT(rank)));
  if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
      OMPI_SINGLE_INT_2_FINT(rank);
  }
}
