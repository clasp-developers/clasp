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
#pragma weak PMPI_GROUP_EXCL = mpi_group_excl_f
#pragma weak pmpi_group_excl = mpi_group_excl_f
#pragma weak pmpi_group_excl_ = mpi_group_excl_f
#pragma weak pmpi_group_excl__ = mpi_group_excl_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GROUP_EXCL,
                           pmpi_group_excl,
                           pmpi_group_excl_,
                           pmpi_group_excl__,
                           pmpi_group_excl_f,
                           (MPI_Fint *group, MPI_Fint *n, MPI_Fint *ranks, MPI_Fint *newgroup, MPI_Fint *ierr),
                           (group, n, ranks, newgroup, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GROUP_EXCL = mpi_group_excl_f
#pragma weak mpi_group_excl = mpi_group_excl_f
#pragma weak mpi_group_excl_ = mpi_group_excl_f
#pragma weak mpi_group_excl__ = mpi_group_excl_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GROUP_EXCL,
                           mpi_group_excl,
                           mpi_group_excl_,
                           mpi_group_excl__,
                           mpi_group_excl_f,
                           (MPI_Fint *group, MPI_Fint *n, MPI_Fint *ranks, MPI_Fint *newgroup, MPI_Fint *ierr),
                           (group, n, ranks, newgroup, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_group_excl_f(MPI_Fint *group, MPI_Fint *n,
		      MPI_Fint *ranks, MPI_Fint *newgroup, 
		      MPI_Fint *ierr)
{
  ompi_group_t *c_group, *c_newgroup;
  OMPI_ARRAY_NAME_DECL(ranks);

  /* Make the fortran to c representation conversion */
  c_group = MPI_Group_f2c(*group);

  OMPI_ARRAY_FINT_2_INT(ranks, *n);
  *ierr = OMPI_INT_2_FINT(MPI_Group_excl(c_group, 
					 OMPI_FINT_2_INT(*n),
					 OMPI_ARRAY_NAME_CONVERT(ranks),
					 &c_newgroup));

  /* translate the results from c to fortran */
  if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
      *newgroup = c_newgroup->grp_f_to_c_index;
  }
}
