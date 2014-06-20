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
#include "ompi/mpi/f77/constants.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_BSEND = mpi_bsend_f
#pragma weak pmpi_bsend = mpi_bsend_f
#pragma weak pmpi_bsend_ = mpi_bsend_f
#pragma weak pmpi_bsend__ = mpi_bsend_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_BSEND,
                           pmpi_bsend,
                           pmpi_bsend_,
                           pmpi_bsend__,
                           pmpi_bsend_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_BSEND = mpi_bsend_f
#pragma weak mpi_bsend = mpi_bsend_f
#pragma weak mpi_bsend_ = mpi_bsend_f
#pragma weak mpi_bsend__ = mpi_bsend_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_BSEND,
                           mpi_bsend,
                           mpi_bsend_,
                           mpi_bsend__,
                           mpi_bsend_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_bsend_f(char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_type = MPI_Type_f2c(*datatype);

    c_comm = MPI_Comm_f2c (*comm);
  
    *ierr = OMPI_INT_2_FINT(MPI_Bsend(OMPI_F2C_BOTTOM(buf), OMPI_FINT_2_INT(*count),
				      c_type, OMPI_FINT_2_INT(*dest),
				      OMPI_FINT_2_INT(*tag), c_comm));
}
