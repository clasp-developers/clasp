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
#pragma weak PMPI_IRECV = mpi_irecv_f
#pragma weak pmpi_irecv = mpi_irecv_f
#pragma weak pmpi_irecv_ = mpi_irecv_f
#pragma weak pmpi_irecv__ = mpi_irecv_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_IRECV,
                           pmpi_irecv,
                           pmpi_irecv_,
                           pmpi_irecv__,
                           pmpi_irecv_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                           (buf, count, datatype, source, tag, comm, request, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_IRECV = mpi_irecv_f
#pragma weak mpi_irecv = mpi_irecv_f
#pragma weak mpi_irecv_ = mpi_irecv_f
#pragma weak mpi_irecv__ = mpi_irecv_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_IRECV,
                           mpi_irecv,
                           mpi_irecv_,
                           mpi_irecv__,
                           mpi_irecv_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                           (buf, count, datatype, source, tag, comm, request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_irecv_f(char *buf, MPI_Fint *count, MPI_Fint *datatype,
		 MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm,
		 MPI_Fint *request, MPI_Fint *ierr)
{
   MPI_Datatype c_type = MPI_Type_f2c(*datatype);
   MPI_Request c_req;
   MPI_Comm c_comm;

   c_comm = MPI_Comm_f2c (*comm);

   *ierr = OMPI_INT_2_FINT(MPI_Irecv(OMPI_F2C_BOTTOM(buf), OMPI_FINT_2_INT(*count),
                                     c_type, OMPI_FINT_2_INT(*source),
                                     OMPI_FINT_2_INT(*tag), c_comm, &c_req));

   if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
      *request = MPI_Request_c2f(c_req);
   }
}
