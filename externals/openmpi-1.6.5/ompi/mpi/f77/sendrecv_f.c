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
#pragma weak PMPI_SENDRECV = mpi_sendrecv_f
#pragma weak pmpi_sendrecv = mpi_sendrecv_f
#pragma weak pmpi_sendrecv_ = mpi_sendrecv_f
#pragma weak pmpi_sendrecv__ = mpi_sendrecv_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_SENDRECV,
                           pmpi_sendrecv,
                           pmpi_sendrecv_,
                           pmpi_sendrecv__,
                           pmpi_sendrecv_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, MPI_Fint *dest, MPI_Fint *sendtag, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *source, MPI_Fint *recvtag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, dest, sendtag, recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_SENDRECV = mpi_sendrecv_f
#pragma weak mpi_sendrecv = mpi_sendrecv_f
#pragma weak mpi_sendrecv_ = mpi_sendrecv_f
#pragma weak mpi_sendrecv__ = mpi_sendrecv_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_SENDRECV,
                           mpi_sendrecv,
                           mpi_sendrecv_,
                           mpi_sendrecv__,
                           mpi_sendrecv_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, MPI_Fint *dest, MPI_Fint *sendtag, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *source, MPI_Fint *recvtag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, dest, sendtag, recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_sendrecv_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype,
		    MPI_Fint *dest, MPI_Fint *sendtag, char *recvbuf,
		    MPI_Fint *recvcount, MPI_Fint *recvtype,
		    MPI_Fint *source, MPI_Fint *recvtag, MPI_Fint *comm,
		    MPI_Fint *status, MPI_Fint *ierr)
{
   MPI_Comm c_comm;
   MPI_Datatype c_sendtype = MPI_Type_f2c(*sendtype);
   MPI_Datatype c_recvtype = MPI_Type_f2c(*recvtype);
   MPI_Status c_status;
   
   c_comm = MPI_Comm_f2c (*comm);
   
   *ierr = OMPI_INT_2_FINT(MPI_Sendrecv(OMPI_F2C_BOTTOM(sendbuf), OMPI_FINT_2_INT(*sendcount),
                                        c_sendtype,
                                        OMPI_FINT_2_INT(*dest),
                                        OMPI_FINT_2_INT(*sendtag),
                                        OMPI_F2C_BOTTOM(recvbuf), *recvcount,
                                        c_recvtype, OMPI_FINT_2_INT(*source),
                                        OMPI_FINT_2_INT(*recvtag),
                                        c_comm, &c_status));

   if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr) &&
       !OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
       MPI_Status_c2f(&c_status, status);
   }
}
