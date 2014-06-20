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
#pragma weak PMPI_SENDRECV_REPLACE = mpi_sendrecv_replace_f
#pragma weak pmpi_sendrecv_replace = mpi_sendrecv_replace_f
#pragma weak pmpi_sendrecv_replace_ = mpi_sendrecv_replace_f
#pragma weak pmpi_sendrecv_replace__ = mpi_sendrecv_replace_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_SENDRECV_REPLACE,
                           pmpi_sendrecv_replace,
                           pmpi_sendrecv_replace_,
                           pmpi_sendrecv_replace__,
                           pmpi_sendrecv_replace_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *sendtag, MPI_Fint *source, MPI_Fint *recvtag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (buf, count, datatype, dest, sendtag, source, recvtag, comm, status, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_SENDRECV_REPLACE = mpi_sendrecv_replace_f
#pragma weak mpi_sendrecv_replace = mpi_sendrecv_replace_f
#pragma weak mpi_sendrecv_replace_ = mpi_sendrecv_replace_f
#pragma weak mpi_sendrecv_replace__ = mpi_sendrecv_replace_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_SENDRECV_REPLACE,
                           mpi_sendrecv_replace,
                           mpi_sendrecv_replace_,
                           mpi_sendrecv_replace__,
                           mpi_sendrecv_replace_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *sendtag, MPI_Fint *source, MPI_Fint *recvtag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (buf, count, datatype, dest, sendtag, source, recvtag, comm, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_sendrecv_replace_f(char *buf, MPI_Fint *count, MPI_Fint *datatype,
			    MPI_Fint *dest, MPI_Fint *sendtag,
			    MPI_Fint *source, MPI_Fint *recvtag,
			    MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr)
{
   MPI_Datatype c_type = MPI_Type_f2c(*datatype);
   MPI_Comm c_comm;
   MPI_Status c_status;

   c_comm = MPI_Comm_f2c (*comm);
   
   *ierr = OMPI_INT_2_FINT(MPI_Sendrecv_replace(OMPI_F2C_BOTTOM(buf),
                                                OMPI_FINT_2_INT(*count),
                                                c_type, 
                                                OMPI_FINT_2_INT(*dest), 
                                                OMPI_FINT_2_INT(*sendtag), 
                                                OMPI_FINT_2_INT(*source), 
                                                OMPI_FINT_2_INT(*recvtag),
                                                c_comm, &c_status));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr) &&
        !OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
      MPI_Status_c2f(&c_status, status);
   }
}
