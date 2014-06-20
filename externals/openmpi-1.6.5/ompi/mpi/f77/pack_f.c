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
#pragma weak PMPI_PACK = mpi_pack_f
#pragma weak pmpi_pack = mpi_pack_f
#pragma weak pmpi_pack_ = mpi_pack_f
#pragma weak pmpi_pack__ = mpi_pack_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_PACK,
                           pmpi_pack,
                           pmpi_pack_,
                           pmpi_pack__,
                           pmpi_pack_f,
                           (char *inbuf, MPI_Fint *incount, MPI_Fint *datatype, char *outbuf, MPI_Fint *outsize, MPI_Fint *position, MPI_Fint *comm, MPI_Fint *ierr),
                           (inbuf, incount, datatype, outbuf, outsize, position, comm, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_PACK = mpi_pack_f
#pragma weak mpi_pack = mpi_pack_f
#pragma weak mpi_pack_ = mpi_pack_f
#pragma weak mpi_pack__ = mpi_pack_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_PACK,
                           mpi_pack,
                           mpi_pack_,
                           mpi_pack__,
                           mpi_pack_f,
                           (char *inbuf, MPI_Fint *incount, MPI_Fint *datatype, char *outbuf, MPI_Fint *outsize, MPI_Fint *position, MPI_Fint *comm, MPI_Fint *ierr),
                           (inbuf, incount, datatype, outbuf, outsize, position, comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_pack_f(char *inbuf, MPI_Fint *incount, MPI_Fint *datatype,
		char *outbuf, MPI_Fint *outsize, MPI_Fint *position, 
		MPI_Fint *comm, MPI_Fint *ierr)
{
   MPI_Comm c_comm;
   MPI_Datatype c_type;
   OMPI_SINGLE_NAME_DECL(position);

   c_comm = MPI_Comm_f2c(*comm);
   c_type = MPI_Type_f2c(*datatype);
   OMPI_SINGLE_FINT_2_INT(position);
   
   *ierr = OMPI_INT_2_FINT(MPI_Pack(OMPI_F2C_BOTTOM(inbuf), OMPI_FINT_2_INT(*incount),
                                    c_type, outbuf,
                                    OMPI_FINT_2_INT(*outsize),
                                    OMPI_SINGLE_NAME_CONVERT(position),
                                    c_comm));
				     
   if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
       OMPI_SINGLE_INT_2_FINT(position);			     
   }
}
