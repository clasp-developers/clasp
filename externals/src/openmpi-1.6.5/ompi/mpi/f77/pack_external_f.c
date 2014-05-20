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
#pragma weak PMPI_PACK_EXTERNAL = mpi_pack_external_f
#pragma weak pmpi_pack_external = mpi_pack_external_f
#pragma weak pmpi_pack_external_ = mpi_pack_external_f
#pragma weak pmpi_pack_external__ = mpi_pack_external_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_PACK_EXTERNAL,
                           pmpi_pack_external,
                           pmpi_pack_external_,
                           pmpi_pack_external__,
                           pmpi_pack_external_f,
                           (char *datarep, char *inbuf, MPI_Fint *incount, MPI_Fint *datatype, char *outbuf, MPI_Aint *outsize, MPI_Aint *position, MPI_Fint *ierr),
                           (datarep, inbuf, incount, datatype, outbuf, outsize, position, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_PACK_EXTERNAL = mpi_pack_external_f
#pragma weak mpi_pack_external = mpi_pack_external_f
#pragma weak mpi_pack_external_ = mpi_pack_external_f
#pragma weak mpi_pack_external__ = mpi_pack_external_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_PACK_EXTERNAL,
                           mpi_pack_external,
                           mpi_pack_external_,
                           mpi_pack_external__,
                           mpi_pack_external_f,
                           (char *datarep, char *inbuf, MPI_Fint *incount, MPI_Fint *datatype, char *outbuf, MPI_Aint *outsize, MPI_Aint *position, MPI_Fint *ierr),
                           (datarep, inbuf, incount, datatype, outbuf, outsize, position, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_pack_external_f(char *datarep, char *inbuf, MPI_Fint *incount,
			 MPI_Fint *datatype, char *outbuf, 
			 MPI_Aint *outsize, MPI_Aint *position,
			 MPI_Fint *ierr)
{
   MPI_Datatype type = MPI_Type_f2c(*datatype);

   *ierr = OMPI_INT_2_FINT(MPI_Pack_external(datarep, OMPI_F2C_BOTTOM(inbuf),
                                             OMPI_FINT_2_INT(*incount),
                                             type, outbuf,
                                             *outsize,
                                             position));
}
