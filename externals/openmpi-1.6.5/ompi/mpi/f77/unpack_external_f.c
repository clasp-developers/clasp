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
#pragma weak PMPI_UNPACK_EXTERNAL = mpi_unpack_external_f
#pragma weak pmpi_unpack_external = mpi_unpack_external_f
#pragma weak pmpi_unpack_external_ = mpi_unpack_external_f
#pragma weak pmpi_unpack_external__ = mpi_unpack_external_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_UNPACK_EXTERNAL,
                           pmpi_unpack_external,
                           pmpi_unpack_external_,
                           pmpi_unpack_external__,
                           pmpi_unpack_external_f,
                           (char *datarep, char *inbuf, MPI_Aint *insize, MPI_Aint *position, char *outbuf, MPI_Fint *outcount, MPI_Fint *datatype, MPI_Fint *ierr),
                           (datarep, inbuf, insize, position, outbuf, outcount, datatype, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_UNPACK_EXTERNAL = mpi_unpack_external_f
#pragma weak mpi_unpack_external = mpi_unpack_external_f
#pragma weak mpi_unpack_external_ = mpi_unpack_external_f
#pragma weak mpi_unpack_external__ = mpi_unpack_external_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_UNPACK_EXTERNAL,
                           mpi_unpack_external,
                           mpi_unpack_external_,
                           mpi_unpack_external__,
                           mpi_unpack_external_f,
                           (char *datarep, char *inbuf, MPI_Aint *insize, MPI_Aint *position, char *outbuf, MPI_Fint *outcount, MPI_Fint *datatype, MPI_Fint *ierr),
                           (datarep, inbuf, insize, position, outbuf, outcount, datatype, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_unpack_external_f (char *datarep, char *inbuf, MPI_Aint *insize,
			    MPI_Aint *position, char *outbuf, 
			    MPI_Fint *outcount, MPI_Fint *datatype,
			    MPI_Fint *ierr)
{
   MPI_Datatype c_type;
   c_type = MPI_Type_f2c(*datatype);

   *ierr = OMPI_INT_2_FINT(MPI_Unpack_external(datarep, inbuf, 
                                               *insize, 
                                               position,
                                               OMPI_F2C_BOTTOM(outbuf),
                                               OMPI_FINT_2_INT(*outcount),
                                               c_type));
}
