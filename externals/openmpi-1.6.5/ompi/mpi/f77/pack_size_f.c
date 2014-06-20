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

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_PACK_SIZE = mpi_pack_size_f
#pragma weak pmpi_pack_size = mpi_pack_size_f
#pragma weak pmpi_pack_size_ = mpi_pack_size_f
#pragma weak pmpi_pack_size__ = mpi_pack_size_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_PACK_SIZE,
                           pmpi_pack_size,
                           pmpi_pack_size_,
                           pmpi_pack_size__,
                           pmpi_pack_size_f,
                           (MPI_Fint *incount, MPI_Fint *datatype, MPI_Fint *comm, MPI_Fint *size, MPI_Fint *ierr),
                           (incount, datatype, comm, size, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_PACK_SIZE = mpi_pack_size_f
#pragma weak mpi_pack_size = mpi_pack_size_f
#pragma weak mpi_pack_size_ = mpi_pack_size_f
#pragma weak mpi_pack_size__ = mpi_pack_size_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_PACK_SIZE,
                           mpi_pack_size,
                           mpi_pack_size_,
                           mpi_pack_size__,
                           mpi_pack_size_f,
                           (MPI_Fint *incount, MPI_Fint *datatype, MPI_Fint *comm, MPI_Fint *size, MPI_Fint *ierr),
                           (incount, datatype, comm, size, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_pack_size_f(MPI_Fint *incount, MPI_Fint *datatype, 
		     MPI_Fint *comm, MPI_Fint *size, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_type;
    OMPI_SINGLE_NAME_DECL(size);
    
    c_comm = MPI_Comm_f2c(*comm);
    c_type = MPI_Type_f2c(*datatype);

    *ierr = OMPI_INT_2_FINT(MPI_Pack_size(OMPI_FINT_2_INT(*incount),
					  c_type, c_comm, 
					  OMPI_SINGLE_NAME_CONVERT(size)));

    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        OMPI_SINGLE_INT_2_FINT(size);
    }
}
