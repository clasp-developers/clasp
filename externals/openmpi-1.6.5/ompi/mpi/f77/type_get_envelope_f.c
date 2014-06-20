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
#pragma weak PMPI_TYPE_GET_ENVELOPE = mpi_type_get_envelope_f
#pragma weak pmpi_type_get_envelope = mpi_type_get_envelope_f
#pragma weak pmpi_type_get_envelope_ = mpi_type_get_envelope_f
#pragma weak pmpi_type_get_envelope__ = mpi_type_get_envelope_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_GET_ENVELOPE,
                           pmpi_type_get_envelope,
                           pmpi_type_get_envelope_,
                           pmpi_type_get_envelope__,
                           pmpi_type_get_envelope_f,
                           (MPI_Fint *type, MPI_Fint *num_integers, MPI_Fint *num_addresses, MPI_Fint *num_datatypes, MPI_Fint *combiner, MPI_Fint *ierr),
                           (type, num_integers, num_addresses, num_datatypes, combiner, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_GET_ENVELOPE = mpi_type_get_envelope_f
#pragma weak mpi_type_get_envelope = mpi_type_get_envelope_f
#pragma weak mpi_type_get_envelope_ = mpi_type_get_envelope_f
#pragma weak mpi_type_get_envelope__ = mpi_type_get_envelope_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_GET_ENVELOPE,
                           mpi_type_get_envelope,
                           mpi_type_get_envelope_,
                           mpi_type_get_envelope__,
                           mpi_type_get_envelope_f,
                           (MPI_Fint *type, MPI_Fint *num_integers, MPI_Fint *num_addresses, MPI_Fint *num_datatypes, MPI_Fint *combiner, MPI_Fint *ierr),
                           (type, num_integers, num_addresses, num_datatypes, combiner, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_type_get_envelope_f(MPI_Fint *type, MPI_Fint *num_integers,
			     MPI_Fint *num_addresses, 
			     MPI_Fint *num_datatypes, MPI_Fint *combiner,
			     MPI_Fint *ierr)
{
    MPI_Datatype c_type = MPI_Type_f2c(*type);
    OMPI_SINGLE_NAME_DECL(num_integers);
    OMPI_SINGLE_NAME_DECL(num_addresses);
    OMPI_SINGLE_NAME_DECL(num_datatypes);
    OMPI_SINGLE_NAME_DECL(combiner);

    *ierr = OMPI_INT_2_FINT(MPI_Type_get_envelope(c_type,
				 OMPI_SINGLE_NAME_CONVERT(num_integers), 
				 OMPI_SINGLE_NAME_CONVERT(num_addresses), 
				 OMPI_SINGLE_NAME_CONVERT(num_datatypes), 
				 OMPI_SINGLE_NAME_CONVERT(combiner)));

    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        OMPI_SINGLE_INT_2_FINT(num_integers);
        OMPI_SINGLE_INT_2_FINT(num_addresses);
        OMPI_SINGLE_INT_2_FINT(num_datatypes);
        OMPI_SINGLE_INT_2_FINT(combiner);
    }
}
