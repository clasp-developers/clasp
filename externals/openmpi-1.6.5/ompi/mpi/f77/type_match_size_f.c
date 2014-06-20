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
 * Copyright (c) 2009      Sun Microsystems, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/f77/bindings.h"
#include "ompi/mpi/f77/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/datatype/ompi_datatype_internal.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/communicator/communicator.h"
#include "ompi/runtime/params.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_MATCH_SIZE = mpi_type_match_size_f
#pragma weak pmpi_type_match_size = mpi_type_match_size_f
#pragma weak pmpi_type_match_size_ = mpi_type_match_size_f
#pragma weak pmpi_type_match_size__ = mpi_type_match_size_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_MATCH_SIZE,
                           pmpi_type_match_size,
                           pmpi_type_match_size_,
                           pmpi_type_match_size__,
                           pmpi_type_match_size_f,
                           (MPI_Fint *typeclass, MPI_Fint *size, MPI_Fint *type, MPI_Fint *ierr),
                           (typeclass, size, type, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_MATCH_SIZE = mpi_type_match_size_f
#pragma weak mpi_type_match_size = mpi_type_match_size_f
#pragma weak mpi_type_match_size_ = mpi_type_match_size_f
#pragma weak mpi_type_match_size__ = mpi_type_match_size_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_MATCH_SIZE,
                           mpi_type_match_size,
                           mpi_type_match_size_,
                           mpi_type_match_size__,
                           mpi_type_match_size_f,
                           (MPI_Fint *typeclass, MPI_Fint *size, MPI_Fint *type, MPI_Fint *ierr),
                           (typeclass, size, type, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Type_match_size_f";

/*  We cannot use the C function as from Fortran we should check for Fortran types. The only
 * difference is the type of predefined datatypes we are looking for.
 */
void mpi_type_match_size_f(MPI_Fint *typeclass, MPI_Fint *size, MPI_Fint *type, MPI_Fint *ierr)
{
    MPI_Datatype c_type;
    int c_size = OMPI_FINT_2_INT( *size );

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    }

    switch( OMPI_FINT_2_INT(*typeclass) ) {
    case MPI_TYPECLASS_REAL:
        c_type = (MPI_Datatype)ompi_datatype_match_size( c_size, OMPI_DATATYPE_FLAG_DATA_FLOAT, OMPI_DATATYPE_FLAG_DATA_FORTRAN );
        break;
    case MPI_TYPECLASS_INTEGER:
        c_type = (MPI_Datatype)ompi_datatype_match_size( c_size, OMPI_DATATYPE_FLAG_DATA_INT, OMPI_DATATYPE_FLAG_DATA_FORTRAN );
        break;
    case MPI_TYPECLASS_COMPLEX:
        c_type = (MPI_Datatype)ompi_datatype_match_size( c_size, OMPI_DATATYPE_FLAG_DATA_COMPLEX, OMPI_DATATYPE_FLAG_DATA_FORTRAN );
        break;
    default:
        c_type = &ompi_mpi_datatype_null.dt;
    }
    *type = MPI_Type_c2f( c_type );
    if ( c_type != &ompi_mpi_datatype_null.dt ) {
        *ierr = OMPI_INT_2_FINT( MPI_SUCCESS );
    } else {
        *ierr = OMPI_INT_2_FINT( MPI_ERR_ARG );
        (void)OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
    }
}
