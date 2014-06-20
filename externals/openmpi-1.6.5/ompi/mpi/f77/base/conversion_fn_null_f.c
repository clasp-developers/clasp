/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/*
 * This file defines the 5 back-end functions for Fortran's version of
 * MPI_CONVERSION_FN_NULL.  These functions will never be invoked, but
 * are rather used as sentinel values by the Fortran
 * MPI_REGISTER_DATAREP function.  However, given the way Fortran
 * sentinel values have to be handled (4 versions for the 4 different
 * symbol conventions), and given that these symbols must be the same
 * size as a function pointer (because the user could pass either
 * MPI_REGISTER_DATAREP *or* a real function), it just seemed either
 * to use the normal mechanisms we already have in place for Fortran
 * bindings and sentinel values -- even though it's a bit overkill
 * (e.g., the back-end mpi_conversion_fn_null_f function will never be
 * used for anything).
 *
 * See also the comments in ompi/mpi/f77/datarep.h.
 */

#include "ompi_config.h"

#include "ompi/mpi/f77/datarep.h"
#include "ompi/mpi/f77/fint_2_int.h"
#include "ompi/mpi/f77/bindings.h"

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CONVERSION_FN_NULL   = mpi_conversion_fn_null_f
#pragma weak mpi_conversion_fn_null   = mpi_conversion_fn_null_f
#pragma weak mpi_conversion_fn_null_  = mpi_conversion_fn_null_f
#pragma weak mpi_conversion_fn_null__ = mpi_conversion_fn_null_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS(MPI_CONVERSION_FN_NULL,
                           mpi_conversion_fn_null,
                           mpi_conversion_fn_null_,
                           mpi_conversion_fn_null__,
                           mpi_conversion_fn_null_f,
                           (char *userbuf, MPI_Fint *datatype, MPI_Fint *count, char *filebuf, MPI_Offset *position, MPI_Aint *extra_state, MPI_Fint *ierr),
                           (userbuf, datatype, count, filebuf, position, extra_state, ierr) )
#endif


void mpi_conversion_fn_null_f(char *userbuf, MPI_Fint *datatype, 
                              MPI_Fint *count, char *filebuf, 
                              MPI_Offset *position, 
                              MPI_Aint *extra_state, 
                              MPI_Fint *ierr)
{
    /* Per MPI-2:9.5.3, this function will never be called; it's only
       used as a sentinel value for comparison.  But we need to put
       something here so that the compiler/linker doesn't optimize it
       out. */
    *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
}
