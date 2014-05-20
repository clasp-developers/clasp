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
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/f77/bindings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GREQUEST_START = mpi_grequest_start_f
#pragma weak pmpi_grequest_start = mpi_grequest_start_f
#pragma weak pmpi_grequest_start_ = mpi_grequest_start_f
#pragma weak pmpi_grequest_start__ = mpi_grequest_start_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GREQUEST_START,
                           pmpi_grequest_start,
                           pmpi_grequest_start_,
                           pmpi_grequest_start__,
                           pmpi_grequest_start_f,
                           (MPI_F_Grequest_query_function* query_fn, MPI_F_Grequest_free_function* free_fn, MPI_F_Grequest_cancel_function* cancel_fn, MPI_Aint *extra_state, MPI_Fint *request, MPI_Fint *ierr),
                           (query_fn, free_fn, cancel_fn, extra_state, request, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GREQUEST_START = mpi_grequest_start_f
#pragma weak mpi_grequest_start = mpi_grequest_start_f
#pragma weak mpi_grequest_start_ = mpi_grequest_start_f
#pragma weak mpi_grequest_start__ = mpi_grequest_start_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GREQUEST_START,
                           mpi_grequest_start,
                           mpi_grequest_start_,
                           mpi_grequest_start__,
                           mpi_grequest_start_f,
                           (MPI_F_Grequest_query_function* query_fn, MPI_F_Grequest_free_function* free_fn, MPI_F_Grequest_cancel_function* cancel_fn, MPI_Aint *extra_state, MPI_Fint *request, MPI_Fint *ierr),
                           (query_fn, free_fn, cancel_fn, extra_state, request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_grequest_start_f(MPI_F_Grequest_query_function* query_fn,
                          MPI_F_Grequest_free_function* free_fn,
			  MPI_F_Grequest_cancel_function* cancel_fn,
                          MPI_Aint *extra_state,
			  MPI_Fint *request, MPI_Fint *ierr)
{
    MPI_Request c_req;
    *ierr = OMPI_INT_2_FINT(MPI_Grequest_start(
			       (MPI_Grequest_query_function *) query_fn,
			       (MPI_Grequest_free_function *) free_fn,
			       (MPI_Grequest_cancel_function *) cancel_fn, 
			       extra_state, &c_req));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        /* Manually override the function pointer type flag on the
           grequest to indicate that these are Fortran functions */
        ompi_grequest_t *g = (ompi_grequest_t*) c_req;
        g->greq_funcs_are_c = false;

        *request = MPI_Request_c2f(c_req);
    }
}
