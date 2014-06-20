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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/f77/bindings.h"
#include "ompi/mpi/f77/f77_strings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_OPEN_PORT = mpi_open_port_f
#pragma weak pmpi_open_port = mpi_open_port_f
#pragma weak pmpi_open_port_ = mpi_open_port_f
#pragma weak pmpi_open_port__ = mpi_open_port_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_OPEN_PORT,
                           pmpi_open_port,
                           pmpi_open_port_,
                           pmpi_open_port__,
                           pmpi_open_port_f,
                           (MPI_Fint *info, char *port_name, MPI_Fint *ierr, int port_name_len),
                           (info, port_name, ierr, port_name_len) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_OPEN_PORT = mpi_open_port_f
#pragma weak mpi_open_port = mpi_open_port_f
#pragma weak mpi_open_port_ = mpi_open_port_f
#pragma weak mpi_open_port__ = mpi_open_port_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_OPEN_PORT,
                           mpi_open_port,
                           mpi_open_port_,
                           mpi_open_port__,
                           mpi_open_port_f,
                           (MPI_Fint *info, char *port_name, MPI_Fint *ierr, int port_name_len),
                           (info, port_name, ierr, port_name_len) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_open_port_f(MPI_Fint *info, char *port_name, MPI_Fint *ierr, int port_name_len)
{
    MPI_Info c_info;
    char c_port_name[MPI_MAX_PORT_NAME];

    c_info = MPI_Info_f2c(*info);

    *ierr = OMPI_INT_2_FINT(MPI_Open_port(c_info, c_port_name));

    if ( MPI_SUCCESS == OMPI_FINT_2_INT (*ierr )) {
	ompi_fortran_string_c2f(c_port_name, port_name, port_name_len );
    }
}
