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
#pragma weak PMPI_CLOSE_PORT = mpi_close_port_f
#pragma weak pmpi_close_port = mpi_close_port_f
#pragma weak pmpi_close_port_ = mpi_close_port_f
#pragma weak pmpi_close_port__ = mpi_close_port_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_CLOSE_PORT,
                           pmpi_close_port,
                           pmpi_close_port_,
                           pmpi_close_port__,
                           pmpi_close_port_f,
                           (char *port_name, MPI_Fint *ierr, int port_name_len),
                           (port_name, ierr, port_name_len) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CLOSE_PORT = mpi_close_port_f
#pragma weak mpi_close_port = mpi_close_port_f
#pragma weak mpi_close_port_ = mpi_close_port_f
#pragma weak mpi_close_port__ = mpi_close_port_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_CLOSE_PORT,
                           mpi_close_port,
                           mpi_close_port_,
                           mpi_close_port__,
                           mpi_close_port_f,
                           (char *port_name, MPI_Fint *ierr, int port_name_len),
                           (port_name, ierr, port_name_len) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_close_port_f(char *port_name, MPI_Fint *ierr, int port_name_len)
{
    char *c_port_name;

    ompi_fortran_string_f2c(port_name, port_name_len, &c_port_name);
    *ierr = OMPI_INT_2_FINT(MPI_Close_port(c_port_name));
    free ( c_port_name);
}
