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
#pragma weak PMPI_COMM_CONNECT = mpi_comm_connect_f
#pragma weak pmpi_comm_connect = mpi_comm_connect_f
#pragma weak pmpi_comm_connect_ = mpi_comm_connect_f
#pragma weak pmpi_comm_connect__ = mpi_comm_connect_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_CONNECT,
                           pmpi_comm_connect,
                           pmpi_comm_connect_,
                           pmpi_comm_connect__,
                           pmpi_comm_connect_f,
                           (char *port_name, MPI_Fint *info, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *ierr, int port_name_len),
                           (port_name, info, root, comm, newcomm, ierr, port_name_len) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_CONNECT = mpi_comm_connect_f
#pragma weak mpi_comm_connect = mpi_comm_connect_f
#pragma weak mpi_comm_connect_ = mpi_comm_connect_f
#pragma weak mpi_comm_connect__ = mpi_comm_connect_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_CONNECT,
                           mpi_comm_connect,
                           mpi_comm_connect_,
                           mpi_comm_connect__,
                           mpi_comm_connect_f,
                           (char *port_name, MPI_Fint *info, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *ierr, int port_name_len),
                           (port_name, info, root, comm, newcomm, ierr, port_name_len) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_comm_connect_f(char *port_name, MPI_Fint *info,
			MPI_Fint *root, MPI_Fint *comm,
			MPI_Fint *newcomm, MPI_Fint *ierr, 
			int port_name_len)
{
    MPI_Comm c_comm, c_new_comm;
    MPI_Info c_info;
    char *c_port_name;

    c_comm = MPI_Comm_f2c(*comm);
    c_info = MPI_Info_f2c(*info);
    ompi_fortran_string_f2c(port_name, port_name_len, &c_port_name);

    *ierr = OMPI_INT_2_FINT(MPI_Comm_connect(c_port_name, c_info, 
					     OMPI_FINT_2_INT(*root),
					     c_comm, &c_new_comm));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *newcomm = MPI_Comm_c2f(c_new_comm);
    }
    free ( c_port_name );
}

