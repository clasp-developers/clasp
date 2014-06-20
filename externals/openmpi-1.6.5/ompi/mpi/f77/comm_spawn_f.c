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
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/f77/bindings.h"
#include "ompi/mpi/f77/constants.h"
#include "ompi/mpi/f77/f77_strings.h"
#include "opal/util/argv.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_COMM_SPAWN = mpi_comm_spawn_f
#pragma weak pmpi_comm_spawn = mpi_comm_spawn_f
#pragma weak pmpi_comm_spawn_ = mpi_comm_spawn_f
#pragma weak pmpi_comm_spawn__ = mpi_comm_spawn_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_SPAWN,
                            pmpi_comm_spawn,
                            pmpi_comm_spawn_,
                            pmpi_comm_spawn__,
                            pmpi_comm_spawn_f,
                            (char *command, char *argv, MPI_Fint *maxprocs, MPI_Fint *info, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *intercomm, MPI_Fint *array_of_errcodes, MPI_Fint *ierr, int cmd_len, int string_len),
                            (command, argv, maxprocs, info, root, comm, intercomm, array_of_errcodes, ierr, cmd_len, string_len) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_SPAWN = mpi_comm_spawn_f
#pragma weak mpi_comm_spawn = mpi_comm_spawn_f
#pragma weak mpi_comm_spawn_ = mpi_comm_spawn_f
#pragma weak mpi_comm_spawn__ = mpi_comm_spawn_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_SPAWN,
                            mpi_comm_spawn,
                            mpi_comm_spawn_,
                            mpi_comm_spawn__,
                            mpi_comm_spawn_f,
                            (char *command, char *argv, MPI_Fint *maxprocs, MPI_Fint *info, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *intercomm, MPI_Fint *array_of_errcodes, MPI_Fint *ierr, int cmd_len, int string_len),
                            (command, argv, maxprocs, info, root, comm, intercomm, array_of_errcodes, ierr, cmd_len, string_len) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_comm_spawn_f(char *command, char *argv, MPI_Fint *maxprocs, 
		      MPI_Fint *info, MPI_Fint *root, MPI_Fint *comm, 
		      MPI_Fint *intercomm, MPI_Fint *array_of_errcodes,
		      MPI_Fint *ierr, int cmd_len, int string_len)
{
    MPI_Comm c_comm, c_new_comm;
    MPI_Info c_info;
    int size;
    int *c_errs;
    char **c_argv;
    char *c_command;
    OMPI_ARRAY_NAME_DECL(array_of_errcodes);
    
    c_comm = MPI_Comm_f2c(*comm);
    c_info = MPI_Info_f2c(*info);
    MPI_Comm_size(c_comm, &size);
    ompi_fortran_string_f2c(command, cmd_len, &c_command);

    /* It's allowed to ignore the errcodes */

    if (OMPI_IS_FORTRAN_ERRCODES_IGNORE(array_of_errcodes)) {
        c_errs = MPI_ERRCODES_IGNORE;
    } else {
        OMPI_ARRAY_FINT_2_INT_ALLOC(array_of_errcodes, size);
        c_errs = OMPI_ARRAY_NAME_CONVERT(array_of_errcodes);
    }

    /* It's allowed to have no argv */

    if (OMPI_IS_FORTRAN_ARGV_NULL(argv)) {
        c_argv = MPI_ARGV_NULL;
    } else {
        ompi_fortran_argv_f2c(argv, string_len, string_len, &c_argv);
    }

    *ierr = OMPI_INT_2_FINT(MPI_Comm_spawn(c_command, c_argv, 
					   OMPI_FINT_2_INT(*maxprocs),
					   c_info,
					   OMPI_FINT_2_INT(*root),
					   c_comm, &c_new_comm, c_errs));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *intercomm = MPI_Comm_c2f(c_new_comm);
    }
    free(c_command);
    if (MPI_ARGV_NULL != c_argv && NULL != c_argv) {
        opal_argv_free(c_argv);
    }
    if (!OMPI_IS_FORTRAN_ERRCODES_IGNORE(array_of_errcodes)) {
	OMPI_ARRAY_INT_2_FINT(array_of_errcodes, size);
    }
}

