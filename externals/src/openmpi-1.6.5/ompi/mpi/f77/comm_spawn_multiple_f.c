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
#pragma weak PMPI_COMM_SPAWN_MULTIPLE = mpi_comm_spawn_multiple_f
#pragma weak pmpi_comm_spawn_multiple = mpi_comm_spawn_multiple_f
#pragma weak pmpi_comm_spawn_multiple_ = mpi_comm_spawn_multiple_f
#pragma weak pmpi_comm_spawn_multiple__ = mpi_comm_spawn_multiple_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_SPAWN_MULTIPLE,
                           pmpi_comm_spawn_multiple,
                           pmpi_comm_spawn_multiple_,
                           pmpi_comm_spawn_multiple__,
                           pmpi_comm_spawn_multiple_f,
                           (MPI_Fint *count, char *array_of_commands, char *array_of_argv, MPI_Fint *array_of_maxprocs, MPI_Fint *array_of_info, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *intercomm, MPI_Fint *array_of_errcodes, MPI_Fint *ierr, int cmd_string_len, int argv_string_len),
                           (count, array_of_commands, array_of_argv, array_of_maxprocs, array_of_info, root, comm, intercomm, array_of_errcodes, ierr, cmd_string_len, argv_string_len) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_SPAWN_MULTIPLE = mpi_comm_spawn_multiple_f
#pragma weak mpi_comm_spawn_multiple = mpi_comm_spawn_multiple_f
#pragma weak mpi_comm_spawn_multiple_ = mpi_comm_spawn_multiple_f
#pragma weak mpi_comm_spawn_multiple__ = mpi_comm_spawn_multiple_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_SPAWN_MULTIPLE,
                           mpi_comm_spawn_multiple,
                           mpi_comm_spawn_multiple_,
                           mpi_comm_spawn_multiple__,
                           mpi_comm_spawn_multiple_f,
                           (MPI_Fint *count, char *array_of_commands, char *array_of_argv, MPI_Fint *array_of_maxprocs, MPI_Fint *array_of_info, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *intercomm, MPI_Fint *array_of_errcodes, MPI_Fint *ierr, int cmd_string_len, int argv_string_len),
                           (count, array_of_commands, array_of_argv, array_of_maxprocs, array_of_info, root, comm, intercomm, array_of_errcodes, ierr, cmd_string_len, argv_string_len) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_comm_spawn_multiple_f(MPI_Fint *count, char *array_commands,
			       char *array_argv,
			       MPI_Fint *array_maxprocs,
			       MPI_Fint *array_info, MPI_Fint *root,
			       MPI_Fint *comm, MPI_Fint *intercomm,
			       MPI_Fint *array_errcds, MPI_Fint *ierr,
			       int cmd_string_len, int argv_string_len)
{
    MPI_Comm c_comm, c_new_comm;
    MPI_Info *c_info;
    int size, array_size, i;
    int *c_errs;
    char **c_array_commands;
    char ***c_array_argv;
    OMPI_ARRAY_NAME_DECL(array_maxprocs);
    OMPI_ARRAY_NAME_DECL(array_errcds);
    
    c_comm = MPI_Comm_f2c(*comm);
    
    MPI_Comm_size(c_comm, &size);

    array_size = OMPI_FINT_2_INT(*count);

    /* It's allowed to ignore the errcodes */

    if (OMPI_IS_FORTRAN_ERRCODES_IGNORE(array_errcds)) {
        c_errs = MPI_ERRCODES_IGNORE;
    } else {
        OMPI_ARRAY_FINT_2_INT_ALLOC(array_errcds, size);
        c_errs = OMPI_ARRAY_NAME_CONVERT(array_errcds);
    }

    /* It's allowed to have no argv */

    if (OMPI_IS_FORTRAN_ARGVS_NULL(array_argv)) {
        c_array_argv = MPI_ARGVS_NULL;
    } else {
	ompi_fortran_multiple_argvs_f2c(OMPI_FINT_2_INT(*count), array_argv, 
					argv_string_len, &c_array_argv);
    }

    OMPI_ARRAY_FINT_2_INT(array_maxprocs, array_size);
    
    ompi_fortran_argv_f2c(array_commands, cmd_string_len, 
                          cmd_string_len, &c_array_commands);
	
    c_info = (MPI_Info *) malloc (array_size * sizeof(MPI_Info));
    for (i = 0; i < array_size; ++i) {
	c_info[i] = MPI_Info_f2c(array_info[i]);
    }

    *ierr = 
	OMPI_INT_2_FINT(MPI_Comm_spawn_multiple(OMPI_FINT_2_INT(*count),
				       c_array_commands,
				       c_array_argv, 
				       OMPI_ARRAY_NAME_CONVERT(array_maxprocs),
				       c_info,
				       OMPI_FINT_2_INT(*root),
				       c_comm, &c_new_comm,
				       c_errs));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *intercomm = MPI_Comm_c2f(c_new_comm);
    }

    if (!OMPI_IS_FORTRAN_ERRCODES_IGNORE(array_errcds)) {
	OMPI_ARRAY_INT_2_FINT(array_errcds, size);
    }
    OMPI_ARRAY_FINT_2_INT_CLEANUP(array_maxprocs);

    opal_argv_free(c_array_commands);

    if (MPI_ARGVS_NULL != c_array_argv && NULL != c_array_argv) {
	for (i = 0; i < OMPI_FINT_2_INT(*count); ++i) { 
	    opal_argv_free(c_array_argv[i]);
	}
    }
    free(c_array_argv);
}
