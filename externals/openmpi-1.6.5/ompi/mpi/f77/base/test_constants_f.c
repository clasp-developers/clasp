/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/f77/bindings.h"
#include "ompi/mpi/f77/constants.h"

/* This is an internal test function for Open MPI; it does not have a
   profiled equivalent. */

PN(void, ompi_test_fortran_constants, OMPI_TEST_FORTRAN_CONSTANTS, (char *bottom, char *in_place, char *argv, char *argvs, char *status, char *statuses, MPI_Fint *flag));

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak OMPI_TEST_FORTRAN_CONSTANTS = ompi_test_fortran_constants_f
#pragma weak ompi_test_fortran_constants = ompi_test_fortran_constants_f
#pragma weak ompi_test_fortran_constants_ = ompi_test_fortran_constants_f
#pragma weak ompi_test_fortran_constants__ = ompi_test_fortran_constants_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS
OMPI_GENERATE_F77_BINDINGS (OMPI_TEST_FORTRAN_CONSTANTS,
                            ompi_test_fortran_constants,
                            ompi_test_fortran_constants_,
                            ompi_test_fortran_constants__,
                            ompi_test_fortran_constants_f,
                            (char *bottom, char *in_place, char *argv, char *argvs, char *status, char *statuses, MPI_Fint *flag),
                            (bottom, in_place, argv, argvs, status, statuses, flag) )
#endif

void ompi_test_fortran_constants_f(char *bottom, char *in_place,
                                   char *argv, char *argvs,
                                   char *status, char *statuses,
                                   MPI_Fint *flag)
{
    *flag = 1;
    if (!OMPI_IS_FORTRAN_BOTTOM(bottom)) {
        fprintf(stderr, "WARNING: Fortran MPI_BOTTOM not recognized properly\n");
        *flag = 0;
    }
    if (!OMPI_IS_FORTRAN_IN_PLACE(in_place)) {
        fprintf(stderr, "WARNING: Fortran MPI_IN_PLACE not recognized properly\n");
        *flag = 0;
    }
    if (!OMPI_IS_FORTRAN_ARGV_NULL(argv)) {
        fprintf(stderr, "WARNING: Fortran MPI_ARGV_NULL not recognized properly\n");
        *flag = 0;
    }
    if (!OMPI_IS_FORTRAN_ARGVS_NULL(argvs)) {
        fprintf(stderr, "WARNING: Fortran MPI_ARGVS_NULL not recognized properly\n");
        *flag = 0;
    }
    if (!OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
        fprintf(stderr, "WARNING: Fortran MPI_STATUS_IGNORE not recognized properly\n");
        *flag = 0;
    }
    if (!OMPI_IS_FORTRAN_STATUSES_IGNORE(statuses)) {
        fprintf(stderr, "WARNING: Fortran MPI_STATUSES not recognized properly\n");
        *flag = 0;
    }
}
