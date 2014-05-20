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
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/f77/bindings.h"

/* These functions solely exist so that the F90 bindings can call a C
   function from Fortran, so we provide all 4 variants.  Specifically,
   the F90 bindings for MPI_WTICK and MPI_WTIME need to call the
   back-end C functions to effect the functionality -- they cannot
   call the back-end F77 functions because there is an overload of
   names and types.  So we create these new functions with different
   names (MPI_WTICK_F90 and MPI_WTIME_F90 vs. MPI_WTICK and MPI_WTIME)
   so that the F90 bindings can call these functions directly.

   Rather than try to be clever with weak symbols, especially since
   the function implementations are 1 line long, it just seemed
   simpler to provide 4 copies of each [1 line] function. */

OMPI_DECLSPEC void MPI_WTIME_F90(double *w);
OMPI_DECLSPEC void mpi_wtime_f90(double *w);
OMPI_DECLSPEC void mpi_wtime_f90_(double *w);
OMPI_DECLSPEC void mpi_wtime_f90__(double *w);

OMPI_DECLSPEC void MPI_WTICK_F90(double *w);
OMPI_DECLSPEC void mpi_wtick_f90(double *w);
OMPI_DECLSPEC void mpi_wtick_f90_(double *w);
OMPI_DECLSPEC void mpi_wtick_f90__(double *w);

/**********************************************************************/

void MPI_WTIME_F90(double *w)
{
    *w = MPI_Wtime();
}

void mpi_wtime_f90(double *w)
{
    *w = MPI_Wtime();
}

void mpi_wtime_f90_(double *w)
{
    *w = MPI_Wtime();
}

void mpi_wtime_f90__(double *w)
{
    *w = MPI_Wtime();
}

/**********************************************************************/

void MPI_WTICK_F90(double *w)
{
    *w = MPI_Wtick();
}

void mpi_wtick_f90(double *w)
{
    *w = MPI_Wtick();
}

void mpi_wtick_f90_(double *w)
{
    *w = MPI_Wtick();
}

void mpi_wtick_f90__(double *w)
{
    *w = MPI_Wtick();
}

