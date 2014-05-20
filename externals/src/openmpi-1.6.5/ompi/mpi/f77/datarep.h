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

/**
 * @file
 *
 * This file does two things:
 *
 * 1. Provides typedef for the Fortran versions of the callbacks
 * registered by MPI_REGISTER_DATAREP.  These typedefs are needed for
 * the Fortran MPI API.
 *
 * 2. Provides the sentinel value functions/function pointers for
 * Fortran's version of MPI_CONVERSION_FN_NULL, and some helpful
 * macros for testing whether an argument passed to a Fortran MPI API
 * function is that sentinel value or not.
 */

#ifndef OMPI_F77_DATAREP_H
#define OMPI_F77_DATAREP_H

#include "ompi_config.h"

#include "mpi.h"

BEGIN_C_DECLS

/** 
 * Function typedef for the conversion function pointer in
 * MPI_REGISTER_DATAREP */
typedef void (ompi_mpi2_fortran_datarep_conversion_fn_t)
    (char *userbuf, MPI_Fint *datatype, MPI_Fint *count, char *filebuf, 
     MPI_Offset *position, MPI_Aint *extra_state, MPI_Fint *ierr);

/** 
 * Function typedef for the extent function pointer in
 * MPI_REGISTER_DATAREP */
typedef void (ompi_mpi2_fortran_datarep_extent_fn_t)
    (MPI_Fint *datatype, MPI_Aint *extent, MPI_Aint *extra_state, 
     MPI_Fint *ierr);

/** 
 * Macro for declaring each of the 5 back-end Fortran functions for
 * MPI_CONVERSION_FN_NULL.  We need the 4 fortran compiler convetions
 * and 1 for the "real" back-end function (even though these functions
 * are never invoked -- they're only used as sentinel values -- it's
 * simpler to use the same kind of code structure that we use for the
 * Fortran MPI API bindings and other callback functions).
 */
#define OMPI_DATAREP_FORTRAN_DECLARE(lower_name, upper_name, args) \
  OMPI_DECLSPEC void lower_name##_f args;    \
  OMPI_DECLSPEC void lower_name args;        \
  OMPI_DECLSPEC void lower_name##_ args;     \
  OMPI_DECLSPEC void lower_name##__ args;    \
  OMPI_DECLSPEC void upper_name args;

/*
 * Declare the 5 functions.
 */
OMPI_DATAREP_FORTRAN_DECLARE(mpi_conversion_fn_null, MPI_CONVERSION_FN_NULL, (char *userbuf, MPI_Fint *datatype, MPI_Fint *count, char *filebuf, MPI_Offset *position, MPI_Aint *extra_state, MPI_Fint *ierr))

/* Be social and remove this private macro from the global header file
   space */
#undef OMPI_DATAREP_FORTRAN_DECLARE

/** 
 * Declare the test macro in all of its forms.  This macro provides a
 * convenient way to check whether an argument is the sentinel value
 * MPI_CONVERSION_FN_NULL.
 */
#if OPAL_HAVE_WEAK_SYMBOLS
#define OMPI_IS_FORTRAN_CONVERSION_FN_NULL(addr) \
  (MPI_CONVERSION_FN_NULL == addr || \
   mpi_conversion_fn_null == addr || \
   mpi_conversion_fn_null_ == addr || \
   mpi_conversion_fn_null__ == addr)
#elif OMPI_F77_CAPS
#define OMPI_IS_FORTRAN_CONVERSION_FN_NULL(addr) \
  (MPI_CONVERSION_FN_NULL == addr)
#elif OMPI_F77_PLAIN
#define OMPI_IS_FORTRAN_CONVERSION_FN_NULL(addr) \
  (mpi_conversion_fn_null == addr)
#elif OMPI_F77_SINGLE_UNDERSCORE
#define OMPI_IS_FORTRAN_CONVERSION_FN_NULL(addr) \
  (mpi_conversion_fn_null_ == addr)
#else
#define OMPI_IS_FORTRAN_CONVERSION_FN_NULL(addr) \
  (mpi_conversion_fn_null__ == addr)
#endif

END_C_DECLS

#endif
