/* -*- c -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Function: - OS, CPU and compiler dependent configuration
 */

#ifndef OMPI_CONFIG_H
#define OMPI_CONFIG_H

#include "opal_config.h"

#define OMPI_IDENT_STRING OPAL_IDENT_STRING

/***********************************************************************
 *
 * OMPI-specific Fortran code that should be in ompi_config.h, but not
 * in the other projects.
 *
 **********************************************************************/

/* MPI_Fint is the same as ompi_fortran_INTEGER_t */
#define MPI_Fint ompi_fortran_integer_t

#if OMPI_HAVE_FORTRAN_REAL && OMPI_HAVE_FORTRAN_COMPLEX
/* * C type for Fortran COMPLEX */
typedef struct {
  ompi_fortran_real_t real;
  ompi_fortran_real_t imag;
} ompi_fortran_complex_t;
#endif

#if OMPI_HAVE_FORTRAN_REAL4 && OMPI_HAVE_FORTRAN_COMPLEX8
/* * C type for Fortran COMPLEX*8 */
typedef struct {
  ompi_fortran_real4_t real;
  ompi_fortran_real4_t imag;
} ompi_fortran_complex8_t;
#endif

#if OMPI_HAVE_FORTRAN_REAL8 && OMPI_HAVE_FORTRAN_COMPLEX16
/* * C type for Fortran COMPLEX*16 */
typedef struct {
  ompi_fortran_real8_t real;
  ompi_fortran_real8_t imag;
} ompi_fortran_complex16_t;
#endif

#if OMPI_HAVE_FORTRAN_REAL16 && OMPI_HAVE_FORTRAN_COMPLEX32
/* * C type for Fortran COMPLEX*32 */
typedef struct {
  ompi_fortran_real16_t real;
  ompi_fortran_real16_t imag;
} ompi_fortran_complex32_t;
#endif

#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
/* * C type for Fortran DOUBLE COMPLEX */
typedef struct {
  ompi_fortran_double_precision_t real;
  ompi_fortran_double_precision_t imag;
} ompi_fortran_double_complex_t;
#endif


#if defined(__WINDOWS__)

#  if defined(_USRDLL)    /* building shared libraries (.DLL) */
#    if defined(OMPI_EXPORTS)
#      define OMPI_DECLSPEC        __declspec(dllexport)
#      define OMPI_MODULE_DECLSPEC
#    else
#      define OMPI_DECLSPEC        __declspec(dllimport)
#      if defined(OMPI_MODULE_EXPORTS)
#        define OMPI_MODULE_DECLSPEC __declspec(dllexport)
#      else
#        define OMPI_MODULE_DECLSPEC __declspec(dllimport)
#      endif  /* defined(OMPI_MODULE_EXPORTS) */
#    endif  /* defined(OMPI_EXPORTS) */
#  else          /* building static library */
#    if defined(OMPI_IMPORTS)
#      define OMPI_DECLSPEC        __declspec(dllimport)
#    else
#      define OMPI_DECLSPEC
#    endif  /* defined(OMPI_IMPORTS) */
#    define OMPI_MODULE_DECLSPEC
#  endif  /* defined(_USRDLL) */

#else

#  if OPAL_C_HAVE_VISIBILITY
#    ifndef OMPI_DECLSPEC
#      define OMPI_DECLSPEC            __opal_attribute_visibility__("default")
#    endif
#    ifndef OMPI_MODULE_DECLSPEC
#      define OMPI_MODULE_DECLSPEC     __opal_attribute_visibility__("default")
#    endif
#  else
#    ifndef OMPI_DECLSPEC
#      define OMPI_DECLSPEC
#    endif
#    ifndef OMPI_MODULE_DECLSPEC
#      define OMPI_MODULE_DECLSPEC
#    endif
#  endif
#endif  /* defined(__WINDOWS__) */

#endif
