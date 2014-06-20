# -*- shell-script -*-
#
# Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2006-2008 Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
#                         reserved. 
# Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AC_DEFUN([OMPI_SETUP_MPI_FORTRAN],[
    
    #-----------
    # Fortran 77
    #-----------
    
    OMPI_SETUP_F77
    
    # This allows us to mark bogus types, but still have them be a valid
    # [sentinel] value
    
    AC_DEFINE([ompi_fortran_bogus_type_t], [int],
              [A bogus type that allows us to have sentinel type values that are still valid])
    
    # We want to set the #define's for all of these, so invoke the macros
    # regardless of whether we have F77 support or not.
    OMPI_F77_CHECK([CHARACTER], [yes],
                   [char, int32_t, int, int64_t, long long, long], [-1])
    
    OMPI_F77_CHECK([LOGICAL], [yes],
                   [char, int32_t, int, int64_t, long long, long], [-1])
    OMPI_F77_CHECK([LOGICAL*1], [yes],
                   [char, int8_t, short, int32_t, int, int64_t, long long, long], [1])
    OMPI_F77_CHECK([LOGICAL*2], [yes],
                   [short, int16_t, int32_t, int, int64_t, long long, long], [2])
    OMPI_F77_CHECK([LOGICAL*4], [yes],
                   [int32_t, int, int64_t, long long, long], [4])
    OMPI_F77_CHECK([LOGICAL*8], [yes],
                   [int, int64_t, long long, long], [8])
    
    OMPI_F77_CHECK([INTEGER], [yes],
                   [int32_t, int, int64_t, long long, long], [-1])
    OMPI_F77_CHECK([INTEGER*1], [no],
                   [char, int8_t, short, int, int64_t, long long, long], [1])
    OMPI_F77_CHECK([INTEGER*2], [no],
                   [short, int16_t, int32_t, int, int64_t, long long, long], [2])
    OMPI_F77_CHECK([INTEGER*4], [no],
                   [int32_t, int, int64_t, long long, long], [4])
    OMPI_F77_CHECK([INTEGER*8], [no],
                   [int, int64_t, long long, long], [8])
    OMPI_F77_CHECK([INTEGER*16], [no],
                   [int, int64_t, long long, long], [16])
    
    OMPI_F77_CHECK([REAL], [yes],
                   [float, double, long double], [-1])
    OMPI_F77_CHECK([REAL*2], [no],
                   [float, double, long double], [2])
    OMPI_F77_CHECK([REAL*4], [no],
                   [float, double, long double], [4])
    OMPI_F77_CHECK([REAL*8], [no],
                   [float, double, long double], [8])
    OMPI_F77_CHECK([REAL*16], [no],
                   [float, double, long double], [16])
    
    # In some compilers, the bit representation of REAL*16 is not the same
    # as the C counterpart that we found.  If this is the case, then we
    # want to disable reduction support for MPI_REAL16 (per ticket #1603).
    OMPI_F77_CHECK_REAL16_C_EQUIV
    
    OMPI_F77_CHECK([DOUBLE PRECISION], [yes],
                   [float, double, long double], [-1])
    
    OMPI_F77_CHECK([COMPLEX], [yes], [], [-1])
    # Double precision complex types are not standard, but many compilers support it.
    # Code should be wrapped with #ifdef OMPI_HAVE_FORTRAN_DOUBLE_COMPLEX
    OMPI_F77_CHECK([DOUBLE COMPLEX], [yes], [], [-1])
    
    # The complex*N tests are a bit different (note: the complex tests are
    # the same as all the rest, because complex is a composite of two
    # reals, which we *have* to have.  It's only the complex*N tests that
    # are different).  The fortran complex types are composites of the
    # real*(N/2) types.  So for us to support complex*N, two conditions
    # must be true:
    #
    # a) we must support real*(N/2) (i.e., compiler supports it and we
    #    have a back-end C type for it)
    # b) compiler supports complex*N
    
    OMPI_F77_CHECK([COMPLEX*8], [no], [], [8])
    OMPI_F77_CHECK([COMPLEX*16], [no], [], [16])
    OMPI_F77_CHECK([COMPLEX*32], [no], [], [32])
    
    # Regardless of whether we have fortran bindings, or even a fortran
    # compiler, get the max value for a fortran MPI handle (this macro
    # handles the case where we don't have a fortran compiler).
    
    OMPI_F77_GET_FORTRAN_HANDLE_MAX
    
    #
    # Check for Fortran compilers value of TRUE and for the correct assumption
    # on LOGICAL for conversion into what C considers to be a true value
    #
    OMPI_F77_GET_VALUE_TRUE
    OMPI_F77_CHECK_LOGICAL_ARRAY
    
    # How big should MPI_STATUS_SIZE be?  (i.e., the size of
    # MPI_STATUS, expressed in units of Fortran INTEGERs).  The C
    # equivalent of MPI_Status contains 4 C ints and a size_t.

    AC_MSG_CHECKING([for the value of MPI_STATUS_SIZE])
    OMPI_FORTRAN_STATUS_SIZE=0
    if test $OMPI_WANT_F77_BINDINGS -eq 0; then
        AC_MSG_RESULT([skipped (no Fortran bindings)])
    else
        bytes=`expr 4 \* $ac_cv_sizeof_int + $ac_cv_sizeof_size_t`
        num_integers=`expr $bytes / $OMPI_SIZEOF_FORTRAN_INTEGER`
        sanity=`expr $num_integers \* $OMPI_SIZEOF_FORTRAN_INTEGER`
        AS_IF([test "$sanity" != "$bytes"],
              [AC_MSG_RESULT([unknown!])
               AC_MSG_WARN([WARNING: Size of C int: $ac_cv_sizeof_int])
               AC_MSG_WARN([WARNING: Size of C size_t: $ac_cv_sizeof_size_t])
               AC_MSG_WARN([WARNING: Size of Fortran INTEGER: $OMPI_SIZEOF_FORTRAN_INTEGER])
               AC_MSG_WARN([Could not make this work out evenly...!])
               AC_MSG_ERROR([Cannot continue])])
        OMPI_FORTRAN_STATUS_SIZE=$num_integers
        AC_MSG_RESULT([$OMPI_FORTRAN_STATUS_SIZE Fortran INTEGERs])
    fi
    AC_SUBST(OMPI_FORTRAN_STATUS_SIZE)

    #
    # There are 2 layers to the MPI f77 layer. The only extra thing that
    # determine f77 bindings is that fortran can be disabled by user. In
    # such cases, we need to not build the target at all.  One layer
    # generates MPI_f77* bindings. The other layer generates PMPI_f77*
    # bindings. The following conditions determine whether each (or both)
    # these layers are built.
    #
    # Superceeding clause:
    #   - fortran77 bindings should be enabled, else everything is
    #     disabled
    # 1. MPI_f77* bindings are needed if:
    #   - Profiling is not required
    #   - Profiling is required but weak symbols are not
    #     supported
    # 2. PMPI_* bindings are needed if profiling is required.  Hence we
    # define 2 conditionals which tell us whether each of these layers
    # need to be built or NOT
    #
    AM_CONDITIONAL(WANT_MPI_F77_BINDINGS_LAYER,
                   test \( "$WANT_MPI_PROFILING" = 0 -o "$OMPI_PROFILING_COMPILE_SEPARATELY" = 1 \) -a "$OMPI_WANT_F77_BINDINGS" = 1)
    
    AM_CONDITIONAL(WANT_PMPI_F77_BINDINGS_LAYER,
                   test "$OMPI_WANT_F77_BINDINGS" = 1 -a "$WANT_MPI_PROFILING" = 1)
    
    #-----------
    # Fortran 90
    #-----------
    
    OMPI_SETUP_F90
    
    # Look for the fortran module compiler flag
    OMPI_F90_FIND_MODULE_INCLUDE_FLAG
    
    # Look for all the types
    OMPI_F90_IKINDS=""
    OMPI_F90_RKINDS=""
    OMPI_F90_CKINDS=""
    
    OMPI_F90_CHECK([CHARACTER])
    
    # LOGICAL and friends
    OMPI_F90_CHECK([LOGICAL])
    OMPI_F90_CHECK([LOGICAL*1], [1])
    OMPI_F90_CHECK([LOGICAL*2], [2])
    OMPI_F90_CHECK([LOGICAL*4], [4])
    OMPI_F90_CHECK([LOGICAL*8], [8])
    
    # INTEGER and friends
    OMPI_F90_CHECK([INTEGER])
    OMPI_F90_CHECK([INTEGER*1], [1])
    OMPI_F90_CHECK([INTEGER*2], [2])
    OMPI_F90_CHECK([INTEGER*4], [4])
    OMPI_F90_CHECK([INTEGER*8], [8])
    OMPI_F90_CHECK([INTEGER*16], [16])
    
    # REAL, DOUBLE PRECISION, REAL*4, *8, *16
    OMPI_F90_CHECK([REAL])
    OMPI_F90_CHECK([REAL*2], [2])
    OMPI_F90_CHECK([REAL*4], [4])
    OMPI_F90_CHECK([REAL*8], [8])
    OMPI_F90_CHECK([REAL*16], [16])
    OMPI_F90_CHECK([DOUBLE PRECISION])
    
    # COMPLEX, DOUBLE COMPLEX, COMPLEX*8, *16, *32
    OMPI_F90_CHECK([COMPLEX])
    OMPI_F90_CHECK([COMPLEX*8], [8])
    OMPI_F90_CHECK([COMPLEX*16], [16])
    OMPI_F90_CHECK([COMPLEX*32], [32])
    OMPI_F90_CHECK([DOUBLE COMPLEX])
    
    AC_SUBST(OMPI_F90_IKINDS)
    AC_SUBST(OMPI_F90_RKINDS)
    AC_SUBST(OMPI_F90_CKINDS)
    
    # get kind value for Fortran MPI_INTEGER_KIND (corresponding to
    # whatever is the same size as a F77 INTEGER -- for the
    # most-likely-will-never-occur case where F77 INTEGER is smaller than
    # an F90 INTEGER; see MPI-2 4.12.6.5
    if test "$OMPI_SIZEOF_FORTRAN_INTEGER" = "2"; then
        OMPI_F90_GET_INT_KIND(MPI_INTEGER_KIND, 4, OMPI_MPI_INTEGER_KIND)
    elif test "$OMPI_SIZEOF_FORTRAN_INTEGER" = "4"; then
        OMPI_F90_GET_INT_KIND(MPI_INTEGER_KIND, 9, OMPI_MPI_INTEGER_KIND)
    elif test "$OMPI_SIZEOF_FORTRAN_INTEGER" = "8"; then
        OMPI_F90_GET_INT_KIND(MPI_INTEGER_KIND, 18, OMPI_MPI_INTEGER_KIND)
    elif test "$OMPI_SIZEOF_FORTRAN_INTEGER" = "16"; then
        OMPI_F90_GET_INT_KIND(MPI_INTEGER_KIND, 19, OMPI_MPI_INTEGER_KIND)
        AC_MSG_ERROR([Cannot support Fortran MPI_INTEGER_KIND!])
    fi
    AC_SUBST(OMPI_MPI_INTEGER_KIND)
    
    # get kind value for Fortran MPI_ADDRESS_KIND (corresponding to
    # whatever is big enough to hold (void*))
    if test $ac_cv_sizeof_void_p = 2 ; then
        OMPI_F90_GET_INT_KIND(MPI_ADDRESS_KIND, 4, OMPI_MPI_ADDRESS_KIND)
    elif test $ac_cv_sizeof_void_p = 4 ; then
        OMPI_F90_GET_INT_KIND(MPI_ADDRESS_KIND, 9, OMPI_MPI_ADDRESS_KIND)
    elif test $ac_cv_sizeof_void_p = 8 ; then
        OMPI_F90_GET_INT_KIND(MPI_ADDRESS_KIND, 18, OMPI_MPI_ADDRESS_KIND)
    elif test $ac_cv_sizeof_void_p = 16 ; then
        OMPI_F90_GET_INT_KIND(MPI_ADDRESS_KIND, 19, OMPI_MPI_ADDRESS_KIND)
    else
        AC_MSG_ERROR([Cannot support Fortran MPI_ADDRESS_KIND!])
    fi
    AC_SUBST(OMPI_MPI_ADDRESS_KIND)
    
    # get kind value for Fortran MPI_OFFSET_KIND (corresponding to INTEGER*8)
    OMPI_F90_GET_INT_KIND(MPI_OFFSET_KIND, 18, OMPI_MPI_OFFSET_KIND)
    AC_SUBST(OMPI_MPI_OFFSET_KIND)
])
