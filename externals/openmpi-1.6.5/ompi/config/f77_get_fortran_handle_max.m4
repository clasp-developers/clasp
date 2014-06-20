dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2013      Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# OMPI_F77_GET_FORTRAN_HANDLE_MAX()
# ---------------------------------------------------------------
# Find the maximum value of fortran integers, then calculate
# min(INT_MAX, max fortran INTEGER).  This represents the maximum
# number of fortran MPI handle index.
AC_DEFUN([OMPI_F77_GET_FORTRAN_HANDLE_MAX],[
    AC_CACHE_CHECK([for max Fortran MPI handle index],
        [ompi_cv_f77_fortran_handle_max],
        [ # Find max fortran INTEGER value.  Set to sentinel value if we don't
         # have a Fortran compiler (e.g., if --disable-f77 was given). 
         if test "$OMPI_WANT_F77_BINDINGS" = "0" ; then
             ompi_fint_max=0
         else
             OPAL_COMPUTE_MAX_VALUE([$OMPI_SIZEOF_FORTRAN_INTEGER], [ompi_fint_max])
         fi

         # Get INT_MAX.  Compute a SWAG if we are cross compiling or something
         # goes wrong.
         rm -f conftest.out >/dev/null 2>&1
         AC_RUN_IFELSE([AC_LANG_PROGRAM([[
#include <stdio.h>
#include <limits.h>
]],[[FILE *fp = fopen("conftest.out", "w");
long cint = INT_MAX;
fprintf(fp, "%ld", cint);
fclose(fp);]])], 
             [ompi_cint_max=`cat conftest.out`], 
             [ompi_cint_max=0],
             [ #cross compiling is fun.  compute INT_MAX same as INTEGER max
              OPAL_COMPUTE_MAX_VALUE([$ac_cv_sizeof_int], [ompi_cint_max])])

         if test "$ompi_cint_max" = "0" ; then
             # wow - something went really wrong.  Be conservative
             ompi_cv_f77_fortran_handle_max=32767
         elif test "$ompi_fint_max" = "0" ; then
             # we aren't compiling Fortran - just set it to C INT_MAX
             ompi_cv_f77_fortran_handle_max=$ompi_cint_max
         else
             # take the lesser of C INT_MAX and Fortran INTEGER
             # max.  The resulting value will then be storable in
             # either type.  There's no easy way to do this in
             # the shell, so make the preprocessor do it.
             ompi_cv_f77_fortran_handle_max="( $ompi_fint_max < $ompi_cint_max ? $ompi_fint_max : $ompi_cint_max )"
          fi
          rm -f conftest.out > /dev/null 2>&1 ])

    AC_DEFINE_UNQUOTED([OMPI_FORTRAN_HANDLE_MAX],
        [$ompi_cv_f77_fortran_handle_max],
        [Max handle value for fortran MPI handles, effectively min(INT_MAX, max fortran INTEGER value)])
])dnl
