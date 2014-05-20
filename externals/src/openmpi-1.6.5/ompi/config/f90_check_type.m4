dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# OMPI_F90_CHECK_TYPE([type, action if found, action if not found])
# -----------------------------------------------------------------
AC_DEFUN([OMPI_F90_CHECK_TYPE],[
    # Use of m4_translit suggested by Eric Blake:
    # http://lists.gnu.org/archive/html/bug-autoconf/2010-10/msg00016.html
    AS_VAR_PUSHDEF([type_var],
       m4_translit([[ompi_cv_f90_have_$1]], [*], [p]))

    # Determine Fortran datatype size.
    # First arg is type, 2nd arg is config var to define

    AC_CACHE_CHECK([if Fortran 90 compiler supports $1], type_var,
        [AC_LANG_PUSH([Fortran])
         AC_COMPILE_IFELSE([AC_LANG_SOURCE([[program main
    $1 :: x
end]])],
             [AS_VAR_SET(type_var, "yes")],
             [AS_VAR_SET(type_var, "no")])
         AC_LANG_POP([Fortran])])

    AS_VAR_IF(type_var, [yes], [$2], [$3])
    AS_VAR_POPDEF([type_var])dnl
])dnl
