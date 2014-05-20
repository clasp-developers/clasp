dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
dnl                         reserved. 
dnl Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# OMPI_CHECK_COMPILER_WORKS(language, headers, body,
#            [action-if-found], [action-if-not-found])
# ----------------------------------------------------
# Try to compile and run a simple application in 'language'.  A
# warning is always printed if the application fails to run.
# Action-if-found is evaluated if the application runs successfully
# (or compiles if cross-compiling), and action-if-not-found is
# evaluated if the application fails to run.
#
# headers are any headers needed to compile the body (e.g., #include
# statements), and body is the program to compile.  It should include
# a clean exit from the application (e.g., "return 0" in C/C++, empty in
# fortran).
AC_DEFUN([OMPI_CHECK_COMPILER_WORKS],
[   AS_VAR_PUSHDEF([lang_var], [ompi_cv_$1_works])

    AC_CACHE_CHECK([if $1 compiler works], lang_var,
        [AC_LANG_PUSH($1)
         AC_RUN_IFELSE([AC_LANG_PROGRAM([$2], [$3])],
                       [AS_VAR_SET(lang_var, ["yes"])],
                       [AS_VAR_SET(lang_var, ["no"])],
                       [AC_LINK_IFELSE([AC_LANG_PROGRAM([$2], [$3])],
                           [AS_VAR_SET(lang_var, ["links (cross compiling)"])],
                           [AS_VAR_SET(lang_var, ["no"])])])
         AC_LANG_POP($1)])
    AS_VAR_IF(lang_var, [no], 
          [cat <<EOF >&2
**********************************************************************
* It appears that your $1 compiler is unable to produce working
* executables.  A simple test application failed to properly
* execute.  Note that this is likely not a problem with Open MPI,
* but a problem with the local compiler installation.  More
* information (including exactly what command was given to the 
* compiler and what error resulted when the command was executed) is
* available in the config.log file in this directory.
**********************************************************************
EOF
           $5], [$4])

    AS_VAR_POPDEF([lang_var])dnl
])
