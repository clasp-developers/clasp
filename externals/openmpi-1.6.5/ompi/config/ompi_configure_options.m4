dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2006-2008 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2009      IBM Corporation.  All rights reserved.
dnl Copyright (c) 2009      Los Alamos National Security, LLC.  All rights
dnl                         reserved.
dnl Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
dnl
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([OMPI_CONFIGURE_OPTIONS],[
ompi_show_subtitle "OMPI Configuration options"

#
# Do we want to enable MPI interface warnings (e.g. deprecated functionality and others)?
#
# XXX This __disabled__ by default for 1.5, but will be __enabled__ for 1.7 by default
# Users should be notified about this proposed change.
#

AC_MSG_CHECKING([if want compile-time warnings inside of mpi.h])
AC_ARG_ENABLE(mpi-interface-warning,
    AC_HELP_STRING([--enable-mpi-interface-warning],
                   [enable warnings in wrong (e.g. deprecated) usage in user-level code (default: disabled)]))
if test "$enable_mpi_interface_warning" = "yes"; then
    AC_MSG_RESULT([yes])
    OMPI_WANT_MPI_INTERFACE_WARNING=1
else
    AC_MSG_RESULT([no])
    OMPI_WANT_MPI_INTERFACE_WARNING=0
fi
AC_DEFINE_UNQUOTED([OMPI_WANT_MPI_INTERFACE_WARNING], [$OMPI_WANT_MPI_INTERFACE_WARNING],
    [Enable warnings in wrong usage (e.g. deprecated) in user-level code])

#
# Sparse Groups
#

AC_MSG_CHECKING([if want sparse process groups])
AC_ARG_ENABLE(sparse-groups,
    AC_HELP_STRING([--enable-sparse-groups],
                   [enable sparse process groups (default: not enabled)]))
if test "$enable_sparse_groups" = "yes"; then
    AC_MSG_RESULT([yes])
    GROUP_SPARSE=1
else
    AC_MSG_RESULT([no])
    GROUP_SPARSE=0
fi
AC_DEFINE_UNQUOTED([OMPI_GROUP_SPARSE],$GROUP_SPARSE,
    [Wether we want sparse process groups])


#
# Do we want to enable peruse interface?
#

AC_MSG_CHECKING([if want peruse support])
AC_ARG_ENABLE(peruse,
    AC_HELP_STRING([--enable-peruse],
                   [enable PERUSE interface (default: disabled)]))
if test "$enable_peruse" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_PERUSE=1
else
    AC_MSG_RESULT([no])
    WANT_PERUSE=0
fi
AC_DEFINE_UNQUOTED([OMPI_WANT_PERUSE],
                   [$WANT_PERUSE],
                   [if the peruse interface should be enabled])
AM_CONDITIONAL(WANT_PERUSE, test "$WANT_PERUSE" = "1")

#
# Fortran 77
#

AC_MSG_CHECKING([if want Fortran 77 bindings])
AC_ARG_ENABLE(mpi-f77,
    AC_HELP_STRING([--enable-mpi-f77],
                   [enable f77 MPI bindings (default: enabled)]))
if test "$enable_mpi_f77" != "no"; then
    AC_MSG_RESULT([yes])
    OMPI_WANT_F77_BINDINGS=1
else
    AC_MSG_RESULT([no])
    OMPI_WANT_F77_BINDINGS=0
fi

#
# Fortran 90
#

AC_MSG_CHECKING([if want Fortran 90 bindings])
AC_ARG_ENABLE(mpi-f90,
    AC_HELP_STRING([--enable-mpi-f90],
                   [enable f90 MPI bindings (default: enabled)]))
if test "$enable_mpi_f90" != "no"; then
    AC_MSG_RESULT([yes])
    OMPI_WANT_F90_BINDINGS=1
else
    AC_MSG_RESULT([no])
    OMPI_WANT_F90_BINDINGS=0
fi

AC_MSG_CHECKING([Fortran 90 bindings "size"])
AC_ARG_WITH(mpi-f90-size,
    AC_HELP_STRING([--with-mpi-f90-size=SIZE],
                   [specify the types of functions in the Fortran 90 MPI module, where SIZE is one of: trivial (MPI-2 F90-specific functions only), small (trivial + all MPI functions without choice buffers), medium (small + all MPI functions with one choice buffer), large (medium + all MPI functions with 2 choice buffers, but only when both buffers are the same type).  Default SIZE is "small".]))

if test "$OMPI_WANT_F90_BINDINGS" = "0"; then
    AC_MSG_RESULT([disabled (Fortran 90 bindings disabled)])
elif test "$with_mpi_f90_size" = "no"; then
    OMPI_WANT_F90_BINDINGS=0
    AC_MSG_RESULT([disabling F90 MPI module (used specified)])
else
    # Default value
    if test "$with_mpi_f90_size" = ""; then
        with_mpi_f90_size=small
    fi

    # Check for each of the sizes
    if test "$with_mpi_f90_size" = "trivial"; then
        OMPI_F90_BUILD_SIZE=trivial
    elif test "$with_mpi_f90_size" = "small"; then
        OMPI_F90_BUILD_SIZE=small
    elif test "$with_mpi_f90_size" = "medium"; then
        OMPI_F90_BUILD_SIZE=medium
    elif test "$with_mpi_f90_size" = "large"; then
        OMPI_F90_BUILD_SIZE=large
    else
        AC_MSG_RESULT([Unrecognized size: $with_mpi_f90_size])
        AC_MSG_ERROR([Cannot continue])
    fi
fi

AM_CONDITIONAL([OMPI_WANT_BUILD_F90_TRIVIAL],
               [test "$OMPI_F90_BUILD_SIZE" = "trivial"])
AM_CONDITIONAL([OMPI_WANT_BUILD_F90_SMALL],
               [test "$OMPI_F90_BUILD_SIZE" = "small"])
AM_CONDITIONAL([OMPI_WANT_BUILD_F90_MEDIUM],
               [test "$OMPI_F90_BUILD_SIZE" = "medium"])
AM_CONDITIONAL([OMPI_WANT_BUILD_F90_LARGE],
               [test "$OMPI_F90_BUILD_SIZE" = "large"])

AC_SUBST(OMPI_F90_BUILD_SIZE)
if test "$OMPI_WANT_F90_BINDINGS" != "0"; then
    AC_MSG_RESULT([$OMPI_F90_BUILD_SIZE])
fi

#
# MPI profiling
#

AC_MSG_CHECKING([if want PMPI])
AC_ARG_ENABLE(mpi-profile,
    AC_HELP_STRING([--enable-mpi-profile],
                   [enable MPI profiling (default: enabled)]))
if test "$enable_mpi_profile" != "no"; then
    AC_MSG_RESULT([yes])
    WANT_MPI_PROFILING=1
    MPIF_H_PMPI_W_FUNCS=", PMPI_WTICK, PMPI_WTIME"
else
    AC_MSG_RESULT([no])
    WANT_MPI_PROFILING=0
    MPIF_H_PMPI_W_FUNCS=
fi
AC_SUBST(MPIF_H_PMPI_W_FUNCS)


#
# C++
#

AC_MSG_CHECKING([if want C++ bindings])
AC_ARG_ENABLE(mpi-cxx,
    AC_HELP_STRING([--enable-mpi-cxx],
                   [enable C++ MPI bindings (default: enabled)]))
if test "$enable_mpi_cxx" != "no"; then
    AC_MSG_RESULT([yes])
    WANT_MPI_CXX_SUPPORT=1
else
    AC_MSG_RESULT([no])
    WANT_MPI_CXX_SUPPORT=0
fi

AC_MSG_CHECKING([if want MPI::SEEK_SET support])
AC_ARG_ENABLE([mpi-cxx-seek],
    [AC_HELP_STRING([--enable-mpi-cxx-seek],
                   [enable support for MPI::SEEK_SET, MPI::SEEK_END, and MPI::SEEK_POS in C++ bindings (default: enabled)])])
if test "$enable_mpi_cxx_seek" != "no" ; then
  AC_MSG_RESULT([yes])
  OMPI_WANT_MPI_CXX_SEEK=1
else
  AC_MSG_RESULT([no])
  OMPI_WANT_MPI_CXX_SEEK=0
fi
AC_DEFINE_UNQUOTED([OMPI_WANT_MPI_CXX_SEEK], [$OMPI_WANT_MPI_CXX_SEEK],
    [do we want to try to work around C++ bindings SEEK_* issue?])


#
# Do we want to disable MPI parameter checking at run-time?
#

AC_MSG_CHECKING([if want run-time MPI parameter checking])
AC_ARG_WITH(mpi-param-check,
    AC_HELP_STRING([--with-mpi-param-check(=VALUE)],
                   [behavior of MPI function parameter checking.  Valid values are: always, never, runtime.  If --with-mpi-param-check is specified with no VALUE argument, it is equivalent to a VALUE of "always"; --without-mpi-param-check is equivalent to "never" (default: runtime).]))
mpi_param_check=ompi_mpi_param_check
if test "$with_mpi_param_check" = "no" -o \
    "$with_mpi_param_check" = "never"; then
    mpi_param_check=0
    AC_MSG_RESULT([never])
elif test "$with_mpi_param_check" = "yes" -o \
    "$with_mpi_param_check" = "always"; then
    mpi_param_check=1
    AC_MSG_RESULT([always])
elif test "$with_mpi_param_check" = "runtime" -o \
    -z "$with_mpi_params_check"; then
    AC_MSG_RESULT([runtime])
else
    AC_MSG_RESULT([unknown])
    AC_MSG_WARN([*** Unrecognized --with-mpi-param-check value])
    AC_MSG_WARN([*** See "configure --help" output])
    AC_MSG_WARN([*** Defaulting to "runtime"])
fi
AC_DEFINE_UNQUOTED(MPI_PARAM_CHECK, $mpi_param_check,
    [Whether we want to check MPI parameters always, never, or decide at run-time])


#
# What is the max array rank that we want to support in the f90 bindings?
#

OMPI_FORTRAN_MAX_ARRAY_RANK=4
AC_MSG_CHECKING([max supported array dimension in F90 MPI bindings])
AC_ARG_WITH(f90-max-array-dim,
    AC_HELP_STRING([--with-f90-max-array-dim=<DIM>],
                   [The maximum array dimension supported in the F90 MPI bindings (default: $OMPI_FORTRAN_MAX_ARRAY_RANK).]))
if test ! -z "$with_f90_max_array_dim" -a "$with_f90_max_array_dim" != "no"; then
    # Ensure it's a number (hopefully an integer!), and >=1 and <=7
    happy=1
    expr $with_f90_max_array_dim + 1 > /dev/null 2> /dev/null
    AS_IF([test "$?" != "0"], [happy=0],
          [expr $with_f90_max_array_dim \>= 1 \& $with_f90_max_array_dim \<= 7 > /dev/null 2>/dev/null
           AS_IF([test "$?" != "0"], [happy=0])])

    # If badness in the above tests, bail
    AS_IF([test "$happy" = "0"],
          [AC_MSG_RESULT([bad value ($with_f90_max_array_dim)])
           AC_MSG_WARN([--with-f90-max-array-dim value must be >=1 and <=7])
           AC_MSG_ERROR([Cannot continue])])
    OMPI_FORTRAN_MAX_ARRAY_RANK="$with_f90_max_array_dim"
fi
AC_MSG_RESULT([$OMPI_FORTRAN_MAX_ARRAY_RANK])
AC_SUBST(OMPI_FORTRAN_MAX_ARRAY_RANK)

])dnl

