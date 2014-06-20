# -*- shell-script -*-
#
# Copyright (c) 2009      The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2007      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# OMPI_CHECK_FTB(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if FTB (Fault Tolerance Backplane) support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_FTB],[
    AC_ARG_WITH([ftb],
        [AC_HELP_STRING([--with-ftb(=DIR)],
                [Build FTB (Fault Tolerance Backplane) support, searching for libraries in DIR])])
    AC_ARG_WITH([ftb-libdir],
        [AC_HELP_STRING([--with-ftb-libdir=DIR],
                [Search for FTB (Fault Tolerance Backplane) libraries in DIR])])
    
    AS_IF([test "$with_ftb" != "no"],
        [AS_IF([test ! -z "$with_ftb" -a "$with_ftb" != "yes"],
                [ompi_check_ftb_dir="$with_ftb"])
            AS_IF([test ! -z "$with_ftb_libdir" -a "$with_ftb_libdir" != "yes"],
                [ompi_check_ftb_libdir="$with_ftb_libdir"])
            
            OMPI_CHECK_PACKAGE([$1],
                [libftb.h],
                [ftb],
                [FTB_Connect],
                [],
                [$ompi_check_ftb_dir],
                [$ompi_check_ftb_libdir],
                [ompi_check_ftb_happy="yes"],
                [ompi_check_ftb_happy="no"])
            ],
        [ompi_check_ftb_happy="no"])
    
    AS_IF([test "$ompi_check_ftb_happy" = "yes"],
        [$2],
        [AS_IF([test ! -z "$with_ftb" -a "$with_ftb" != "no"],
                [AC_MSG_ERROR([FTB (Fault Tolerance Backplane) support requested but not found.  Aborting])])
            $3])
    ])


# MCA_notifier_ftb_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_notifier_ftb_CONFIG], [
    OMPI_CHECK_FTB([notifier_ftb],
                     [notifier_ftb_happy="yes"],
                     [notifier_ftb_happy="no"])

    AS_IF([test "$notifier_ftb_happy" = "yes"],
          [notifier_ftb_WRAPPER_EXTRA_LDFLAGS="$notifier_ftb_LDFLAGS"
           notifier_ftb_WRAPPER_EXTRA_LIBS="$notifier_ftb_LIBS"
           $1],
          [$2])

    # substitute in the things needed to build ftb
    AC_SUBST([notifier_ftb_CFLAGS])
    AC_SUBST([notifier_ftb_CPPFLAGS])
    AC_SUBST([notifier_ftb_LDFLAGS])
    AC_SUBST([notifier_ftb_LIBS])
])dnl
