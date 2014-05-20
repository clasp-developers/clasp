# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#


# MCA_mtl_mx_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_common_mx_CONFIG],[
    OMPI_CHECK_MX([common_mx],
                     [common_mx_happy="yes"],
                     [common_mx_happy="no"])

    AS_IF([test "$common_mx_happy" = "yes"],
          [common_mx_WRAPPER_EXTRA_LDFLAGS="$common_mx_LDFLAGS"
           common_mx_WRAPPER_EXTRA_LIBS="$common_mx_LIBS"
           $1],
          [$2])

    # substitute in the things needed to build mx
    AC_SUBST([common_mx_CFLAGS])
    AC_SUBST([common_mx_CPPFLAGS])
    AC_SUBST([common_mx_LDFLAGS])
    AC_SUBST([common_mx_LIBS])
])dnl

