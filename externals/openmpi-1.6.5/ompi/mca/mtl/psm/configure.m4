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


# MCA_mtl_psm_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_mtl_psm_CONFIG],[
    OMPI_CHECK_PSM([mtl_psm],
                     [mtl_psm_happy="yes"],
                     [mtl_psm_happy="no"])

    AS_IF([test "$mtl_psm_happy" = "yes"],
          [mtl_psm_WRAPPER_EXTRA_LDFLAGS="$mtl_psm_LDFLAGS"
           mtl_psm_WRAPPER_EXTRA_LIBS="$mtl_psm_LIBS"
           $1],
          [$2])

    # substitute in the things needed to build psm
    AC_SUBST([mtl_psm_CFLAGS])
    AC_SUBST([mtl_psm_CPPFLAGS])
    AC_SUBST([mtl_psm_LDFLAGS])
    AC_SUBST([mtl_psm_LIBS])
])dnl

