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
# Copyright (c) 2006      Sandia National Laboratories. All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# MCA_btl_ofud_CONFIG([action-if-can-compile], 
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_btl_ofud_CONFIG],[
    OMPI_CHECK_OPENIB([btl_ofud],
                        [btl_ofud_happy="yes"],
                        [btl_ofud_happy="no"])

    AS_IF([test "$btl_ofud_happy" = "yes"],
          [btl_ofud_WRAPPER_EXTRA_LDFLAGS="$btl_ofud_LDFLAGS"
           btl_ofud_WRAPPER_EXTRA_LIBS="$btl_ofud_LIBS"
           $1],
          [$2])


    # substitute in the things needed to build OFUD
    AC_SUBST([btl_ofud_CFLAGS])
    AC_SUBST([btl_ofud_CPPFLAGS])
    AC_SUBST([btl_ofud_LDFLAGS])
    AC_SUBST([btl_ofud_LIBS])
])dnl
