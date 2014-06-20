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


# MCA_btl_portals_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_btl_portals_CONFIG],[
    OMPI_CHECK_PORTALS([btl_portals],
                     [btl_portals_happy="yes"],
                     [btl_portals_happy="no"])

    AS_IF([test "$btl_portals_happy" = "yes"],
          [btl_portals_WRAPPER_EXTRA_LDFLAGS="$btl_portals_LDFLAGS"
           btl_portals_WRAPPER_EXTRA_LIBS="$btl_portals_LIBS"
           $1],
          [$2])

    AC_CHECK_HEADERS([catamount/cnos_mpi_os.h], [],
                      [AC_CHECK_HEADERS([cnos_mpi_os.h], [], [$2],
                          [AC_INCLUDES_DEFAULT])],
                      [AC_INCLUDES_DEFAULT])


    # substitute in the things needed to build portals
    AC_SUBST([btl_portals_CPPFLAGS])
    AC_SUBST([btl_portals_LDFLAGS])
    AC_SUBST([btl_portals_LIBS])
])dnl
