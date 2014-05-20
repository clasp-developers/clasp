# -*- shell-script -*-
#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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


# MCA_mtl_portals_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_mtl_portals_CONFIG],[
    OMPI_CHECK_PORTALS([mtl_portals],
                     [mtl_portals_happy="yes"],
                     [mtl_portals_happy="no"])

    AS_IF([test "$mtl_portals_happy" = "yes"],
          [mtl_portals_WRAPPER_EXTRA_LDFLAGS="$mtl_portals_LDFLAGS"
           mtl_portals_WRAPPER_EXTRA_LIBS="$mtl_portals_LIBS"
           $1],
          [$2])

    # substitute in the things needed to build portals
    AC_SUBST([mtl_portals_CPPFLAGS])
    AC_SUBST([mtl_portals_LDFLAGS])
    AC_SUBST([mtl_portals_LIBS])
])dnl
