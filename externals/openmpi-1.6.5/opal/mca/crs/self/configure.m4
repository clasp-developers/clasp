# -*- shell-script -*-
#
# Copyright (c) 2004-2010 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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

# MCA_crs_self_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_crs_self_CONFIG],[
    # If we don't want FT, don't compile this component
    AS_IF([test "$ompi_want_ft_cr" = "1"],
        [crs_self_good="yes"],
        [crs_self_good="no"])

    # We need the dlfcn.h so we can access dlsym and friends
    AS_IF([test "$crs_self_good" = "yes"],
        [AC_CHECK_HEADER([dlfcn.h],
                         [crs_self_good="yes"],
                         [crs_self_good="no"])])

    # If they did not ask for dlopen support,
    # they probably do not want this component either
    AS_IF([test "$crs_self_good" = "yes"],
        [AS_IF([test "$OPAL_ENABLE_DLOPEN_SUPPORT" = "1"],
                [crs_self_good="yes"],
                [crs_self_good="no"])])

    AS_IF([test "$crs_self_good" = "yes"],
        [$1],
        [$2])

])dnl
