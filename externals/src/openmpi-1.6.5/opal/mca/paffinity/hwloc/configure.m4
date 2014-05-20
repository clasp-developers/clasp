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
# Copyright (c) 2007-2010 Cisco Systems, Inc. All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_paffinity_hwloc_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([MCA_paffinity_hwloc_CONFIG],[
    AC_REQUIRE([MCA_hwloc_CONFIG_REQUIRE])

    # All we check for is whether $OPAL_HAVE_HWLOC is 1.
    # See big comment in opal/mca/hwloc/configure.m4.
    AC_MSG_CHECKING([if hwloc is enabled])
    AS_IF([test $OPAL_HAVE_HWLOC -eq 1],
          [AC_MSG_RESULT([yes])
           $1],
          [AC_MSG_RESULT([no])
           $2])
])dnl
