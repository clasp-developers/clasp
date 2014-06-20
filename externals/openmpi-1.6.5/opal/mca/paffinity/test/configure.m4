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
# Copyright (c) 2007-2009 Cisco Systems, Inc. All rights reserved.
# Copyright (c) 2008      Sun Microsystems, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_paffinity_test_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_paffinity_test_CONFIG],[
    # check to see if we have <mach/mach_host.h>
    # as this is a Darwin-specific thing and
    # we are a test module for that environment

    AC_MSG_CHECKING([if this is an --enable-debug build])
    AS_IF([test "$WANT_DEBUG" = "1"],
          [AC_MSG_RESULT([yes])
           AC_CHECK_HEADER([mach/mach_host.h], [$1], [$2])],
          [AC_MSG_RESULT([no (component disabled)])
           $2])
])dnl

