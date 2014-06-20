# -*- shell-script -*-
#
# Copyright (c) 2009-2011 Cisco Systems, Inc.  All rights reserved. 
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_sysinfo_linux_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_sysinfo_linux_CONFIG],[

    AC_REQUIRE([OPAL_CHECK_OS_FLAVORS])

    AC_MSG_CHECKING([if we are on Linux])
    # If we are on Linux, then we're happy.
    AS_IF([test "$opal_found_linux" = "yes"],
          [AC_MSG_RESULT([yes])
           $1],
          [AC_MSG_RESULT([no])
           $2])
])
