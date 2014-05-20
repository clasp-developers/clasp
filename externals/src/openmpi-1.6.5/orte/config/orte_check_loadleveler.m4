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
# Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# 1. if --with-loadleveler is given, always build
# 2. if --without-loadleveler is given, never build
# 3. if neither is given, build if-and-only-if the OS is Linux or AIX

# ORTE_CHECK_LOADLEVELER(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([ORTE_CHECK_LOADLEVELER],[
    AC_ARG_WITH([loadleveler],
                [AC_HELP_STRING([--with-loadleveler],
                                [Build LoadLeveler scheduler component (default: yes)])])

    if test "$with_loadleveler" = "no" ; then
        orte_check_loadleveler_happy="no"
    elif test "$with_loadleveler" = "" ; then
        # unless user asked, only build LoadLeveler component on Linux
        # and AIX (these are the platforms that LoadLeveler supports)
        case $host in
            *-linux*|*-aix*)
                orte_check_loadleveler_happy="yes"
                ;;
            *)
                orte_check_loadleveler_happy="no"
                ;;
        esac
    else
        orte_check_loadleveler_happy="yes"
    fi

    AS_IF([test "$orte_check_loadleveler_happy" = "yes"], 
          [$2], 
          [$3])
])
