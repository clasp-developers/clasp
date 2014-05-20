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
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_ras_loadleveler_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_ras_loadleveler_CONFIG],[
    ORTE_CHECK_LOADLEVELER([ras_loadleveler], [ras_loadleveler_good=1], [ras_loadleveler_good=0])

    # if check worked, set wrapper flags if so.  
    # Evaluate succeed / fail
    AS_IF([test "$ras_loadleveler_good" = "1"],
          [ras_loadleveler_WRAPPER_EXTRA_LDFLAGS="$ras_loadleveler_LDFLAGS"
           ras_loadleveler_WRAPPER_EXTRA_LIBS="$ras_loadleveler_LIBS"
           $1],
          [$2])

    # set build flags to use in makefile
    AC_SUBST([ras_loadleveler_CPPFLAGS])
    AC_SUBST([ras_loadleveler_LDFLAGS])
    AC_SUBST([ras_loadleveler_LIBS])
])dnl
