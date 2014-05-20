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

# MCA_ess_cnos_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_ess_cnos_CONFIG],[
    # check for cnos functions
    # a bit of a hack,,, we don't want ess_cnos if alps 
    # was requested, and we can't rely on build priority because 
    # ess_alps uses priorty 10 so that ess_hnp is built as well. 
    AC_CHECK_FUNC([cnos_get_rank], 
                  [ORTE_CHECK_ALPS([ess_cnos], [$2], [$1])],
                  [$2])
])dnl
