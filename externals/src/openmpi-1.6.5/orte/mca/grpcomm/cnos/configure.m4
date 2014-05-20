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

# MCA_grpcomm_cnos_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_grpcomm_cnos_CONFIG],[
    grpcomm_cnos_happy="no"
    # see if we should enable super secret utcp support
    if test "$with_grpcomm_cnos" = "utcp" ; then
        grpcomm_cnos_happy="yes"
        grpcomm_cnos_barrier=0
    else
        # check for cnos functions
        AC_CHECK_FUNC([cnos_barrier], 
                      [grpcomm_cnos_happy="yes"
                       grpcomm_cnos_barrier=1], 
                      [grpcomm_cnos_happy="no"
                       grpcomm_cnos_barrier=0])
    fi

    AC_DEFINE_UNQUOTED([OMPI_GRPCOMM_CNOS_HAVE_BARRIER], [$grpcomm_cnos_barrier],
                       [whether to use cnos_barrier or not])
    AS_IF([test "$grpcomm_cnos_happy" = "yes"], [$1], [$2])
])dnl
