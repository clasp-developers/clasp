# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_odls_process_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_odls_process_CONFIG],[
    AC_CHECK_FUNC([CreateProcess], [$1], [$2])
])dnl
