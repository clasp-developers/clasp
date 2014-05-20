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

# MCA_oob_tcp_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_oob_tcp_CONFIG],[
    # check for sockaddr_in (a good sign we have TCP)
    AC_CHECK_TYPES([struct sockaddr_in], 
                   [$1],
                   [$2], 
                   [AC_INCLUDES_DEFAULT
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif])
])dnl
