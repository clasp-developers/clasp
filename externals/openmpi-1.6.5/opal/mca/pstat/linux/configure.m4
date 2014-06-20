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
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_pstat_linux_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_pstat_linux_CONFIG],[

   case "${host}" in
   i?86-*linux*|x86_64*linux*|ia64-*linux*|powerpc-*linux*|powerpc64-*linux*|sparc*-*linux*)
              AS_IF([test -r "/proc/cpuinfo"],
                     [pstat_linux_happy="yes"],
                     [pstat_linux_happy="no"])
        ;;
   *)
        pstat_linux_happy="no"
        ;;
   esac

   AS_IF([test "$pstat_linux_happy" = "yes"], 
         [AC_CHECK_DECLS([HZ],
            [], [pstat_linux_happy="no"], [AC_INCLUDES_DEFAULT
#include <sys/param.h>
])])

   AS_IF([test "$pstat_linux_happy" = "yes"], 
         [$1], 
         [$2])
])
