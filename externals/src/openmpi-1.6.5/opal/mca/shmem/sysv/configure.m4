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
# Copyright (c) 2010-2011 Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_shmem_sysv_CONFIG(action-if-can-compile,
#                       [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_shmem_sysv_CONFIG],[
    # do we have the sysv shm stuff?
    AC_MSG_CHECKING([if want SYSV shared memory support])
    AC_ARG_ENABLE(sysv-shmem,
        AC_HELP_STRING([--disable-sysv-shmem],
                       [disable sysv shared memory support (default: enabled)]))
    AS_IF([test "$enable_sysv_shmem" = "no"],
          [AC_MSG_RESULT([no])
           shmem_sysv_sm_build_sysv=0],
          [AC_MSG_RESULT([yes])
          AC_CHECK_FUNC(shmget,
                  [shmem_sysv_sm_build_sysv=1],
                  [shmem_sysv_sm_build_sysv=0])])
    AS_IF([test "$enable_sysv_shmem" = "yes" -a "$shmem_sysv_sm_build_sysv" = "0"],
          [AC_MSG_WARN([SYSV shared memory support requested but not found])
           AC_MSG_ERROR([Cannot continue])])

    AS_IF([test "$shmem_sysv_sm_build_sysv" = "1"], [$1], [$2])

    AC_DEFINE_UNQUOTED([OPAL_SHMEM_SYSV],
                       [$shmem_sysv_sm_build_sysv],
                       [Whether we have shared memory support for SYSV or not])
])dnl
