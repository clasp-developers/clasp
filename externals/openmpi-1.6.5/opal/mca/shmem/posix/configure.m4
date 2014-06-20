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

# MCA_shmem_posix_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_shmem_posix_CONFIG],[
    # do we have the posix shm stuff?
    AC_MSG_CHECKING([if want POSIX shared memory support])
    AC_ARG_ENABLE(posix-shmem,
        AC_HELP_STRING([--disable-posix-shmem],
                       [disable posix shared memory support (default: enabled)]))
    AS_IF([test "$enable_posix_shmem" = "no"],
          [AC_MSG_RESULT([no])
           shmem_posix_sm_build_posix=0],
          [AC_MSG_RESULT([yes])
           AC_SEARCH_LIBS([shm_open], [rt],
                  [shmem_posix_sm_build_posix=1],
                  [shmem_posix_sm_build_posix=0])])
    AS_IF([test "$enable_posix_shmem" = "yes" -a "$shmem_posix_sm_build_posix" = "0"],
          [AC_MSG_WARN([POSIX shared memory support requested but not found])
           AC_MSG_ERROR([Cannot continue])])

    AS_IF([test "$shmem_posix_sm_build_posix" = "1"], [$1], [$2])

    AC_DEFINE_UNQUOTED([OPAL_SHMEM_POSIX],
                       [$shmem_posix_sm_build_posix],
                       [Whether we have shared memory support for POSIX or not])
])dnl
