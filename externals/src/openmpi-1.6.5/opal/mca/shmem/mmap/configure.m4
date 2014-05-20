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

# MCA_shmem_mmap_CONFIG(action-if-can-compile,
#                       [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_shmem_mmap_CONFIG],[
    # do we have the mmap shm stuff?
    AC_MSG_CHECKING([if want mmap shared memory support])
    AC_ARG_ENABLE(mmap-shmem,
        AC_HELP_STRING([--disable-mmap-shmem],
                       [disable mmap shared memory support (default: enabled)]))
    AS_IF([test "$enable_mmap_shmem" = "no"],
          [AC_MSG_RESULT([no])
           shmem_mmap_sm_build_mmap=0],
          [AC_MSG_RESULT([yes])
           AC_SEARCH_LIBS([mmap], [c],
                  [shmem_mmap_sm_build_mmap=1],
                  [shmem_mmap_sm_build_mmap=0])])
    AS_IF([test "$enable_mmap_shmem" = "yes" -a "$shmem_mmap_sm_build_mmap" = "0"],
          [AC_MSG_WARN([mmap shared memory support requested but not found])
           AC_MSG_ERROR([Cannot continue])])

    AS_IF([test "$shmem_mmap_sm_build_mmap" = "1"], [$1], [$2])

    AC_DEFINE_UNQUOTED([OPAL_SHMEM_MMAP],
                       [$shmem_mmap_sm_build_mmap],
                       [Whether we have shared memory support for mmap or not])
])dnl
