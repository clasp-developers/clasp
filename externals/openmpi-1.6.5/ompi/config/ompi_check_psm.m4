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
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006      QLogic Corp. All rights reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# OMPI_CHECK_PSM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if PSM support can be found.  sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_PSM],[
    AC_ARG_WITH([psm],
        [AC_HELP_STRING([--with-psm(=DIR)],
             [Build PSM (Qlogic InfiniPath) support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    OMPI_CHECK_WITHDIR([psm], [$with_psm], [include/psm.h])
    AC_ARG_WITH([psm-libdir],
        [AC_HELP_STRING([--with-psm-libdir=DIR],
             [Search for PSM (QLogic InfiniPath PSM) libraries in DIR])])
    OMPI_CHECK_WITHDIR([psm-libdir], [$with_psm_libdir], [libpsm_infinipath.*])

    ompi_check_psm_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_psm_$1_save_LDFLAGS="$LDFLAGS"
    ompi_check_psm_$1_save_LIBS="$LIBS"

    AS_IF([test "$with_psm" != "no"],
          [AS_IF([test ! -z "$with_psm" -a "$with_psm" != "yes"],
                 [ompi_check_psm_dir="$with_psm"])
           AS_IF([test ! -z "$with_psm_libdir" -a "$with_psm_libdir" != "yes"],
                 [ompi_check_psm_libdir="$with_psm_libdir"])

           OMPI_CHECK_PACKAGE([$1],
                              [psm.h],
                              [psm_infinipath],
                              [psm_finalize],
			      [],
                              [$ompi_check_psm_dir],
                              [$ompi_check_psm_libdir],
                              [ompi_check_psm_happy="yes"],
                              [ompi_check_psm_happy="no"])],
          [ompi_check_psm_happy="no"])

    CPPFLAGS="$ompi_check_psm_$1_save_CPPFLAGS"
    LDFLAGS="$ompi_check_psm_$1_save_LDFLAGS"
    LIBS="$ompi_check_psm_$1_save_LIBS"

    AS_IF([test "$ompi_check_psm_happy" = "yes" -a "$enable_progress_threads" = "yes"],
          [AC_MSG_WARN([PSM driver does not currently support progress threads.  Disabling BTL.])
           ompi_check_psm_happy="no"])

    AS_IF([test "$ompi_check_psm_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_psm" -a "$with_psm" != "no"],
                 [AC_MSG_ERROR([PSM support requested but not found.  Aborting])])
           $3])
])

