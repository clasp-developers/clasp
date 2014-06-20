# -*- shell-script -*-
#
# Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_CHECK_MXM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if MXM support can be found.  sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_MXM],[
    AC_ARG_WITH([mxm],
        [AC_HELP_STRING([--with-mxm(=DIR)],
             [Build Mellanox Messaging support])])
    OMPI_CHECK_WITHDIR([mxm], [$with_mxm], [include/mxm/api/mxm_api.h])
    AC_ARG_WITH([mxm-libdir],
        [AC_HELP_STRING([--with-mxm-libdir=DIR],
             [Search for Mellanox Messaging libraries in DIR])])
    OMPI_CHECK_WITHDIR([mxm-libdir], [$with_mxm_libdir], [libmxm.*])

    ompi_check_mxm_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_mxm_$1_save_LDFLAGS="$LDFLAGS"
    ompi_check_mxm_$1_save_LIBS="$LIBS"

    AS_IF([test "$with_mxm" != "no"],
          [AS_IF([test ! -z "$with_mxm" -a "$with_mxm" != "yes"],
                 [ompi_check_mxm_dir="$with_mxm"])
           AS_IF([test ! -z "$with_mxm_libdir" -a "$with_mxm_libdir" != "yes"],
                 [ompi_check_mxm_libdir="$with_mxm_libdir"])

           OMPI_CHECK_PACKAGE([$1],
                              [mxm/api/mxm_api.h],
                              [mxm],
                              [mxm_cleanup],
			      [],
                              [$ompi_check_mxm_dir],
                              [$ompi_check_mxm_libdir],
                              [ompi_check_mxm_happy="yes"],
                              [ompi_check_mxm_happy="no"])],
          [ompi_check_mxm_happy="no"])

    CPPFLAGS="$ompi_check_mxm_$1_save_CPPFLAGS"
    LDFLAGS="$ompi_check_mxm_$1_save_LDFLAGS"
    LIBS="$ompi_check_mxm_$1_save_LIBS"

    AS_IF([test "$ompi_check_mxm_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_mxm" -a "$with_mxm" != "no"],
                 [AC_MSG_ERROR([MXM support requested but not found.  Aborting])])
           $3])
])

