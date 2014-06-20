# -*- shell-script -*-
#
# Copyright (c) 2009      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_CHECK_KNEM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if knem support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_KNEM],[
    OMPI_VAR_SCOPE_PUSH([ompi_check_knem_happy ompi_check_knem_$1_save_CPPFLAGS ompi_check_knem_dir])
    AC_ARG_WITH([knem],
        [AC_HELP_STRING([--with-knem(=DIR)],
             [Build knem Linux kernel module support, searching for headers in DIR])])

    OMPI_CHECK_WITHDIR([knem], [$with_knem], [include/knem_io.h])
    ompi_check_knem_$1_save_CPPFLAGS="$CPPFLAGS"

    AS_IF([test "$with_knem" != "no"],
          [AS_IF([test ! -z "$with_knem" -a "$with_knem" != "yes"],
                 [ompi_check_knem_dir="$with_knem"])

           _OMPI_CHECK_PACKAGE_HEADER([$1],
                              [knem_io.h],
                              [$ompi_check_knem_dir],
                              [ompi_check_knem_happy="yes"],
                              [ompi_check_knem_happy="no"])],
          [ompi_check_knem_happy="no"])

    CPPFLAGS="$CPPFLAGS $$1_CPPFLAGS"

    # need at least version 0x0000000b
    AS_IF([test "$ompi_check_knem_happy" = "yes"],
          [AC_CACHE_CHECK([for knem ABI version 0xb or later],
                          [ompi_cv_knem_version_ok],
                          [AC_PREPROC_IFELSE(
                            [AC_LANG_PROGRAM([
#include <knem_io.h>
                             ],[
#if KNEM_ABI_VERSION < 0xc
#error "Version less than 0xc"
#endif
                             ])],
                            [ompi_cv_knem_version_ok=yes],
                            [ompi_cv_knem_version_ok=no])])])

    CPPFLAGS="$ompi_check_knem_$1_save_CPPFLAGS"

    AS_IF([test "$ompi_check_knem_happy" = "yes" -a "$ompi_cv_knem_version_ok" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_knem" -a "$with_knem" != "no"],
                 [AC_MSG_ERROR([KNEM support requested but not found.  Aborting])])
           $3])
    OMPI_VAR_SCOPE_POP
])dnl

# MCA_btl_sm_CONFIG([action-if-can-compile],
#                   [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_btl_sm_CONFIG],[
    OMPI_VAR_SCOPE_PUSH([btl_sm_knem_happy])
    OMPI_CHECK_KNEM([btl_sm],
        [btl_sm_knem_happy=1],
        [btl_sm_knem_happy=0])

    AC_DEFINE_UNQUOTED([OMPI_BTL_SM_HAVE_KNEM],
        [$btl_sm_knem_happy],
        [If knem support can be enabled])
    [$1]
    # substitute in the things needed to build KNEM
    AC_SUBST([btl_sm_CPPFLAGS])
    OMPI_VAR_SCOPE_POP
])dnl
