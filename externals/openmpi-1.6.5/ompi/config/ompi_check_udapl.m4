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
# Copyright (c) 2007-2009 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# OMPI_CHECK_UDAPL(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if uDAPL support can be found.  sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_UDAPL],[
    AC_ARG_WITH([udapl],
        [AC_HELP_STRING([--with-udapl(=DIR)],
             [Build uDAPL support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    OMPI_CHECK_WITHDIR([udapl], [$with_udapl], [include/dat/udat.h])
    AC_ARG_WITH([udapl-libdir],
       [AC_HELP_STRING([--with-udapl-libdir=DIR],
             [Search for uDAPL libraries in DIR])])
    OMPI_CHECK_WITHDIR([udapl-libdir], [$with_udapl_libdir], [libdat.*])

    # Special case for OFED/Linux: the default /etc/dat.conf that
    # ships with OFED is broken in that it includes DAT providers that
    # are not guarnateed to work (e.g., it includes providers for ib0,
    # ib1, ib2, ib3, and bond0).  Usually, a sysadmin will need to
    # edit this file to configure it for the specific environment in
    # which it will be used.  Hence, if you run the udapl BTL on
    # Linux/OFED, you'll get a bunch of warning messages about the
    # providers that don't work.  However, on Linux/OFED, you don't
    # really want to use udapl anyway; you likely really want to use
    # the openib BTL (i.e., native verbs, not udapl).  

    # So after exploring many different scenarios, the least evil
    # solution seemed to be to disable building the udapl BTL on
    # Linux/OFED *unless the user specifically asks for it.* To be
    # specific: on Linux/OFED, if you do not specify
    # --with-udapl(=DIR), the udapl BTL will not be built.
    AS_IF([test -z "$with_udapl"],
          [case $host in
              *linux*) 
                  AC_MSG_WARN([On Linux and --with-udapl was not specified])
                  AC_MSG_WARN([Not building the udapl BTL])
                  with_udapl=no
                  ;;
           esac])

    AS_IF([test ! -z "$with_udapl" -a "$with_udapl" != "yes"],
          [ompi_check_udapl_dir="$with_udapl"])
    AS_IF([test ! -z "$with_udapl_libdir" -a "$with_udapl_libdir" != "yes"],
          [ompi_check_udapl_libdir="$with_udapl_libdir"])
    AS_IF([test "$with_udapl" = "no"],
          [ompi_check_udapl_happy="no"],
          [ompi_check_udapl_happy="yes"])

dnl Do not use ompi_check_package directly, because then we have
dnl to test for the header file twice, and caching is disabled
dnl for all ompi_check_package checks.  Instead, do what
dnl ompi_check_package does, but only do the header check once.
dnl Still do the lib check twice, the second time if it turns
dnl out we need -ldapl to link (looks like udapl over GM).

    ompi_check_package_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_package_$1_save_LDFLAGS="$LDFLAGS"
    ompi_check_package_$1_save_LIBS="$LIBS"

    ompi_check_package_$1_orig_CPPFLAGS="$$1_CPPFLAGS"
    ompi_check_package_$1_orig_LDFLAGS="$$1_LDFLAGS"
    ompi_check_package_$1_orig_LIBS="$$1_LIBS"

    AS_IF([test "$ompi_check_udapl_happy" = "yes"],
          [_OMPI_CHECK_PACKAGE_HEADER([$1], 
                [dat/udat.h],
                [$ompi_check_udapl_dir],
                [ompi_check_udapl_happy="yes"],
                [ompi_check_udapl_happy="no"])])

    AS_IF([test "$ompi_check_udapl_happy" = "yes"],
          [_OMPI_CHECK_PACKAGE_LIB([$1],
                [dat],
                [dat_registry_list_providers],
                [],
                [$ompi_check_udapl_dir],
                [$ompi_check_udapl_libdir],
                [ompi_check_udapl_happy="yes"],
                [_OMPI_CHECK_PACKAGE_LIB([$1],
                      [dat],
                      [dat_registry_list_providers],
                      [-ldapl],
                      [$ompi_check_udapl_dir],
                      [$ompi_check_udapl_libdir],
                      [ompi_check_udapl_happy="yes"],
                      [ompi_check_udapl_happy="no"])])])

    CPPFLAGS="$ompi_check_package_$1_save_CPPFLAGS"
    LDFLAGS="$ompi_check_package_$1_save_LDFLAGS"
    LIBS="$ompi_check_package_$1_save_LIBS"

    AS_IF([test "$ompi_check_udapl_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_udapl" -a "$with_udapl" != "no"],
                 [AC_MSG_ERROR([uDAPL support requested but not found.  Aborting])])
           $3])
])

