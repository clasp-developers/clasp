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
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# OMPI_CHECK_PORTALS(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if PORTALS support can be found.  sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_PORTALS],[
    # save compiler flags so that we don't alter them for later
    # components.
    check_portals_save_CPPFLAGS="$CPPFLAGS"
    check_portals_save_LDFLAGS="$LDFLAGS"
    check_portals_save_LIBS="$LIBS"

    check_portals_CPPFLAGS=
    check_portals_LDFLAGS=
    check_portals_LIBS=

    check_portals_configuration="none"
    ompi_check_portals_happy="yes"

    # Get some configuration information
    AC_ARG_WITH([portals],
        [AC_HELP_STRING([--with-portals(=DIR)],
             [Build Portals support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    OMPI_CHECK_WITHDIR([portals], [$with_portals], [include/portals3.h])
    AC_ARG_WITH([portals-config],
            AC_HELP_STRING([--with-portals-config],
                           [configuration to use for Portals support.
                            One of "utcp", "xt3", "xt3-modex".  (default: utcp)]))
    AC_ARG_WITH([portals-libs], 
        [AC_HELP_STRING([--with-portals-libs=LIBS],
                       [Libraries to link with for portals])])

    AC_MSG_CHECKING([for Portals configuration])
    if test "$with_portals_config" = "" ; then
        with_portals_config="utcp"
    elif test "$with_portals_config" = "redstorm" ; then
        with_portals_config="xt3"
    elif test "$with_portals_config" = "cnl_modex" ; then
	with_portals_config="xt3-modex"
    fi
    OMPI_PORTALS_UTCP=0
    OMPI_PORTALS_CRAYXT3=0
    OMPI_PORTALS_CRAYXT3_MODEX=0
    case "$with_portals_config" in
        "utcp")
            OMPI_PORTALS_UTCP=1
            OMPI_PORTALS_HAVE_EVENT_UNLINK=1
            check_portals_LIBS="-lp3utcp -lp3api -lp3lib -lp3rt -lp3utcp"
            check_portals_header_prefix=
            AC_MSG_RESULT([utcp])
            ;;
        "xt3")
            OMPI_PORTALS_CRAYXT3=1
            OMPI_PORTALS_HAVE_EVENT_UNLINK=0
            check_portals_LIBS=
            check_portals_header_prefix="portals/"
            AC_MSG_RESULT([Cray XT3])
            ;;
        "xt3-modex")
            OMPI_PORTALS_CRAYXT3_MODEX=1
            OMPI_PORTALS_HAVE_EVENT_UNLINK=0
            check_portals_LIBS=
            check_portals_header_prefix="portals/"
            AC_MSG_RESULT([Cray XT3 - Modex])
            ;;
        *)
            # ok to call ERROR here - the user specified something invalid.
            # that should be brought to his attention
            AC_MSG_ERROR([unknown Portals configuration.  Can not continue])
            ;;
    esac

    AC_DEFINE_UNQUOTED([OMPI_PORTALS_HAVE_EVENT_UNLINK], 
                        [$OMPI_PORTALS_HAVE_EVENT_UNLINK],
                        [Does Portals send a PTL_EVENT_UNLINK event])

    AC_DEFINE_UNQUOTED([OMPI_PORTALS_UTCP], [$OMPI_PORTALS_UTCP],
                       [Use the UTCP reference implementation of Portals])

    AC_DEFINE_UNQUOTED([OMPI_PORTALS_CRAYXT3], [$OMPI_PORTALS_CRAYXT3],
                       [Use the Cray XT-3 implementation of Portals])
    
    AC_DEFINE_UNQUOTED([OMPI_PORTALS_CRAYXT3_MODEX], [$OMPI_PORTALS_CRAYXT3_MODEX],
                       [Use the Cray XT-3 implementation of Portals using Modex])

    # Add correct -I and -L flags
    AS_IF([test -n "$with_portals"],
          [AS_IF([test -d "$with_portals/include"],
                 [check_portals_CPPFLAGS="-I$with_portals/include"
                  CPPFLAGS="$CPPFLAGS $check_portals_CPPFLAGS"], [])
           AS_IF([test -d "$with_portals/lib"],
                 [check_portals_LDFLAGS="-L$with_portals/lib"
                  LDFLAGS="$LDFLAGS $check_portals_LDFLAGS"], [])])

    # Try to find all the portals libraries (this is not fun!)
    if test -n "$with_portals_libs" ; then
        check_portals_LIBS=""
        for lib in $with_portals_libs ; do
            check_portals_LIBS="$check_portals_LIBS -l$lib"
        done
    fi

    # check for portals
    LIBS="$LIBS $check_portals_LIBS"
    AC_CHECK_HEADERS([${check_portals_header_prefix}portals3.h],
        [AC_MSG_CHECKING([if possible to link Portals application])
         AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <${check_portals_header_prefix}portals3.h>], 
                                         [int i; PtlInit(&i);])],
              [AC_MSG_RESULT([yes])
               ompi_check_portals_happy="yes"],
              [AC_MSG_RESULT([no])
               ompi_check_portals_happy="no"])],
        [ompi_check_portals_happy="no"])

    # Deal with static-only Portals UTCP libs.  See note in
    # ompi/mca/common/portals/configure.m4.  Then possibly cry.
    if test "$with_portals_config" = "utcp" ; then
        if test "$1" != "common_portals" ; then
            check_portals_LIBS=
        fi
    fi

    # reset the flags for the next test
    CPPFLAGS="$check_portals_save_CPPFLAGS"
    LDFLAGS="$check_portals_save_LDFLAGS"
    LIBS="$check_portals_save_LIBS"

    $1_CPPFLAGS="$check_portals_CPPFLAGS"
    $1_LDFLAGS="$check_portals_LDFLAGS"
    $1_LIBS="$check_portals_LIBS"

    AS_IF([test "$ompi_check_portals_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_portals" -a "$with_portals" != "no"],
                 [AC_MSG_ERROR([Portals support requested but not found.  Aborting])])
           $3])
])

