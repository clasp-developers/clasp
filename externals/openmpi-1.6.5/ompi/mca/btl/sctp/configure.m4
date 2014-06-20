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
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# OMPI_CHECK_SCTP(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if SCTP support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_SCTP],[
    AC_ARG_WITH([sctp],
        [AC_HELP_STRING([--with-sctp(=DIR)],
                [Build SCTP support, searching for libraries in DIR])])
    OMPI_CHECK_WITHDIR([sctp], [$with_sctp], [include/netinet/sctp.h])
    AC_ARG_WITH([sctp-libdir],
        [AC_HELP_STRING([--with-sctp-libdir=DIR],
                [Search for SCTP libraries in DIR])])
    
    btl_sctp_CFLAGS="`echo $CFLAGS`"
#only try to build this on Solaris, Linux, Mac OS X, or some BSD variant    
    ompi_sctp_try_to_build="no"
    ompi_sctp_api_libname="sctp"
    case "$host" in
    *linux*)
        ompi_sctp_try_to_build="yes"
        AC_DEFINE(OMPI_MCA_BTL_SCTP_USE_ONE_TO_ONE_SOCKET, 0,
               [Default value for socket style to use with SCTP BTL])
        AC_DEFINE(OMPI_MCA_BTL_SCTP_CONCATENATES_IOVS, 0,
               [False if you can use iovec's directly with SCTP BTL])
        ;;
    *bsd*)
        case "$host" in
        *freebsd[789]*|*freebsd1[0-9]*)
            # FreeBSD >= 7 has SCTP in an unpatched default kernel with
            #  the SCTP API contained within libc. 
            ompi_sctp_api_libname="c"
            ;;
        *)
            # keep the default lib (sctp not c)
            ;;
        esac

        # only add -DFREEBSD once to get extra sin_len field
        btl_sctp_CFLAGS="`echo $btl_sctp_CFLAGS | sed 's/-DFREEBSD//g'`"
        btl_sctp_CFLAGS="$btl_sctp_CFLAGS -DFREEBSD"
        ompi_sctp_try_to_build="yes"
        AC_DEFINE(OMPI_MCA_BTL_SCTP_USE_ONE_TO_ONE_SOCKET, 0,
               [Default value for socket style to use with SCTP BTL])
        AC_DEFINE(OMPI_MCA_BTL_SCTP_CONCATENATES_IOVS, 0,
               [False if you can use iovec's directly with SCTP BTL])
        AC_MSG_WARN([Adding -DFREEBSD to set extra sin_len field in sockaddr.])
        ;;
# Mac OS X support for SCTP NKE. Adjustments should look like *bsd*...
    *darwin*)
        # only add -DFREEBSD once to get extra sin_len field
        btl_sctp_CFLAGS="`echo $btl_sctp_CFLAGS | sed 's/-DFREEBSD//g'`"
        btl_sctp_CFLAGS="$btl_sctp_CFLAGS -DFREEBSD"
        ompi_sctp_try_to_build="yes"
        AC_DEFINE(OMPI_MCA_BTL_SCTP_USE_ONE_TO_ONE_SOCKET, 0,
               [Default value for socket style to use with SCTP BTL])
        AC_DEFINE(OMPI_MCA_BTL_SCTP_CONCATENATES_IOVS, 0,
               [False if you can use iovec's directly with SCTP BTL])
        AC_MSG_WARN([Adding -DFREEBSD to set extra sin_len field in sockaddr.])
        ;;
    *solaris*)
        # Solaris SCTP stack makes different assumptions about one-to-many
        #   sockets so change the default to use one-to-one sockets
        ompi_sctp_try_to_build="yes"
        AC_DEFINE(OMPI_MCA_BTL_SCTP_USE_ONE_TO_ONE_SOCKET, 1,
               [Default value for socket style to use with SCTP BTL])
        AC_DEFINE(OMPI_MCA_BTL_SCTP_CONCATENATES_IOVS, 1,
               [False if you can use iovec's directly with SCTP BTL])
        ;;
    *)
        AC_MSG_WARN([Only build sctp BTL on Solaris, Linux, Mac OS X, and BSD variants])
        ;;
    esac

    OMPI_CHECK_WITHDIR([sctp-libdir], [$with_sctp_libdir], [lib${ompi_sctp_api_libname}.*])

    AS_IF([test "$with_sctp" != "no" -a "$ompi_sctp_try_to_build" = "yes"],
        [AS_IF([test ! -z "$with_sctp" -a "$with_sctp" != "yes"],
                [ompi_check_sctp_dir="$with_sctp"])
            AS_IF([test ! -z "$with_sctp_libdir" -a "$with_sctp_libdir" != "yes"],
                [ompi_check_sctp_libdir="$with_sctp_libdir"])
            AC_CHECK_HEADERS([netinet/in.h])
            OMPI_CHECK_PACKAGE([$1],
                [netinet/sctp.h],
                [$ompi_sctp_api_libname],
                [sctp_recvmsg],
                [],
                [$ompi_check_sctp_dir],
                [$ompi_check_sctp_libdir],
                [ompi_check_sctp_happy="yes"],
                [ompi_check_sctp_happy="no"],
                [
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
                ])
            ],
        [ompi_check_sctp_happy="no"])

    AS_IF([test "$ompi_check_sctp_happy" = "yes"],
        [$2],
        [AS_IF([test ! -z "$with_sctp" -a "$with_sctp" != "no"],
                [AC_MSG_ERROR([SCTP support requested but not found.  Aborting])])
            $3])
    ])


# MCA_btl_sctp_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_btl_sctp_CONFIG],[
    OMPI_CHECK_SCTP([btl_sctp],
                   [btl_sctp_happy="yes"],
                   [btl_sctp_happy="no"])

    AS_IF([test "$btl_sctp_happy" = "yes"],
          [btl_sctp_WRAPPER_EXTRA_LDFLAGS="$btl_sctp_LDFLAGS"
           btl_sctp_WRAPPER_EXTRA_LIBS="$btl_sctp_LIBS"
           btl_sctp_WRAPPER_EXTRA_CPPFLAGS="$btl_sctp_CPPFLAGS"
           btl_sctp_WRAPPER_EXTRA_CFLAGS="$btl_sctp_CFLAGS"
           $1],
          [$2])


    # substitute in the things needed to build sctp
    AC_SUBST([btl_sctp_CFLAGS])
    AC_SUBST([btl_sctp_CPPFLAGS])
    AC_SUBST([btl_sctp_LDFLAGS])
    AC_SUBST([btl_sctp_LIBS])
])dnl
