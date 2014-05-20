# -*- shell-script -*-
#
# Copyright (c) 2007      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_CHECK_ELAN(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if Elan support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_ELAN],[
    AC_ARG_WITH([elan],
	[AC_HELP_STRING([--with-elan(=DIR)],
		[Build Elan (QsNet2) support, searching for libraries in DIR])])
    OMPI_CHECK_WITHDIR([elan], [$with_elan], [include/elan/elan.h])
    AC_ARG_WITH([elan-libdir],
	[AC_HELP_STRING([--with-elan-libdir=DIR],
		[Search for Elan (QsNet2) libraries in DIR])])
    OMPI_CHECK_WITHDIR([elan-libdir], [$with_elan_libdir], [libelan.*])
    
    AS_IF([test "$with_elan" != "no"],
	[AS_IF([test ! -z "$with_elan" -a "$with_elan" != "yes"],
		[ompi_check_elan_dir="$with_elan"])
	    AS_IF([test ! -z "$with_elan_libdir" -a "$with_elan_libdir" != "yes"],
		[ompi_check_elan_libdir="$with_elan_libdir"])
	    
	    OMPI_CHECK_PACKAGE([$1],
		[elan/elan.h],
		[elan],
		[elan_init],
		[],
		[$ompi_check_elan_dir],
		[$ompi_check_elan_libdir],
		[ompi_check_elan_happy="yes"],
		[ompi_check_elan_happy="no"])
	    ],
	[ompi_check_elan_happy="no"])
    
    AS_IF([test "$ompi_check_elan_happy" = "yes"],
	[$2],
	[AS_IF([test ! -z "$with_elan" -a "$with_elan" != "no"],
		[AC_MSG_ERROR([Elan (QsNet2) support requested but not found.  Aborting])])
	    $3])
    ])

# MCA_btl_elan_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_btl_elan_CONFIG],[
    OMPI_CHECK_ELAN([btl_elan],
                     [btl_elan_happy="yes"],
                     [btl_elan_happy="no"])

    AS_IF([test "$btl_elan_happy" = "yes"],
          [btl_elan_WRAPPER_EXTRA_LDFLAGS="$btl_elan_LDFLAGS"
           btl_elan_WRAPPER_EXTRA_LIBS="$btl_elan_LIBS"
           $1],
          [$2])

    # substitute in the things needed to build elan
    AC_SUBST([btl_elan_CFLAGS])
    AC_SUBST([btl_elan_CPPFLAGS])
    AC_SUBST([btl_elan_LDFLAGS])
    AC_SUBST([btl_elan_LIBS])
])dnl

