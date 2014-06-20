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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# ORTE_CHECK_ALPS(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([ORTE_CHECK_ALPS],[
	AC_ARG_WITH([alps],
	    [AC_HELP_STRING([--with-alps],
		    [Build ALPS scheduler component (default: no)])])
	if test "$with_alps" = "yes" ; then
	    orte_check_alps_happy="yes"
           # Only need to do these tests once (this macro is invoked
           # from multiple different components' configure.m4 scripts
           if test -z "$orte_check_alps_pmi_happy"; then
               # if pmi support is requested, then ORTE_CHECK_PMI
               # will have added the -lpmi flag to LIBS. We then need
               # to add a couple of alps libs to support static
               # builds
               orte_check_alps_pmi_happy=no
               if test "$orte_enable_pmi" = 1 ; then
                   AC_MSG_CHECKING([for /usr/lib/alps])
                   AS_IF([test -d /usr/lib/alps],
                         [AC_MSG_RESULT([found])
                          orte_check_alps_pmi_happy=yes],
                         [AC_MSG_RESULT([not found])])

                   AS_IF([test "$orte_check_alps_pmi_happy" = "yes" -a "$orte_without_full_support" = 0],
                         [WRAPPER_EXTRA_LDFLAGS="$WRAPPER_EXTRA_LDFLAGS -L/usr/lib/alps"
                          WRAPPER_EXTRA_LIBS="$WRAPPER_EXTRA_LIBS -lalpslli -lalpsutil"],
                         [AC_MSG_WARN([PMI support for Alps requested but not found])
                          AC_MSG_ERROR([Cannot continue])])
               fi
           fi
        else
            orte_check_alps_happy="no"
	fi
	AS_IF([test "$orte_check_alps_happy" = "yes"], 
	    [$2], 
	    [$3])
	])
