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
# Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AC_DEFUN([MCA_timer_aix_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

AC_DEFUN([MCA_timer_aix_POST_CONFIG],[
    AS_IF([test "$1" = "1"], [timer_base_include="aix/timer_aix.h"])
])dnl


# MCA_timer_aix_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_timer_aix_CONFIG],[
    AC_ARG_WITH([timer],
        [AC_HELP_STRING([--with-timer=TYPE],
                        [Build high resolution timer component TYPE])])

    AS_IF([test "$with_timer" = "aix"],
          [timer_aix_happy="yes"
           timer_aix_should_use=1],
          [timer_aix_should_use=0
           AS_IF([test "$with_timer" = ""],
                 [timer_aix_happy="yes"],
                 [timer_aix_happy="no"])])

    AS_IF([test "$timer_aix_happy" = "yes"],
          [AC_CHECK_FUNC([time_base_to_time], 
                         [timer_aix_happy="yes"],
                         [timer_aix_happy="no"])])

    # look to see if -lpmapi is available
    timer_aix_LIBS=
    timer_aix_LIBS_SAVE="$LIBS"
    AS_IF([test "$timer_aix_happy" = "yes"],
          [AC_CHECK_LIB([pmapi],
                        [pm_cycles],
                        [LIBS="$LIBS -lpmapi"
                         timer_aix_LIBS="-lpmapi"],
                        [timer_aix_LIBS=""])])

    # get us a HAVE_PM_CYCLES #define
    AS_IF([test "$timer_aix_happy" = "yes"],
          [AC_CHECK_FUNCS([pm_cycles])
           AC_CHECK_HEADERS([pmapi.h])])
   LIBS="$timer_aix_LIBS_SAVE"

    AS_IF([test "$timer_aix_happy" = "no" -a \
                "$timer_aix_should_use" = "1"],
          [AC_MSG_ERROR([AIX timer requested but not available.  Aborting.])])

    AC_SUBST(timer_aix_LIBS)

    AS_IF([test "$timer_aix_happy" = "yes"], 
          [timer_aix_WRAPPER_EXTRA_LIBS="$timer_aix_LIBS"
           $1], 
          [$2])
])
