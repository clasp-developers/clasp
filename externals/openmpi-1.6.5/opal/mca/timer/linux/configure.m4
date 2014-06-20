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
# Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AC_DEFUN([MCA_timer_linux_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

AC_DEFUN([MCA_timer_linux_POST_CONFIG],[
    AS_IF([test "$1" = "1"], [timer_base_include="linux/timer_linux.h"])
])dnl

# MCA_timer_linux_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_timer_linux_CONFIG],[
    AC_ARG_WITH([timer],
        [AC_HELP_STRING([--with-timer=TYPE],
                        [Build high resolution timer component TYPE])])

    AS_IF([test "$with_timer" = "linux"],
          [timer_linux_happy="yes"
           timer_linux_should_use=1],
          [timer_linux_should_use=0
           AS_IF([test "$with_timer" = ""],
                 [timer_linux_happy="yes"],
                 [timer_linux_happy="no"])])

   case "${host}" in
   i?86-*linux*|x86_64*linux*|ia64-*linux*|powerpc-*linux*|powerpc64-*linux*|sparc*-*linux*)
        AS_IF([test "$timer_linux_happy" = "yes"],
              [AS_IF([test -r "/proc/cpuinfo"],
                     [timer_linux_happy="yes"],
                     [timer_linux_happy="no"])])
        ;;
   *)
        timer_linux_happy="no"
        ;;
   esac

   AS_IF([test "$timer_linux_happy" = "no" -a \
               "$timer_linux_should_use" = "1"],
         [AC_MSG_ERROR([Linux timer requested but not available.  Aborting.])])

    AS_IF([test "$timer_linux_happy" = "yes"], 
          [$1], 
          [$2])
])
