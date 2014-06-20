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

AC_DEFUN([MCA_timer_altix_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

AC_DEFUN([MCA_timer_altix_POST_CONFIG],[
    AS_IF([test "$1" = "1"], [timer_base_include="altix/timer_altix.h"])
])dnl

# MCA_timer_altix_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_timer_altix_CONFIG],[
    AC_ARG_WITH([timer],
        [AC_HELP_STRING([--with-timer=TYPE],
                        [Build high resolution timer component TYPE])])

    AS_IF([test "$with_timer" = "altix"],
          [timer_altix_happy="yes"
           timer_altix_should_use=1],
          [timer_altix_should_use=0
           AS_IF([test "$with_timer" = ""],
                 [timer_altix_happy="yes"],
                 [timer_altix_happy="no"])])

    AS_IF([test "$timer_altix_happy" = "yes"],
          [AC_CHECK_HEADER([sn/mmtimer.h],
			   [timer_altix_happy="yes"],
			   [timer_altix_happy="no"])])

    AS_IF([test "$timer_altix_happy" = "yes"],
          [AC_CHECK_HEADERS([sys/ioctl.h sys/mman.h])])

    AS_IF([test "$timer_altix_happy" = "yes"],
          [AC_CACHE_CHECK([if MM timer can be opened],
           	          [ompi_cv_mm_timer_mmap],
                          [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sn/mmtimer.h>
], [
    int fd;
    fd = open(MMTIMER_FULLNAME, O_RDONLY);
    if (-1 ==fd) return 1;])],
                                             [ompi_cv_mm_timer_mmap="yes"],
			                     [ompi_cv_mm_timer_mmap="no"])])
           AS_IF([test "$ompi_cv_mm_timer_mmap" = "yes"],
	         [timer_altix_happy="yes"],
                 [timer_altix_happy="no"])])

    AS_IF([test "$timer_altix_happy" = "no" -a \
                "$timer_altix_should_use" = "1"],
          [AC_MSG_ERROR([Altix timer requested but not available.  Aborting.])])

    AS_IF([test "$timer_altix_happy" = "yes"], 
          [$1], 
          [$2])
])
