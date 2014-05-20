dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl
dnl There was some mentioning of broken qsort happened for Solaris that could
dnl cause qsort to return a bad pointer which could cause some badness.
dnl The problem should have been corrected with these patches from SunSolve.
dnl Solaris 10 should be free from this problem.
dnl
dnl 5.8_sparc #108827-27 or later
dnl 5.8_x86   #108828-28 or later
dnl 5.9_sparc #112874-20 or later
dnl 5.9_x86   #114432-07 or later
dnl
dnl For users who could not patch their systems or are convinced that their
dnl native qsort is broken, they could specify this configure flag to use 
dnl the opal_qsort instead.

# check for broken qsort
# OMPI_CHECK_BROKEN_QSORT(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([OMPI_CHECK_BROKEN_QSORT],[
  AC_ARG_WITH([broken-qsort],
              [AC_HELP_STRING([--with-broken-qsort],
                              [Build with FreeBSD qsort instead of native qsort (default: no)])])
  AC_MSG_CHECKING([for broken qsort])

  if test "$with_broken_qsort" = "yes"; then
    result="yes"
    define_result=1
  else
    result="no"
    define_result=0
  fi
  AC_MSG_RESULT([$result])
  AC_DEFINE_UNQUOTED([OPAL_HAVE_BROKEN_QSORT], [$define_result],
                     [whether qsort is broken or not])
])
