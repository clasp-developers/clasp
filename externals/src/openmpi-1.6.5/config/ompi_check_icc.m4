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
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OMPI_CHECK_ICC_VARARGS],[
dnl
dnl On EM64T, icc-8.1 before version 8.1.027 segfaulted, since 
dnl va_start was miscompiled...
dnl
AC_MSG_CHECKING([whether icc-8.1 for EM64T works with variable arguments])
AC_TRY_RUN([
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

void func (int c, char * f, ...)
{
  va_list arglist;
  va_start (arglist, f);
  /* vprintf (f, arglist); */
  va_end (arglist);
}

int main ()
{
  FILE *f;
  func (4711, "Help %d [%s]\n", 10, "ten");
  f=fopen ("conftestval", "w");
  if (!f) exit (1);
  return 0;
}

],[ompi_ac_icc_varargs=`test -f conftestval`],[ompi_ac_icc_varargs=1],[ompi_ac_icc_varargs=1])

if test "$ompi_ac_icc_varargs" = "1"; then
    AC_MSG_WARN([*** Problem running configure test!])
    AC_MSG_WARN([*** Your icc-8.1 compiler seems to miscompile va_start!])
    AC_MSG_WARN([*** Please upgrade compiler to at least version 8.1.027])
    AC_MSG_ERROR([*** Cannot continue.])
fi

AC_MSG_RESULT([yes])

rm -rf conftest*])dnl
