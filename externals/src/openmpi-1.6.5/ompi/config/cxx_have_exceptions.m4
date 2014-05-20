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
dnl Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OMPI_CXX_HAVE_EXCEPTIONS],[
#
# Arguments: None
#
# Depdencies: None
#
# Check to see if the C++ compiler can handle exceptions 
#
# Sets OMPI_CXX_EXCEPTIONS to 1 if compiler has exceptions, 0 if not
#

AC_MSG_CHECKING([for throw/catch])
AC_LANG_SAVE
AC_LANG_CPLUSPLUS
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[int i=1; throw(i);]])], 
    OMPI_CXX_EXCEPTIONS=1, OMPI_CXX_EXCPTIONS=0)
if test "$OMPI_CXX_EXCEPTIONS" = "1"; then
    AC_MSG_RESULT([yes])
else
    AC_MSG_RESULT([no])
fi

# Clean up
AC_LANG_RESTORE])dnl
