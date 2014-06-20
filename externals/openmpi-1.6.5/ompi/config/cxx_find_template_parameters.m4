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

AC_DEFUN([OMPI_CXX_FIND_TEMPLATE_PARAMETERS],[
#
# Arguments: none
#
# Dependencies: None
#
# Get the C++ compiler template parameters.
#
# Adds to CXXFLAGS

AC_MSG_CHECKING([for C++ compiler template parameters])
if test "$BASECXX" = "KCC"; then                              
  new_flags="--one_instantiation_per_object"
  CXXFLAGS="$CXXFLAGS $new_flags" 
else
  new_flags="none needed"
fi
AC_MSG_RESULT([$new_flags])

#
# Clean up
#
unset new_flags
])
