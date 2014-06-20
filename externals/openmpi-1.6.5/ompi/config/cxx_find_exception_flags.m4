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

AC_DEFUN([OMPI_CXX_FIND_EXCEPTION_FLAGS],[
#
# Arguments: none
#
# Dependencies: none
#
# Get the exception handling flags for the C++ compiler.  Leaves
# CXXFLAGS undisturbed.
# Provides --with-exflags command line argument for configure as well.
#
# Sets OMPI_CXX_EXCEPTION_CXXFLAGS and OMPI_CXX_EXCEPTION_LDFLAGS as
# appropriate.
# Must call AC_SUBST manually
#

# Command line flags

AC_ARG_WITH(exflags,
  AC_HELP_STRING([--with-exflags],
                 [Specify flags necessary to enable C++ exceptions]), 
  ompi_force_exflags="$withval")

ompi_CXXFLAGS_SAVE="$CXXFLAGS"
AC_MSG_CHECKING([for compiler exception flags])

# See which flags to use

if test "$ompi_force_exflags" != ""; then

    # If the user supplied flags, use those

    ompi_exflags="$ompi_force_exflags"
elif test "$GXX" = "yes"; then

    # g++ has changed their flags a few times.  Sigh.

    CXXFLAGS="$CXXFLAGS -fexceptions"

    AC_LANG_SAVE
    AC_LANG_CPLUSPLUS
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[try { int i = 0; } catch(...) { int j = 2; }]])], ompi_happy=1, ompi_happy=0)

    if test "$ompi_happy" = "1"; then
	ompi_exflags="-fexceptions";
    else
	CXXFLAGS="$CXXFLAGS_SAVE -fhandle-exceptions"
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[try { int i = 0; } catch(...) { int j = 2; }]])], ompi_happy=1, ompi_happy=0)
	if test "$ompi_happy" = "1"; then
	    ompi_exflags="-fhandle-exceptions";
	fi
    fi
    AC_LANG_RESTORE
elif test "`basename $CXX`" = "KCC"; then

    # KCC flags

    ompi_exflags="--exceptions"
fi
CXXFLAGS="$ompi_CXXFLAGS_SAVE"

# Save the result

OMPI_CXX_EXCEPTIONS_CXXFLAGS="$ompi_exflags"
OMPI_CXX_EXCEPTIONS_LDFLAGS="$ompi_exflags"
if test "$ompi_exflags" = ""; then
    AC_MSG_RESULT([none necessary])
else
    AC_MSG_RESULT([$ompi_exflags])
fi

# Clean up

unset ompi_force_exflags ompi_CXXFLAGS_SAVE ompi_exflags ompi_happy])dnl

