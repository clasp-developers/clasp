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
dnl Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
dnl                         reserved. 
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl
dnl sets:
dnl  F77                   : full pathname to compiler
dnl  BASEF77               : compiler name (no path)
dnl  OMPI_WANT_F77_BINDINGS : (actually set by ompi_configure_options, may be
dnl                          redefined here)
dnl  FC                    : Same as F77.  Side effect of AC_PROG_FC.  Should
dnl                          not be used
dnl defines:
dnl  OMPI_F77               : same as F77
dnl  OMPI_WANT_F77_BINDINGS :
dnl am_conditional:
dnl  OMPI_WANT_F77_BINDINGS :

# This macro is necessary to get the title to be displayed first.  :-)
AC_DEFUN([OMPI_SETUP_F77_BANNER],[
    ompi_show_subtitle "Fortran 77 compiler" 
])

# This macro is necessary because PROG_FC is REQUIREd by multiple
# places in SETUP_F90.
AC_DEFUN([OMPI_PROG_F77],[
    OMPI_VAR_SCOPE_PUSH([ompi_fflags_save])
    ompi_fflags_save="$FFLAGS"
    AC_PROG_F77([gfortran g77 f77 xlf frt ifort pgf77 fort77 fl32 af77])
    FFLAGS="$ompi_fflags_save"
    OMPI_VAR_SCOPE_POP
])

AC_DEFUN([OMPI_SETUP_F77],[
    AC_REQUIRE([OMPI_SETUP_F77_BANNER])

#
# Check for the compiler
#
# Note that we don't actually *use* the fortran compiler to build
# anything in OMPI; it's only used here in configure to find out
# symbol conventions, type sizes, etc.  We also pass it down to
# the wrapper compiler mpif77.
#
# Always run this test, even if fortran isn't wanted so that F77 has
# value for the Fint tests
#

# Must REQUIRE the PROG_F77 macro and not call it directly here for
# reasons well-described in the AC2.64 (and beyond) docs.
AC_REQUIRE([OMPI_PROG_F77])

if test -z "$F77"; then
    AC_MSG_WARN([*** Fortran 77 bindings disabled (could not find compiler)])
    OMPI_WANT_F77_BINDINGS=0
    OMPI_F77="none"
    BASEF77="none"
    OMPI_F77_ABSOLUTE="none"
else
    OMPI_F77="$F77"
    set dummy $OMPI_F77
    OMPI_F77_ARGV0=[$]2
    BASEF77="`basename $OMPI_F77_ARGV0`"
    OMPI_WHICH([$OMPI_F77_ARGV0], [OMPI_F77_ABSOLUTE])
    
    if test "$OMPI_WANT_F77_BINDINGS" = "0" ; then
        AC_MSG_WARN([*** Fortran 77 bindings disabled by user])
        OMPI_WANT_F77_BINDINGS=0
    else
        OMPI_WANT_F77_BINDINGS=1
    fi
fi

# make sure the compiler actually works, if not cross-compiling
# Don't just use the AC macro so that we can have a pretty
# message.
AS_IF([test $OMPI_WANT_F77_BINDINGS -eq 1],
       [OMPI_CHECK_COMPILER_WORKS([Fortran 77], [], [], [], 
           [AC_MSG_ERROR([Could not run a simple Fortran 77 program.  Aborting.])])])

# now make sure we know our linking convention...
OMPI_F77_FIND_EXT_SYMBOL_CONVENTION

# Make sure we can link with C code...
AS_IF([test $OMPI_WANT_F77_BINDINGS -eq 1],
  [OMPI_LANG_LINK_WITH_C([Fortran 77], [],
    [cat <<EOF
**********************************************************************
It appears that your Fortran 77 compiler is unable to link against
object files created by your C compiler.  This typically indicates
one of a few possibilities:

  - A conflict between CFLAGS and FFLAGS
  - A problem with your compiler installation(s)
  - Different default build options between compilers (e.g., C
    building for 32 bit and Fortran building for 64 bit)
  - Incompatible compilers

Such problems can usually be solved by picking compatible compilers
and/or CFLAGS and FFLAGS.  More information (including exactly what
command was given to the compilers and what error resulted when the
commands were executed) is available in the config.log file in this
directory.
**********************************************************************
EOF
     AC_MSG_ERROR([C and Fortran 77 compilers are not link compatible.  Can not continue.])])])

# Test to see if the F77 compilers likes the C++ exceptions flags.  If
# it doesn't, just abort.  We *could* handle this scenario (e.g.,
# probe the F77 compiler for what flags would be necessary), but we're
# kinda assuming that no one will care.  If they do, they'll e-mail us.
AC_MSG_CHECKING([to see if F77 compiler likes the C++ exception flags])
if test $OMPI_WANT_F77_BINDINGS -eq 0; then
    AC_MSG_RESULT([skipped (no F77 bindings)])
elif test "$OMPI_CXX_EXCEPTIONS_CXXFLAGS" = ""; then
    AC_MSG_RESULT([skipped (no C++ exceptions flags)])
else
    FFLAGS="$FFLAGS $OMPI_CXX_EXCEPTIONS_CXXFLAGS"
    AC_LANG_PUSH(Fortran 77)
    AC_COMPILE_IFELSE(AC_LANG_PROGRAM([], [[        INTEGER I
        I = 3]]),
                      [AC_MSG_RESULT([yes])],
                      [AC_MSG_RESULT([no])
                       AC_MSG_WARN([C++ exception flags are different between the C and C++ compilers; this configure script cannot currently handle this scenario.  Either disable C++ exception support or send mail to the Open MPI users list.])
                       AC_MSG_ERROR([*** Cannot continue])])
    AC_LANG_POP
fi

# Per #1982, on OS X, we may need some esoteric linker flags in the
# wrapper compilers.  Assume that we need it for both F77 and FC flags
# (note that in an upcoming update where there will only be one
# Fortran compiler, anyway).
AC_MSG_CHECKING([to see if mpif77/mpif90 compilers need additional linker flags])
if test $OMPI_WANT_F77_BINDINGS -eq 0; then
    AC_MSG_RESULT([none (no F77 bindings)])
else
    case "$host" in
    *apple-darwin*)
        # Test whether -Wl,-commons,use_dylibs works; if it does, use it.
        LDFLAGS_save=$LDFLAGS
        LDFLAGS="$LDFLAGS -Wl,-commons,use_dylibs"
        AC_LANG_PUSH(Fortran 77)
        AC_LINK_IFELSE([AC_LANG_SOURCE([[       program test
        integer :: i
        end program]])],
                       [OMPI_FORTRAN_WRAPPER_FLAGS="-Wl,-commons,use_dylibs"
                        WRAPPER_EXTRA_FFLAGS="$WRAPPER_EXTRA_FFLAGS $OMPI_FORTRAN_WRAPPER_FLAGS"
                        WRAPPER_EXTRA_FCFLAGS="$WRAPPER_EXTRA_FCFLAGS $OMPI_FORTRAN_WRAPPER_FLAGS"],
                       [OMPI_FORTRAN_WRAPPER_FLAGS=none])
        AC_LANG_POP
        LDFLAGS=$LDFLAGS_save
        AC_MSG_RESULT([$OMPI_FORTRAN_WRAPPER_FLAGS])
        ;;
    *)
        AC_MSG_RESULT([none])
        ;;
    esac
fi

AC_DEFINE_UNQUOTED(OMPI_WANT_F77_BINDINGS, $OMPI_WANT_F77_BINDINGS,
    [Whether we want the MPI f77 bindings or not])
AC_DEFINE_UNQUOTED(OMPI_F77, "$OMPI_F77", [OMPI underlying F77 compiler])
AM_CONDITIONAL(OMPI_WANT_F77_BINDINGS, test "$OMPI_WANT_F77_BINDINGS" = "1")
AC_SUBST(OMPI_F77_ABSOLUTE)
])
