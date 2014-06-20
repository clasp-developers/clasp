dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
dnl                         reserved. 
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl
dnl OMPI_SETUP_F90
dnl
dnl sets:
dnl  F90                   : full pathname to compiler
dnl  BASEF90               : compiler name (no path)
dnl  OMPI_WANT_F90_BINDINGS : (actually set by ompi_configure_options, may be
dnl                          redefined here)
dnl  FC                    : Same as F90.  Side effect of AC_PROG_FC.  Should
dnl                          not be used
dnl defines:
dnl  OMPI_F90               : same as F90
dnl  OMPI_WANT_F90_BINDINGS :
dnl am_conditional:
dnl  OMPI_WANT_F90_BINDINGS :

# This macro is necessary to get the title to be displayed first.  :-)
AC_DEFUN([OMPI_SETUP_F90_BANNER],[
    ompi_show_subtitle "Fortran 90/95 compiler" 
])

# This macro is necessary because PROG_FC is REQUIREd by multiple
# places in SETUP_F90.
AC_DEFUN([OMPI_PROG_FC],[
    OMPI_VAR_SCOPE_PUSH([ompi_fcflags_save])
    ompi_fcflags_save="$FCFLAGS"
    AC_PROG_FC([gfortran f95 fort xlf95 ifort ifc efc pgf95 lf95 f90 xlf90 pgf90 epcf90])
    FCFLAGS="$ompi_fcflags_save"
    OMPI_VAR_SCOPE_POP
])dnl

AC_DEFUN([OMPI_SETUP_F90],[
    AC_REQUIRE([OMPI_SETUP_F90_BANNER])
    AC_REQUIRE([AC_PROG_GREP])

if test "$OMPI_WANT_F77_BINDINGS" = "0" ; then
    AC_MSG_WARN([*** Fortran 90/95 bindings implicitly disabled (because])
    AC_MSG_WARN([*** Fortran 77 bindings were disabled)])

    OMPI_WANT_F90_BINDINGS=0
    OMPI_F90="none"
    BASEF90="none"
    OMPI_F90_ABSOLUTE="none"
    if test "$enable_f90" = "yes"; then
        AC_MSG_WARN([*** but --enable-f90 was explicitly specified])
        AC_MSG_ERROR([Cannot continue])
    fi
elif test "$OMPI_WANT_F90_BINDINGS" = "0" ; then
    AC_MSG_WARN([*** Fortran 90/95 bindings disabled by user])
    OMPI_WANT_F90_BINDINGS=0
    OMPI_F90="none"
    BASEF90="none"
    OMPI_F90_ABSOLUTE="none"
else

    #
    # Check for the compiler
    #
    # Note that we don't actually *use* the fortran compiler to build
    # anything in OMPI; it's only used here in configure to find out
    # symbol conventions, type sizes, etc.  We also pass it down to
    # the wrapper compiler mpif90.
    #
    # Note that AC_PROG_FC will look for *any* fortran compiler, and
    # we don't want it to find an F77 compiler.  The AC docs don't
    # recommend using the "dialect" feature of AC_PROG_FC (and you can
    # only look for one of Fortran 90 or 95 -- not both), so instead
    # use the optional first parameter and steal the list of Fortran
    # compilers (excluding the f77 compiler names) from AC's default
    # list of 95 and 90 compilers and use it here.
    #

    # Must REQUIRE the PROG_FC macro and not call it directly here for
    # reasons well-described in the AC2.64 (and beyond) docs.
    AC_REQUIRE([OMPI_PROG_FC])

    if test -z "$FC"; then
        AC_MSG_WARN([*** Fortran 90/95 bindings disabled (could not find compiler)])
        OMPI_WANT_F90_BINDINGS=0
        OMPI_F90="none"
        BASEF90="none"
        OMPI_F90_ABSOLUTE="none"
    else
        OMPI_WANT_F90_BINDINGS=1
        OMPI_F90="$FC"
        set dummy $OMPI_F90
        OMPI_F90_ARGV0=[$]2
        BASEF90="`basename $OMPI_F90_ARGV0`"
        OMPI_WHICH([$OMPI_F90_ARGV0], [OMPI_F90_ABSOLUTE])
    fi
fi
# make sure the compiler actually works, if not cross-compiling
# Don't just use the AC macro so that we can have a pretty
# message.
AS_IF([test $OMPI_WANT_F90_BINDINGS -eq 1],
       [OMPI_CHECK_COMPILER_WORKS([Fortran], [], [], [],
           [AC_MSG_ERROR([Could not run a simple Fortran program.  Aborting.])])])

# check to see if the F77 and F90 compilers are compatible
AS_IF([test $OMPI_WANT_F90_BINDINGS -eq 1],
    [AC_MSG_CHECKING([whether $OMPI_F77 and $OMPI_F90 compilers are compatible])
     OMPI_INTL_F90_F77_INTERACTION([fortran_goodness=1], [fortran_goodness=0])
     if test "$fortran_goodness" = "0" ; then
         AC_MSG_RESULT([no])
         AC_MSG_WARN([*** Fortran 77 and Fortran 90 compilers are not link compatible])
         AC_MSG_WARN([*** Disabling MPI Fortran 90/95 bindings])
         OMPI_WANT_F90_BINDINGS=0
     else
         AC_MSG_RESULT([yes])
     fi
])

# BWB - FIX ME - remove once everyone updates to LT 2.0.
# 
AS_IF([test $OMPI_WANT_F90_BINDINGS -eq 1],
      [if test $BASEF77 != $BASEF90; then
           lt_ver=`$GREP '^VERSION' $srcdir/config/ltmain.sh | cut -f2 -d= | cut -f1 -d'.'`
           if test $lt_ver -lt 2 ; then 
               AC_MSG_ERROR([You appear to be trying to build the Fortran 90
 layer with Libtool 1.5.x or earlier and a Fortran 77 / Fortran 90 compiler
 combination that will not work with this configuration.  You must either
 use a different Fortran 77 / Fortran 90 compiler (one where it is the same
 compiler for both languages), upgrade to Libtool 2.x, or disable the
 Fortran 90 bindings.])
           fi
       fi
 ])

# OS X before 10.3 (deployment target) does not allow undefined common
# symbols in shared libraries.  Because we can't figure out how to
# implement MPI_STATUSES_IGNORE and friends wihtout common symbols, on
# OS X we can't build the F90 bindings as a shared library.
AC_MSG_CHECKING([for extra arguments to build a shared library])
case "$host" in
    *apple-darwin*)
        if test -z ${MACOSX_DEPLOYMENT_TARGET} ; then
            AC_MSG_RESULT([impossible -- -static])
            OMPI_F90_EXTRA_SHARED_LIBRARY_FLAGS="-static"
        else
            case ${MACOSX_DEPLOYMENT_TARGET} in
            10.[012])
                AC_MSG_RESULT([impossible -- -static])
                OMPI_F90_EXTRA_SHARED_LIBRARY_FLAGS="-static"
                ;;
            10.*)
                AC_MSG_RESULT([-Wl,-single_module])
                OMPI_F90_EXTRA_SHARED_LIBRARY_FLAGS="-Wl,-single_module"
            esac
        fi
    ;;
    *)
        AC_MSG_RESULT([none needed])
        OMPI_F90_EXTRA_SHARED_LIBRARY_FLAGS=""
    ;;
esac
AC_SUBST(OMPI_F90_EXTRA_SHARED_LIBRARY_FLAGS)

# if we're still good, then save the extra file types.  Do this last
# because it implies tests that should be invoked by the above tests
# (e.g., running the fortran compiler).
AS_IF([test $OMPI_WANT_F90_BINDINGS -eq 1],
       [AC_LANG_PUSH(Fortran)
        AC_FC_SRCEXT(f)
        AC_FC_SRCEXT(f90)
        AC_LANG_POP(Fortran)])

# Test to see if the F90 compilers likes the C++ exceptions flags.  If
# it doesn't, just abort.  We *could* handle this scenario (e.g.,
# probe the F90 compiler for what flags would be necessary), but we're
# kinda assuming that no one will care.  If they do, they'll e-mail us.
AC_MSG_CHECKING([to see if F90 compiler likes the C++ exception flags])
if test "$OMPI_WANT_F90_BINDINGS" = "0"; then
    AC_MSG_RESULT([skipped (no F90 bindings)])
elif test "$OMPI_CXX_EXCEPTIONS_CXXFLAGS" = ""; then
    AC_MSG_RESULT([skipped (no C++ exceptions flags)])
else
    FCFLAGS="$FFLAGS $OMPI_CXX_EXCEPTIONS_CXXFLAGS"
    AC_LANG_PUSH(Fortran)
    AC_COMPILE_IFELSE(AC_LANG_PROGRAM([], [[ INTEGER I
 I = 3]]),
                      [AC_MSG_RESULT([yes])],
                      [AC_MSG_RESULT([no])
                       AC_MSG_WARN([C++ exception flags are different between the C and C++ compilers; this configure script cannot currently handle this scenario.  Either disable C++ exception support or send mail to the Open MPI users list.])
                       AC_MSG_ERROR([*** Cannot continue])])
    AC_LANG_POP
fi

# All done -- save values
AC_DEFINE_UNQUOTED(OMPI_WANT_F90_BINDINGS, $OMPI_WANT_F90_BINDINGS,
    [Whether we want the MPI f90 bindings or not])
AC_DEFINE_UNQUOTED(OMPI_F90, "$OMPI_F90", [OMPI underlying F90 compiler])
AM_CONDITIONAL(OMPI_WANT_F90_BINDINGS, test "$OMPI_WANT_F90_BINDINGS" = "1")
AC_SUBST(OMPI_F90_ABSOLUTE)
unset fortran_goodness
])

#############################################################################

AC_DEFUN([OMPI_INTL_F90_F77_INTERACTION], [
# make sure that we can combine F90 and F77 code
AC_LANG_PUSH(Fortran)
# Fortran module
cat > conftestf77.f <<EOF
        subroutine Conf1_test()
        end
EOF
cat > conftestf90.f <<EOF
        program main
        call Conf1_test()
        end
EOF

# Try the compile
OMPI_LOG_COMMAND(
    [$OMPI_F90 $FCFLAGS $FCFLAGS_f -c conftestf90.f],
    OMPI_LOG_COMMAND(
        [$OMPI_F77 $FFLAGS -c conftestf77.f],
        OMPI_LOG_COMMAND(
            [$OMPI_F90 $FCFLAGS -o conftest conftestf90.o conftestf77.o $LDFLAGS $LIBS],
            [HAPPY=1],
            [HAPPY=0]),
	[HAPPY=0]),
    [HAPPY=0])

if test "$HAPPY" = "1"; then
   $1
else
    OMPI_LOG_MSG([here is the F77 program:], 1)
    OMPI_LOG_FILE([conftestf77.f])
    OMPI_LOG_MSG([here is the F90 program:], 1)
    OMPI_LOG_FILE([conftestf90.f])
    $2
fi

unset HAPPY ompi_conftest_h
rm -rf conftest*

AC_LANG_POP(Fortran)
])dnl
