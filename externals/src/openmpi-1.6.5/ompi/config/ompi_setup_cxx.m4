dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2006 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2006 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
dnl                         reserved. 
dnl Copyright (c) 2007-2009 Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# This macro is necessary to get the title to be displayed first.  :-)
AC_DEFUN([OMPI_SETUP_CXX_BANNER],[
    ompi_show_subtitle "C++ compiler and preprocessor" 
])

# This macro is necessary because PROG_CXX* is REQUIREd by multiple
# places in SETUP_CXX.
AC_DEFUN([OMPI_PROG_CXX],[
    OMPI_VAR_SCOPE_PUSH([ompi_cxxflags_save])
    ompi_cxxflags_save="$CXXFLAGS"
    AC_PROG_CXX
    AC_PROG_CXXCPP
    CXXFLAGS="$ompi_cxxflags_save"
    OMPI_VAR_SCOPE_POP
])

# OMPI_SETUP_CXX()
# ----------------
# Do everything required to setup the C++ compiler.  Safe to AC_REQUIRE
# this macro.
AC_DEFUN([OMPI_SETUP_CXX],[
    AC_REQUIRE([OMPI_SETUP_CXX_BANNER])

    _OMPI_SETUP_CXX_COMPILER

    _OMPI_CXX_CHECK_EXCEPTIONS

    AS_IF([test "$WANT_MPI_CXX_SUPPORT" = "1"],
          [OMPI_CXX_FIND_TEMPLATE_REPOSITORY
           OMPI_CXX_FIND_TEMPLATE_PARAMETERS
           OMPI_CHECK_IDENT([CXX], [CXXFLAGS], [cc], [C++])])

    _OMPI_CXX_CHECK_BUILTIN

    _OMPI_CXX_CHECK_2D_CONST_CAST

    AM_CONDITIONAL(WANT_MPI_CXX_BINDINGS, [test "$WANT_MPI_CXX_SUPPORT" = 1])
    AC_DEFINE_UNQUOTED(OMPI_WANT_CXX_BINDINGS, $WANT_MPI_CXX_SUPPORT,
        [Whether we want MPI C++ support or not])
])

# _OMPI_SETUP_CXX_COMPILER()
# --------------------------
# Setup the CXX compiler
AC_DEFUN([_OMPI_SETUP_CXX_COMPILER],[
    OMPI_VAR_SCOPE_PUSH(ompi_cxx_compiler_works)

    # There's a few cases here:
    #
    # 1. --enable-mpi-cxx was supplied: error if we don't find a C++
    #    compiler
    # 2. --disable-mpi-cxx was supplied: check for a C++ compiler anyway
    #    (so we can have a functional mpic++ wrapper compiler), but
    #    don't error if we don't find one.
    # 3. neither was specified: same was #2
    #
    # Then only proceed to do all the rest of the C++ checks if we
    # both found a c++ compiler and want the C++ bindings (i.e., either
    # case #1 or #3)

    # Must REQUIRE the PROG_CXX macro and not call it directly here for
    # reasons well-described in the AC2.64 (and beyond) docs.
    AC_REQUIRE([OMPI_PROG_CXX])
    BASECXX="`basename $CXX`"

    AS_IF([test "x$CXX" = "x"], [CXX=none])
    set dummy $CXX
    ompi_cxx_argv0=[$]2
    OMPI_WHICH([$ompi_cxx_argv0], [OMPI_CXX_ABSOLUTE])
    AS_IF([test "x$OMPI_CXX_ABSOLUTE" = "x"], [OMPI_CXX_ABSOLUTE=none])

    AC_DEFINE_UNQUOTED(OMPI_CXX, "$CXX", [OMPI underlying C++ compiler])
    AC_SUBST(OMPI_CXX_ABSOLUTE)

    # Make sure that the C++ compiler both works and is actually a C++
    # compiler (if not cross-compiling).  Don't just use the AC macro
    # so that we can have a pretty message.  Do something here that
    # should force the linking of C++-specific things (e.g., STL
    # strings) so that we can force a hard check of compiling,
    # linking, and running a C++ application.  Note that some C
    # compilers, such as at least some versions of the GNU and Intel
    # compilers, will detect that the file extension is ".cc" and
    # therefore switch into a pseudo-C++ personality which works for
    # *compiling*, but does not work for *linking*.  So in this test,
    # we want to cover the entire spectrum (compiling, linking,
    # running).  Note that it is not a fatal error if the C++ compiler
    # does not work unless the user specifically requested the C++
    # bindings.
    AS_IF([test "$CXX" = "none"],
          [ompi_cxx_compiler_works=no],
          [AS_IF([test "$ompi_cv_cxx_compiler_vendor" = "microsoft" ],
                 [ompi_cxx_compiler_works=yes],
                 [OMPI_CHECK_COMPILER_WORKS([C++], [#include <string>
], 
                                            [std::string foo = "Hello, world"],
                                            [ompi_cxx_compiler_works=yes],
                                            [ompi_cxx_compiler_works=no])])])

    AS_IF([test "$ompi_cxx_compiler_works" = "yes"],
          [_OMPI_SETUP_CXX_COMPILER_BACKEND],
          [AS_IF([test "$enable_mpi_cxx" = "yes"],
                 [AC_MSG_WARN([Could not find functional C++ compiler, but])
                  AC_MSG_WARN([support for the C++ MPI bindings was requested.])
                  AC_MSG_ERROR([Cannot continue])],
                 [WANT_MPI_CXX_SUPPORT=0])])

    AC_MSG_CHECKING([if able to build the MPI C++ bindings])
    AS_IF([test "$WANT_MPI_CXX_SUPPORT" = "1"],
          [AC_MSG_RESULT([yes])], 
          [AC_MSG_RESULT([no])
           AS_IF([test "$enable_mpi_cxx" = "yes"],
                 [AC_MSG_WARN([MPI C++ binding support requested but not delivered])
                  AC_MSG_ERROR([Cannot continue])])])

    AS_IF([test "$WANT_MPI_CXX_SUPPORT" = "1"],
          [OMPI_CXX_COMPILER_VENDOR([ompi_cxx_vendor])])
    OMPI_VAR_SCOPE_POP
])

# _OMPI_SETUP_CXX_COMPILER_BACKEND()
# ----------------------------------
# Back end of _OMPI_SETUP_CXX_COMPILER_BACKEND()
AC_DEFUN([_OMPI_SETUP_CXX_COMPILER_BACKEND],[
    # Do we want code coverage
    if test "$WANT_COVERAGE" = "1"; then 
        if test "$ompi_cxx_vendor" = "gnu" ; then
            AC_MSG_WARN([$OMPI_COVERAGE_FLAGS has been added to CFLAGS (--enable-coverage)])
            WANT_DEBUG=1
            CXXFLAGS="${CXXFLAGS} $OMPI_COVERAGE_FLAGS"
            WRAPPER_EXTRA_CXXFLAGS="${WRAPPER_EXTRA_CXXFLAGS} $OMPI_COVERAGE_FLAGS"
        else
            AC_MSG_WARN([Code coverage functionality is currently available only with GCC suite])
            AC_MSG_ERROR([Configure: cannot continue])
        fi
    fi

    # Do we want debugging?
    if test "$WANT_DEBUG" = "1" -a "$enable_debug_symbols" != "no" ; then
        CXXFLAGS="$CXXFLAGS -g"
        OMPI_UNIQ(CXXFLAGS)
        AC_MSG_WARN([-g has been added to CXXFLAGS (--enable-debug)])
    fi

    # These flags are generally g++-specific; even the g++-impersonating
    # compilers won't accept them.
    OMPI_CXXFLAGS_BEFORE_PICKY="$CXXFLAGS"
    if test "$WANT_PICKY_COMPILER" = 1 -a "$ompi_cxx_vendor" = "gnu"; then
        add="-Wall -Wundef -Wno-long-long"

        # see if -Wno-long-double works...
        AC_LANG_PUSH(C++)
        CXXFLAGS_orig="$CXXFLAGS"
        CXXFLAGS="$CXXFLAGS $add -Wno-long-double -fstrict-prototype"
        AC_CACHE_CHECK([if $CXX supports -Wno-long-double],
                   [ompi_cv_cxx_wno_long_double],
                   [AC_TRY_COMPILE([], [], 
                                   [ompi_cv_cxx_wno_long_double="yes"],
                                   [ompi_cv_cxx_wno_long_double="no"])])
        CXXFLAGS="$CXXFLAGS_orig"
        AC_LANG_POP(C++)
        if test "$ompi_cv_cxx_wno_long_double" = "yes" ; then
            add="$add -Wno-long-double"
        fi

        CXXFLAGS="$CXXFLAGS $add"
        OMPI_UNIQ(CXXFLAGS)
        if test "$add" != "" ; then
            AC_MSG_WARN([$add has been added to CXXFLAGS (--enable-picky)])
        fi
        unset add
    fi

    # See if this version of g++ allows -finline-functions
    if test "$GXX" = "yes"; then
        CXXFLAGS_orig="$CXXFLAGS"
        CXXFLAGS="$CXXFLAGS -finline-functions"
        add=
        AC_CACHE_CHECK([if $CXX supports -finline-functions],
                   [ompi_cv_cxx_finline_functions],
                   [AC_TRY_COMPILE([], [],
                                   [ompi_cv_cxx_finline_functions="yes"],
                                   [ompi_cv_cxx_finline_functions="no"])])
        if test "$ompi_cv_cxx_finline_functions" = "yes" ; then
            add=" -finline-functions"
        fi
        CXXFLAGS="$CXXFLAGS_orig$add"
        OMPI_UNIQ(CXXFLAGS)
        if test "$add" != "" ; then
            AC_MSG_WARN([$add has been added to CXXFLAGS])
        fi
        unset add
    fi

    # Make sure we can link with the C compiler
    if[ test "$ompi_cv_cxx_compiler_vendor" != "microsoft" ]; then
      OMPI_LANG_LINK_WITH_C([C++], [],
        [cat <<EOF >&2
**********************************************************************
* It appears that your C++ compiler is unable to link against object
* files created by your C compiler.  This generally indicates either
* a conflict between the options specified in CFLAGS and CXXFLAGS
* or a problem with the local compiler installation.  More
* information (including exactly what command was given to the 
* compilers and what error resulted when the commands were executed) is
* available in the config.log file in this directory.
**********************************************************************
EOF
         AC_MSG_ERROR([C and C++ compilers are not link compatible.  Can not continue.])])
    fi

    # If we are on HP-UX, ensure that we're using aCC
    case "$host" in
    *hpux*)
        if test "$BASECXX" = "CC"; then
            AC_MSG_WARN([*** You will probably have problems compiling the MPI 2])
            AC_MSG_WARN([*** C++ bindings with the HP-UX CC compiler.  You should])
            AC_MSG_WARN([*** probably be using the aCC compiler.  Re-run configure])
            AC_MSG_WARN([*** with the environment variable "CXX=aCC".])
        fi
        ;;
    esac

    # Note: gcc-imperonating compilers accept -O3
    if test "$WANT_DEBUG" = "1"; then
        OPTFLAGS=
    else
        if test "$GXX" = yes; then
            OPTFLAGS="-O3"
        else
            OPTFLAGS="-O"
        fi
    fi

    # config/ompi_ensure_contains_optflags.m4
    OMPI_ENSURE_CONTAINS_OPTFLAGS(["$CXXFLAGS"])
    AC_MSG_CHECKING([for C++ optimization flags])
    AC_MSG_RESULT([$co_result])
    CXXFLAGS="$co_result"

    # bool type size and alignment
    AC_LANG_PUSH(C++)
    AC_CHECK_SIZEOF(bool)
    OMPI_C_GET_ALIGNMENT(bool, OPAL_ALIGNMENT_CXX_BOOL)
    AC_LANG_POP(C++)
])


# _OMPI_CXX_CHECK_EXCEPTIONS()
# ----------------------------
# Check for exceptions, skipping the test if we don't want the C++
# bindings
AC_DEFUN([_OMPI_CXX_CHECK_EXCEPTIONS],[
    # Check for special things due to C++ exceptions
    ENABLE_CXX_EXCEPTIONS=no
    HAVE_CXX_EXCEPTIONS=0
    AC_ARG_ENABLE([cxx-exceptions], 
        [AC_HELP_STRING([--enable-cxx-exceptions],
	                [enable support for C++ exceptions (default: disabled)])],
        [ENABLE_CXX_EXCEPTIONS="$enableval"])

    AC_MSG_CHECKING([if want C++ exception handling])

    AS_IF([test "$WANT_MPI_CXX_SUPPORT" = "0"],
          [AS_IF([test "$$enable_cxx_exceptions" = "yes"],
                 [AC_MSG_RESULT([error])
                  AC_MSG_WARN([--enable-cxx-exceptions was specified, but the MPI C++ bindings were disabled])
                  AC_MSG_ERROR([Cannot continue])],
                 [AC_MSG_RESULT([skipped])])],
          [_OMPI_CXX_CHECK_EXCEPTIONS_BACKEND])

    AC_DEFINE_UNQUOTED(OMPI_HAVE_CXX_EXCEPTION_SUPPORT, $HAVE_CXX_EXCEPTIONS,
        [Whether or not we have compiled with C++ exceptions support])
])

# _OMPI_CXX_CHECK_EXCEPTIONS_BACKEND()
# ------------------------------------
# Back end of _OMPI_CXX_CHECK_EXCEPTIONS
AC_DEFUN([_OMPI_CXX_CHECK_EXCEPTIONS_BACKEND],[
    AC_MSG_RESULT([$ENABLE_CXX_EXCEPTIONS])
    if test "$ENABLE_CXX_EXCEPTIONS" = "yes"; then
        # config/cxx_have_exceptions.m4
        OMPI_CXX_HAVE_EXCEPTIONS
        # config/cxx_find_exception_flags.m4
        OMPI_CXX_FIND_EXCEPTION_FLAGS
        if test "$OMPI_CXX_EXCEPTIONS" = "1"; then
            HAVE_CXX_EXCEPTIONS=1

            # Test to see if the C compiler likes these flags
            AC_MSG_CHECKING([to see if C compiler likes the exception flags])
            CFLAGS="$CFLAGS $OMPI_CXX_EXCEPTIONS_CXXFLAGS"
            AC_LANG_SAVE
            AC_LANG_C
            AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[int i = 0;]])],
                              [AC_MSG_RESULT([yes])],
                              [AC_MSG_RESULT([no])
                               AC_MSG_WARN([C++ exception flags are different between the C and C++ compilers; this configure script cannot currently handle this scenario.  Either disable C++ exception support or send mail to the Open MPI users list.])
                               AC_MSG_ERROR([*** Cannot continue])])
            AC_LANG_RESTORE

            # We can't test the F77 and F90 compilers now because we
            # haven't found/set the up yet.  So just save the flags
            # and test them later (in ompi_setup_f77.m4 and
            # ompi_setup_f90.m4).

            CXXFLAGS="$CXXFLAGS $OMPI_CXX_EXCEPTIONS_CXXFLAGS"
            LDFLAGS="$LDFLAGS $OMPI_CXX_EXCEPTIONS_LDFLAGS"

            WRAPPER_EXTRA_CFLAGS="$OMPI_CXX_EXCEPTIONS_CXXFLAGS ${WRAPPER_EXTRA_CFLAGS}"
            WRAPPER_EXTRA_FFLAGS="$OMPI_CXX_EXCEPTIONS_CXXFLAGS ${WRAPPER_EXTRA_FFLAGS}"
            WRAPPER_EXTRA_FCFLAGS="$OMPI_CXX_EXCEPTIONS_CXXFLAGS ${WRAPPER_EXTRA_FCFLAGS}"
            WRAPPER_EXTRA_CXXFLAGS="$OMPI_CXX_EXCEPTIONS_CXXFLAGS ${WRAPPER_EXTRA_CXXFLAGS}"
        fi
    fi
])

# _OMPI_CXX_CHECK_BUILTIN
# -----------------------
# Check for __builtin_* stuff
AC_DEFUN([_OMPI_CXX_CHECK_BUILTIN],[
    OMPI_VAR_SCOPE_PUSH([have_cxx_builtin_expect have_cxx_builtin_prefetch])
    have_cxx_builtin_expect=0
    have_cxx_builtin_prefetch=0

    AS_IF([test "$WANT_MPI_CXX_SUPPORT" = "1"],
          [_OMPI_CXX_CHECK_BUILTIN_BACKEND])

    AC_DEFINE_UNQUOTED([OMPI_CXX_HAVE_BUILTIN_EXPECT], 
                       [$have_cxx_builtin_expect],
                       [Whether C++ compiler supports __builtin_expect])
    AC_DEFINE_UNQUOTED([OMPI_CXX_HAVE_BUILTIN_PREFETCH], 
                       [$have_cxx_builtin_prefetch],
                       [Whether C++ compiler supports __builtin_prefetch])

    OMPI_VAR_SCOPE_POP
])

# _OMPI_CXX_CHECK_BUILTIN_BACKEND
# -------------------------------
# Back end of _OMPI_CXX_CHECK_BUILTIN
AC_DEFUN([_OMPI_CXX_CHECK_BUILTIN_BACKEND],[

    # see if the C++ compiler supports __builtin_expect
    AC_LANG_PUSH(C++)
    AC_CACHE_CHECK([if $CXX supports __builtin_expect],
        [ompi_cv_cxx_supports___builtin_expect],
        [AC_TRY_LINK([],
          [void *ptr = (void*) 0;
           if (__builtin_expect (ptr != (void*) 0, 1)) return 0;],
          [ompi_cv_cxx_supports___builtin_expect="yes"],
          [ompi_cv_cxx_supports___builtin_expect="no"])])
    if test "$ompi_cv_cxx_supports___builtin_expect" = "yes" ; then
        have_builtin_expect=1
    fi
    AC_LANG_POP(C++)

    # see if the C compiler supports __builtin_prefetch
    AC_LANG_PUSH(C++)
    AC_CACHE_CHECK([if $CXX supports __builtin_prefetch],
        [ompi_cv_cxx_supports___builtin_prefetch],
        [AC_TRY_LINK([],
          [int ptr;
           __builtin_prefetch(&ptr,0,0);],
          [ompi_cv_cxx_supports___builtin_prefetch="yes"],
          [ompi_cv_cxx_supports___builtin_prefetch="no"])])
    if test "$ompi_cv_cxx_supports___builtin_prefetch" = "yes" ; then
        have_builtin_prefetch=1
    fi
    AC_LANG_POP(C++)
])


# _OMPI_CXX_CHECK_2D_CONST_CAST
# -----------------------------
# Check for compiler support of 2D const casts
AC_DEFUN([_OMPI_CXX_CHECK_2D_CONST_CAST],[
    OMPI_VAR_SCOPE_PUSH([use_2d_const_cast])
    use_2d_const_cast=0

    AS_IF([test "$WANT_MPI_CXX_SUPPORT" = "1"],
          [_OMPI_CXX_CHECK_2D_CONST_CAST_BACKEND])

    AC_DEFINE_UNQUOTED([OMPI_CXX_SUPPORTS_2D_CONST_CAST],
                       [$use_2d_const_cast],
                       [Whether a const_cast on a 2-d array will work with the C++ compiler])

    OMPI_VAR_SCOPE_POP
])

# _OMPI_CXX_CHECK_2D_CONST_CAST_BACKEND
# ---------------------------------
# Back end of _OMPI_CHECK_2D_CONST_CAST
AC_DEFUN([_OMPI_CXX_CHECK_2D_CONST_CAST_BACKEND],[
    # see if the compiler supports const_cast of 2-dimensional arrays
    AC_LANG_PUSH(C++)
    AC_CACHE_CHECK([if $CXX supports const_cast<> properly],
       [ompi_cv_cxx_supports_2d_const_cast],
       [AC_TRY_COMPILE([int non_const_func(int ranges[][3]);
int cast_test(const int ranges[][3]) {
  return non_const_func(const_cast<int(*)[3]>(ranges));  
}],
            [],
            [ompi_cv_cxx_supports_2d_const_cast="yes"],
            [ompi_cv_cxx_supports_2d_const_cast="no"])])
    if test "$ompi_cv_cxx_supports_2d_const_cast" = "yes" ; then
        use_2d_const_cast=1
    fi
    AC_LANG_POP(C++)
])
