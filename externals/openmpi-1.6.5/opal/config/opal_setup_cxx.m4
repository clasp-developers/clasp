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
AC_DEFUN([OPAL_SETUP_CXX_BANNER],[
    ompi_show_subtitle "C++ compiler and preprocessor" 
])

# This macro is necessary because PROG_CXX* is REQUIREd by multiple
# places in SETUP_CXX.
AC_DEFUN([OPAL_PROG_CXX],[
    OMPI_VAR_SCOPE_PUSH([opal_cxxflags_save])
    opal_cxxflags_save="$CXXFLAGS"
    AC_PROG_CXX
    AC_PROG_CXXCPP
    CXXFLAGS="$opal_cxxflags_save"
    OMPI_VAR_SCOPE_POP
])

# OPAL_SETUP_CXX()
# ----------------
# Do everything required to setup the C++ compiler.  Safe to AC_REQUIRE
# this macro.
AC_DEFUN([OPAL_SETUP_CXX],[
    AC_REQUIRE([OPAL_SETUP_CXX_BANNER])

    _OPAL_SETUP_CXX_COMPILER

    OMPI_CXX_COMPILER_VENDOR([opal_cxx_vendor])

    _OPAL_SETUP_CXX_COMPILER_BACKEND
])

# _OPAL_SETUP_CXX_COMPILER()
# --------------------------
# Setup the CXX compiler
AC_DEFUN([_OPAL_SETUP_CXX_COMPILER],[
    OMPI_VAR_SCOPE_PUSH(opal_cxx_compiler_works)

    # Must REQUIRE the PROG_CXX macro and not call it directly here for
    # reasons well-described in the AC2.64 (and beyond) docs.
    AC_REQUIRE([OPAL_PROG_CXX])
    BASECXX="`basename $CXX`"

    AS_IF([test "x$CXX" = "x"], [CXX=none])
    set dummy $CXX
    opal_cxx_argv0=[$]2
    OMPI_WHICH([$opal_cxx_argv0], [OPAL_CXX_ABSOLUTE])
    AS_IF([test "x$OPAL_CXX_ABSOLUTE" = "x"], [OPAL_CXX_ABSOLUTE=none])

    AC_DEFINE_UNQUOTED(OPAL_CXX, "$CXX", [OPAL underlying C++ compiler])
    AC_SUBST(OPAL_CXX_ABSOLUTE)

    OMPI_VAR_SCOPE_POP
])

# _OPAL_SETUP_CXX_COMPILER_BACKEND()
# ----------------------------------
# Back end of _OPAL_SETUP_CXX_COMPILER_BACKEND()
AC_DEFUN([_OPAL_SETUP_CXX_COMPILER_BACKEND],[
    # Do we want code coverage
    if test "$WANT_COVERAGE" = "1"; then 
        if test "$opal_cxx_vendor" = "gnu" ; then
            AC_MSG_WARN([$OPAL_COVERAGE_FLAGS has been added to CFLAGS (--enable-coverage)])
            WANT_DEBUG=1
            CXXFLAGS="${CXXFLAGS} $OPAL_COVERAGE_FLAGS"
            WRAPPER_EXTRA_CXXFLAGS="${WRAPPER_EXTRA_CXXFLAGS} $OPAL_COVERAGE_FLAGS"
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
    OPAL_CXXFLAGS_BEFORE_PICKY="$CXXFLAGS"
    if test "$WANT_PICKY_COMPILER" = 1 -a "$opal_cxx_vendor" = "gnu"; then
        add="-Wall -Wundef -Wno-long-long"

        # see if -Wno-long-double works...
        AC_LANG_PUSH(C++)
        CXXFLAGS_orig="$CXXFLAGS"
        CXXFLAGS="$CXXFLAGS $add -Wno-long-double -fstrict-prototype"
        AC_CACHE_CHECK([if $CXX supports -Wno-long-double],
                   [opal_cv_cxx_wno_long_double],
                   [AC_TRY_COMPILE([], [], 
                                   [opal_cv_cxx_wno_long_double="yes"],
                                   [opal_cv_cxx_wno_long_double="no"])])
        CXXFLAGS="$CXXFLAGS_orig"
        AC_LANG_POP(C++)
        if test "$opal_cv_cxx_wno_long_double" = "yes" ; then
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
                   [opal_cv_cxx_finline_functions],
                   [AC_TRY_COMPILE([], [],
                                   [opal_cv_cxx_finline_functions="yes"],
                                   [opal_cv_cxx_finline_functions="no"])])
        if test "$opal_cv_cxx_finline_functions" = "yes" ; then
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
    if[ test "$opal_cv_cxx_compiler_vendor" != "microsoft" ]; then
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

    # config/opal_ensure_contains_optflags.m4
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
