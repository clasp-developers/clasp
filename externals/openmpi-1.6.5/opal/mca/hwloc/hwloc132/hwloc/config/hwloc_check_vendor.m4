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
dnl Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

dnl ------------------------------------------------------------------
dnl This m4 file originally copied from Open MPI
dnl config/ompi_check_vendor.m4.
dnl ------------------------------------------------------------------


# HWLOC_C_COMPILER_VENDOR(VENDOR_VARIABLE)
# ---------------------------------------
# Set shell variable VENDOR_VARIABLE to the name of the compiler
# vendor for the current C compiler.
#
# See comment for _HWLOC_CHECK_COMPILER_VENDOR for a complete
# list of currently detected compilers.
AC_DEFUN([_HWLOC_C_COMPILER_VENDOR], [
    AC_REQUIRE([AC_PROG_CC])

    AC_CACHE_CHECK([for the C compiler vendor],
        [hwloc_cv_c_compiler_vendor],
        [AC_LANG_PUSH(C)
         _HWLOC_CHECK_COMPILER_VENDOR([hwloc_cv_c_compiler_vendor])
         AC_LANG_POP(C)])

    $1="$hwloc_cv_c_compiler_vendor"
])


# workaround to avoid syntax error with Autoconf < 2.68:
m4_ifndef([AC_LANG_DEFINES_PROVIDED],
	  [m4_define([AC_LANG_DEFINES_PROVIDED])])

# HWLOC_IFDEF_IFELSE(symbol, [action-if-defined], 
#                   [action-if-not-defined])
# ----------------------------------------------
# Run compiler to determine if preprocessor symbol "symbol" is
# defined by the compiler.
AC_DEFUN([HWLOC_IFDEF_IFELSE], [
    AC_COMPILE_IFELSE([AC_LANG_DEFINES_PROVIDED
#ifndef $1
#error "symbol $1 not defined"
choke me
#endif], [$2], [$3])])


# HWLOC_IF_IFELSE(symbol, [action-if-defined], 
#                [action-if-not-defined])
# ----------------------------------------------
# Run compiler to determine if preprocessor symbol "symbol" is
# defined by the compiler.
AC_DEFUN([HWLOC_IF_IFELSE], [
    AC_COMPILE_IFELSE([AC_LANG_DEFINES_PROVIDED
#if !( $1 )
#error "condition $1 not met"
choke me
#endif], [$2], [$3])])


# _HWLOC_CHECK_COMPILER_VENDOR(VENDOR_VARIABLE)
# --------------------------------------------
# Set shell variable VENDOR_VARIABLE to the name of the compiler
# vendor for the compiler for the current language.  Language must be
# one of C, OBJC, or C++.
#
# thanks to http://predef.sourceforge.net/precomp.html for the list
# of defines to check.
AC_DEFUN([_HWLOC_CHECK_COMPILER_VENDOR], [
    hwloc_check_compiler_vendor_result="unknown"

    # GNU is probably the most common, so check that one as soon as
    # possible.  Intel pretends to be GNU, so need to check Intel
    # before checking for GNU.

    # Intel
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IF_IFELSE([defined(__INTEL_COMPILER) || defined(__ICC)], 
               [hwloc_check_compiler_vendor_result="intel"])])

    # GNU
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IFDEF_IFELSE([__GNUC__], 
               [hwloc_check_compiler_vendor_result="gnu"])])

    # Borland Turbo C
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IFDEF_IFELSE([__TURBOC__], 
               [hwloc_check_compiler_vendor_result="borland"])])

    # Borland C++
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IFDEF_IFELSE([__BORLANDC__], 
               [hwloc_check_compiler_vendor_result="borland"])])

    # Comeau C++
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IFDEF_IFELSE([__COMO__], 
               [hwloc_check_compiler_vendor_result="comeau"])])

    # Compaq C/C++
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IF_IFELSE([defined(__DECC) || defined(VAXC) || defined(__VAXC)],
               [hwloc_check_compiler_vendor_result="compaq"],
               [HWLOC_IF_IFELSE([defined(__osf__) && defined(__LANGUAGE_C__)],
                    [hwloc_check_compiler_vendor_result="compaq"],
                    [HWLOC_IFDEF_IFELSE([__DECCXX],
                         [hwloc_check_compiler_vendor_result="compaq"])])])])

    # Cray C/C++
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IFDEF_IFELSE([_CRAYC], 
               [hwloc_check_compiler_vendor_result="cray"])])

    # Diab C/C++
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IFDEF_IFELSE([__DCC__], 
               [hwloc_check_compiler_vendor_result="diab"])])

    # Digital Mars
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IF_IFELSE([defined(__DMC__) || defined(__SC__) || defined(__ZTC__)],
               [hwloc_check_compiler_vendor_result="digital mars"])])

    # HP ANSI C / aC++
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IF_IFELSE([defined(__HP_cc) || defined(__HP_aCC)],
               [hwloc_check_compiler_vendor_result="hp"])])

    # IBM XL C/C++
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IF_IFELSE([defined(__xlC__) || defined(__IBMC__) || defined(__IBMCPP__)],
               [hwloc_check_compiler_vendor_result="ibm"],
               [HWLOC_IF_IFELSE([defined(_AIX) && !defined(__GNUC__)],
                    [hwloc_check_compiler_vendor_result="ibm"])])])

    # KAI C++ (rest in peace)
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IFDEF_IFELSE([__KCC],
               [hwloc_check_compiler_vendor_result="kai"])])

    # LCC
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IFDEF_IFELSE([__LCC__],
               [hwloc_check_compiler_vendor_result="lcc"])])

    # MetaWare High C/C++
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IFDEF_IFELSE([__HIGHC__],
               [hwloc_check_compiler_vendor_result="metaware high"])])

    # Metrowerks Codewarrior
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IFDEF_IFELSE([__MWERKS__],
               [hwloc_check_compiler_vendor_result="metrowerks"])])

    # MIPSpro (SGI)
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IF_IFELSE([defined(sgi) || defined(__sgi)], 
               [hwloc_check_compiler_vendor_result="sgi"])])

    # MPW C++
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IF_IFELSE([defined(__MRC__) || defined(MPW_C) || defined(MPW_CPLUS)], 
               [hwloc_check_compiler_vendor_result="mpw"])])

    # Microsoft
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [# Always use C compiler when checking for Microsoft, as 
           # Visual C++ doesn't recognize .cc as a C++ file.
           AC_LANG_PUSH(C)
           HWLOC_IF_IFELSE([defined(_MSC_VER) || defined(__MSC_VER)], 
               [hwloc_check_compiler_vendor_result="microsoft"])
           AC_LANG_POP(C)])

    # Norcroft C
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IFDEF_IFELSE([__CC_NORCROFT],
               [hwloc_check_compiler_vendor_result="norcroft"])])

    # Pelles C
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IFDEF_IFELSE([__POCC__],
               [hwloc_check_compiler_vendor_result="pelles"])])

    # Portland Group
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IFDEF_IFELSE([__PGI], 
               [hwloc_check_compiler_vendor_result="portland group"])])

    # SAS/C
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IF_IFELSE([defined(SASC) || defined(__SASC) || defined(__SASC__)],
               [hwloc_check_compiler_vendor_result="sas"])])

    # Sun Workshop C/C++
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IF_IFELSE([defined(__SUNPRO_C) || defined(__SUNPRO_CC)],
               [hwloc_check_compiler_vendor_result="sun"])])

    # TenDRA C/C++
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IFDEF_IFELSE([__TenDRA__],
               [hwloc_check_compiler_vendor_result="tendra"])])

    # Tiny C
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IFDEF_IFELSE([__TINYC__],
               [hwloc_check_compiler_vendor_result="tiny"])])

    # USL C
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IFDEF_IFELSE([__USLC__],
               [hwloc_check_compiler_vendor_result="usl"])])

    # Watcom C++
    AS_IF([test "$hwloc_check_compiler_vendor_result" = "unknown"],
          [HWLOC_IFDEF_IFELSE([__WATCOMC__],
               [hwloc_check_compiler_vendor_result="watcom"])])

    $1="$hwloc_check_compiler_vendor_result"
    unset hwloc_check_compiler_vendor_result
])
