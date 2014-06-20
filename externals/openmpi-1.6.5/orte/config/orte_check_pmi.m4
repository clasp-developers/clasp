# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009-2011 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011      Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# ORTE_CHECK_PMI(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([ORTE_CHECK_PMI],[
    AC_ARG_WITH([pmi],
                [AC_HELP_STRING([--with-pmi],
                                [Build PMI support (default: no)])],
	                        [], with_pmi=no)
    AC_ARG_WITH([cray-pmi2-ext],
                [AC_HELP_STRING([--with-cray-pmi-ext],
                                [Include Cray PMI2 extensions (default: no)])],
                                [], with_cray_pmi2_ext=no)

    orte_enable_pmi=0
    orte_use_cray_pmi2_ext=0

    # save flags
    orte_check_pmi_$1_save_CPPFLAGS="$CPPFLAGS"
    orte_check_pmi_$1_save_LDFLAGS="$LDFLAGS"
    orte_check_pmi_$1_save_LIBS="$LIBS"

    # set defaults
    orte_check_pmi_$1_LDFLAGS=
    orte_check_pmi_$1_CPPFLAGS=
    orte_check_pmi_$1_LIBS=

    AC_MSG_CHECKING([if user requested PMI support])
    AS_IF([test "$with_pmi" = "no"],
          [AC_MSG_RESULT([no])
           orte_use_cray_pmi2_ext=0
           $3],
          [AC_MSG_RESULT([yes])
           AC_MSG_CHECKING([if PMI support installed])
           # cannot use OMPI_CHECK_PACKAGE as its backend header
           # support appends "include" to the path, which won't
           # work with slurm :-(
           AS_IF([test ! -z "$with_pmi" -a "$with_pmi" != "yes"],
                 [AS_IF([test -d "$with_pmi/lib64"],
                        [orte_check_pmi_$1_LDFLAGS="-L$with_pmi/lib64"
                         orte_check_pmi_$1_LIBS="-lpmi -Wl,-rpath=$with_pmi/lib64"],
                        [orte_check_pmi_$1_LDFLAGS="-L$with_pmi/lib"
                         orte_check_pmi_$1_LIBS="-lpmi -Wl,-rpath=$with_pmi/lib"])
                  AS_IF([test -f "$with_pmi/include/pmi.h"],
                        [orte_check_pmi_$1_CPPFLAGS="-I$with_pmi/include"],
                        [AS_IF([test -f "$with_pmi/include/slurm/pmi.h"],
                               [orte_check_pmi_$1_CPPFLAGS="-I$with_pmi/include/slurm"])])],
                 [AS_IF([test -f "/usr/include/slurm/pmi.h"],
                        [orte_check_pmi_$1_CPPFLAGS="-I/usr/include/slurm"])])

           LDFLAGS="$LDFLAGS $orte_check_pmi_$1_LDFLAGS"
           CPPFLAGS="$CPPFLAGS $orte_check_pmi_$1_CPPFLAGS"
           LIBS="$LIBS $orte_check_pmi_$1_LIBS"
           orte_have_pmi_support=no
           AC_CHECK_HEADERS([pmi.h],
                            [AC_CHECK_LIB([pmi], [PMI_Init],
                            [orte_have_pmi_support=yes])])

           AS_IF([test "$orte_have_pmi_support" = "yes"],
                 [AC_MSG_RESULT([yes])
                  orte_enable_pmi=1
                  $1_LDFLAGS="$orte_check_pmi_$1_LDFLAGS"
                  $1_CPPFLAGS="$orte_check_pmi_$1_CPPFLAGS"
                  $1_LIBS="-lpmi"
                  $2],
                 [AC_MSG_RESULT([no])
                  AC_MSG_WARN([PMI support requested (via --with-pmi) but not found.])
                  AC_MSG_ERROR([Aborting.])
                  $3])

           AC_MSG_CHECKING([if user requested Cray PMI2 extensions])
           AS_IF([test "$with_cray_pmi2_ext" = "no"],
                 [AC_MSG_RESULT([no])
	          orte_use_pmi2_ext=0],
                 [AC_MSG_RESULT([yes])
                  # check to see if pmi2.h header is present. if it is, then we
                  # will use some of the functions in it.
                  AC_MSG_CHECKING([if PMI2 extensions installed])
                  AS_IF([test -f "$with_pmi/include/pmi2.h"],
                        [orte_use_pmi2_ext=1
                         AC_MSG_RESULT(yes)],
                        [AC_MSG_RESULT([no])
                         AC_MSG_WARN([PMI2 extensions requested (via --with-cray-pmi2-ext) but not found.])
                         AC_MSG_ERROR([Aborting.])
                         orte_use_pmi2_ext=0
                         orte_enable_pmi=0
                         $3])])])

    # restore flags - have to add CPPFLAGS so base functions can find pmi.h
    CPPFLAGS="$orte_check_pmi_$1_save_CPPFLAGS $orte_check_pmi_$1_CPPFLAGS"
    LDFLAGS="$orte_check_pmi_$1_save_LDFLAGS"
    LIBS="$orte_check_pmi_$1_save_LIBS"

   AC_DEFINE_UNQUOTED([WANT_PMI_SUPPORT],
                      [$orte_enable_pmi],
                      [Whether we want PMI support])
   AC_DEFINE_UNQUOTED([WANT_CRAY_PMI2_EXT],
                      [$orte_use_pmi2_ext],
                      [Whether we want to use Cray PMI2 extensions])
   AM_CONDITIONAL(WANT_PMI_SUPPORT, [test "$orte_enable_pmi" = 1])
])
