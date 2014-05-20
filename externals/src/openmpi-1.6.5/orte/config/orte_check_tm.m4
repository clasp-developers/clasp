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
# Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# ORTE_CHECK_TM_LIBS_FLAGS(prefix, [LIBS or LDFLAGS])
# ---------------------------------------------------
AC_DEFUN([ORTE_CHECK_TM_LIBS_FLAGS],[
    orte_check_tm_flags=`$orte_check_tm_pbs_config --libs`
    for orte_check_tm_val in $orte_check_tm_flags; do
        if test "`echo $orte_check_tm_val | cut -c1-2`" = "-l"; then
            if test "$2" = "LIBS"; then
                $1_$2="$$1_$2 $orte_check_tm_val"
            fi
        else
            if test "$2" = "LDFLAGS"; then
                $1_$2="$$1_$2 $orte_check_tm_val"
            fi
        fi
    done
])


# ORTE_CHECK_TM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([ORTE_CHECK_TM],[
    AC_ARG_WITH([tm],
                [AC_HELP_STRING([--with-tm(=DIR)],
                                [Build TM (Torque, PBSPro, and compatible) support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    OMPI_CHECK_WITHDIR([tm], [$with_tm], [include/tm.h])

    orte_check_tm_found=no
    AS_IF([test "$with_tm" = "no"],
          [orte_check_tm_happy="no"],
          [orte_check_tm_happy="yes"
           AS_IF([test ! -z "$with_tm" -a "$with_tm" != "yes"],
                 [orte_check_tm_dir="$with_tm"],
                 [orte_check_tm_dir=""])])

    AS_IF([test "$orte_check_tm_happy" = "yes"],
          [AC_MSG_CHECKING([for pbs-config])
           orte_check_tm_pbs_config="not found"
           AS_IF([test "$orte_check_tm_dir" != "" -a -d "$orte_check_tm_dir" -a -x "$orte_check_tm_dir/bin/pbs-config"],
                 [orte_check_tm_pbs_config="$orte_check_tm_dir/bin/pbs-config"],
                 [AS_IF([pbs-config --prefix >/dev/null 2>&1],
                        [orte_check_tm_pbs_config="pbs-config"])])
           AC_MSG_RESULT([$orte_check_tm_pbs_config])])

    # If we have pbs-config, get the flags we need from there and then
    # do simplistic tests looking for the tm headers and symbols

    AS_IF([test "$orte_check_tm_happy" = "yes" -a "$orte_check_tm_pbs_config" != "not found"],
          [$1_CPPFLAGS=`$orte_check_tm_pbs_config --cflags`
           OMPI_LOG_MSG([$1_CPPFLAGS from pbs-config: $$1_CPPFLAGS], 1)

           ORTE_CHECK_TM_LIBS_FLAGS([$1], [LDFLAGS])
           OMPI_LOG_MSG([$1_LDFLAGS from pbs-config: $$1_LDFLAGS], 1)

           ORTE_CHECK_TM_LIBS_FLAGS([$1], [LIBS])
           OMPI_LOG_MSG([$1_LIBS from pbs-config: $$1_LIBS], 1)

           # Now that we supposedly have the right flags, try them out.

           CPPFLAGS_save="$CPPFLAGS"
           LDFLAGS_save="$LDFLAGS"
           LIBS_save="$LIBS"

           CPPFLAGS="$CPPFLAGS $$1_CPPFLAGS"
           LIBS="$LIBS $$1_LIBS"
           LDFLAGS="$LDFLAGS $$1_LDFLAGS"

           AC_CHECK_HEADER([tm.h],
               [AC_CHECK_FUNC([tm_finalize],
                   [orte_check_tm_found="yes"])])

           CPPFLAGS="$CPPFLAGS_save"
           LDFLAGS="$LDFLAGS_save"
           LIBS="$LIBS_save"])

    # If we don't have pbs-config, then we have to look around
    # manually.

    # Note that Torque 2.1.0 changed the name of their back-end
    # library to "libtorque".  So we have to check for both libpbs and
    # libtorque.  First, check for libpbs.

    orte_check_package_$1_save_CPPFLAGS="$CPPFLAGS"
    orte_check_package_$1_save_LDFLAGS="$LDFLAGS"
    orte_check_package_$1_save_LIBS="$LIBS"

    orte_check_package_$1_orig_CPPFLAGS="$$1_CPPFLAGS"
    orte_check_package_$1_orig_LDFLAGS="$$1_LDFLAGS"
    orte_check_package_$1_orig_LIBS="$$1_LIBS"

    AS_IF([test "$orte_check_tm_found" = "no"],
          [AS_IF([test "$orte_check_tm_happy" = "yes"],
                 [_OMPI_CHECK_PACKAGE_HEADER([$1], 
                       [tm.h],
                       [$orte_check_tm_dir],
                       [orte_check_tm_found="yes"],
                       [orte_check_tm_found="no"])])

           AS_IF([test "$orte_check_tm_found" = "yes"],
                 [_OMPI_CHECK_PACKAGE_LIB([$1],
                       [pbs],
                       [tm_init],
                       [],
                       [$orte_check_tm_dir],
                       [$orte_check_tm_libdir],
                       [orte_check_tm_found="yes"],
                       [_OMPI_CHECK_PACKAGE_LIB([$1],
                             [torque],
                             [tm_init],
                             [],
                             [$orte_check_tm_dir],
                             [$orte_check_tm_libdir],
                             [orte_check_tm_found="yes"],
                             [orte_check_tm_found="no"])])])])

    CPPFLAGS="$orte_check_package_$1_save_CPPFLAGS"
    LDFLAGS="$orte_check_package_$1_save_LDFLAGS"
    LIBS="$orte_check_package_$1_save_LIBS"

    # Did we find the right stuff?
    AS_IF([test "$orte_check_tm_happy" = "yes" -a "$orte_check_tm_found" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_tm" -a "$with_tm" != "no"],
                 [AC_MSG_ERROR([TM support requested but not found.  Aborting])])
           $3])
])
