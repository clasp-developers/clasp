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
# Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# OMPI_SETUP_COMPONENT_PACKAGE(1: framework_name, 
#                              2: component_name,
#                              3: option_name,
#                              4: withdir_dir_check_file,
#                              5: withdir_libdir_check_file,
#                              6: header, 
#                              7: library, 
#                              8: function, 
#                              9: extra-libraries, 
#                              10: [action-if-found], 
#                              11: [action-if-not-found])
# ------------------------------------------------
# Many components need to just check for one package, and if it's all
# good, set themselves up with appropriate CPPFLAGS, LDFLAGS, and
# LIBS.  This macro templates all of that for the common case.
#
# This macro does the following:
#
# - Assumes that this component should be built by default if all
#   headers and libraries can be found
# - Adds --with-<name> and --with-<name>-libdir options to configure
# - Sanity checks directory names given to the above options (i.e.,
#   look for a token file in each, but only if the directory argument
#   is given)
# - Assumes that if --with-<name> is supplied and we can't build the
#   component, it's a fatal error.
# - Assumes that if --with-<name> is NOT supplied and we can't build
#   the component, it's NOT a fatal error.
# - Run OMPI_CHECK_PACKAGE (check for the specific presence of header
#   files and/or libraries) to determine if the package is available
# - Set <framework>_<component>_WRAPPER_EXTRA_LDFLAGS
# - Set <framework>_<component>_WRAPPER_EXTRA_LIBS
# - Set and AC_SUBST <framework>_<component>_CPPFLAGS
# - Set and AC_SUBST <framework>_<component>_CFLAGS
# - Set and AC_SUBST <framework>_<component>_LDFLAGS
# - Set and AC_SUBST <framework>_<component>_LIBS
#
# Enjoy.
#
AC_DEFUN([OPAL_SETUP_COMPONENT_PACKAGE],[
    AC_ARG_WITH([$3],
        [AC_HELP_STRING([--with-$3(=DIR)],
                        [Build $3 support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    OMPI_CHECK_WITHDIR([$3], [$with_$3], [$4])
    AC_ARG_WITH([$3-libdir],
        [AC_HELP_STRING([--with-$3-libdir=DIR],
                        [Search for the $3 libraries in DIR])])
    OMPI_CHECK_WITHDIR([$3-libdir], [$with_$3_libdir], [$5])

    AS_IF([test ! -z "$with_$3" -a "$with_$3" != "yes"],
          [$1_$2_dir="$with_$3"])
    AS_IF([test ! -z "$with_$3_libdir" -a "$with_$3_libdir" != "yes"],
          [$1_$2_libdir="$with_$3_libdir"])

    AS_IF([test "$with_$3" = "no"],
          [$1_$2_happy="no"],
          [$1_$2_happy="yes"])

    AS_IF([test "$$1_$2_happy" = "yes"],
          [OMPI_CHECK_PACKAGE([$1_$2],
                              [$6],
                              [$7],
                              [$8],
                              [$9],
                              [$$1_$2_dir],
                              [$$1_$2_libdir],
                              [$1_$2_happy="yes"],
                              [$1_$2_happy="no"])])

    AS_IF([test "$$1_$2_happy" = "yes"],
          [$1_$2_WRAPPER_EXTRA_LDFLAGS="$$1_$2_LDFLAGS"
           $1_$2_WRAPPER_EXTRA_LIBS="$$1_$2_LIBS"
           $10],
          [$11])

    # sanity check
    AS_IF([test "$$1_$2_happy" = "no"],
          [AS_IF([test "$with_$3" != "no" -a ! -z "$with_$3"],
                 [AC_MSG_WARN([$1:$2 requested but not found])
                  AC_MSG_ERROR([Cannot continue])])])

    # substitute in the things needed to build libnuma
    AC_SUBST([$1_$2_CFLAGS])
    AC_SUBST([$1_$2_CPPFLAGS])
    AC_SUBST([$1_$2_LDFLAGS])
    AC_SUBST([$1_$2_LIBS])
])
