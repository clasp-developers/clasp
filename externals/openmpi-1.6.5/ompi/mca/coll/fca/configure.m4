# -*- shell-script -*-
#
#
# Copyright (c) 2011 Mellanox Technologies. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#


# MCA_coll_fca_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_coll_fca_CONFIG],[
    OMPI_CHECK_FCA([coll_fca],
                     [coll_fca_happy="yes"],
                     [coll_fca_happy="no"])

    AS_IF([test "$coll_fca_happy" = "yes"],
          [coll_fca_WRAPPER_EXTRA_LDFLAGS="$coll_fca_LDFLAGS"
           coll_fca_CPPFLAGS="$coll_fca_CPPFLAGS"
           coll_fca_WRAPPER_EXTRA_CPPFLAGS="$coll_fca_CPPFLAGS"
           coll_fca_WRAPPER_EXTRA_LIBS="$coll_fca_LIBS"
           $1],
          [$2])

    # substitute in the things needed to build fca
    AC_SUBST([coll_fca_CFLAGS])
    AC_SUBST([coll_fca_CPPFLAGS])
    AC_SUBST([coll_fca_LDFLAGS])
    AC_SUBST([coll_fca_LIBS])
	AC_SUBST(coll_fca_HOME, "$ompi_check_fca_dir")
])dnl

