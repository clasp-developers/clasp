# -*- shell-script -*-
#
# Copyright (c) 2011 Mellanox Technologies. All rights reserved.

# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# OMPI_CHECK_FCA(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if fca support can be found.  sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_FCA],[
    AC_ARG_WITH([fca],
        [AC_HELP_STRING([--with-fca(=DIR)],
             [Build fca (Mellanox Fabric Collective Accelerator) support, searching for libraries in DIR])])
    OMPI_CHECK_WITHDIR([fca], [$with_fca], [lib/libfca.so])

    AS_IF([test "$with_fca" != "no"],
          [AS_IF([test ! -z "$with_fca" -a "$with_fca" != "yes"],
			  [ompi_check_fca_dir="$with_fca"
			   ompi_check_fca_libdir="$ompi_check_fca_dir/lib"
			   ompi_check_fca_incdir="$ompi_check_fca_dir/include"
			   ompi_check_fca_libs="fca"

			   CPPFLAGS_save="$CPPFLAGS"
			   LDFLAGS_save="$LDFLAGS"
			   LIBS_save="$LIBS"
			   CPPFLAGS="$CPPFLAGS -I$ompi_check_fca_dir/include/fca -I$ompi_check_fca_dir/include/fca_core"

			   OMPI_LOG_MSG([$1_CPPFLAGS : $$1_CPPFLAGS], 1)
			   OMPI_LOG_MSG([$1_LDFLAGS  : $$1_LDFLAGS], 1)
			   OMPI_LOG_MSG([$1_LIBS     : $$1_LIBS], 1)

			   OMPI_CHECK_PACKAGE([$1],
				   [fca_api.h],
				   [$ompi_check_fca_libs],
				   [fca_get_version],
				   [-l$ompi_check_fca_libs],
				   [$ompi_check_fca_dir],
				   [$ompi_check_fca_libdir],
				   [ompi_check_fca_happy="yes"],
				   [ompi_check_fca_happy="no"])

			   CPPFLAGS="$CPPFLAGS_save"
			   LDFLAGS="$LDFLAGS_save"
			   LIBS="$LIBS_save"],
			   [ompi_check_fca_happy="no"])])

    AS_IF([test "$ompi_check_fca_happy" = "yes" -a "$enable_progress_threads" = "yes"],
          [AC_MSG_WARN([fca driver does not currently support progress threads.  Disabling FCA.])
           ompi_check_fca_happy="no"])

    AS_IF([test "$ompi_check_fca_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_fca" -a "$with_fca" != "no"],
                 [AC_MSG_ERROR([FCA support requested but not found.  Aborting])])
           $3])
])

