# -*- shell-script -*-
#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AC_DEFUN([MCA_io_romio_POST_CONFIG], [
    AS_IF([test $1 -eq 0 -a "$enable_dist" = "yes"],
          [AC_MSG_ERROR([ROMIO disabled but --enable-dist specifed.  This will result in a bad tarball.  Aborting configure.])])
    AM_CONDITIONAL([MCA_io_romio_SHOULD_BUILD], [test $1 -eq 1])

])


# MCA_io_romio_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_io_romio_CONFIG],[
    OMPI_VAR_SCOPE_PUSH([io_romio_flags io_romio_flags_define io_romio_happy io_romio_save_LIBS])
    AC_ARG_ENABLE([io-romio],
                  [AC_HELP_STRING([--disable-io-romio],
                                  [Disable the ROMIO MPI-IO component])])
    AC_ARG_WITH([io-romio-flags], 
                [AC_HELP_STRING([--with-io-romio-flags=FLAGS],
                                [Pass FLAGS to the ROMIO distribution configuration script])])
    AC_DEFINE_UNQUOTED([MCA_io_romio_USER_CONFIGURE_FLAGS], ["$with_io_romio_flags"], [Set of user-defined configure flags given to ROMIOs configure script via --with-io-romio-flags])
    AC_MSG_CHECKING([if want ROMIO component])
    AS_IF([test "$enable_io_romio" = "no"],
           [AC_MSG_RESULT([no])
            $2], 
           [AC_MSG_RESULT([yes])
            AC_MSG_CHECKING([if MPI profiling is enabled])
            AS_IF([test "$enable_mpi_profile" = "no"],
                  [AC_MSG_RESULT([no])
                   AC_MSG_WARN([*** The ROMIO io component requires the MPI profiling layer])
                   AS_IF([test "$enable_io_romio" = "yes"],
                         [AC_MSG_ERROR([*** ROMIO requested but not available.  Aborting])])
                   $2],
                  [AC_MSG_RESULT([yes])

                   AS_IF([test -n "$with_io_romio_flags" -a "$with_io_romio_flags" != "no"],
                         [io_romio_flags="$with_io_romio_flags $io_romio_flags"],
                         [io_romio_flags=])
                   # If ROMIO is going to end up in a DSO, all we need is
                   # shared library-ized objects, as we're only building a
                   # DSO (which is always shared).  Otherwise, build with
                   # same flags as OMPI, as we might need any combination of
                   # shared and static-ized objects...
                   AS_IF([test "$compile_mode" = "dso"],
                         [io_romio_shared=enable
                          io_romio_static=disable],
                         [AS_IF([test "$enable_shared" = "yes"],
                                [io_romio_shared=enable],
                                [io_romio_shared=disable])
                          AS_IF([test "$enable_static" = "yes"], 
                                [io_romio_static=enable],
                                [io_romio_static=disable])])
                   AS_IF([test -n "$prefix" -a "$prefix" != "NONE"], 
                         [io_romio_prefix_arg="--prefix=$prefix"], 
                         [io_romio_prefix_arg=])

                   AS_IF([test "$cross_compiling" = "yes"],
                       [AS_IF([test ! -z $build], [io_romio_flags="$io_romio_flags --build=$build"])
                        AS_IF([test ! -z $host], [io_romio_flags="$io_romio_flags --host=$host"])
                        AS_IF([test ! -z $target], [io_romio_flags="$io_romio_flags --target=$target"])])
                   io_romio_flags_define="$io_romio_flags CFLAGS='$CFLAGS' CPPFLAGS='$CPPFLAGS' FFLAGS='$FFLAGS' LDFLAGS='$LDFLAGS' --$io_romio_shared-shared --$io_romio_static-static $io_romio_flags $io_romio_prefix_arg --with-mpi=open_mpi --disable-aio"
                   AC_DEFINE_UNQUOTED([MCA_io_romio_COMPLETE_CONFIGURE_FLAGS], ["$io_romio_flags_define"], [Complete set of command line arguments given to ROMIOs configure script])

                   io_romio_flags="$io_romio_flags CFLAGS="'"'"$CFLAGS"'"'" CPPFLAGS="'"'"$CPPFLAGS"'"'" FFLAGS="'"'"$FFLAGS"'"'" LDFLAGS="'"'"$LDFLAGS"'"'" --$io_romio_shared-shared --$io_romio_static-static $io_romio_flags $io_romio_prefix_arg --with-mpi=open_mpi --disable-aio"

                   ompi_show_subtitle "Configuring ROMIO distribution"
                   OMPI_CONFIG_SUBDIR([ompi/mca/io/romio/romio], 
                                      [$io_romio_flags],
                                      [io_romio_happy=1], [io_romio_happy=0])

                   AS_IF([test "$io_romio_happy" = "1"],
                         [ # grab the libraries list from ROMIO.  We don't
                           # need this for building the component, as libtool
                           # will figure that part out.  But we do need it for
                           # the wrapper settings
                          io_romio_save_LIBS="$LIBS"
                          LIBS=
                          . ompi/mca/io/romio/romio/localdefs
                          io_romio_LIBS="$LIBS"
                          LIBS="$io_romio_save_LIBS"

                          echo "ROMIO distribution configured successfully"
                          io_romio_WRAPPER_EXTRA_LIBS="$io_romio_LIBS"
                          $1],
                         [AS_IF([test "$enable_io_romio" = "yes"],
                                [AC_MSG_ERROR([ROMIO distribution did not configure successfully])],
                                [AC_MSG_WARN([ROMIO distribution did not configure successfully])])
                          $2])])])
    OMPI_VAR_SCOPE_POP
])
