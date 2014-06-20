# -*- shell-script -*-
#
# Copyright (c) 2004-2010 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_crs_blcr_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_crs_blcr_CONFIG],[
    AC_ARG_WITH([blcr],
                [AC_HELP_STRING([--with-blcr(=DIR)],
                                [Path to BLCR Installation])])
    OMPI_CHECK_WITHDIR([blcr], [$with_blcr], [include/libcr.h])
    AC_ARG_WITH([blcr-libdir],
                [AC_HELP_STRING([--with-blcr-libdir=DIR],
                                [Search for BLCR libraries in DIR])])
    OMPI_CHECK_WITHDIR([blcr-libdir], [$with_blcr_libdir], [libcr.*])

    check_crs_blcr_good="no"

    # If we do not want FT, don't compile this component
    #
    # If we wanted BLCR, but did not specify the FT option,
    # error out with a warning for the user
    AS_IF([test "$ompi_want_ft_cr" = "0"],
          [$2
           check_crs_blcr_good="no"
           AS_IF([test ! -z "$with_blcr" -a "$with_blcr" != "no"],
                 [AC_MSG_WARN([BLCR support requested, but FT support not requested. You need to specify the --with-ft=cr configure option.])
                  AC_MSG_ERROR([Aborting.])])
          ],
          [check_crs_blcr_good="yes"])

    # If we do not want BLCR, then do not compile it
    AS_IF([test "$with_blcr" = "no" -o "$check_crs_blcr_good" = "no"],
          [$2
           check_crs_blcr_good="no"],
          [check_crs_blcr_good="yes"])

    # Defaults
    check_crs_blcr_dir_msg="compiler default"
    check_crs_blcr_libdir_msg="linker default"
    check_crs_blcr_dir=""
    check_crs_blcr_libdir=""

    # Determine the search paths for the headers and libraries
    AS_IF([test "$check_crs_blcr_good" != "yes"], [$2],
          [AS_IF([test ! -z "$with_blcr" -a "$with_blcr" != "yes"],
                 [check_crs_blcr_dir="$with_blcr"
                  check_crs_blcr_dir_msg="$with_blcr (from --with-blcr)"])
           AS_IF([test ! -z "$with_blcr_libdir" -a "$with_blcr_libdir" != "yes"],
                 [check_crs_blcr_libdir="$with_blcr_libdir"
                  check_crs_blcr_libdir_msg="$with_blcr_libdir (from --with-blcr-libdir)"])
          ])

    AS_IF([test "$check_crs_blcr_good" != "yes"], [$2],
          [AC_MSG_CHECKING([for BLCR dir])
           AC_MSG_RESULT([$check_crs_blcr_dir_msg])
           AC_MSG_CHECKING([for BLCR library dir])
           AC_MSG_RESULT([$check_crs_blcr_libdir_msg])
           OMPI_CHECK_PACKAGE([crs_blcr_check],
                              [libcr.h],
                              [cr],
                              [cr_init],
                              [],
                              [$check_crs_blcr_dir],
                              [$check_crs_blcr_libdir],
                              [check_crs_blcr_good="yes"],
                              [check_crs_blcr_good="no"])
          ])

    crs_blcr_save_CFLAGS="$CFLAGS"
    crs_blcr_save_CPPFLAGS="$CPPFLAGS"
    crs_blcr_save_LDFLAGS="$LDFLAGS"
    crs_blcr_save_LIBS="$LIBS"

    crs_blcr_CFLAGS="$CFLAGS $crs_blcr_check_CFLAGS"
    crs_blcr_CPPFLAGS="$CPPFLAGS $crs_blcr_check_CPPFLAGS"
    crs_blcr_LDFLAGS="$LDFLAGS $crs_blcr_check_LDFLAGS"
    crs_blcr_LIBS="$LIBS $crs_blcr_check_LIBS"

    # Check to see if we found the BLCR libcr.h library
    # If we did then add the arguments to the wrapper compiler
    AS_IF([test "$check_crs_blcr_good" != "yes"], [$2],
          [
           #
           # Since BLCR libraries are not fully ISO99 C compliant
           # -pedantic and -Wundef raise a bunch of warnings, so
           # we just strip them off for this component
           AC_MSG_WARN([Removed -pedantic and -Wundef from CFLAGS for blcr component because libcr.h is not really ANSI C])
           # Strip off problematic arguments
           crs_blcr_CFLAGS="`echo $crs_blcr_CFLAGS | sed 's/-pedantic//g'`"
           crs_blcr_CFLAGS="`echo $crs_blcr_CFLAGS | sed 's/-Wundef//g'`"
           crs_blcr_CPPFLAGS="`echo $crs_blcr_CPPFLAGS | sed 's/-pedantic//g'`"
           crs_blcr_CPPFLAGS="`echo $crs_blcr_CPPFLAGS | sed 's/-Wundef//g'`"
           crs_blcr_LDFLAGS="$crs_blcr_LDFLAGS"
           crs_blcr_LIBS="$crs_blcr_LIBS"
           #
           # Setup wrapper options for static builds
           #
           crs_blcr_WRAPPER_EXTRA_CPPFLAGS="$crs_blcr_CPPFLAGS"
           crs_blcr_WRAPPER_EXTRA_LDFLAGS="$crs_blcr_LDFLAGS"
           crs_blcr_WRAPPER_EXTRA_LIBS="$crs_blcr_LIBS"
           $1])

    #
    # Check for version difference which may have:
    # - working cr_request_file
    # - working cr_request_checkpoint (which should be used instead of cr_request_file)
    # - 'requester' parameter to checkpoint_info
    #
    AS_IF([test "$check_crs_blcr_good" != "yes"], [$2], [
            CFLAGS="$crs_blcr_CFLAGS"
            CPPFLAGS="$crs_blcr_CPPFLAGS"
            LDFLAGS="$crs_blcr_LDFLAGS"
            LIBS="$crs_blcr_LIBS"
           #
           # First look for the cr_request_file function
           #
           crs_blcr_have_working_cr_request=0
           AC_MSG_CHECKING(for BLCR working cr_request)
           OMPI_CHECK_FUNC_LIB([cr_request_file],[cr],
               [AC_TRY_COMPILE([#include <libcr.h>],
                       [#if CR_RELEASE_MAJOR <= 0 && CR_RELEASE_MINOR < 6
                        #error Version earlier than 0.6.0
                        #endif
                           ],
                       [crs_blcr_have_working_cr_request=1
                           ],
                       [crs_blcr_have_working_cr_request=0
                           AC_MSG_WARN([This BLCR version does not contain a known working version of cr_request_file])
                           ])],
               [crs_blcr_have_working_cr_request=0
                   AC_MSG_WARN([This BLCR version does not contain the cr_request_file function])
                   ])
           AC_DEFINE_UNQUOTED([CRS_BLCR_HAVE_CR_REQUEST], [$crs_blcr_have_working_cr_request],
               [BLCR cr_request_file check])

           #
           # Look for the cr_request_checkpoint function
           #
           crs_blcr_have_cr_request_checkpoint=0
           AC_MSG_CHECKING(for BLCR cr_request_checkpoint)
           OMPI_CHECK_FUNC_LIB([cr_request_checkpoint],[cr],
               [crs_blcr_have_cr_request_checkpoint=1
                   ],
               [crs_blcr_have_cr_request_checkpoint=0
                   AC_MSG_WARN([This BLCR version does not contain the cr_request_checkpoint function])
                   ])
           AC_DEFINE_UNQUOTED([CRS_BLCR_HAVE_CR_REQUEST_CHECKPOINT], [$crs_blcr_have_cr_request_checkpoint],
               [BLCR cr_request_checkpoint check])

           #
           # Look for the cr_checkpoint_info.requester member
           #
           crs_blcr_have_info_requester=0
           AC_CHECK_MEMBER([struct cr_checkpoint_info.requester],
               [crs_blcr_have_info_requester=1],
               [AC_MSG_WARN([This BLCR version does not contain a 'requester' member of the 'cr_checkpoint_info' struct])],
               [#include <libcr.h>])
           AC_DEFINE_UNQUOTED([CRS_BLCR_HAVE_INFO_REQUESTER], [$crs_blcr_have_info_requester],
               [BLCRs cr_checkpoint_info.requester member availability])
           $1])

    #
    # Reset the flags
    #
    CFLAGS="$crs_blcr_save_CFLAGS"
    CPPFLAGS="$crs_blcr_save_CPPFLAGS"
    LDFLAGS="$crs_blcr_save_LDFLAGS"
    LIBS="$crs_blcr_save_LIBS"

    #
    AS_IF([test "$check_crs_blcr_good" = "yes"],
          [ AC_SUBST([crs_blcr_WRAPPER_EXTRA_LDFLAGS])
            AC_SUBST([crs_blcr_WRAPPER_EXTRA_LIBS])
            AC_SUBST([crs_blcr_WRAPPER_EXTRA_CPPFLAGS])
            AC_SUBST([crs_blcr_CFLAGS])
            AC_SUBST([crs_blcr_CPPFLAGS])
            AC_SUBST([crs_blcr_LDFLAGS])
            AC_SUBST([crs_blcr_LIBS])
            $1],
          [AS_IF([test ! -z "$with_blcr" -a "$with_blcr" != "no"],
                 [AC_MSG_WARN([BLCR support requested but not found.  Perhaps you need to specify the location of the BLCR libraries.])
                  AC_MSG_ERROR([Aborting.])])
           $3])

])dnl
