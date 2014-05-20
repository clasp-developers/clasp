# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2009-2011 Oracle and/or its affiliates.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# OMPI_CHECK_VISIBILITY
# --------------------------------------------------------
AC_DEFUN([OMPI_CHECK_VISIBILITY],[
    AC_REQUIRE([AC_PROG_GREP])

    # Check if the compiler has support for visibility, like some
    # versions of gcc, icc Sun Studio cc.
    AC_ARG_ENABLE(visibility, 
        AC_HELP_STRING([--enable-visibility],
            [enable visibility feature of certain compilers/linkers (default: enabled)]))

    opal_visibility_define=0
    opal_msg="whether to enable symbol visibility"

    if test "$enable_visibility" = "no"; then
        AC_MSG_CHECKING([$opal_msg])
        AC_MSG_RESULT([no (disabled)]) 
    else
        CFLAGS_orig=$CFLAGS

        opal_add=
        case "$ompi_c_vendor" in
        sun)
            # Check using Sun Studio -xldscope=hidden flag
            opal_add=-xldscope=hidden
            CFLAGS="$OMPI_CFLAGS_BEFORE_PICKY $opal_add -errwarn=%all"
            ;;

        *)
            # Check using -fvisibility=hidden
            opal_add=-fvisibility=hidden
            CFLAGS="$OMPI_CFLAGS_BEFORE_PICKY $opal_add -Werror"
            ;;
        esac

        AC_MSG_CHECKING([if $CC supports $opal_add])
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdio.h>
            __attribute__((visibility("default"))) int foo;
            ]],[[fprintf(stderr, "Hello, world\n");]])],
            [AS_IF([test -s conftest.err],
                   [$GREP -iq visibility conftest.err
                    # If we find "visibility" in the stderr, then
                    # assume it doesn't work
                    AS_IF([test "$?" = "0"], [opal_add=])])
            ], [opal_add=])
        AS_IF([test "$opal_add" = ""],
              [AC_MSG_RESULT([no])],
              [AC_MSG_RESULT([yes])])

        CFLAGS=$CFLAGS_orig
        OPAL_VISIBILITY_CFLAGS=$opal_add

        if test "$opal_add" != "" ; then
            opal_visibility_define=1
            AC_MSG_CHECKING([$opal_msg])
            AC_MSG_RESULT([yes (via $opal_add)])
        elif test "$enable_visibility" = "yes"; then
            AC_MSG_ERROR([Symbol visibility support requested but compiler does not seem to support it.  Aborting])
        else
            AC_MSG_CHECKING([$opal_msg])
            AC_MSG_RESULT([no (unsupported)])
        fi
        unset opal_add
    fi
    AC_DEFINE_UNQUOTED([OPAL_C_HAVE_VISIBILITY], [$opal_visibility_define],
            [Whether C compiler supports symbol visibility or not])

])
