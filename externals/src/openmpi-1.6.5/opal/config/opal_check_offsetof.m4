# -*- shell-script -*-
#
# Copyright (c) 2009      IBM Corporation.  All rights reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This macro checks to ensure that the compiler properly supports
# offsetof().  The PGI compilers had a problem with this macro in some
# versions of their compiler on some platforms (e.g., 9.0-1 on
# x86_64).  The workaround is to use -DNO_PGI_OFFSET in these cases.
# A bug report was submitted to PGI support in late June 2009; the
# problem was apparently a trivial typo in one of their header files
# and should be fixed in subsequent releases (e.g., 9.0-2?).

AC_DEFUN([OPAL_CHECK_OFFSETOF],[
    OMPI_VAR_SCOPE_PUSH([have_offsetof_msg])

    AC_MSG_CHECKING(for functional offsetof macro)
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include<stddef.h>]],
                                      [[struct foo {int a, b;}; size_t offset = offsetof(struct foo, b); ]])],
                      [have_offsetof_msg="yes"], [have_offsetof_msg="no"])
    if test "$have_offsetof_msg" = "no"; then
        CPPFLAGS="$CPPFLAGS -DNO_PGI_OFFSET"
        AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include<stddef.h>]],
                                          [[struct foo {int a, b;}; size_t offset = offsetof(struct foo, b); ]])],
                          [have_offsetof_msg="yes"], [have_offsetof_msg="no"])

        if test "$have_offsetof_msg" = "no"; then
            AC_MSG_RESULT([$have_offsetof_msg])
            AC_MSG_WARN([Your compiler does not support offsetof macro])
            AC_MSG_ERROR([Configure: Cannot continue])
        fi
    fi

    AC_MSG_RESULT([$have_offsetof_msg])
    OMPI_VAR_SCOPE_POP
])dnl

