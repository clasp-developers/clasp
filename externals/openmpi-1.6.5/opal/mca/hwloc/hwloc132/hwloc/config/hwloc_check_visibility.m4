# This macro set originally copied from Open MPI:
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
# Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
# and renamed/modified for hwloc:
# Copyright (c) 2009 inria.  All rights reserved.
# Copyright (c) 2009-2010 Université Bordeaux 1
# Copyright © 2010-2012 Cisco Systems, Inc.  All rights reserved.
# See COPYING in top-level directory.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
# 
# - Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
# 
# - Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer listed
#   in this license in the documentation and/or other materials
#   provided with the distribution.
# 
# - Neither the name of the copyright holders nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
# 
# The copyright holders provide no reassurances that the source code
# provided does not infringe any patent, copyright, or any other
# intellectual property rights of third parties.  The copyright holders
# disclaim any liability to any recipient for claims brought against
# recipient by any third party for infringement of that parties
# intellectual property rights.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

# _HWLOC_CHECK_VISIBILITY
# --------------------------------------------------------
AC_DEFUN([_HWLOC_CHECK_VISIBILITY],[
    # Be safe for systems that have ancient Autoconf's (e.g., RHEL5)
    m4_ifdef([AC_PROG_GREP], 
             [AC_REQUIRE([AC_PROG_GREP])],
             [GREP=grep])

    # Check if the compiler has support for visibility, like some
    # versions of gcc, icc, Sun Studio cc.
    AC_ARG_ENABLE(visibility, 
        AC_HELP_STRING([--enable-visibility],
            [enable visibility feature of certain compilers/linkers (default: enabled on platforms that support it)]))

    case ${target} in
        *-*-aix*|*-*-mingw*|*-*-cygwin*|*-*-hpux*)
            enable_visibility=no
            ;;
    esac

    hwloc_visibility_define=0
    hwloc_msg="whether to enable symbol visibility"
    if test "$enable_visibility" = "no"; then
        AC_MSG_CHECKING([$hwloc_msg])
        AC_MSG_RESULT([no (disabled)]) 
    else
        CFLAGS_orig=$CFLAGS

        hwloc_add=
        case "$hwloc_c_vendor" in
        sun)
            # Check using Sun Studio -xldscope=hidden flag
            hwloc_add=-xldscope=hidden
            CFLAGS="$CFLAGS_orig $hwloc_add -errwarn=%all"
            ;;

        *)
            # Check using -fvisibility=hidden
            hwloc_add=-fvisibility=hidden
            CFLAGS="$CFLAGS_orig $hwloc_add -Werror"
            ;;
        esac

        AC_MSG_CHECKING([if $CC supports $hwloc_add])
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdio.h>
            __attribute__((visibility("default"))) int foo;
            ]],[[fprintf(stderr, "Hello, world\n");]])],
            [AS_IF([test -s conftest.err],
                   [$GREP -iq visibility conftest.err
                    # If we find "visibility" in the stderr, then
                    # assume it doesn't work
                    AS_IF([test "$?" = "0"], [hwloc_add=])])
            ], [hwloc_add=])
        AS_IF([test "$hwloc_add" = ""],
              [AC_MSG_RESULT([no])],
              [AC_MSG_RESULT([yes])])

        CFLAGS=$CFLAGS_orig
        HWLOC_VISIBILITY_CFLAGS=$hwloc_add

        if test "$hwloc_add" != "" ; then
            hwloc_visibility_define=1
            AC_MSG_CHECKING([$hwloc_msg])
            AC_MSG_RESULT([yes (via $hwloc_add)]) 
        elif test "$enable_visibility" = "yes"; then
            AC_MSG_ERROR([Symbol visibility support requested but compiler does not seem to support it.  Aborting])
        else 
            AC_MSG_CHECKING([$hwloc_msg])
            AC_MSG_RESULT([no (unsupported)]) 
        fi
        unset hwloc_add 
    fi

    AC_DEFINE_UNQUOTED([HWLOC_C_HAVE_VISIBILITY], [$hwloc_visibility_define],
            [Whether C compiler supports symbol visibility or not])
])
