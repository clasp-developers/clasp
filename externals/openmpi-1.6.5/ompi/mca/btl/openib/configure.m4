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
# Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2008-2011 Mellanox Technologies.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_btl_openib_POST_CONFIG([should_build])
# ------------------------------------------
AC_DEFUN([MCA_btl_openib_POST_CONFIG], [
    AM_CONDITIONAL([MCA_btl_openib_have_xrc], [test $1 -eq 1 -a "x$btl_openib_have_xrc" = "x1"])
    AM_CONDITIONAL([MCA_btl_openib_have_rdmacm], [test $1 -eq 1 -a "x$btl_openib_have_rdmacm" = "x1"])
    AM_CONDITIONAL([MCA_btl_openib_have_dynamic_sl], [test $1 -eq 1 -a "x$btl_openib_have_opensm_devel" = "x1"])
])


# MCA_btl_openib_CONFIG([action-if-can-compile], 
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_btl_openib_CONFIG],[
    OMPI_VAR_SCOPE_PUSH([cpcs have_threads])
    cpcs="oob"

    OMPI_CHECK_OPENIB([btl_openib],
                     [btl_openib_happy="yes"],
                     [btl_openib_happy="no"])

    AS_IF([test "$btl_openib_happy" = "yes"],
          [btl_openib_WRAPPER_EXTRA_LDFLAGS="$btl_openib_LDFLAGS"
           btl_openib_WRAPPER_EXTRA_LIBS="$btl_openib_LIBS"

           # With the new openib flags, look for ibv_fork_init
           LDFLAGS_save="$LDFLAGS"
           LIBS_save="$LIBS"
           LDFLAGS="$LDFLAGS $btl_openib_LDFLAGS"
           LIBS="$LIBS $btl_openib_LIBS"
           AC_CHECK_FUNCS([ibv_fork_init])
           LDFLAGS="$LDFLAGS_save"
           LIBS="$LIBS_save"
           $1],
          [$2])

    AC_MSG_CHECKING([for thread support (needed for rdmacm)])
    have_threads=`echo $THREAD_TYPE | awk '{ print [$]1 }'`
    if test "x$have_threads" = "x"; then
        have_threads=none
    fi
    AC_MSG_RESULT([$have_threads])

    AS_IF([test "$btl_openib_happy" = "yes"],
          [if test "x$btl_openib_have_xrc" = "x1"; then
              cpcs="$cpcs xoob"
          fi
          if test "x$btl_openib_have_rdmacm" = "x1" -a \
                  "$have_threads" != "none"; then
              cpcs="$cpcs rdmacm"
          fi
          AC_MSG_CHECKING([which openib btl cpcs will be built])
          AC_MSG_RESULT([$cpcs])])

    # Enable openib device failover.  It is disabled by default.
    AC_MSG_CHECKING([whether openib failover is enabled])
    AC_ARG_ENABLE([btl-openib-failover],
       [AC_HELP_STRING([--enable-btl-openib-failover],
           [enable openib BTL failover (default: disabled)])])
    if test "$enable_btl_openib_failover" = "yes"; then
        AC_MSG_RESULT([yes])
        btl_openib_failover_enabled=1
    else
        AC_MSG_RESULT([no])
        btl_openib_failover_enabled=0
    fi
    AC_DEFINE_UNQUOTED([BTL_OPENIB_FAILOVER_ENABLED], [$btl_openib_failover_enabled],
                       [enable openib BTL failover])
    AM_CONDITIONAL([MCA_btl_openib_enable_failover], [test "x$btl_openib_failover_enabled" = "x1"])


    # Check for __malloc_hook availability
    AC_ARG_ENABLE(btl-openib-malloc-alignment,
    	AC_HELP_STRING([--enable-btl-openib-malloc-alignment], [Enable support for allocated memory alignment. Default: enabled if supported, disabled otherwise.]))

    btl_openib_malloc_hooks_enabled=0
    AS_IF([test "$enable_btl_openib_malloc_alignment" != "no"],
        [AC_CHECK_HEADER([malloc.h],
             [AC_CHECK_FUNC([__malloc_hook],
                  [AC_CHECK_FUNC([__realloc_hook],
                       [AC_CHECK_FUNC([__free_hook],
                            [btl_openib_malloc_hooks_enabled=1])])])])])

    AS_IF([test "$enable_btl_openib_malloc_alignment" = "yes" -a "$btl_openib_malloc_hooks_enabled" = "0"],
          [AC_MSG_ERROR([openib malloc alignment is requested but __malloc_hook is not available])])
    AC_MSG_CHECKING([whether the openib BTL will use malloc hooks])
    AS_IF([test "$btl_openib_malloc_hooks_enabled" = "0"],
          [AC_MSG_RESULT([no])],
          [AC_MSG_RESULT([yes])])

    AC_DEFINE_UNQUOTED(BTL_OPENIB_MALLOC_HOOKS_ENABLED, [$btl_openib_malloc_hooks_enabled],
                       [Whether the openib BTL malloc hooks are enabled]) 
   
    # substitute in the things needed to build openib
    AC_SUBST([btl_openib_CFLAGS])
    AC_SUBST([btl_openib_CPPFLAGS])
    AC_SUBST([btl_openib_LDFLAGS])
    AC_SUBST([btl_openib_LIBS])

    OMPI_VAR_SCOPE_POP
])dnl
