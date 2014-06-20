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
# Copyright (c) 2006-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
#                         reserved.
# Copyright (c) 2006-2009 Mellanox Technologies. All rights reserved.
# Copyright (c) 2010-2012 Oracle and/or its affiliates.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# OMPI_CHECK_OPENIB(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if OPENIB support can be found.  sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_OPENIB],[
    OMPI_VAR_SCOPE_PUSH([$1_msg])

    #
    # Openfabrics support
    #
    AC_ARG_WITH([openib],
        [AC_HELP_STRING([--with-openib(=DIR)],
             [Build OpenFabrics support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    OMPI_CHECK_WITHDIR([openib], [$with_openib], [include/infiniband/verbs.h])
    AC_ARG_WITH([openib-libdir],
       [AC_HELP_STRING([--with-openib-libdir=DIR],
             [Search for OpenFabrics libraries in DIR])])
    OMPI_CHECK_WITHDIR([openib-libdir], [$with_openib_libdir], [libibverbs.*])

    #
    # Add padding to OpenIB header
    #
    AC_ARG_ENABLE([openib-control-hdr-padding],
        [AC_HELP_STRING([--enable-openib-control-hdr-padding],
            [Add padding bytes to the openib control header (default:disabled)])])
    AC_MSG_CHECKING([if want to add padding to the openib control header])
    if test "$enable_openib_control_hdr_padding" = "yes"; then
        AC_MSG_RESULT([yes])
        ompi_openib_pad_hdr=1
    elif test "$enable_openib_control_hdr_padding" = "no"; then
        AC_MSG_RESULT([no])
        ompi_openib_pad_hdr=0
    else
        #
        # Enable padding for SPARC platforms by default, because the
        # btl will segv otherwise.  Keep padding disabled for other 
        # platforms since there are some performance implications with
        # padding on for those plaforms.
        #
        case "${host}" in
        sparc*)
            AC_MSG_RESULT([yes (enabled by default on SPARC)])
            ompi_openib_pad_hdr=1
            ;;
        *)
            AC_MSG_RESULT([no])
            ompi_openib_pad_hdr=0
            ;;
        esac
    fi
    AC_DEFINE_UNQUOTED([OMPI_OPENIB_PAD_HDR], [$ompi_openib_pad_hdr],
                       [Add padding bytes to the openib control header])

    #
    # ConnectX XRC support
    #
    AC_ARG_ENABLE([openib-connectx-xrc],
        [AC_HELP_STRING([--enable-openib-connectx-xrc],
                        [Enable ConnectX XRC support. If you do not have InfiniBand ConnectX adapters, you may disable the ConnectX XRC support. If you do not know which InfiniBand adapter is installed on your cluster, leave this option enabled (default: enabled)])],
                        [enable_connectx_xrc="$enableval"], [enable_connectx_xrc="yes"])

    #
    # Openfabrics RDMACM
    #
    AC_ARG_ENABLE([openib-rdmacm],
        [AC_HELP_STRING([--enable-openib-rdmacm],
                        [Enable Open Fabrics RDMACM support in openib BTL (default: enabled)])])

    AS_IF([test ! -z "$with_openib" -a "$with_openib" != "yes"],
          [ompi_check_openib_dir="$with_openib"])
    AS_IF([test ! -z "$with_openib_libdir" -a "$with_openib_libdir" != "yes"],
          [ompi_check_openib_libdir="$with_openib_libdir"])
    AS_IF([test "$with_openib" = "no"],
          [ompi_check_openib_happy="no"],
          [ompi_check_openib_happy="yes"])

    ompi_check_openib_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_openib_$1_save_LDFLAGS="$LDFLAGS"
    ompi_check_openib_$1_save_LIBS="$LIBS"

    AS_IF([test "$ompi_check_openib_happy" = "yes"],
          [AS_IF([test "$THREAD_TYPE" != "posix" -a "$memory_ptmalloc2_happy" = "yes"],
                 [AS_IF([test "$enable_ptmalloc2_internal" = "yes"],
                        [AC_MSG_WARN([POSIX threads are disabled, but])
                         AC_MSG_WARN([--enable-ptmalloc2-internal was specified.  This will])
                         AC_MSG_WARN([cause memory corruption with OpenFabrics.])
                         AC_MSG_WARN([Not building component.])
                         ompi_check_openib_happy="no"],
                        [AC_MSG_WARN([POSIX threads are disabled, but the ptmalloc2 memory])
                         AC_MSG_WARN([manager is being built.  Compiling MPI applications with])
                         AC_MSG_WARN([-lopenmpi-malloc will result in memory corruption; Open])
                         AC_MSG_WARN([MPI will disable the openib BTL at run-time if such a])
                         AC_MSG_WARN([combination is detected.])
                         AC_MSG_WARN([You have been warned.])])])])

    AS_IF([test "$ompi_check_openib_happy" = "yes"], 
            [AC_CHECK_HEADERS(
                fcntl.h sys/poll.h,
                    [],
                    [AC_MSG_WARN([fcntl.h sys/poll.h not found.  Can not build component.])
                    ompi_check_openib_happy="no"])]) 

    AS_IF([test "$ompi_check_openib_happy" = "yes"], 
          [OMPI_CHECK_PACKAGE([$1],
                              [infiniband/verbs.h],
                              [ibverbs],
                              [ibv_open_device],
                              [],
                              [$ompi_check_openib_dir],
                              [$ompi_check_openib_libdir],
                              [ompi_check_openib_happy="yes"],
                              [ompi_check_openib_happy="no"])])

    CPPFLAGS="$CPPFLAGS $$1_CPPFLAGS"
    LDFLAGS="$LDFLAGS $$1_LDFLAGS"
    LIBS="$LIBS $$1_LIBS"

    AS_IF([test "$ompi_check_openib_happy" = "yes"],
          [AC_CACHE_CHECK(
              [number of arguments to ibv_create_cq],
              [ompi_cv_func_ibv_create_cq_args],
              [AC_LINK_IFELSE(
                 [AC_LANG_PROGRAM(
                    [[#include <infiniband/verbs.h> ]],
                    [[ibv_create_cq(NULL, 0, NULL, NULL, 0);]])],
                 [ompi_cv_func_ibv_create_cq_args=5],
                 [AC_LINK_IFELSE(
                    [AC_LANG_PROGRAM(
                       [[#include <infiniband/verbs.h> ]],
                       [[ibv_create_cq(NULL, 0, NULL);]])],
                    [ompi_cv_func_ibv_create_cq_args=3],
                    [ompi_cv_func_ibv_create_cq_args="unknown"])])])
           AS_IF([test "$ompi_cv_func_ibv_create_cq_args" = "unknown"],
                 [AC_MSG_WARN([Can not determine number of args to ibv_create_cq.])
                  AC_MSG_WARN([Not building component.])
                  ompi_check_openib_happy="no"],
                 [AC_DEFINE_UNQUOTED([OMPI_IBV_CREATE_CQ_ARGS],
                                     [$ompi_cv_func_ibv_create_cq_args],
                                     [Number of arguments to ibv_create_cq])])])

    #
    # OpenIB dynamic SL
    #
    AC_ARG_ENABLE([openib-dynamic-sl],
        [AC_HELP_STRING([--enable-openib-dynamic-sl],
                        [Enable openib BTL to query Subnet Manager for IB SL (default: enabled)])])

    # Set these up so that we can do an AC_DEFINE below
    # (unconditionally)
    $1_have_xrc=0
    $1_have_rdmacm=0
    $1_have_opensm_devel=0

    # If we have the openib stuff available, find out what we've got
    AS_IF([test "$ompi_check_openib_happy" = "yes"],
          [AC_CHECK_DECLS([IBV_EVENT_CLIENT_REREGISTER, IBV_ACCESS_SO], [], [], 
                          [#include <infiniband/verbs.h>])
           AC_CHECK_FUNCS([ibv_get_device_list ibv_resize_cq])

           # struct ibv_device.transport_type was added in OFED v1.2
           AC_CHECK_MEMBERS([struct ibv_device.transport_type], [], [],
                            [#include <infiniband/verbs.h>])

           # ibv_create_xrc_rcv_qp was added in OFED 1.3
           if test "$enable_connectx_xrc" = "yes"; then
               AC_CHECK_FUNCS([ibv_create_xrc_rcv_qp], [$1_have_xrc=1])
           fi

           if test "no" != "$enable_openib_dynamic_sl"; then
               # We need ib_types.h file, which is installed with opensm-devel
               # package. However, ib_types.h has a bad include directive,
               # which will cause AC_CHECK_HEADER to fail.
               # So instead, we will look for another file that is also
               # installed as part of opensm-devel package and included in
               # ib_types.h, but it doesn't include any other IB-related files.
               AC_CHECK_HEADER([infiniband/complib/cl_types_osd.h],
                               [AC_CHECK_LIB([osmcomp], [cl_map_init],
                                             [$1_have_opensm_devel=1],[])],
                               [],
                               [])
               # Abort if dynamic SL support was explicitly requested but opensm-devel
               # package wasn't found. Otherwise, OMPI will be built w/o dynamic SL.
               AC_MSG_CHECKING([if can use dynamic SL support])
               AS_IF([test "$$1_have_opensm_devel" = "1"],
                     [AC_MSG_RESULT([yes])],
                     [AC_MSG_RESULT([no])
                      AS_IF([test "$enable_openib_dynamic_sl" = "yes"],
                            [AC_MSG_WARN([--enable-openib-dynamic-sl was specified but the])
                             AC_MSG_WARN([appropriate header/library files could not be found])
                             AC_MSG_WARN([Please install opensm-devel if you need dynamic SL support])
                             AC_MSG_ERROR([Cannot continue])])])
           fi

           # Do we have a recent enough RDMA CM?  Need to have the
           # rdma_get_peer_addr (inline) function (originally appeared
           # in OFED v1.3).
           if test "$enable_openib_rdmacm" != "no"; then
                 AC_CHECK_HEADERS([rdma/rdma_cma.h],
                     [AC_CHECK_LIB([rdmacm], [rdma_create_id],
                         [AC_MSG_CHECKING([for rdma_get_peer_addr])
                         $1_msg=no
                         AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include "rdma/rdma_cma.h"
                                 ]], [[void *ret = (void*) rdma_get_peer_addr((struct rdma_cm_id*)0);]])],
                             [$1_have_rdmacm=1 
                             $1_msg=yes])
                         AC_MSG_RESULT([$$1_msg])])])

                 if test "1" = "$$1_have_rdmacm"; then
                     $1_LIBS="-lrdmacm $$1_LIBS"
                 else
                     AS_IF([test "$enable_openib_rdmacm" = "yes"],
                           [AC_MSG_WARN([--enable-openib-rdmacm was specified but the])
                            AC_MSG_WARN([appropriate files could not be found])
                            AC_MSG_WARN([Please install librdmacm and librdmacm-devel or disable rdmacm support])
                            AC_MSG_ERROR([Cannot continue])])
                 fi
           fi

           # Check support for RDMAoE devices
           $1_have_rdmaoe=0
           AC_CHECK_DECLS([IBV_LINK_LAYER_ETHERNET],
                          [$1_have_rdmaoe=1], [],
                          [#include <infiniband/verbs.h>])

           AC_MSG_CHECKING([if RDMAoE support is enabled])
           if test "1" = "$$1_have_rdmaoe"; then
                AC_DEFINE_UNQUOTED([OMPI_HAVE_RDMAOE], [$$1_have_rdmaoe], [Enable RDMAoE support])
                AC_MSG_RESULT([yes])
           else
                AC_MSG_RESULT([no])
           fi

          ])

    # Check to see if <infiniband/driver.h> works.  It is known to
    # create problems on some platforms with some compilers (e.g.,
    # RHEL4U3 with the PGI 32 bit compiler).  Use undocumented (in AC
    # 2.63) feature of AC_CHECK_HEADERS: if you explicitly pass in
    # AC_INCLUDES_DEFAULT as the 4th arg to AC_CHECK_HEADERS, the test
    # will fail if the header is present but not compilable, *but it
    # will not print the big scary warning*.  See
    # http://lists.gnu.org/archive/html/autoconf/2008-10/msg00143.html.
    AS_IF([test "$ompi_check_openib_happy" = "yes"],
          [AC_CHECK_HEADERS([infiniband/driver.h], [], [], 
                            [AC_INCLUDES_DEFAULT])])

    AC_MSG_CHECKING([if ConnectX XRC support is enabled])
    AC_DEFINE_UNQUOTED([OMPI_HAVE_CONNECTX_XRC], [$$1_have_xrc],
        [Enable features required for ConnectX XRC support])
    if test "1" = "$$1_have_xrc"; then
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
    fi

    AC_MSG_CHECKING([if dynamic SL is enabled])
    AC_DEFINE_UNQUOTED([OMPI_ENABLE_DYNAMIC_SL], [$$1_have_opensm_devel],
        [Enable features required for dynamic SL support])
    if test "1" = "$$1_have_opensm_devel"; then
        AC_MSG_RESULT([yes])
        $1_LIBS="-losmcomp $$1_LIBS"
    else
        AC_MSG_RESULT([no])
    fi

    AC_MSG_CHECKING([if OpenFabrics RDMACM support is enabled])
    AC_DEFINE_UNQUOTED([OMPI_HAVE_RDMACM], [$$1_have_rdmacm],
        [Whether RDMA CM is available or not])
    if test "1" = "$$1_have_rdmacm"; then
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
    fi

    AS_IF([test -z "$ompi_check_openib_dir"],
          [openib_include_dir="/usr/include"],
          [openib_include_dir="$ompi_check_openib_dir/include"])

    CPPFLAGS="$ompi_check_openib_$1_save_CPPFLAGS -I$openib_include_dir/infiniband"
    LDFLAGS="$ompi_check_openib_$1_save_LDFLAGS"
    LIBS="$ompi_check_openib_$1_save_LIBS"

    AS_IF([test "$ompi_check_openib_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_openib" -a "$with_openib" != "no"],
                 [AC_MSG_WARN([OpenFabrics support requested (via --with-openib) but not found.])
                  AC_MSG_WARN([If you are using libibverbs v1.0 (i.e., OFED v1.0 or v1.1), you *MUST* have both the libsysfs headers and libraries installed.  Later versions of libibverbs do not require libsysfs.])
                  AC_MSG_ERROR([Aborting.])])
           $3])

     OMPI_VAR_SCOPE_POP
])

