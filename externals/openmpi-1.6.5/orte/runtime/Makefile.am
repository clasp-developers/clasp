# -*- makefile -*-
#
# Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This makefile.am does not stand on its own - it is included from orte/Makefile.am

dist_pkgdata_DATA += runtime/help-orte-runtime.txt

headers += \
        runtime/runtime.h \
	runtime/orte_locks.h \
        runtime/orte_globals.h \
        runtime/runtime_internals.h \
        runtime/data_type_support/orte_dt_support.h

libopen_rte_la_SOURCES += \
        runtime/orte_finalize.c \
        runtime/orte_init.c \
        runtime/orte_locks.c \
        runtime/orte_globals.c \
        runtime/data_type_support/orte_dt_compare_fns.c \
        runtime/data_type_support/orte_dt_copy_fns.c \
        runtime/data_type_support/orte_dt_print_fns.c \
        runtime/data_type_support/orte_dt_release_fns.c \
        runtime/data_type_support/orte_dt_size_fns.c \
        runtime/data_type_support/orte_dt_packing_fns.c \
        runtime/data_type_support/orte_dt_unpacking_fns.c \
        runtime/orte_mca_params.c

if !ORTE_DISABLE_FULL_SUPPORT

headers += \
	runtime/orte_wait.h \
        runtime/orte_cr.h \
        runtime/orte_data_server.h

libopen_rte_la_SOURCES += \
        runtime/orte_wait.c \
        runtime/orte_cr.c \
        runtime/orte_data_server.c
endif
