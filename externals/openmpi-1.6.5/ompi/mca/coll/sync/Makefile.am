#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
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

EXTRA_DIST = .windows

dist_pkgdata_DATA = help-coll-sync.txt

sources = \
        coll_sync.h \
        coll_sync_component.c \
        coll_sync_module.c \
        coll_sync_bcast.c \
        coll_sync_exscan.c \
        coll_sync_gather.c \
        coll_sync_gatherv.c \
        coll_sync_reduce.c \
        coll_sync_reduce_scatter.c \
        coll_sync_scan.c \
        coll_sync_scatter.c \
        coll_sync_scatterv.c

if OMPI_BUILD_coll_sync_DSO
component_noinst =
component_install = mca_coll_sync.la
else
component_noinst = libmca_coll_sync.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_sync_la_SOURCES = $(sources)
mca_coll_sync_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_sync_la_SOURCES =$(sources)
libmca_coll_sync_la_LDFLAGS = -module -avoid-version
