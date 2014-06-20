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

dist_pkgdata_DATA = help-mpi-coll-sm.txt

not_used_yet = \
        coll_sm_allgather.c \
        coll_sm_allgatherv.c \
        coll_sm_alltoall.c \
        coll_sm_alltoallv.c \
        coll_sm_alltoallw.c \
        coll_sm_gather.c \
        coll_sm_gatherv.c \
        coll_sm_reduce_scatter.c \
        coll_sm_scan.c \
        coll_sm_exscan.c \
        coll_sm_scatter.c \
        coll_sm_scatterv.c

sources = \
        coll_sm.h \
        coll_sm_allreduce.c \
        coll_sm_barrier.c \
        coll_sm_bcast.c \
        coll_sm_component.c \
        coll_sm_module.c \
        coll_sm_reduce.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

component_noinst =
component_install =
if OMPI_BUILD_coll_sm_DSO
component_install += mca_coll_sm.la
else
component_noinst += libmca_coll_sm.la
endif

# See ompi/mca/btl/sm/Makefile.am for an explanation of
# libmca_common_sm.la.

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_sm_la_SOURCES = $(sources)
mca_coll_sm_la_LDFLAGS = -module -avoid-version
mca_coll_sm_la_LIBADD = \
        $(top_ompi_builddir)/ompi/mca/common/sm/libmca_common_sm.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_sm_la_SOURCES =$(sources)
libmca_coll_sm_la_LDFLAGS = -module -avoid-version
