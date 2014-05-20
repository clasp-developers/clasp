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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST = .windows

sources = \
        coll_self.h \
        coll_self_allgather.c \
        coll_self_allgatherv.c \
        coll_self_allreduce.c \
        coll_self_alltoall.c \
        coll_self_alltoallv.c \
        coll_self_alltoallw.c \
        coll_self_barrier.c \
        coll_self_bcast.c \
        coll_self_component.c \
        coll_self_gather.c \
        coll_self_gatherv.c \
        coll_self_module.c \
        coll_self_reduce.c \
        coll_self_reduce_scatter.c \
        coll_self_scan.c \
        coll_self_exscan.c \
        coll_self_scatter.c \
        coll_self_scatterv.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_coll_self_DSO
component_noinst =
component_install = mca_coll_self.la
else
component_noinst = libmca_coll_self.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_self_la_SOURCES = $(sources)
mca_coll_self_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_self_la_SOURCES =$(sources)
libmca_coll_self_la_LDFLAGS = -module -avoid-version
