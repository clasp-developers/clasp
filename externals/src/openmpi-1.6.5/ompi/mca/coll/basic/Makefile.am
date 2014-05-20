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
        coll_basic.h \
        coll_basic_allgather.c \
        coll_basic_allgatherv.c \
        coll_basic_allreduce.c \
        coll_basic_alltoall.c \
        coll_basic_alltoallv.c \
        coll_basic_alltoallw.c \
        coll_basic_barrier.c \
        coll_basic_bcast.c \
        coll_basic_component.c \
        coll_basic_gather.c \
        coll_basic_gatherv.c \
        coll_basic_module.c \
        coll_basic_reduce.c \
        coll_basic_reduce_scatter.c \
        coll_basic_scan.c \
        coll_basic_exscan.c \
        coll_basic_scatter.c \
        coll_basic_scatterv.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_coll_basic_DSO
component_noinst =
component_install = mca_coll_basic.la
else
component_noinst = libmca_coll_basic.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_basic_la_SOURCES = $(sources)
mca_coll_basic_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_basic_la_SOURCES =$(sources)
libmca_coll_basic_la_LDFLAGS = -module -avoid-version
