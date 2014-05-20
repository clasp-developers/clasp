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

AM_CPPFLAGS = $(mpool_rdma_CPPFLAGS)

sources = \
	mpool_rdma_module.c \
	mpool_rdma_component.c

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/ompi/mca/mpool/rdma
ompi_HEADERS = mpool_rdma.h
else
ompidir = $(includedir)
ompi_HEADERS =
endif

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_mpool_rdma_DSO
component_noinst =
component_install = mca_mpool_rdma.la
else
component_noinst = libmca_mpool_rdma.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_mpool_rdma_la_SOURCES = $(sources)
mca_mpool_rdma_la_LDFLAGS = -module -avoid-version
mca_mpool_rdma_la_LIBADD = $(mpool_rdma_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_mpool_rdma_la_SOURCES = $(sources)
libmca_mpool_rdma_la_LDFLAGS = -module -avoid-version
libmca_mpool_rdma_la_LIBADD = $(mpool_rdma_LIBS)
