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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_btl_portals_DSO
component_noinst =
component_install = mca_btl_portals.la
else
component_noinst = libmca_btl_portals.la
component_install =
endif

portals_SOURCES = \
	btl_portals.h \
	btl_portals_endpoint.h \
	btl_portals_frag.h \
	btl_portals_send.h \
	btl_portals_recv.h \
	btl_portals.c \
	btl_portals_component.c \
	btl_portals_frag.c \
	btl_portals_send.c \
	btl_portals_recv.c \
	btl_portals_rdma.c

AM_CPPFLAGS = $(btl_portals_CPPFLAGS)

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_btl_portals_la_SOURCES = $(portals_SOURCES)
nodist_mca_btl_portals_la_SOURCES = $(portals_nodist_SOURCES)
mca_btl_portals_la_LIBADD = \
    $(btl_portals_LIBS) \
    $(top_ompi_builddir)/ompi/mca/common/portals/libmca_common_portals.la
mca_btl_portals_la_LDFLAGS = -module -avoid-version $(btl_portals_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_btl_portals_la_SOURCES = $(portals_SOURCES)
nodist_libmca_btl_portals_la_SOURCES = $(portals_nodist_SOURCES)
libmca_btl_portals_la_LIBADD = $(btl_portals_LIBS)
libmca_btl_portals_la_LDFLAGS = -module -avoid-version $(btl_portals_LDFLAGS)
