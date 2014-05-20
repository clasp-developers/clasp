#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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

AM_CPPFLAGS = $(mtl_portals_CPPFLAGS)

if OMPI_BUILD_mtl_portals_DSO
component_noinst =
component_install = mca_mtl_portals.la
else
component_noinst = libmca_mtl_portals.la
component_install =
endif

local_sources = \
	mtl_portals_component.c \
	mtl_portals.c \
	mtl_portals.h \
	mtl_portals_endpoint.h \
	mtl_portals_recv.c \
	mtl_portals_recv.h \
	mtl_portals_recv_short.h \
	mtl_portals_recv_short.c \
	mtl_portals_request.h \
	mtl_portals_send_short.h \
	mtl_portals_send_short.c \
	mtl_portals_send.c \
	mtl_portals_probe.c

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_mtl_portals_la_SOURCES = $(local_sources)
mca_mtl_portals_la_LIBADD = \
    $(mtl_portals_LIBS) \
    $(top_ompi_builddir)/ompi/mca/common/portals/libmca_common_portals.la
mca_mtl_portals_la_LDFLAGS = -module -avoid-version $(mtl_portals_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_mtl_portals_la_SOURCES = $(local_sources)
libmca_mtl_portals_la_LIBADD = $(mtl_portals_LIBS)
libmca_mtl_portals_la_LDFLAGS = -module -avoid-version $(mtl_portals_LDFLAGS)

