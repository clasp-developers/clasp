#
# Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(mtl_mxm_CPPFLAGS)

dist_pkgdata_DATA = help-mtl-mxm.txt

mtl_mxm_sources = \
	       mtl_mxm.c \
	       mtl_mxm.h \
	       mtl_mxm_cancel.c \
	       mtl_mxm_component.c \
	       mtl_mxm_endpoint.c \
	       mtl_mxm_endpoint.h \
	       mtl_mxm_probe.c \
	       mtl_mxm_recv.c \
	       mtl_mxm_request.h \
	       mtl_mxm_send.c \
	       mtl_mxm_debug.h \
	       mtl_mxm_types.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_mtl_mxm_DSO
component_noinst =
component_install = mca_mtl_mxm.la
else
component_noinst = libmca_mtl_mxm.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_mtl_mxm_la_SOURCES = $(mtl_mxm_sources)
mca_mtl_mxm_la_LIBADD = $(mtl_mxm_LIBS)
mca_mtl_mxm_la_LDFLAGS = -module -avoid-version $(mtl_mxm_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_mtl_mxm_la_SOURCES = $(mtl_mxm_sources)
libmca_mtl_mxm_la_LIBADD = $(mtl_mxm_LIBS)
libmca_mtl_mxm_la_LDFLAGS = -module -avoid-version $(mtl_mxm_LDFLAGS)
