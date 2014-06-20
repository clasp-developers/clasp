#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST = post_configure.sh

AM_CPPFLAGS = $(mtl_psm_CPPFLAGS)

dist_pkgdata_DATA = help-mtl-psm.txt

mtl_psm_sources = \
	       mtl_psm.c \
	       mtl_psm.h \
	       mtl_psm_cancel.c \
	       mtl_psm_component.c \
	       mtl_psm_endpoint.c \
	       mtl_psm_endpoint.h \
	       mtl_psm_probe.c \
	       mtl_psm_recv.c \
	       mtl_psm_request.h \
	       mtl_psm_send.c \
	       mtl_psm_types.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_mtl_psm_DSO
component_noinst =
component_install = mca_mtl_psm.la
else
component_noinst = libmca_mtl_psm.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_mtl_psm_la_SOURCES = $(mtl_psm_sources)
mca_mtl_psm_la_LIBADD = $(mtl_psm_LIBS)
mca_mtl_psm_la_LDFLAGS = -module -avoid-version $(mtl_psm_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_mtl_psm_la_SOURCES = $(mtl_psm_sources)
libmca_mtl_psm_la_LIBADD = $(mtl_psm_LIBS)
libmca_mtl_psm_la_LDFLAGS = -module -avoid-version $(mtl_psm_LDFLAGS)
