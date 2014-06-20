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

AM_CPPFLAGS = $(mtl_mx_CPPFLAGS)

mtl_mx_sources = \
	       mtl_mx.c \
	       mtl_mx.h \
	       mtl_mx_cancel.c \
	       mtl_mx_component.c \
	       mtl_mx_endpoint.c \
	       mtl_mx_endpoint.h \
	       mtl_mx_probe.c \
	       mtl_mx_recv.c \
	       mtl_mx_request.h \
	       mtl_mx_send.c \
	       mtl_mx_types.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_mtl_mx_DSO
component_noinst =
component_install = mca_mtl_mx.la
else
component_noinst = libmca_mtl_mx.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_mtl_mx_la_SOURCES = $(mtl_mx_sources)
mca_mtl_mx_la_LIBADD = \
    $(mtl_mx_LIBS) \
    $(top_ompi_builddir)/ompi/mca/common/mx/libmca_common_mx.la
mca_mtl_mx_la_LDFLAGS = -module -avoid-version $(mtl_mx_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_mtl_mx_la_SOURCES = $(mtl_mx_sources)
libmca_mtl_mx_la_LIBADD = $(mtl_mx_LIBS)
libmca_mtl_mx_la_LDFLAGS = -module -avoid-version $(mtl_mx_LDFLAGS)
