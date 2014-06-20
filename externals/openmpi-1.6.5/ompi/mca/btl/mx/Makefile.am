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

AM_CPPFLAGS = $(btl_mx_CPPFLAGS)

btl_mx_sources = btl_mx.c btl_mx.h btl_mx_component.c btl_mx_endpoint.c \
    btl_mx_endpoint.h btl_mx_frag.c btl_mx_frag.h btl_mx_proc.c btl_mx_proc.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_btl_mx_DSO
component_noinst =
component_install = mca_btl_mx.la
else
component_noinst = libmca_btl_mx.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_btl_mx_la_SOURCES = $(btl_mx_sources)
mca_btl_mx_la_LIBADD = \
    $(btl_mx_LIBS) \
    $(top_ompi_builddir)/ompi/mca/common/mx/libmca_common_mx.la
mca_btl_mx_la_LDFLAGS = -module -avoid-version $(btl_mx_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_btl_mx_la_SOURCES = $(btl_mx_sources)
libmca_btl_mx_la_LIBADD = $(btl_mx_LIBS)
libmca_btl_mx_la_LDFLAGS = -module -avoid-version $(btl_mx_LDFLAGS)
