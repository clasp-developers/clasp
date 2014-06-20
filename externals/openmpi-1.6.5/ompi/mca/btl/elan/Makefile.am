#
# Copyright (c) 2004-2007 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(btl_elan_CPPFLAGS)

btl_elan_sources = btl_elan.c btl_elan.h btl_elan_component.c btl_elan_endpoint.c \
    btl_elan_endpoint.h btl_elan_frag.c btl_elan_frag.h btl_elan_proc.c btl_elan_proc.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_btl_elan_DSO
component_noinst =
component_install = mca_btl_elan.la
else
component_noinst = libmca_btl_elan.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_btl_elan_la_SOURCES = $(btl_elan_sources)
mca_btl_elan_la_LIBADD = $(btl_elan_LIBS)
mca_btl_elan_la_LDFLAGS = -module -avoid-version $(btl_elan_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_btl_elan_la_SOURCES = $(btl_elan_sources)
libmca_btl_elan_la_LIBADD = $(btl_elan_LIBS)
libmca_btl_elan_la_LDFLAGS = -module -avoid-version $(btl_elan_LDFLAGS)
