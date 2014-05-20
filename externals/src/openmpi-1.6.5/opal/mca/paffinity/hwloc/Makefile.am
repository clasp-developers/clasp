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
# Copyright (c) 2010-2011 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# To find hwloc_bottom.h.  EMBEDDED flags are for if we OMPI's
# internal hwloc is used; paffinity_hwloc_CPPFLAGS is if we are using
# an external install.
AM_CPPFLAGS = $(HWLOC_EMBEDDED_CPPFLAGS) $(paffinity_hwloc_CPPFLAGS)
# To get the cflags for the stuff in hwloc.h
AM_CFLAGS = $(HWLOC_EMBEDDED_CFLAGS) $(paffinity_hwloc_CFLAGS)

sources = \
        paffinity_hwloc.h \
        paffinity_hwloc_component.c \
        paffinity_hwloc_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_paffinity_hwloc_DSO
component_noinst =
component_install = mca_paffinity_hwloc.la
else
component_noinst = libmca_paffinity_hwloc.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_paffinity_hwloc_la_SOURCES = $(sources)
mca_paffinity_hwloc_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_paffinity_hwloc_la_SOURCES =$(sources)
libmca_paffinity_hwloc_la_LDFLAGS = -module -avoid-version
