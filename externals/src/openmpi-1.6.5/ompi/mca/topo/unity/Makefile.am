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
    topo_unity.h \
    topo_unity_cart_map.c \
    topo_unity_graph_map.c \
    topo_unity.c \
    topo_unity_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_topo_unity_DSO
lib =
lib_sources = 
component = mca_topo_unity.la
component_sources = $(sources)
else
lib = libmca_topo_unity.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_topo_unity_la_SOURCES = $(component_sources)
mca_topo_unity_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(lib)
libmca_topo_unity_la_SOURCES = $(lib_sources)
libmca_topo_unity_la_LDFLAGS = -module -avoid-version

