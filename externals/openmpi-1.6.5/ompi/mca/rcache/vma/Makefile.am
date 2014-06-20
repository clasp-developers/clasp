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

sources = \
    rcache_vma.c \
    rcache_vma.h \
    rcache_vma_component.c \
    rcache_vma_tree.c \
    rcache_vma_tree.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_rcache_vma_DSO
lib =
lib_sources =
component = mca_rcache_vma.la
component_sources = $(sources)
else
lib = libmca_rcache_vma.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_rcache_vma_la_SOURCES = $(component_sources)
mca_rcache_vma_la_LDFLAGS = -module -avoid-version $(rcache_vma_LDFLAGS)
mca_rcache_vma_la_LIBADD = $(rcache_vma_LIBS)

noinst_LTLIBRARIES = $(lib)
libmca_rcache_vma_la_SOURCES = $(lib_sources)
libmca_rcache_vma_la_LDFLAGS = -module -avoid-version$ $(rcache_vma_LDFLAGS)
libmca_rcache_vma_la_LIBADD = $(rcache_vma_LIBS)
