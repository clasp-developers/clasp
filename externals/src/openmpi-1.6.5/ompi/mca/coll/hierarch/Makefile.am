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

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_coll_hierarch_DSO
component_noinst =
component_install = mca_coll_hierarch.la
else
component_noinst = libmca_coll_hierarch.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_hierarch_la_SOURCES = $(sources)
mca_coll_hierarch_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_hierarch_la_SOURCES = $(sources)
libmca_coll_hierarch_la_LDFLAGS = -module -avoid-version

# Source files

sources = \
        coll_hierarch.h \
        coll_hierarch.c \
        coll_hierarch_allreduce.c \
        coll_hierarch_barrier.c \
        coll_hierarch_bcast.c \
        coll_hierarch_component.c \
        coll_hierarch_reduce.c \
	coll_hierarch_tmpcoll.c
