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
	mpool_sm.h \
	mpool_sm_module.c \
	mpool_sm_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_mpool_sm_DSO
component_noinst =
component_install = mca_mpool_sm.la
else
component_noinst = libmca_mpool_sm.la
component_install =
endif

# See ompi/mca/common/sm/Makefile.am for an explanation of
# libmca_common_sm.la.

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_mpool_sm_la_SOURCES = $(sources)
mca_mpool_sm_la_LDFLAGS = -module -avoid-version
mca_mpool_sm_la_LIBADD = \
    $(top_ompi_builddir)/ompi/mca/common/sm/libmca_common_sm.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_mpool_sm_la_SOURCES = $(sources)
libmca_mpool_sm_la_LDFLAGS = -module -avoid-version
