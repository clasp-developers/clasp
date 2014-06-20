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
# Copyright (c) 2008      Myricom.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(mpool_fake_CPPFLAGS)

sources = \
	mpool_fake_module.c \
	mpool_fake_component.c

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/ompi/mca/mpool/fake
ompi_HEADERS = mpool_fake.h
else
ompidir = $(includedir)
ompi_HEADERS =
endif

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_mpool_fake_DSO
component_noinst =
component_install = mca_mpool_fake.la
else
component_noinst = libmca_mpool_fake.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_mpool_fake_la_SOURCES = $(sources)
mca_mpool_fake_la_LDFLAGS = -module -avoid-version
mca_mpool_fake_la_LIBADD = $(mpool_fake_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_mpool_fake_la_SOURCES = $(sources)
libmca_mpool_fake_la_LDFLAGS = -module -avoid-version
libmca_mpool_fake_la_LIBADD = $(mpool_fake_LIBS)
