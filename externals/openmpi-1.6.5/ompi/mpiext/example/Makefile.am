#
# Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

headers = \
	mpiext_example_c.h

sources = \
	c/progress.c

lib = libext_mpiext_example.la
lib_sources = $(sources)

extcomponentdir = $(pkglibdir)

noinst_LTLIBRARIES = $(lib)
libext_mpiext_example_la_SOURCES = $(lib_sources)
libext_mpiext_example_la_LDFLAGS = -module -avoid-version

ompidir = $(includedir)/openmpi/ompi/mpiext/example
ompi_HEADERS = \
        $(headers)
