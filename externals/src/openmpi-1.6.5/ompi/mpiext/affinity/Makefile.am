#
# Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/Makefile.man-page-rules

headers = mpiext_affinity_c.h

sources = c/affinity_str.c

man_pages = OMPI_Affinity_str.3

lib = libext_mpiext_affinity.la
lib_sources = $(sources)

extcomponentdir = $(pkglibdir)

noinst_LTLIBRARIES = $(lib)
libext_mpiext_affinity_la_SOURCES = $(lib_sources)
libext_mpiext_affinity_la_LDFLAGS = -module -avoid-version

ompidir = $(includedir)/openmpi/ompi/mpiext/affinity
ompi_HEADERS = $(headers)

nodist_man_MANS = $(man_pages)
EXTRA_DIST = $(man_pages:.3=.3in)
