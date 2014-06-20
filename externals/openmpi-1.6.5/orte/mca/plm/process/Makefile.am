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
# Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST = .windows

AM_CPPFLAGS = $(plm_process_CPPFLAGS)

sources = \
        plm_process.h \
        plm_process_component.c \
        plm_process_module.c


# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_plm_process_DSO
lib =
lib_sources = 
component = mca_plm_process.la
component_sources = $(sources)
else
lib = libmca_plm_process.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_plm_process_la_SOURCES = $(component_sources)
mca_plm_process_la_LDFLAGS = -module -avoid-version $(plm_process_LDFLAGS)
mca_plm_process_la_LIBADD = $(plm_process_LIBS)

noinst_LTLIBRARIES = $(lib)
libmca_plm_process_la_SOURCES = $(lib_sources)
libmca_plm_process_la_LDFLAGS = -module -avoid-version $(plm_process_LDFLAGS)
libmca_plm_process_la_LIBADD = $(plm_process_LIBS)
