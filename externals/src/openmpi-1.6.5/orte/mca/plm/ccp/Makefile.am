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

AM_CPPFLAGS = $(plm_ccp_CPPFLAGS)

dist_pkgdata_DATA = help-plm-ccp.txt

sources = \
        plm_ccp.h \
        plm_ccp_component.c \
        plm_ccp_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_plm_ccp_DSO
lib =
lib_sources = 
component = mca_plm_ccp.la
component_sources = $(sources)
else
lib = libmca_plm_ccp.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_plm_ccp_la_SOURCES = $(component_sources)
mca_plm_ccp_la_LDFLAGS = -module -avoid-version $(plm_ccp_LDFLAGS)
mca_plm_ccp_la_LIBADD = $(plm_ccp_LIBS)

noinst_LTLIBRARIES = $(lib)
libmca_plm_ccp_la_SOURCES = $(lib_sources)
libmca_plm_ccp_la_LDFLAGS = -module -avoid-version $(plm_ccp_LDFLAGS)
libmca_plm_ccp_la_LIBADD = $(plm_ccp_LIBS)
