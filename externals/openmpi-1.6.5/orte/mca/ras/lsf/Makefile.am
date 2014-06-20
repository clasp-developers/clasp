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
# Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_pkgdata_DATA = help-ras-lsf.txt

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_ras_lsf_DSO
component_noinst =
component_install = mca_ras_lsf.la
else
component_noinst = libmca_ras_lsf.la
component_install =
endif

AM_CPPFLAGS= $(ras_lsf_CPPFLAGS)

proxy_SOURCES = \
    ras_lsf.h \
    ras_lsf_module.c \
    ras_lsf_component.c

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_ras_lsf_la_SOURCES = $(proxy_SOURCES)
mca_ras_lsf_la_LIBADD = $(ras_lsf_LIBS)
mca_ras_lsf_la_LDFLAGS = -module -avoid-version $(ras_lsf_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_ras_lsf_la_SOURCES = $(proxy_SOURCES)
libmca_ras_lsf_la_LIBADD = $(ras_lsf_LIBS)
libmca_ras_lsf_la_LDFLAGS = -module -avoid-version $(ras_lsf_LDFLAGS)
