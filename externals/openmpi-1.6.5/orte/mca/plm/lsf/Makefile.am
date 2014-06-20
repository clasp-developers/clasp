#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2008 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2008      Institut National de Recherche en Informatique
#                         et Automatique. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(plm_lsf_CPPFLAGS)

dist_pkgdata_DATA = help-plm-lsf.txt

sources = \
        plm_lsf.h \
        plm_lsf_component.c \
        plm_lsf_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_plm_lsf_DSO
component_noinst = 
component_install = mca_plm_lsf.la
else
component_noinst = libmca_plm_lsf.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_plm_lsf_la_SOURCES = $(sources)
mca_plm_lsf_la_LDFLAGS = -module -avoid-version $(plm_lsf_LDFLAGS)
mca_plm_lsf_la_LIBADD = $(plm_lsf_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_plm_lsf_la_SOURCES = $(sources)
libmca_plm_lsf_la_LDFLAGS = -module -avoid-version $(plm_lsf_LDFLAGS)
libmca_plm_lsf_la_LIBADD = $(plm_lsf_LIBS)
