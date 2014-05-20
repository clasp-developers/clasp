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

AM_LFLAGS = -Pcarto_file_
LEX_OUTPUT_ROOT = lex.carto_file_

dist_pkgdata_DATA = \
        help-opal-carto-file.txt


sources = \
        carto_file.h \
        carto_file_lex.h \
        carto_file_lex.l \
        carto_file_component.c \
        carto_file_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_carto_file_DSO
component_noinst =
component_install = mca_carto_file.la
else
component_noinst = libmca_carto_file.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_carto_file_la_SOURCES = $(sources)
mca_carto_file_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_carto_file_la_SOURCES = $(sources)
libmca_carto_file_la_LDFLAGS = -module -avoid-version
