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
# Copyright (c) 2008      Voltaire. All rights reserved
# 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_LFLAGS = -Porte_rmaps_rank_file_
LEX_OUTPUT_ROOT = lex.orte_rmaps_rank_file_
dist_pkgdata_DATA = help-rmaps_rank_file.txt

sources = \
        rmaps_rank_file.c \
        rmaps_rank_file.h \
        rmaps_rank_file_component.c \
        rmaps_rank_file_lex.l \
        rmaps_rank_file_lex.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_rmaps_rank_file_DSO
component_noinst =
component_install = mca_rmaps_rank_file.la
else
component_noinst = libmca_rmaps_rank_file.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_rmaps_rank_file_la_SOURCES = $(sources)
mca_rmaps_rank_file_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_rmaps_rank_file_la_SOURCES =$(sources)
libmca_rmaps_rank_file_la_LDFLAGS = -module -avoid-version

