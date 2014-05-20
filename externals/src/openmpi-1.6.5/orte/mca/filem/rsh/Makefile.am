#
# Copyright (c) 2004-2007 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_pkgdata_DATA = help-orte-filem-rsh.txt

sources = \
        filem_rsh.h \
        filem_rsh_component.c \
        filem_rsh_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_filem_rsh_DSO
component_noinst =
component_install = mca_filem_rsh.la
else
component_noinst = libmca_filem_rsh.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_filem_rsh_la_SOURCES = $(sources)
mca_filem_rsh_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_filem_rsh_la_SOURCES = $(sources)
libmca_filem_rsh_la_LDFLAGS = -module -avoid-version
