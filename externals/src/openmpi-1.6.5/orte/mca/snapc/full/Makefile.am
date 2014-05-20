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

dist_pkgdata_DATA = help-orte-snapc-full.txt

sources = \
        snapc_full.h \
        snapc_full_component.c \
        snapc_full_module.c \
        snapc_full_global.c \
        snapc_full_local.c \
        snapc_full_app.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_snapc_full_DSO
component_noinst =
component_install = mca_snapc_full.la
else
component_noinst = libmca_snapc_full.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_snapc_full_la_SOURCES = $(sources)
mca_snapc_full_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_snapc_full_la_SOURCES = $(sources)
libmca_snapc_full_la_LDFLAGS = -module -avoid-version
