# -*- shell-script -*-
#
#
# Copyright (c) 2011 Mellanox Technologies. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
#

AM_CPPFLAGS = $(coll_fca_CPPFLAGS) -DCOLL_FCA_HOME=\"$(coll_fca_HOME)\" -I$(coll_fca_HOME)/include/fca -I$(coll_fca_HOME)/include/fca_core

#dist_pkgdata_DATA = help-coll-fca.txt
coll_fca_sources = \
        coll_fca.h \
        coll_fca_debug.h \
        coll_fca_api.h \
        coll_fca_convertor.h \
        coll_fca_module.c \
        coll_fca_component.c \
        coll_fca_ops.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_coll_fca_DSO
component_noinst =
component_install = mca_coll_fca.la
else
component_noinst = libmca_coll_fca.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_fca_la_SOURCES = $(coll_fca_sources)
mca_coll_fca_la_LIBADD = $(coll_fca_LIBS)
mca_coll_fca_la_LDFLAGS = -module -avoid-version $(coll_fca_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_fca_la_SOURCES =$(coll_fca_sources)
libmca_coll_fca_la_LIBADD = $(coll_fca_LIBS) 
libmca_coll_fca_la_LDFLAGS = -module -avoid-version $(coll_fca_LDFLAGS)
