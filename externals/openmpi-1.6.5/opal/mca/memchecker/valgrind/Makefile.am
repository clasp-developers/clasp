#
# Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2008-2010 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(opal_memchecker_valgrind_CPPFLAGS)
sources = \
        memchecker_valgrind.h \
        memchecker_valgrind_component.c \
        memchecker_valgrind_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_memchecker_valgrind_DSO
component_noinst =
component_install = mca_memchecker_valgrind.la
else
component_noinst = libmca_memchecker_valgrind.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_memchecker_valgrind_la_SOURCES = $(sources)
mca_memchecker_valgrind_la_LDFLAGS = -module -avoid-version
mca_memchecker_valgrind_la_LIBADD = \
        $(top_ompi_builddir)/opal/libopal.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_memchecker_valgrind_la_SOURCES =$(sources)
libmca_memchecker_valgrind_la_LDFLAGS = -module -avoid-version
