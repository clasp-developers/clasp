#
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        sysinfo_darwin.h \
        sysinfo_darwin_component.c \
        sysinfo_darwin_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_sysinfo_darwin_DSO
component_noinst =
component_install = mca_sysinfo_darwin.la
else
component_noinst = libmca_sysinfo_darwin.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sysinfo_darwin_la_SOURCES = $(sources)
mca_sysinfo_darwin_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_sysinfo_darwin_la_SOURCES =$(sources)
libmca_sysinfo_darwin_la_LDFLAGS = -module -avoid-version
