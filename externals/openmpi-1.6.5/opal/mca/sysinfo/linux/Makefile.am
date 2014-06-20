#
# Copyright (c) 2009      Cisco Systems, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        sysinfo_linux.h \
        sysinfo_linux_component.c \
        sysinfo_linux_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_sysinfo_linux_DSO
component_noinst =
component_install = mca_sysinfo_linux.la
else
component_noinst = libmca_sysinfo_linux.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sysinfo_linux_la_SOURCES = $(sources)
mca_sysinfo_linux_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_sysinfo_linux_la_SOURCES =$(sources)
libmca_sysinfo_linux_la_LDFLAGS = -module -avoid-version
