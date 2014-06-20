#
# Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        sysinfo_solaris.h \
	chiptype.h \
	chiptype.c \
        sysinfo_solaris_component.c \
        sysinfo_solaris_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_sysinfo_solaris_DSO
component_noinst =
component_install = mca_sysinfo_solaris.la
else
component_noinst = libmca_sysinfo_solaris.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sysinfo_solaris_la_SOURCES = $(sources)
mca_sysinfo_solaris_la_LIBADD = $(sysinfo_solaris_LIBS)
mca_sysinfo_solaris_la_LDFLAGS = -module -avoid-version $(sysinfo_solaris_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_sysinfo_solaris_la_SOURCES =$(sources)
libmca_sysinfo_solaris_la_LIBADD = $(sysinfo_solaris_LIBS)
libmca_sysinfo_solaris_la_LDFLAGS = -module -avoid-version $(sysinfo_solaris_LDFLAGS)
