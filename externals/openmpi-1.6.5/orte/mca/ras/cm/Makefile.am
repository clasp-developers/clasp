#
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        ras_cm.h \
        ras_cm_component.c \
        ras_cm_module.c


# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_ras_cm_DSO
lib =
lib_sources = 
component = mca_ras_cm.la
component_sources = $(sources)
else
lib = libmca_ras_cm.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_ras_cm_la_SOURCES = $(component_sources)
mca_ras_cm_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(lib)
libmca_ras_cm_la_SOURCES = $(lib_sources)
libmca_ras_cm_la_LDFLAGS = -module -avoid-version
