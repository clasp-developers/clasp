#
# Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(ess_pmi_CPPFLAGS)

sources = \
        ess_pmi.h \
        ess_pmi_component.c \
        ess_pmi_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_ess_pmi_DSO
component_noinst =
component_install = mca_ess_pmi.la
else
component_noinst = libmca_ess_pmi.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_ess_pmi_la_SOURCES = $(sources)
mca_ess_pmi_la_LDFLAGS = -module -avoid-version $(ess_pmi_LDFLAGS)
mca_ess_pmi_la_LIBADD = $(ess_pmi_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_ess_pmi_la_SOURCES =$(sources)
libmca_ess_pmi_la_LDFLAGS = -module -avoid-version $(ess_pmi_LDFLAGS)
libmca_ess_pmi_la_LIBADD = $(ess_pmi_LIBS)
