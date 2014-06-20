#
# Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(grpcomm_pmi_CPPFLAGS)

sources = \
        grpcomm_pmi.h \
	grpcomm_pmi_module.c \
        grpcomm_pmi_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_grpcomm_pmi_DSO
component_noinst =
component_install = mca_grpcomm_pmi.la
else
component_noinst = libmca_grpcomm_pmi.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_grpcomm_pmi_la_SOURCES = $(sources)
mca_grpcomm_pmi_la_LDFLAGS = -module -avoid-version $(grpcomm_pmi_LDFLAGS)
mca_grpcomm_pmi_la_LIBADD = $(grpcomm_pmi_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_grpcomm_pmi_la_SOURCES =$(sources)
libmca_grpcomm_pmi_la_LDFLAGS = -module -avoid-version $(grpcomm_pmi_LDFLAGS)
libmca_grpcomm_pmi_la_LIBADD = $(grpcomm_pmi_LIBS)
