#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(ess_lsf_CPPFLAGS)

sources = \
        ess_lsf.h \
        ess_lsf_component.c \
        ess_lsf_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_ess_lsf_DSO
component_noinst =
component_install = mca_ess_lsf.la
else
component_noinst = libmca_ess_lsf.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_ess_lsf_la_SOURCES = $(sources)
mca_ess_lsf_la_LDFLAGS = -module -avoid-version $(ess_lsf_LDFLAGS)
mca_ess_lsf_la_LIBADD = $(ess_lsf_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_ess_lsf_la_SOURCES =$(sources)
libmca_ess_lsf_la_LDFLAGS = -module -avoid-version $(ess_lsf_LDFLAGS)
libmca_ess_lsf_la_LIBADD = $(ess_lsf_LIBS)
