#
# Copyright (c) 2004-2011 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
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

AM_CPPFLAGS = $(notifier_ftb_CPPFLAGS)

dist_pkgdata_DATA = \
	help-orte-notifier-ftb.txt \
	help-ftb-event-schema.txt

sources = \
        notifier_ftb.h \
	notifier_ftb_module.c \
        notifier_ftb_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_notifier_ftb_DSO
component_noinst =
component_install = mca_notifier_ftb.la
else
component_noinst = libmca_notifier_ftb.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_notifier_ftb_la_SOURCES = $(sources)
mca_notifier_ftb_la_LDFLAGS = -module -avoid-version $(notifier_ftb_LDFLAGS)
mca_notifier_ftb_la_LIBADD = $(notifier_ftb_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_notifier_ftb_la_SOURCES =$(sources)
libmca_notifier_ftb_la_LDFLAGS = -module -avoid-version $(notifier_ftb_LDFLAGS)
libmca_notifier_ftb_la_LIBADD = $(notifier_ftb_LIBS)
