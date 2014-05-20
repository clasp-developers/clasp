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
# Copyright (c) 2009 Cisco Systems, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(notifier_smtp_CPPFLAGS)

dist_pkgdata_DATA = \
        help-orte-notifier-smtp.txt

sources = \
        notifier_smtp.h \
	notifier_smtp_module.c \
        notifier_smtp_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_notifier_smtp_DSO
component_noinst =
component_install = mca_notifier_smtp.la
else
component_noinst = libmca_notifier_smtp.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_notifier_smtp_la_SOURCES = $(sources)
mca_notifier_smtp_la_LDFLAGS = -module -avoid-version $(notifier_smtp_LDFLAGS)
mca_notifier_smtp_la_LIBADD = $(notifier_smtp_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_notifier_smtp_la_SOURCES =$(sources)
libmca_notifier_smtp_la_LDFLAGS = -module -avoid-version $(notifier_smtp_LDFLAGS)
libmca_notifier_smtp_la_LIBADD = $(notifier_smtp_LIBS)
