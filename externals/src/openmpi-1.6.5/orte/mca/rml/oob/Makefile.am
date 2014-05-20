#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST = .windows

sources = \
	rml_oob.h \
	rml_oob_component.c \
	rml_oob_contact.c \
	rml_oob_exception.c \
	rml_oob_ping.c \
	rml_oob_recv.c \
	rml_oob_send.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_rml_oob_DSO
component_noinst =
component_install = mca_rml_oob.la
else
component_noinst = libmca_rml_oob.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_rml_oob_la_SOURCES = $(sources)
mca_rml_oob_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_rml_oob_la_SOURCES = $(sources)
libmca_rml_oob_la_LDFLAGS = -module -avoid-version

