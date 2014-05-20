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

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_iof_orted_DSO
component_noinst =
component_install = mca_iof_orted.la
else
component_noinst = libmca_iof_orted.la
component_install =
endif

orted_SOURCES = \
    iof_orted.c \
    iof_orted.h \
    iof_orted_component.c \
    iof_orted_read.c \
    iof_orted_receive.c

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_iof_orted_la_SOURCES = $(orted_SOURCES)
mca_iof_orted_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_iof_orted_la_SOURCES = $(orted_SOURCES)
libmca_iof_orted_la_LIBADD =
libmca_iof_orted_la_LDFLAGS = -module -avoid-version
