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
# Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
#                         Use is subject to license terms.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_pkgdata_DATA = help-ras-gridengine.txt

sources = \
        ras_gridengine.h \
        ras_gridengine_component.c \
        ras_gridengine_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_ras_gridengine_DSO
component_noinst =
component_install = mca_ras_gridengine.la
else
component_noinst = libmca_ras_gridengine.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_ras_gridengine_la_SOURCES = $(sources)
mca_ras_gridengine_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_ras_gridengine_la_SOURCES =$(sources)
libmca_ras_gridengine_la_LDFLAGS = -module -avoid-version
