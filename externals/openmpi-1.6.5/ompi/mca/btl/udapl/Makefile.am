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
# Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

CFLAGS = $(btl_udapl_CFLAGS)
AM_CPPFLAGS = $(btl_udapl_CPPFLAGS) -DPKGDATADIR=\"$(pkgdatadir)\"

dist_pkgdata_DATA = \
        help-mpi-btl-udapl.txt

udapl_sources = \
    btl_udapl.c \
    btl_udapl.h \
    btl_udapl_component.c \
    btl_udapl_eager_rdma.h \
    btl_udapl_endpoint.c \
    btl_udapl_endpoint.h \
    btl_udapl_frag.c \
    btl_udapl_frag.h \
    btl_udapl_mca.c \
    btl_udapl_mca.h \
    btl_udapl_proc.c \
    btl_udapl_proc.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_btl_udapl_DSO
component_noinst =
component_install = mca_btl_udapl.la
else
component_noinst = libmca_btl_udapl.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_btl_udapl_la_SOURCES = $(udapl_sources)
mca_btl_udapl_la_LIBADD = $(btl_udapl_LIBS)
mca_btl_udapl_la_LDFLAGS = -module -avoid-version $(btl_udapl_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_btl_udapl_la_SOURCES = $(udapl_sources)
libmca_btl_udapl_la_LIBADD = $(btl_udapl_LIBS)
libmca_btl_udapl_la_LDFLAGS = -module -avoid-version $(btl_udapl_LDFLAGS)
