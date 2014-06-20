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
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2010-2011 Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        shmem_mmap.h \
        shmem_mmap_component.c \
        shmem_mmap_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_shmem_mmap_DSO
component_noinst =
component_install = mca_shmem_mmap.la
else
component_noinst = libmca_shmem_mmap.la
component_install =
endif

# help file
dist_pkgdata_DATA = help-opal-shmem-mmap.txt

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_shmem_mmap_la_SOURCES = $(sources)
mca_shmem_mmap_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_shmem_mmap_la_SOURCES =$(sources)
libmca_shmem_mmap_la_LDFLAGS = -module -avoid-version
