#
# Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_pml_v_DSO
component_noinst =
component_install = mca_pml_v.la
else
component_noinst = libmca_pml_v.la
component_install =
endif

SUBDIRS = mca/vprotocol $(MCA_vprotocol_DSO_SUBDIRS) $(MCA_vprotocol_STATIC_SUBDIRS)
DIST_SUBDIRS = mca/vprotocol $(MCA_vprotocol_ALL_SUBDIRS)

local_sources = \
        pml_v.c \
        pml_v.h \
        pml_v_component.c \
        pml_v_output.h \
        pml_v_output.c

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_pml_v_la_SOURCES = $(local_sources)
mca_pml_v_la_LIBADD = mca/vprotocol/libmca_vprotocol.la $(MCA_vprotocol_STATIC_LTLIBS)
mca_pml_v_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pml_v_la_SOURCES = $(local_sources)
libmca_pml_v_la_LIBADD = mca/vprotocol/libmca_vprotocol.la $(MCA_vprotocol_STATIC_LTLIBS)
libmca_pml_v_la_LDFLAGS = -module -avoid-version

