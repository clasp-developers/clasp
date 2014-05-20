#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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

pt2pt_sources = \
	osc_rdma.h \
	osc_rdma.c \
	osc_rdma_comm.c \
        osc_rdma_component.c \
	osc_rdma_data_move.h \
	osc_rdma_data_move.c \
	osc_rdma_header.h \
	osc_rdma_longreq.h \
	osc_rdma_longreq.c \
	osc_rdma_obj_convert.h \
	osc_rdma_replyreq.h \
	osc_rdma_replyreq.c \
	osc_rdma_sendreq.h \
	osc_rdma_sendreq.c \
	osc_rdma_sync.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_osc_rdma_DSO
component_noinst =
component_install = mca_osc_rdma.la
else
component_noinst = libmca_osc_rdma.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_osc_rdma_la_SOURCES = $(pt2pt_sources)
mca_osc_rdma_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_osc_rdma_la_SOURCES = $(pt2pt_sources)
libmca_osc_rdma_la_LDFLAGS = -module -avoid-version
