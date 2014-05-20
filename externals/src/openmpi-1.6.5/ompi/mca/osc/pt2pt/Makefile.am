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
	osc_pt2pt.h \
	osc_pt2pt.c \
	osc_pt2pt_buffer.h \
	osc_pt2pt_buffer.c \
	osc_pt2pt_comm.c \
        osc_pt2pt_component.c \
	osc_pt2pt_data_move.h \
	osc_pt2pt_data_move.c \
	osc_pt2pt_header.h \
	osc_pt2pt_longreq.h \
	osc_pt2pt_longreq.c \
	osc_pt2pt_mpireq.h \
	osc_pt2pt_mpireq.c \
	osc_pt2pt_replyreq.h \
	osc_pt2pt_replyreq.c \
	osc_pt2pt_sendreq.h \
	osc_pt2pt_sendreq.c \
	osc_pt2pt_sync.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_osc_pt2pt_DSO
component_noinst =
component_install = mca_osc_pt2pt.la
else
component_noinst = libmca_osc_pt2pt.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_osc_pt2pt_la_SOURCES = $(pt2pt_sources)
mca_osc_pt2pt_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_osc_pt2pt_la_SOURCES = $(pt2pt_sources)
libmca_osc_pt2pt_la_LDFLAGS = -module -avoid-version
