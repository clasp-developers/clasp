#
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

EXTRA_DIST = post_configure.sh .windows

if OMPI_BUILD_pml_cm_DSO
component_noinst =
component_install = mca_pml_cm.la
else
component_noinst = libmca_pml_cm.la
component_install =
endif

local_sources = \
	pml_cm.c \
	pml_cm.h \
	pml_cm_cancel.c \
	pml_cm_component.c \
	pml_cm_component.h \
	pml_cm_probe.c \
	pml_cm_recv.c \
	pml_cm_recvreq.h \
	pml_cm_recvreq.c \
	pml_cm_request.h \
	pml_cm_request.c \
	pml_cm_send.c \
	pml_cm_sendreq.h \
	pml_cm_sendreq.c \
	pml_cm_start.c

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pml_cm_la_SOURCES = $(local_sources)
mca_pml_cm_la_LIBADD = $(pml_cm_LIBS)
mca_pml_cm_la_LDFLAGS = -module -avoid-version $(pml_cm_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_pml_cm_la_SOURCES = $(local_sources)
libmca_pml_cm_la_LIBADD = $(pml_cm_LIBS)
libmca_pml_cm_la_LDFLAGS = -module -avoid-version $(pml_cm_LDFLAGS)

