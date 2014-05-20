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
# Copyright (c) 2009-2010 Oracle and/or its affiliates.  All rights reserved.
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = -DPML_BFO=1

dist_pkgdata_DATA = \
	help-mpi-pml-bfo.txt

EXTRA_DIST = post_configure.sh

bfo_sources  = \
	pml_bfo.c \
	pml_bfo.h \
	pml_bfo_comm.c \
	pml_bfo_comm.h \
	pml_bfo_component.c \
	pml_bfo_component.h \
	pml_bfo_failover.c \
	pml_bfo_failover.h \
	pml_bfo_hdr.h \
	pml_bfo_iprobe.c \
	pml_bfo_irecv.c \
	pml_bfo_isend.c \
	pml_bfo_progress.c \
	pml_bfo_rdma.c \
	pml_bfo_rdma.h \
	pml_bfo_rdmafrag.c \
	pml_bfo_rdmafrag.h \
	pml_bfo_recvfrag.c \
	pml_bfo_recvfrag.h \
	pml_bfo_recvreq.c \
	pml_bfo_recvreq.h \
	pml_bfo_sendreq.c \
	pml_bfo_sendreq.h \
	pml_bfo_start.c 

if OMPI_BUILD_pml_bfo_DSO
component_noinst =
component_install = mca_pml_bfo.la
else
component_noinst = libmca_pml_bfo.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pml_bfo_la_SOURCES = $(bfo_sources)
mca_pml_bfo_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pml_bfo_la_SOURCES = $(bfo_sources)
libmca_pml_bfo_la_LDFLAGS = -module -avoid-version
