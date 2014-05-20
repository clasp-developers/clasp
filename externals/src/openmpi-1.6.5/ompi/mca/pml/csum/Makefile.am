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
# Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_pkgdata_DATA = \
	help-mpi-pml-csum.txt

EXTRA_DIST = post_configure.sh pml_csum_endpoint.c pml_csum_endpoint.h

csum_sources  = \
	pml_csum.c \
	pml_csum.h \
	pml_csum_comm.c \
	pml_csum_comm.h \
	pml_csum_component.c \
	pml_csum_component.h \
	pml_csum_hdr.h \
	pml_csum_iprobe.c \
	pml_csum_irecv.c \
	pml_csum_isend.c \
	pml_csum_progress.c \
	pml_csum_rdma.c \
	pml_csum_rdma.h \
	pml_csum_rdmafrag.c \
	pml_csum_rdmafrag.h \
	pml_csum_recvfrag.c \
	pml_csum_recvfrag.h \
	pml_csum_recvreq.c \
	pml_csum_recvreq.h \
	pml_csum_sendreq.c \
	pml_csum_sendreq.h \
	pml_csum_start.c 

if OMPI_BUILD_pml_csum_DSO
component_noinst =
component_install = mca_pml_csum.la
else
component_noinst = libmca_pml_csum.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pml_csum_la_SOURCES = $(csum_sources)
mca_pml_csum_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pml_csum_la_SOURCES = $(csum_sources)
libmca_pml_csum_la_LDFLAGS = -module -avoid-version
