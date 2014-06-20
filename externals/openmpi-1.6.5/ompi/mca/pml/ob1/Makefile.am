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
# Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_pkgdata_DATA = \
	help-mpi-pml-ob1.txt

EXTRA_DIST = post_configure.sh .windows

ob1_sources  = \
	pml_ob1.c \
	pml_ob1.h \
	pml_ob1_comm.c \
	pml_ob1_comm.h \
	pml_ob1_component.c \
	pml_ob1_component.h \
	pml_ob1_hdr.h \
	pml_ob1_iprobe.c \
	pml_ob1_irecv.c \
	pml_ob1_isend.c \
	pml_ob1_progress.c \
	pml_ob1_rdma.c \
	pml_ob1_rdma.h \
	pml_ob1_rdmafrag.c \
	pml_ob1_rdmafrag.h \
	pml_ob1_recvfrag.c \
	pml_ob1_recvfrag.h \
	pml_ob1_recvreq.c \
	pml_ob1_recvreq.h \
	pml_ob1_sendreq.c \
	pml_ob1_sendreq.h \
	pml_ob1_start.c 

if OMPI_BUILD_pml_ob1_DSO
component_noinst =
component_install = mca_pml_ob1.la
else
component_noinst = libmca_pml_ob1.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pml_ob1_la_SOURCES = $(ob1_sources)
mca_pml_ob1_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pml_ob1_la_SOURCES = $(ob1_sources)
libmca_pml_ob1_la_LDFLAGS = -module -avoid-version
