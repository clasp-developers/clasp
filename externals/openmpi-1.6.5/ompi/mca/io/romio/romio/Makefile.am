#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/Makefile.options

# Left out common/dataloop -- it's not enabled in MPICH2-1.0.7.

SUBDIRS = include adio mpi-io
DIST_SUBDIRS = $(SUBDIRS) doc util

EXTRA_DIST = README COPYRIGHT README_OMPI autogen.sh

noinst_LTLIBRARIES = libromio_dist.la 
libromio_dist_la_SOURCES = 
libromio_dist_la_LIBADD  = \
	adio/libadio.la \
	mpi-io/libmpi-io.la \
	mpi-io/glue/openmpi/libglue.la @ROMIO_LIBLIST@
libromio_dist_la_DEPENDENCIES = \
	adio/libadio.la \
	mpi-io/libmpi-io.la \
	mpi-io/glue/openmpi/libglue.la
