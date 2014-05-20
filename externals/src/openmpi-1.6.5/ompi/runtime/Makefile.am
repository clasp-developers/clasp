#
# Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This makefile.am does not stand on its own - it is included from ompi/Makefile.am

dist_pkgdata_DATA += runtime/help-mpi-runtime.txt

headers += \
        runtime/mpiruntime.h \
	runtime/ompi_cr.h \
        runtime/params.h \
	runtime/ompi_module_exchange.h

libmpi_la_SOURCES += \
        runtime/ompi_mpi_abort.c \
        runtime/ompi_mpi_init.c \
        runtime/ompi_mpi_finalize.c \
        runtime/ompi_mpi_params.c \
        runtime/ompi_mpi_preconnect.c \
	runtime/ompi_cr.c \
	runtime/ompi_module_exchange.c
