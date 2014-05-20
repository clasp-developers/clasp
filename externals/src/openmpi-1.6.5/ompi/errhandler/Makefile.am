# -*- makefile -*-
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
# Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This makefile.am does not stand on its own - it is included from ompi/Makefile.am

dist_pkgdata_DATA += errhandler/help-mpi-errors.txt

headers += \
        errhandler/errcode.h \
        errhandler/errcode-internal.h \
        errhandler/errhandler.h \
        errhandler/errhandler_predefined.h

libmpi_la_SOURCES += \
        errhandler/errhandler.c \
        errhandler/errhandler_invoke.c \
        errhandler/errhandler_predefined.c \
        errhandler/errcode.c \
        errhandler/errcode-internal.c
