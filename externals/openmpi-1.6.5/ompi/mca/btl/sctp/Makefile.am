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
# Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

CFLAGS = $(btl_sctp_CFLAGS)
AM_CPPFLAGS = $(btl_sctp_CPPFLAGS)

sources = \
    btl_sctp.c \
    btl_sctp.h \
    btl_sctp_addr.h \
    btl_sctp_component.c \
    btl_sctp_component.h \
    btl_sctp_endpoint.c \
    btl_sctp_endpoint.h \
    btl_sctp_frag.c \
    btl_sctp_frag.h \
    btl_sctp_hdr.h \
    btl_sctp_proc.c \
    btl_sctp_proc.h \
    btl_sctp_recv_handler.c \
    btl_sctp_recv_handler.h \
    btl_sctp_utils.c \
    btl_sctp_utils.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).


if OMPI_BUILD_btl_sctp_DSO
lib =
lib_sources =
component = mca_btl_sctp.la
component_sources = $(sources)
else
lib = libmca_btl_sctp.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_btl_sctp_la_SOURCES = $(component_sources)
mca_btl_sctp_la_LDFLAGS = -module -avoid-version $(btl_sctp_LDFLAGS)
mca_btl_sctp_la_LIBADD = $(btl_sctp_LIBS)

noinst_LTLIBRARIES = $(lib)
libmca_btl_sctp_la_SOURCES = $(lib_sources)
libmca_btl_sctp_la_LDFLAGS = -module -avoid-version $(btl_sctp_LDFLAGS)
libmca_btl_sctp_la_LIBADD = $(btl_sctp_LIBS)
