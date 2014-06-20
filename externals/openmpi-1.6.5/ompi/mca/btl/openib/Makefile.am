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
# Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
# Copyright (c) 2011      Mellanox Technologies.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(btl_openib_CPPFLAGS)
AM_LFLAGS = -Pbtl_openib_ini_yy
LEX_OUTPUT_ROOT = lex.btl_openib_ini_yy

amca_paramdir = $(AMCA_PARAM_SETS_DIR)
dist_amca_param_DATA = btl-openib-benchmark

dist_pkgdata_DATA = \
        help-mpi-btl-openib.txt \
        connect/help-mpi-btl-openib-cpc-base.txt \
        mca-btl-openib-device-params.ini

sources = \
    btl_openib.c \
    btl_openib.h \
    btl_openib_component.c \
    btl_openib_endpoint.c \
    btl_openib_endpoint.h \
    btl_openib_frag.c \
    btl_openib_frag.h \
    btl_openib_proc.c \
    btl_openib_proc.h \
    btl_openib_eager_rdma.h \
    btl_openib_lex.h \
    btl_openib_lex.l \
    btl_openib_mca.c \
    btl_openib_mca.h \
    btl_openib_ini.c \
    btl_openib_ini.h \
    btl_openib_async.c \
    btl_openib_async.h \
    btl_openib_xrc.c \
    btl_openib_xrc.h \
    btl_openib_fd.h \
    btl_openib_fd.c \
    btl_openib_ip.h \
    btl_openib_ip.c \
    connect/base.h \
    connect/btl_openib_connect_base.c \
    connect/btl_openib_connect_oob.c \
    connect/btl_openib_connect_oob.h \
    connect/btl_openib_connect_empty.c \
    connect/btl_openib_connect_empty.h \
    connect/connect.h

# If we have failover support, build that file
if MCA_btl_openib_enable_failover
sources += \
    btl_openib_failover.c \
    btl_openib_failover.h
endif

# If we have XRC support, build that CPC
if MCA_btl_openib_have_xrc
sources += \
    connect/btl_openib_connect_xoob.c \
    connect/btl_openib_connect_xoob.h
endif

# If we have rdmacm support, build that CPC
if MCA_btl_openib_have_rdmacm
sources += \
    connect/btl_openib_connect_rdmacm.c \
    connect/btl_openib_connect_rdmacm.h

dist_pkgdata_DATA += connect/help-mpi-btl-openib-cpc-rdmacm.txt
endif

# If we have dynamic SL support, build those files
if MCA_btl_openib_have_dynamic_sl
sources += \
    connect/btl_openib_connect_sl.c \
    connect/btl_openib_connect_sl.h
endif

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_btl_openib_DSO
lib =
lib_sources =
component = mca_btl_openib.la
component_sources = $(sources)
else
lib = libmca_btl_openib.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_btl_openib_la_SOURCES = $(component_sources)
mca_btl_openib_la_LDFLAGS = -module -avoid-version $(btl_openib_LDFLAGS) 
mca_btl_openib_la_LIBADD = $(btl_openib_LIBS)

noinst_LTLIBRARIES = $(lib)
libmca_btl_openib_la_SOURCES = $(lib_sources)
libmca_btl_openib_la_LDFLAGS= -module -avoid-version $(btl_openib_LDFLAGS) 
libmca_btl_openib_la_LIBADD = $(btl_openib_LIBS)
