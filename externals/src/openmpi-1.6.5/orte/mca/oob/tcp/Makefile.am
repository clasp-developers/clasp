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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_pkgdata_DATA = help-oob-tcp.txt

EXTRA_DIST = .windows

sources = \
    oob_tcp.c \
    oob_tcp.h \
    oob_tcp_hdr.h \
    oob_tcp_addr.h \
    oob_tcp_addr.c \
    oob_tcp_msg.c \
    oob_tcp_msg.h \
    oob_tcp_peer.c \
    oob_tcp_ping.c \
    oob_tcp_peer.h \
    oob_tcp_recv.c \
    oob_tcp_send.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_oob_tcp_DSO
component_noinst =
component_install = mca_oob_tcp.la
else
component_noinst = libmca_oob_tcp.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_oob_tcp_la_SOURCES = $(sources)
mca_oob_tcp_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_oob_tcp_la_SOURCES = $(sources)
libmca_oob_tcp_la_LDFLAGS = -module -avoid-version

