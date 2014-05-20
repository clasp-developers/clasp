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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = -I$(srcdir)/MoreBacktrace

CFLAGS = $(backtrace_darwin_CFLAGS)

noinst_LTLIBRARIES = libmca_backtrace_darwin.la

libmca_backtrace_darwin_la_SOURCES = \
    backtrace_darwin.c \
    backtrace_darwin_component.c \
    MoreBacktrace/MoreSetup.h \
    MoreBacktrace/MoreDebugging/MoreAddrToSym.c \
    MoreBacktrace/MoreDebugging/MoreAddrToSym.h \
    MoreBacktrace/MoreDebugging/MoreBacktrace.c \
    MoreBacktrace/MoreDebugging/MoreBacktrace.h
