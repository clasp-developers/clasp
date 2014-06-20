# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2010 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
#                         reserved.
# Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

headers = \
        opal_convertor.h \
        opal_convertor_internal.h \
        opal_datatype_checksum.h \
        opal_datatype.h \
        opal_datatype_internal.h \
        opal_datatype_copy.h \
        opal_datatype_memcpy.h \
        opal_datatype_pack.h \
        opal_datatype_prototypes.h \
        opal_datatype_unpack.h


noinst_LTLIBRARIES = \
        libdatatype_reliable.la \
        libdatatype.la

# these sources will be compiled with the special -D
libdatatype_reliable_la_SOURCES = opal_datatype_pack.c opal_datatype_unpack.c
libdatatype_reliable_la_CFLAGS = -DCHECKSUM $(AM_CFLAGS)

# these sources will be compiled with the normal CFLAGS only
libdatatype_la_SOURCES = \
        opal_convertor.c \
        opal_convertor_raw.c \
        opal_copy_functions.c \
        opal_copy_functions_heterogeneous.c \
        opal_datatype_add.c \
        opal_datatype_clone.c \
        opal_datatype_copy.c \
        opal_datatype_create.c \
        opal_datatype_create_contiguous.c \
        opal_datatype_destroy.c \
        opal_datatype_dump.c \
        opal_datatype_fake_stack.c \
        opal_datatype_get_count.c \
        opal_datatype_module.c \
        opal_datatype_optimize.c \
        opal_datatype_pack.c \
        opal_datatype_position.c \
        opal_datatype_resize.c \
        opal_datatype_unpack.c

libdatatype_la_LIBADD = libdatatype_reliable.la

# Conditionally install the header files
if WANT_INSTALL_HEADERS
opaldir = $(includedir)/openmpi/opal/datatype
opal_HEADERS = $(headers)
else
opaldir = $(includedir)
endif
