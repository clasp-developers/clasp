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
# Copyright (c) 2006      Sandia National Laboratories. All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS=$(btl_ofud_CPPFLAGS) 

sources = \
    btl_ofud.c \
    btl_ofud.h \
    btl_ofud_component.c \
    btl_ofud_endpoint.c \
    btl_ofud_endpoint.h \
    btl_ofud_frag.c \
    btl_ofud_frag.h \
    btl_ofud_proc.c \
    btl_ofud_proc.h
     
# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_btl_ofud_DSO
lib =
lib_sources =
component = mca_btl_ofud.la
component_sources = $(sources)
else
lib = libmca_btl_ofud.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_btl_ofud_la_SOURCES = $(component_sources)
mca_btl_ofud_la_LDFLAGS = -module -avoid-version $(btl_ofud_LDFLAGS) 
mca_btl_ofud_la_LIBADD = $(btl_ofud_LIBS)

noinst_LTLIBRARIES = $(lib)
libmca_btl_ofud_la_SOURCES = $(lib_sources)
libmca_btl_ofud_la_LDFLAGS= -module -avoid-version $(btl_ofud_LDFLAGS) 
libmca_btl_ofud_la_LIBADD = $(btl_ofud_LIBS)
