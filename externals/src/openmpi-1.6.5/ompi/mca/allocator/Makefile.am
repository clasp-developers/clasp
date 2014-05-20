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

# main library setup
noinst_LTLIBRARIES = libmca_allocator.la
libmca_allocator_la_SOURCES =

# header setup
nobase_ompi_HEADERS =

# local files
headers = allocator.h
libmca_allocator_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
nobase_ompi_HEADERS += $(headers)
ompidir = $(includedir)/openmpi/ompi/mca/allocator
else
ompidir = $(includedir)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
