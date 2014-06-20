#
# Copyright (c) 2004-2006 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libmca_memcpy.la
libmca_memcpy_la_SOURCES =

# header setup
nobase_opal_HEADERS =
nobase_nodist_opal_HEADERS =

# local files
headers = memcpy.h
nodist_headers = 
libmca_memcpy_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
nobase_opal_HEADERS += $(headers)
nobase_nodist_opal_HEADERS += $(nodist_headers)
opaldir = $(includedir)/openmpi/opal/mca/memcpy
else
opaldir = $(includedir)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
