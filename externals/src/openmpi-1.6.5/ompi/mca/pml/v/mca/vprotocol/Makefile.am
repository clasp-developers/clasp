#
# Copyright (c) 2004-2007 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libmca_vprotocol.la
libmca_vprotocol_la_SOURCES =

# header setup
nobase_ompi_HEADERS =
nobase_nodist_ompi_HEADERS =

# local files
headers = vprotocol.h
libmca_vprotocol_la_SOURCES += $(headers) $(nodist_headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
nobase_ompi_HEADERS += $(headers)
nobase_nodist_ompi_HEADERS += $(nodist_headers)
ompidir = $(includedir)/openmpi/ompi/mca/vprotocol
else
ompidir = $(includedir)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h 
