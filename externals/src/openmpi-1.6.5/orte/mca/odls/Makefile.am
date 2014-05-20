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
noinst_LTLIBRARIES = libmca_odls.la
libmca_odls_la_SOURCES =

# header setup
nobase_orte_HEADERS =
dist_pkgdata_DATA =

# local files
headers = odls.h odls_types.h
libmca_odls_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
nobase_orte_HEADERS += $(headers)
ortedir = $(includedir)/openmpi/orte/mca/odls
else
ortedir = $(includedir)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
