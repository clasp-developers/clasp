#
# Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libmca_memchecker.la
libmca_memchecker_la_SOURCES =

# header setup
nobase_opal_HEADERS =

# local files
headers = memchecker.h
libmca_memchecker_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
nobase_opal_HEADERS += $(headers)
opaldir = $(includedir)/openmpi/opal/mca/memchecker
else
opaldir = $(includedir)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
