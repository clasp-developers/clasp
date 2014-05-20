#
# Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libmca_hwloc.la
libmca_hwloc_la_SOURCES =

# local files
headers = hwloc.h
libmca_hwloc_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
opaldir = $(includedir)/openmpi/$(subdir)
nobase_opal_HEADERS = $(headers)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
