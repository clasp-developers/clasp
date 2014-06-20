#
# Copyright (c) 2007      Los Alamos National Security, LLC.
#                         All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(LTDLINCL)

# main library setup
noinst_LTLIBRARIES = libmca_routed.la
libmca_routed_la_SOURCES =

# header setup
nobase_orte_HEADERS =

# local files
headers = routed.h routed_types.h
libmca_routed_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
nobase_orte_HEADERS += $(headers)
ortedir = $(includedir)/openmpi/orte/mca/routed
else
ortedir = $(includedir)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
