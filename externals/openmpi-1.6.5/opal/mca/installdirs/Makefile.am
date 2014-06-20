#
# Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
#                         reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libmca_installdirs.la
libmca_installdirs_la_SOURCES =

# header setup
nobase_opal_HEADERS =

# local files
headers = installdirs.h
libmca_installdirs_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
nobase_opal_HEADERS += $(headers)
opaldir = $(includedir)/openmpi/opal/mca/installdirs
else
opaldir = $(includedir)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
