#
# Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/Makefile.man-page-rules

# main library setup
noinst_LTLIBRARIES = libmca_filem.la
libmca_filem_la_SOURCES =

# header setup
nobase_orte_HEADERS =

# local files
headers = filem.h
libmca_filem_la_SOURCES += $(headers)

# Manual pages
nodist_man_MANS = orte_filem.7
EXTRA_DIST = $(nodist_man_MANS:.7=.7in)

# Ensure that the man pages are rebuilt if the opal_config.h file
# changes; a "good enough" way to know if configure was run again (and
# therefore the release date or version may have changed)
$(nodist_man_MANS): $(top_builddir)/opal/include/opal_config.h

# Conditionally install the header files
if WANT_INSTALL_HEADERS
nobase_orte_HEADERS += $(headers)
ortedir = $(includedir)/openmpi/orte/mca/filem
else
ortedir = $(includedir)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
	rm -f $(nodist_man_MANS)
