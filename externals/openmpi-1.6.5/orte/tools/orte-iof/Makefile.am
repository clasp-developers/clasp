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
# Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/Makefile.man-page-rules

man_pages = orte-iof.1
EXTRA_DIST = $(man_pages:.1=.1in)

if !ORTE_DISABLE_FULL_SUPPORT
if OMPI_INSTALL_BINARIES

bin_PROGRAMS = orte-iof

nodist_man_MANS = $(man_pages)

# Ensure that the man pages are rebuilt if the opal_config.h file
# changes; a "good enough" way to know if configure was run again (and
# therefore the release date or version may have changed)
$(nodist_man_MANS): $(top_builddir)/opal/include/opal_config.h

dist_pkgdata_DATA = help-orte-iof.txt

endif # OMPI_INSTALL_BINARIES

orte_iof_SOURCES = orte-iof.c
orte_iof_LDADD = $(top_builddir)/orte/libopen-rte.la

endif # !ORTE_DISABLE_FULL_SUPPORT

distclean-local:
	rm -f $(man_pages)
