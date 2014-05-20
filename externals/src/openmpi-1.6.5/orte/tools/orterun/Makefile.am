#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This is not quite in the Automake spirit, but we have to do it.
# Since the totalview portion of the library must be built with -g, we
# must eliminate the CFLAGS that are passed in here by default (which
# may already have debugging and/or optimization flags).  We use
# post-processed forms of the CFLAGS in the library targets down
# below.

CFLAGS = $(CFLAGS_WITHOUT_OPTFLAGS) $(DEBUGGER_CFLAGS)

include $(top_srcdir)/Makefile.man-page-rules

man_pages = orterun.1
EXTRA_DIST = $(man_pages:.1=.1in) CMakeLists.txt

if !ORTE_DISABLE_FULL_SUPPORT
if OMPI_INSTALL_BINARIES

bin_PROGRAMS = orterun

nodist_man_MANS = $(man_pages)

# Ensure that the man pages are rebuilt if the opal_config.h file
# changes; a "good enough" way to know if configure was run again (and
# therefore the release date or version may have changed)
$(nodist_man_MANS): $(top_builddir)/opal/include/opal_config.h

dist_pkgdata_DATA = help-orterun.txt

endif # OMPI_INSTALL_BINARIES

orterun_SOURCES = \
        main.c \
        orterun.c \
        orterun.h
 
orterun_LDADD = $(top_builddir)/orte/libopen-rte.la

endif # !ORTE_DISABLE_FULL_SUPPORT

distclean-local:
	rm -f $(man_pages)
