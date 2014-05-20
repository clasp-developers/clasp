#
# Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
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

man_pages = opal-checkpoint.1
EXTRA_DIST = $(man_pages:.1=.1in) CMakeLists.txt

if OMPI_INSTALL_BINARIES

bin_PROGRAMS = opal-checkpoint

nodist_man_MANS = $(man_pages)

# Ensure that the man pages are rebuilt if the opal_config.h file
# changes; a "good enough" way to know if configure was run again (and
# therefore the release date or version may have changed)
$(nodist_man_MANS): $(top_builddir)/opal/include/opal_config.h

dist_pkgdata_DATA = help-opal-checkpoint.txt

endif # OMPI_INSTALL_BINARIES

opal_checkpoint_SOURCES = opal-checkpoint.c
opal_checkpoint_LDADD = $(top_builddir)/opal/libopen-pal.la

distclean-local:
	rm -f $(man_pages)
