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
# Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This makefile.am does not stand on its own - it is included from opal/Makefile.am

headers += \
        opal/align.h \
	opal/constants.h \
	opal/opal_socket_errno.h \
	opal/types.h \
	opal/prefetch.h \
        opal/hash_string.h

nodist_headers += \
	opal/version.h

include opal/sys/Makefile.am

opal/install_dirs.h: $(top_builddir)/config.status $(srcdir)/opal/install_dirs.h.in
	cd $(top_builddir) && $(SHELL) ./config.status $(subdir)/$@

opal/version.h: $(top_builddir)/config.status $(srcdir)/opal/version.h.in
	cd $(top_builddir) && $(SHELL) ./config.status $(subdir)/$@
