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
# Copyright (c) 2006-2010 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

SUBDIRS = config contrib $(MCA_PROJECT_SUBDIRS) test
EXTRA_DIST = README INSTALL VERSION Doxyfile LICENSE autogen.sh CMakeLists.txt README.WINDOWS.txt

include examples/Makefile.include

dist-hook:
	csh "$(top_srcdir)/config/distscript.csh" "$(top_srcdir)" "$(distdir)" "$(OMPI_VERSION)" "$(OMPI_SVN_R)"

ACLOCAL_AMFLAGS = -I config
