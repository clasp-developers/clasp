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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = \
	$(LTDLINCL) \
        -DBUILDDIR=\"$(OMPI_TOP_BUILDDIR)\" \
        -DSRCDIR=\"$(OMPI_TOP_SRCDIR)\"

#
# We use libsupport.a because we're lazy.  According to Ralph W., we
# *could* use check_LTLIBRARIES, but it's a little weird (see
# http://www.open-mpi.org/community/lists/devel/2008/06/4095.php).
#
check_LIBRARIES = libsupport.a

libsupport_a_SOURCES = \
        components.h \
        components.c \
        support.c \
        support.h
