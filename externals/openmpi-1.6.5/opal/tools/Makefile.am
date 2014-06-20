# -*- makefile -*-
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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST += tools/CMakeLists.txt

# This makefile.am does not stand on its own - it is included from
# opal/Makefile.am

SUBDIRS += \
	tools/wrappers

DIST_SUBDIRS += \
	tools/wrappers \
	tools/opal-checkpoint \
	tools/opal-restart

if WANT_FT
SUBDIRS += \
	tools/opal-checkpoint \
	tools/opal-restart
endif
