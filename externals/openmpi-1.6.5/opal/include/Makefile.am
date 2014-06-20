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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST = CMakeLists.txt

headers = \
	opal_config_bottom.h \
	opal_stdint.h
nodist_headers = \
	opal_config.h

if WANT_INSTALL_HEADERS 
opaldir = $(includedir)/openmpi
nobase_dist_opal_HEADERS = $(headers)
nobase_nodist_opal_HEADERS = $(nodist_headers)
else
opaldir = $(includedir)
nobase_dist_noinst_HEADERS = $(headers)
nobase_nodist_noinst_HEADERS = $(nodist_headers)
endif

include opal/Makefile.am
