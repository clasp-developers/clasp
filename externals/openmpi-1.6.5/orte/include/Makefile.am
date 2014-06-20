# -*- makefile -*-
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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

headers =
nodist_headers = \
	orte_config.h

if WANT_INSTALL_HEADERS 
ortedir = $(includedir)/openmpi
nobase_dist_orte_HEADERS = $(headers)
nobase_nodist_orte_HEADERS = $(nodist_headers)
else
ortedir = $(includedir)
nobase_dist_noinst_HEADERS = $(headers)
nobase_nodist_noinst_HEADERS = $(nodist_headers)
endif

include orte/Makefile.am
