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

dist_pkgdata_DATA += base/help-ess-base.txt

headers += \
        base/base.h

libmca_ess_la_SOURCES += \
        base/ess_base_close.c \
        base/ess_base_open.c \
	base/ess_base_select.c

if !ORTE_DISABLE_FULL_SUPPORT

libmca_ess_la_SOURCES += \
	base/ess_base_get.c \
	base/ess_base_put.c \
	base/ess_base_std_tool.c \
	base/ess_base_std_app.c \
	base/ess_base_std_orted.c \
        base/ess_base_std_prolog.c
endif

