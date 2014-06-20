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
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

headers += \
        base/base.h

libmca_rmaps_la_SOURCES += \
        base/rmaps_base_open.c

if !ORTE_DISABLE_FULL_SUPPORT

dist_pkgdata_DATA = base/help-orte-rmaps-base.txt

headers += \
        base/rmaps_private.h

libmca_rmaps_la_SOURCES += \
        base/rmaps_base_close.c \
        base/rmaps_base_select.c \
        base/rmaps_base_map_job.c \
        base/rmaps_base_get_job_map.c \
        base/rmaps_base_support_fns.c \
	base/rmaps_base_common_mappers.c
endif
