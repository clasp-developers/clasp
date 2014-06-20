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
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST = base/.windows

headers += \
        base/base.h

libmca_plm_la_SOURCES += \
        base/plm_base_open.c

if !ORTE_DISABLE_FULL_SUPPORT

dist_pkgdata_DATA += base/help-plm-base.txt

headers += \
        base/plm_private.h

libmca_plm_la_SOURCES += \
        base/plm_base_close.c \
        base/plm_base_select.c \
        base/plm_base_receive.c \
	base/plm_base_launch_support.c \
	base/plm_base_jobid.c \
	base/plm_base_proxy.c \
	base/plm_base_orted_cmds.c \
        base/plm_base_rsh_support.c \
        base/plm_base_heartbeat.c
endif
