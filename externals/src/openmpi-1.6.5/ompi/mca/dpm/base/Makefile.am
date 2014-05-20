#
# Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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

dist_pkgdata_DATA = base/help-ompi-dpm-base.txt

headers += \
        base/base.h

libmca_dpm_la_SOURCES += \
        base/dpm_base_open.c \
        base/dpm_base_close.c \
        base/dpm_base_select.c \
        base/dpm_base_null_fns.c

if !ORTE_DISABLE_FULL_SUPPORT
libmca_dpm_la_SOURCES += \
        base/dpm_base_common_fns.c
endif
