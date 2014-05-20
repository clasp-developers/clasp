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

headers += \
        base/base.h

libmca_odls_la_SOURCES += \
        base/odls_base_open.c

if !ORTE_DISABLE_FULL_SUPPORT

dist_pkgdata_DATA += base/help-orte-odls-base.txt

headers += \
        base/odls_private.h

libmca_odls_la_SOURCES += \
        base/odls_base_close.c \
        base/odls_base_select.c \
        base/odls_base_default_fns.c \
        base/odls_base_state.c
endif
