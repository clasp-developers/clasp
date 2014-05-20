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

headers += \
        base/base.h

libmca_iof_la_SOURCES += \
        base/iof_base_open.c

if !ORTE_DISABLE_FULL_SUPPORT

headers += \
	base/iof_base_setup.h

libmca_iof_la_SOURCES += \
        base/iof_base_close.c \
	base/iof_base_select.c \
        base/iof_base_output.c \
	base/iof_base_setup.c

endif
