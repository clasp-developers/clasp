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

libmca_rml_la_SOURCES += \
	base/rml_base_components.c \
        base/rml_base_null_fns.c

if !ORTE_DISABLE_FULL_SUPPORT

headers += \
	base/rml_contact.h

libmca_rml_la_SOURCES += \
	base/rml_base_receive.c \
	base/rml_base_contact.c
endif
