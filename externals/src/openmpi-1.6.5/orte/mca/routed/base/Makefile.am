#
# Copyright (c) 2007      Los Alamos National Security, LLC.
#                         All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

headers += \
	base/base.h

libmca_routed_la_SOURCES += \
	base/routed_base_components.c

if !ORTE_DISABLE_FULL_SUPPORT

libmca_routed_la_SOURCES += \
	base/routed_base_register_sync.c \
	base/routed_base_receive.c
endif
