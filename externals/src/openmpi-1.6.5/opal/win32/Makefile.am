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

# This makefile.am does not stand on its own - it is included from opal/Makefile.am

# Source code files
OMPI_WIN32_SRC = \
	win32/ompi_utsname.c \
	win32/ompi_process.c \
	win32/ompi_time.c \
	win32/ompi_uio.c \
	win32/ompi_inet.c

OMPI_WIN32_HDR = \
	win32/ompi_misc.h \
	win32/ompi_process.h \
	win32/ompi_time.h \
	win32/ompi_uio.h \
	win32/ompi_util.h \
	win32/ompi_utsname.h \
	win32/ompi_inet.h \
	win32/win_compat.h

if OMPI_NEED_WINDOWS_REPLACEMENTS
libopen_pal_la_SOURCES += $(OMPI_WIN32_SRC)
headers += $(OMPI_WIN32_HDR)
endif

# Always include Windows files in dist
EXTRA_DIST += $(OMPI_WIN32_HDR) \
              $(OMPI_WIN32_SRC) \
              win_makefile

