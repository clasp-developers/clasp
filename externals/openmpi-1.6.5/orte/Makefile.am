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

SUBDIRS = \
	include \
	$(MCA_orte_FRAMEWORKS_SUBDIRS) \
	$(MCA_orte_FRAMEWORK_COMPONENT_STATIC_SUBDIRS) \
	etc \
        . \
	$(MCA_orte_FRAMEWORK_COMPONENT_DSO_SUBDIRS)

DIST_SUBDIRS = \
	include \
	etc \
	$(MCA_orte_FRAMEWORKS_SUBDIRS) \
	$(MCA_orte_FRAMEWORK_COMPONENT_ALL_SUBDIRS)

# Build the main ORTE library

lib_LTLIBRARIES = libopen-rte.la
libopen_rte_la_SOURCES =
libopen_rte_la_LIBADD = \
	$(MCA_orte_FRAMEWORK_LIBS) \
	$(top_ompi_builddir)/opal/libopen-pal-lt.la
libopen_rte_la_DEPENDENCIES = $(libopen_rte_la_LIBADD)
libopen_rte_la_LDFLAGS = -version-info $(libopen_rte_so_version)

# included subdirectory Makefile.am's and appended-to variables
headers =
noinst_LTLIBRARIES =
nobase_orte_HEADERS = 
dist_pkgdata_DATA =
libopen_rte_la_SOURCES += $(headers)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ortedir = $(includedir)/openmpi/orte
nobase_orte_HEADERS += $(headers)
else
ortedir = $(includedir)
endif

# setup to catch extra files to be included in tarballs
EXTRA_DIST = CMakeLists.txt

include runtime/Makefile.am
include util/Makefile.am
include tools/Makefile.am
include orted/Makefile.am
include test/mpi/Makefile.include
include test/system/Makefile.include
include threads/Makefile.am

# Set the convenience library to be the same as the non-convenience
# library, but a) it's marked as "noinst", so LT knows it's a
# convenience library, and b) no .so version number is assigned here.
# Note that we only need to make the convenience library if the OMPI
# project is being built (i.e., if there is an upper layer that needs
# to slurp in ORTE as a convenience library).

if PROJECT_ORTE
noinst_LTLIBRARIES += libopen-rte-lt.la
endif
libopen_rte_lt_la_SOURCES = $(libopen_rte_la_SOURCES)
libopen_rte_lt_la_LIBADD = $(libopen_rte_la_LIBADD)
libopen_rte_lt_la_DEPENDENCIES = $(libopen_rte_la_DEPENDENCIES)
