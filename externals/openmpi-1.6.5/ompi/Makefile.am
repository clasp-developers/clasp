#
# Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST = CMakeLists.txt

# We always have C bindings, but do we have profiling?

if WANT_MPI_BINDINGS_LAYER
c_mpi_lib = mpi/c/libmpi_c_mpi.la
else
c_mpi_lib = 
endif
if WANT_PMPI_BINDINGS_LAYER
c_pmpi_lib = mpi/c/profile/libmpi_c_pmpi.la
else
c_pmpi_lib = 
endif

# See if we have f77 MPI bindings

if OMPI_WANT_F77_BINDINGS
f77_base_lib = mpi/f77/base/libmpi_f77_base.la
else
f77_base_lib =
endif

# Note that the ordering of "." in SUBDIRS is important: the C++, F77,
# and F90 bindings are all in standalone .la files that depend on
# libmpi.la.  So we must fully build libmpi.la first.

# NOTE: A handful of files in mpi/f77/base must be included in
# libmpi.la.  But we wanted to keep all the Fortran sources together
# in the same tree, so we moved those sources to a separate
# subdirectory with its own Makefile.include that is included in this
# Makefile.am (NOTE: it did *not* work to put all the files -- base
# and non-base -- into mpi/f77 and have both a regular Makefile.am for
# building the f77 bindings library and a separate Makefile.include
# that was included in this top-level Makefile.am; problems occurred
# with "make distclean" and files in the ompi/mpi/f77/.deps directory
# -- it's not clear whether this is an AM bug or whether this behavior
# is simply not supported).  This ompi/mpi/f77/base/Makefile.include
# file makes a convenience LT library that is then sucked into
# libmpi.la (the ompi/mpi/f77/base sources must be compiled with
# special CPPFLAGS; we can't just add the raw sources to
# libmpi_la_SOURCES, unfortunately).

# The end of the result is that libmpi.la -- including a few sources
# from mpi/f77/base -- is fully built before the C++, F77, and F90
# bindings are built.  Therefore, the C++, F77 and F90 bindings
# libraries can all link against libmpi.la.

SUBDIRS = \
	include \
	datatype \
	debuggers \
	etc \
	mpi/c \
	$(EXT_ompi_FRAMEWORKS_SUBDIRS) \
	$(EXT_ompi_FRAMEWORK_COMPONENT_STATIC_SUBDIRS) \
	$(MCA_ompi_FRAMEWORKS_SUBDIRS) \
	$(MCA_ompi_FRAMEWORK_COMPONENT_STATIC_SUBDIRS) \
	. \
	mpi/cxx \
	mpi/f77 \
	mpi/f90 \
	$(MCA_ompi_FRAMEWORK_COMPONENT_DSO_SUBDIRS) \
	$(OMPI_CONTRIB_SUBDIRS)

DIST_SUBDIRS = \
	include \
	datatype \
	debuggers \
	etc \
	mpi/c \
	mpi/cxx \
	mpi/f77 \
	mpi/f90 \
	$(EXT_ompi_FRAMEWORKS_SUBDIRS) \
	$(EXT_ompi_FRAMEWORK_COMPONENT_ALL_SUBDIRS) \
	$(MCA_ompi_FRAMEWORKS_SUBDIRS) \
	$(MCA_ompi_FRAMEWORK_COMPONENT_ALL_SUBDIRS) \
	$(OMPI_CONTRIB_DIST_SUBDIRS)

# Build the main MPI library

lib_LTLIBRARIES = libmpi.la
libmpi_la_SOURCES =
libmpi_la_LIBADD = \
	datatype/libdatatype.la \
	debuggers/libdebuggers.la \
	mpi/c/libmpi_c.la \
	$(c_mpi_lib) \
	$(c_pmpi_lib) \
	$(f77_base_lib) \
	$(MCA_ompi_FRAMEWORK_LIBS) \
	$(EXT_ompi_FRAMEWORK_LIBS) \
	$(OMPI_LIBMPI_EXTRA_LIBS) \
	$(top_ompi_builddir)/orte/libopen-rte-lt.la
libmpi_la_DEPENDENCIES = \
	datatype/libdatatype.la \
	debuggers/libdebuggers.la \
	mpi/c/libmpi_c.la \
	$(c_mpi_lib) \
	$(c_pmpi_lib) \
	$(f77_base_lib) \
	$(MCA_ompi_FRAMEWORK_LIBS) \
	$(EXT_ompi_FRAMEWORK_LIBS) \
	$(top_ompi_builddir)/orte/libopen-rte-lt.la
libmpi_la_LDFLAGS = \
        -version-info $(libmpi_so_version) \
	$(OMPI_LIBMPI_EXTRA_LDFLAGS)

# included subdirectory Makefile.am's and appended-to variables
headers =
noinst_LTLIBRARIES =
include_HEADERS =
nobase_ompi_HEADERS = 
dist_pkgdata_DATA =
libmpi_la_SOURCES += $(headers)
nodist_man_MANS =

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/ompi
nobase_ompi_HEADERS += $(headers)
else
ompidir = $(includedir)
endif

include class/Makefile.am
include attribute/Makefile.am
include communicator/Makefile.am
include errhandler/Makefile.am
include file/Makefile.am
include group/Makefile.am
include info/Makefile.am
include op/Makefile.am
include peruse/Makefile.am
include proc/Makefile.am
include request/Makefile.am
include runtime/Makefile.am
include win/Makefile.am
include tools/Makefile.am
include mpi/Makefile.am
include mpi/f77/base/Makefile.include
include mpi/man/man3/Makefile.extra

# Ensure that the man page directory exists before we try to make man
# page files (because ompi/mpi/man/man3 has no config.status-generated
# Makefile)
dir_stamp = $(top_builddir)/$(subdir)/mpi/man/man3/.dir-stamp

# Also ensure that the man pages are rebuilt if the opal_config.h file
# changes (e.g., configure was run again, meaning that the release
# date or version may have changed)
$(nodist_man_MANS): $(dir_stamp) $(top_builddir)/opal/include/opal_config.h

$(dir_stamp):
	$(MKDIR_P) `dirname $@`
	touch "$@"

# Remove the generated man pages
distclean-local:
	rm -f $(nodist_man_MANS) $(dir_stamp)
