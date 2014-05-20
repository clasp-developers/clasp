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

# A word of explanation...
#
# This library is linked against various MCA components because all
# shared-memory based components (e.g., mpool, ptl, etc.)  need to
# share some common code and data.  There's two cases:
#
# 1. libmca_common_portals.la is a shared library.  By linking that shared
# library to all components that need it, the OS linker will
# automatically load it into the process as necessary, and there will
# only be one copy (i.e., all the components will share *one* copy of
# the code and data).
# 2. libmca_common_portals.la is a static library.  In this case, it
# will be rolled up into the top-level libmpi.la.  It will also be
# rolled into each component, but then the component will also be
# rolled up into the upper-level libmpi.la.  Libtool sorts this all
# out and it all works out in the end.
#
# Note that building this common component statically and linking
# against other dynamic components is *not* supported!


AM_CPPFLAGS = $(common_mx_CPPFLAGS)

# Header files

headers = \
        common_mx.h

# Source files

sources = \
        common_mx.c


lib_LTLIBRARIES =
noinst_LTLIBRARIES =
comp_inst = libmca_common_mx.la
comp_noinst = libmca_common_mx_noinst.la

if OMPI_BUILD_common_mx_DSO
lib_LTLIBRARIES += $(comp_inst)
else
noinst_LTLIBRARIES += $(comp_noinst)
endif

libmca_common_mx_la_SOURCES = $(headers) $(sources)
libmca_common_mx_la_LDFLAGS = -version-info $(libmca_common_mx_so_version) $(common_mx_LDFLAGS)
libmca_common_mx_la_LIBADD = $(common_mx_LIBS)

libmca_common_mx_noinst_la_SOURCES = $(libmca_common_mx_la_SOURCES)
libmca_common_mx_noinst_la_LDFLAGS = $(common_mx_LDFLAGS)
libmca_common_mx_noinst_la_LIBADD = $(common_mx_LIBS)


# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/ompi/mca/common/mx
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

# These two rules will sym link the "noinst" libtool library filename
# to the installable libtool library filename in the case where we are
# compiling this component statically (case 2), described above).

all-local:
	if test -z "$(lib_LTLIBRARIES)"; then \
	  rm -f "$(comp_inst)"; \
	  $(LN_S) "$(comp_noinst)" "$(comp_inst)"; \
	fi

clean-local:
	if test -z "$(lib_LTLIBRARIES)"; then \
	  rm -f "$(comp_inst)"; \
	fi
