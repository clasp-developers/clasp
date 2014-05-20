#
# Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Need to include these files so that these directories are carried in
# the tarball (in case someone invokes autogen.sh on a dist tarball).
EXTRA_DIST = \
        hwloc/doc/README.txt \
        hwloc/tests/README.txt \
        hwloc/utils/README.txt

SUBDIRS = hwloc

# Headers and sources
headers = hwloc132.h
sources = hwloc132_component.c

# We only ever build this component statically
noinst_LTLIBRARIES = libmca_hwloc_hwloc132.la
libmca_hwloc_hwloc132_la_SOURCES = $(headers) $(sources)
nodist_libmca_hwloc_hwloc132_la_SOURCES = $(nodist_headers)
libmca_hwloc_hwloc132_la_LDFLAGS = -module -avoid-version $(opal_hwloc_hwloc132_LDFLAGS)
libmca_hwloc_hwloc132_la_LIBADD = $(opal_hwloc_hwloc132_LIBS)
libmca_hwloc_hwloc132_la_DEPENDENCIES = \
    $(HWLOC_top_builddir)/src/libhwloc_embedded.la

# Since the rest of the code base includes the underlying hwloc.h, we
# also have to install the underlying header files when
# --with-devel-headers is specified.  hwloc doesn't support this; the
# least gross way to make this happen is just to list all of hwloc's
# header files here.  :-(
headers += \
        hwloc/include/hwloc.h \
        hwloc/include/hwloc/bitmap.h \
        hwloc/include/hwloc/cpuset.h \
        hwloc/include/hwloc/helper.h \
        hwloc/include/hwloc/myriexpress.h \
        hwloc/include/hwloc/openfabrics-verbs.h \
        hwloc/include/hwloc/cuda.h \
        hwloc/include/hwloc/cudart.h \
        hwloc/include/hwloc/rename.h \
        hwloc/include/private/private.h \
        hwloc/include/private/debug.h \
        hwloc/include/private/misc.h \
        hwloc/include/private/cpuid.h
nodist_headers = hwloc/include/hwloc/autogen/config.h

if HWLOC_HAVE_LINUX
headers += \
        hwloc/include/hwloc/linux.h \
        hwloc/include/hwloc/linux-libnuma.h
endif HWLOC_HAVE_LINUX

if HWLOC_HAVE_SCHED_SETAFFINITY
headers += hwloc/include/hwloc/glibc-sched.h
endif HWLOC_HAVE_SCHED_SETAFFINITY

# Conditionally install the header files
if WANT_INSTALL_HEADERS
opaldir = $(includedir)/openmpi/$(subdir)
nobase_opal_HEADERS = $(headers)
nobase_nodist_opal_HEADERS = $(nodist_headers)
endif
