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
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = -DMALLOC_DEBUG=0

if OPAL_HAVE_POSIX_THREADS
AM_CPPFLAGS +=  \
    -D_GNU_SOURCE=1 \
    -DMALLOC_HOOKS=1 \
    -I$(srcdir)/sysdeps/pthread
endif
if OPAL_HAVE_SOLARIS_THREADS
AM_CPPFLAGS += -I$(srcdir)/sysdeps/solaris
endif
# this must come *after* the threads -Is
AM_CPPFLAGS += -I$(srcdir)/sysdeps/generic

# ptmalloc requires us to include the copyright notice in the
# software.  So install it in the same place that we install ROMIO's
# copyright notices.

docdir      = $(datadir)/openmpi/doc
doc_DATA    = COPYRIGHT-ptmalloc2.txt

# Help file
dist_pkgdata_DATA = help-opal-memory-linux.txt

# This component is only ever built statically (i.e., slurped into
# libopen-pal) -- it is never built as a DSO.
noinst_LTLIBRARIES = libmca_memory_linux.la
libmca_memory_linux_la_SOURCES = \
    memory_linux.h \
    memory_linux_component.c
libmca_memory_linux_la_LDFLAGS = \
   -module -avoid-version $(memory_linux_LDFLAGS)
libmca_memory_linux_la_LIBADD = $(memory_linux_LIBS)

# Do we have ptmalloc2 support?
if MEMORY_LINUX_PTMALLOC2
libmca_memory_linux_la_SOURCES += \
    memory_linux_ptmalloc2.c \
    memory_linux_munmap.c \
    rename.h \
    malloc.c \
    malloc-stats.c \
    malloc.h
endif

# Do we have ummunotify support?
if MEMORY_LINUX_UMMUNOTIFY
libmca_memory_linux_la_SOURCES += memory_linux_ummunotify.c public.h
endif

# these are included directly and shouldn't be built solo
EXTRA_libmca_memory_linux_la_SOURCES = \
    arena.c \
    hooks.c

EXTRA_DIST = \
    README-open-mpi.txt \
    README-ptmalloc2.txt \
    ChangeLog-ptmalloc2.txt \
    COPYRIGHT-ptmalloc2.txt \
    lran2.h \
    t-test.h \
    t-test1.c \
    t-test2.c \
    tst-mallocstate.c \
    tst-mstats.c \
    sysdeps/sproc/malloc-machine.h \
    sysdeps/sproc/thread-st.h \
    sysdeps/pthread/malloc-machine.h \
    sysdeps/pthread/thread-st.h \
    sysdeps/solaris/malloc-machine.h \
    sysdeps/solaris/thread-st.h \
    sysdeps/generic/malloc-machine.h \
    sysdeps/generic/thread-st.h \
    sysdeps/generic/atomic.h \
    $(doc_DATA)
