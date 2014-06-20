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



TESTS_ENVIRONMENT = $(SHELL) $(srcdir)/run_tests

check_PROGRAMS = \
	atomic_barrier \
	atomic_barrier_noinline \
	atomic_spinlock \
	atomic_spinlock_noinline \
	atomic_math \
	atomic_math_noinline \
	atomic_cmpset \
	atomic_cmpset_noinline

TESTS = \
	$(check_PROGRAMS)

EXTRA_DIST = run_tests

######################################################################

atomic_barrier_SOURCES = atomic_barrier.c
atomic_barrier_LDADD = $(top_builddir)/opal/asm/libasm.la

atomic_barrier_noinline.c:
	ln -s $(top_srcdir)/test/asm/atomic_barrier.c atomic_barrier_noinline.c
atomic_barrier_noinline_SOURCES = atomic_barrier_noinline.c
atomic_barrier_noinline_LDADD = $(top_builddir)/opal/asm/libasm.la
atomic_barrier_noinline_CFLAGS = $(AM_CFLAGS) -DOMPI_DISABLE_INLINE_ASM

######################################################################

atomic_spinlock_SOURCES = atomic_spinlock.c
atomic_spinlock_LDADD = $(top_builddir)/opal/asm/libasm.la $(libs)

atomic_spinlock_noinline.c:
	ln -s $(top_srcdir)/test/asm/atomic_spinlock.c atomic_spinlock_noinline.c
atomic_spinlock_noinline_SOURCES = atomic_spinlock_noinline.c
atomic_spinlock_noinline_CFLAGS = $(AM_CFLAGS) -DOMPI_DISABLE_INLINE_ASM
atomic_spinlock_noinline_LDADD = $(top_builddir)/opal/asm/libasm.la $(libs)

######################################################################

atomic_math_SOURCES = atomic_math.c
atomic_math_LDADD = $(top_builddir)/opal/asm/libasm.la $(libs)

atomic_math_noinline.c:
	ln -s $(top_srcdir)/test/asm/atomic_math.c atomic_math_noinline.c
atomic_math_noinline_SOURCES = atomic_math_noinline.c
atomic_math_noinline_CFLAGS = $(AM_CFLAGS) -DOMPI_DISABLE_INLINE_ASM
atomic_math_noinline_LDADD = $(top_builddir)/opal/asm/libasm.la $(libs)

######################################################################

atomic_cmpset_SOURCES = atomic_cmpset.c
atomic_cmpset_LDADD = $(top_builddir)/opal/asm/libasm.la $(libs)

atomic_cmpset_noinline.c:
	ln -s $(top_srcdir)/test/asm/atomic_cmpset.c atomic_cmpset_noinline.c
atomic_cmpset_noinline_SOURCES = atomic_cmpset_noinline.c
atomic_cmpset_noinline_CFLAGS = $(AM_CFLAGS) -DOMPI_DISABLE_INLINE_ASM
atomic_cmpset_noinline_LDADD = $(top_builddir)/opal/asm/libasm.la $(libs)

######################################################################


maintainer-clean-local:
	rm -f atomic_barrier_noinline.c \
	atomic_spinlock_noinline.c \
	atomic_math_noinline.c \
	atomic_cmpset_noinline.c
