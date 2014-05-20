# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2007 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS="-I$(top_srcdir)/test/support"

if PROJECT_OMPI
  REQUIRES_OMPI = ompi_rb_tree
endif

check_PROGRAMS = \
	$(REQUIRES_OMPI) opal_bitmap \
	opal_hash_table \
	opal_list \
	opal_value_array \
	opal_pointer_array

# The tests are built, but since they're not actively maintained, they
# are not run during "make check".
TESTS =

opal_bitmap_SOURCES = opal_bitmap.c 
opal_bitmap_LDADD = \
        $(top_builddir)/opal/libopen-pal.la \
        $(top_builddir)/test/support/libsupport.a
opal_bitmap_DEPENDENCIES = $(opal_bitmap_LDADD)

opal_list_SOURCES = opal_list.c 
opal_list_LDADD = \
        $(top_builddir)/opal/libopen-pal.la \
        $(top_builddir)/test/support/libsupport.a
opal_list_DEPENDENCIES = $(opal_list_LDADD)

opal_hash_table_SOURCES = opal_hash_table.c
opal_hash_table_LDADD = \
        $(top_builddir)/opal/libopen-pal.la \
        $(top_builddir)/test/support/libsupport.a
opal_hash_table_DEPENDENCIES = $(opal_hash_table_LDADD)

opal_pointer_array_SOURCES = opal_pointer_array.c
opal_pointer_array_LDADD = \
        $(top_builddir)/opal/libopen-pal.la \
        $(top_builddir)/test/support/libsupport.a
opal_pointer_array_DEPENDENCIES = $(opal_pointer_array_LDADD)

opal_value_array_SOURCES = opal_value_array.c
opal_value_array_LDADD = \
        $(top_builddir)/opal/libopen-pal.la \
        $(top_builddir)/test/support/libsupport.a
opal_value_array_DEPENDENCIES = $(opal_value_array_LDADD)

ompi_rb_tree_SOURCES = ompi_rb_tree.c
ompi_rb_tree_LDADD = \
        $(top_builddir)/ompi/libmpi.la \
	$(top_builddir)/test/support/libsupport.a
ompi_rb_tree_DEPENDENCIES = $(ompi_rb_tree_LDADD)

clean-local:
	rm -f opal_bitmap_test_out.txt opal_hash_table_test_out.txt
