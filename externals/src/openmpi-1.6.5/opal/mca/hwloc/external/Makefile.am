#
# Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This is a special component -- its main purpose in life is to have
# its configure.m4 add some things to CPPFLAGS, LDFLAGS, and
# LIBS.  Hence, there's nothing to build here.  :-)

# We only ever build this component statically

noinst_LTLIBRARIES = libmca_hwloc_external.la
libmca_hwloc_external_la_SOURCES = hwloc_external_component.c
libmca_hwloc_external_la_LDFLAGS = \
    -module -avoid-version \
    $(opal_hwloc_external_LDFLAGS)
libmca_hwloc_external_la_LIBADD = $(hwloc_external_LIBS) -lhwloc
