## Process this file with automake to generate Makefile.in

# Copyright 2000, 2001, 2002 Free Software Foundation, Inc.
#
# This file is part of the GNU MP Library.
#
# The GNU MP Library is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at your
# option) any later version.
#
# The GNU MP Library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
# License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with the GNU MP Library.  If not, see http://www.gnu.org/licenses/.


INCLUDES = -I$(top_srcdir) -I$(top_srcdir)/tests
LDADD = $(top_builddir)/tests/libtests.la $(top_builddir)/libmp.la

if WANT_MPBSD
MPBSD_check_OPTION = allfuns t-itom t-mtox
endif

check_PROGRAMS = $(MPBSD_check_OPTION)
TESTS = $(check_PROGRAMS)

# check linking only against libmp
allfuns_LDADD = $(top_builddir)/libmp.la

$(top_builddir)/tests/libtests.la:
	cd $(top_builddir)/tests; $(MAKE) $(AM_MAKEFLAGS) libtests.la
