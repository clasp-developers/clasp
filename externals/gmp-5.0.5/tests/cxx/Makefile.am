## Process this file with automake to generate Makefile.in

# Copyright 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
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


# LDADD has an explicit -L of $(top_builddir)/.libs for the benefit of gcc
# 3.2 on itanium2-hp-hpux11.22.  Without this option, the libgmp.sl.6
# required by libgmpxx.sl (ie. in its NEEDED records) is not found by the
# linker.  FIXME: Presumably libtool should do something about this itself.
#
INCLUDES = -I$(top_srcdir) -I$(top_srcdir)/tests
LDADD = -L$(top_builddir)/.libs \
  $(top_builddir)/tests/libtests.la \
  $(top_builddir)/libgmpxx.la \
  $(top_builddir)/libgmp.la

if WANT_CXX
check_PROGRAMS = t-assign t-binary t-cast t-constr t-headers \
  t-istream t-locale t-misc t-ops t-ostream t-prec t-rand t-ternary t-unary
TESTS = $(check_PROGRAMS)
endif

t_assign_SOURCES  = t-assign.cc
t_binary_SOURCES  = t-binary.cc
t_cast_SOURCES    = t-cast.cc
t_constr_SOURCES  = t-constr.cc
t_headers_SOURCES = t-headers.cc
t_istream_SOURCES = t-istream.cc
t_locale_SOURCES  = t-locale.cc clocale.c
t_misc_SOURCES    = t-misc.cc
t_ops_SOURCES     = t-ops.cc
t_ostream_SOURCES = t-ostream.cc
t_prec_SOURCES    = t-prec.cc
t_rand_SOURCES    = t-rand.cc
t_ternary_SOURCES = t-ternary.cc
t_unary_SOURCES   = t-unary.cc

$(top_builddir)/tests/libtests.la:
	cd $(top_builddir)/tests; $(MAKE) $(AM_MAKEFLAGS) libtests.la


# Libtool (1.5) somehow botches its uninstalled shared library setups on
# OpenBSD 3.2, making the C++ test programs here fail.  libgmpxx.so ends up
# with a NEEDED record asking for ./.libs/libgmp.so.N, but the loader can't
# find that unless it exists in the current directory.
#
# FIXME: Clearly libtool ought to handle this itself, in which case the hack
# here can be removed.
#
# Note this fix applies only when running "make check".  The cp here should
# be done manually if just one program is to be built and run.
#
TESTS_ENVIRONMENT = cp $(top_builddir)/.libs/libgmp.so.* .libs 2>/dev/null || true;
