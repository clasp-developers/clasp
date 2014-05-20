## Process this file with automake to generate Makefile.in

# Copyright 1996, 1999, 2000, 2001, 2002, 2003, 2004 Free Software
# Foundation, Inc.
#
# This file is part of the GNU MP Library.
#
# The GNU MP Library is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 2.1 of the License, or (at your
# option) any later version.
#
# The GNU MP Library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
# License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
# the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
# MA 02110-1301, USA.


INCLUDES = -I$(top_srcdir) -I$(top_srcdir)/tests
LDADD = $(top_builddir)/tests/libtests.la $(top_builddir)/libgmp.la

check_PROGRAMS = t-add t-sub t-conv t-sqrt t-sqrt_ui t-muldiv t-dm2exp reuse \
  t-cmp_d t-cmp_si t-div t-fits t-get_d t-get_d_2exp \
  t-get_si t-get_ui t-gsprec t-inp_str t-int_p t-mul_ui \
  t-set t-set_q t-set_si t-set_ui t-trunc t-ui_div
TESTS = $(check_PROGRAMS)

$(top_builddir)/tests/libtests.la:
	cd $(top_builddir)/tests; $(MAKE) $(AM_MAKEFLAGS) libtests.la
