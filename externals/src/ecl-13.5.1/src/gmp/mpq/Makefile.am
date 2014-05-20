## Process this file with automake to generate Makefile.in

# Copyright 1996, 1998, 2000, 2001, 2002 Free Software Foundation, Inc.
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


INCLUDES = -D__GMP_WITHIN_GMP -I$(top_srcdir)

noinst_LTLIBRARIES = libmpq.la
libmpq_la_SOURCES =							\
  abs.c aors.c canonicalize.c clear.c					\
  cmp.c cmp_si.c cmp_ui.c div.c equal.c					\
  get_d.c get_den.c get_num.c get_str.c					\
  init.c inp_str.c inv.c md_2exp.c mul.c neg.c out_str.c		\
  set.c set_den.c set_num.c set_si.c set_str.c set_ui.c set_z.c set_d.c	\
  set_f.c swap.c
