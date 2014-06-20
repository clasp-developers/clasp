## Process this file with automake to generate Makefile.in

# Copyright 1996, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.
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


# -I$(top_srcdir)/mpz is for #includes done by mpz .c files.  Perhaps most
# compilers are smart enough to look in the same directory as the .c file
# already, but lets make absolutely sure.
#
INCLUDES = -DBERKELEY_MP -D__GMP_WITHIN_GMP -D__gmpz_realloc=_mp_realloc \
	-I$(top_srcdir) -I$(top_srcdir)/mpz

# The mpz sources here all know to look for -DBERKELEY_MP to compile to in
# mpbsd form.
#
libmpbsd_la_SOURCES = itom.c mfree.c min.c mout.c mtox.c rpow.c sdiv.c xtom.c \
  ../mpz/add.c ../mpz/cmp.c ../mpz/gcd.c ../mpz/mul.c ../mpz/powm.c \
  ../mpz/realloc.c ../mpz/set.c ../mpz/sqrtrem.c ../mpz/sub.c ../mpz/tdiv_qr.c

if WANT_MPBSD
noinst_LTLIBRARIES = libmpbsd.la
endif
