## Process this file with automake to generate Makefile.in

# Copyright 2000, 2001, 2002, 2003, 2005 Free Software Foundation, Inc.
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

EXTRA_DIST = alpha.asm pentium.asm sparcv9.asm hppa.asm hppa2.asm hppa2w.asm \
  ia64.asm powerpc.asm powerpc64.asm x86_64.asm many.pl
noinst_HEADERS = speed.h

# Prefer -static on the speed and tune programs, since that can avoid
# overheads of shared library linkages on some systems.  Libtool tends to
# botch -static if configured with --disable-static, perhaps reasonably
# enough.  In any event under --disable-static the only choice is a dynamic
# link so there's no point in -static.
#
if ENABLE_STATIC
STATIC = -static
else
STATIC =
endif


EXTRA_LTLIBRARIES = libspeed.la

libspeed_la_SOURCES =							\
  common.c divrem1div.c divrem1inv.c divrem2div.c divrem2inv.c		\
  freq.c gcd_bin.c gcd_finda_gen.c					\
  gcdext_single.c gcdext_double.c gcdextod.c gcdextos.c			\
  jacbase1.c jacbase2.c jacbase3.c					\
  mod_1_div.c mod_1_inv.c modlinv.c					\
  noop.c powm_mod.c powm_redc.c pre_divrem_1.c				\
  set_strb.c set_strs.c time.c						\
  sb_div.c sb_inv.c

libspeed_la_DEPENDENCIES = $(SPEED_CYCLECOUNTER_OBJ) \
  $(top_builddir)/tests/libtests.la $(top_builddir)/libgmp.la
libspeed_la_LIBADD = $(libspeed_la_DEPENDENCIES) $(LIBM)
libspeed_la_LDFLAGS = $(STATIC)

$(top_builddir)/tests/libtests.la:
	cd $(top_builddir)/tests; $(MAKE) $(AM_MAKEFLAGS) libtests.la


# The library code is faster static than shared on some systems, so do
# tuning and measuring with static, since users who care about maximizing
# speed will be using that.  speed-dynamic exists to show the difference.
#
# On Solaris 8, gcc 2.95.2 -static is somehow broken (it creates executables
# that immediately seg fault), so -all-static is not used.  The only thing
# -all-static does is make libc static linked as well as libgmp, and that
# makes a difference only when measuring malloc and friends in the speed
# program.  This can always be forced with "make speed_LDFLAGS=-all-static
# ..." if desired, see tune/README.

EXTRA_PROGRAMS = speed speed-dynamic speed-ext tuneup

DEPENDENCIES = libspeed.la
LDADD = $(DEPENDENCIES)

speed_SOURCES = speed.c
speed_LDFLAGS = $(STATIC)

speed_dynamic_SOURCES = speed.c

speed_ext_SOURCES = speed-ext.c
speed_ext_LDFLAGS = $(STATIC)

tuneup_SOURCES = tuneup.c
nodist_tuneup_SOURCES = sqr_basecase.c $(TUNE_MPN_SRCS)
tuneup_DEPENDENCIES = $(TUNE_SQR_OBJ) libspeed.la
tuneup_LDADD = $(tuneup_DEPENDENCIES)
tuneup_LDFLAGS = $(STATIC)


tune:
	$(MAKE) $(AM_MAKEFLAGS) tuneup$(EXEEXT)
	./tuneup

allprogs: $(EXTRA_PROGRAMS)

# $(MANY_CLEAN) and $(MANY_DISTCLEAN) are hooks for many.pl
CLEANFILES = $(EXTRA_PROGRAMS) $(EXTRA_LTLIBRARIES) \
	$(TUNE_MPN_SRCS) sqr_asm.asm \
	stg.gnuplot stg.data \
	mtg.gnuplot mtg.data \
	fibg.gnuplot fibg.data \
	graph.gnuplot graph.data \
	$(MANY_CLEAN)
DISTCLEANFILES = sqr_basecase.c  $(MANY_DISTCLEAN)


# Generating these little files at build time seems better than including
# them in the distribution, since the list can be changed more easily.
#
# mpn/generic/tdiv_qr.c uses mpn_divrem_1 and mpn_divrem_2, but only for 1
# and 2 limb divisors, which are never used during tuning, so it doesn't
# matter whether it picks up a tuned or untuned version of those.
#
# divrem_1 and mod_1 are recompiled renamed to "_tune" to avoid a linking
# problem.  If a native divrem_1 provides an mpn_divrem_1c entrypoint then
# common.c will want that, but the generic divrem_1 doesn't provide it,
# likewise for mod_1.  The simplest way around this is to have the tune
# build versions renamed suitably.
#
# FIXME: Would like say mul_n.c to depend on $(top_builddir)/mul_n.c so the
# recompiled object will be rebuilt if that file changes.

TUNE_MPN_SRCS = $(TUNE_MPN_SRCS_BASIC) divrem_1.c mod_1.c
TUNE_MPN_SRCS_BASIC = dc_divrem_n.c divrem_2.c gcd.c gcdext.c get_str.c \
  mul_n.c mullow_n.c mul_fft.c mul.c sb_divrem_mn.c tdiv_qr.c

$(TUNE_MPN_SRCS_BASIC):
	for i in $(TUNE_MPN_SRCS_BASIC); do \
	  echo "#define TUNE_PROGRAM_BUILD 1" >$$i; \
	  echo "#include \"mpn/generic/$$i\"" >>$$i; \
	done

divrem_1.c:
	echo "#define TUNE_PROGRAM_BUILD 1"                >divrem_1.c
	echo "#define __gmpn_divrem_1  mpn_divrem_1_tune" >>divrem_1.c
	echo "#include \"mpn/generic/divrem_1.c\""        >>divrem_1.c

mod_1.c:
	echo "#define TUNE_PROGRAM_BUILD 1"          >mod_1.c
	echo "#define __gmpn_mod_1  mpn_mod_1_tune" >>mod_1.c
	echo "#include \"mpn/generic/mod_1.c\""     >>mod_1.c

sqr_asm.asm: $(top_builddir)/mpn/sqr_basecase.asm
	echo 'define(SQR_KARATSUBA_THRESHOLD_OVERRIDE,SQR_KARATSUBA_THRESHOLD_MAX)' >sqr_asm.asm
	echo 'include(../mpn/sqr_basecase.asm)' >>sqr_asm.asm


include ../mpn/Makeasm.am


# "mk" is multiplication in the karatsuba range
# "st" is squaring in the toom-cook range, etc
# "g" forms produce graphs

mk:
	./speed -s 5-40 -c mpn_mul_basecase mpn_kara_mul_n

MTS = -s 50-150 -c
mt:
	./speed $(MTS) mpn_kara_mul_n mpn_toom3_mul_n
mtg:
	./speed $(MTS) -P mtg mpn_kara_mul_n mpn_toom3_mul_n

sk:
	./speed -s 5-40 -c mpn_sqr_basecase mpn_kara_sqr_n

STS = -s 50-150 -c
st:
	./speed $(STS) mpn_kara_sqr_n mpn_toom3_sqr_n
stg:
	./speed $(STS) -P stg mpn_kara_sqr_n mpn_toom3_sqr_n

dc:
	./speed -s 5-40 -c mpn_dc_divrem_sb mpn_dc_divrem_n mpn_dc_tdiv_qr

fib:
	./speed -s 40-60 -c mpz_fib_ui
fibg:
	./speed -s 10-300 -P fibg mpz_fib_ui


gcd:
	./speed -s 1-20 -c mpn_gcd

udiv:
	./speed -s 1 -c udiv_qrnnd udiv_qrnnd_preinv udiv_qrnnd_preinv2norm invert_limb udiv_qrnnd_c

divn:
	./speed -s 1-30 -c mpn_divrem_1_div.-1 mpn_divrem_1_inv.-1
divun:
	./speed -s 1-30 -c mpn_divrem_1_div.12345 mpn_divrem_1_inv.12345
modn:
	./speed -s 1-30 -c mpn_mod_1_div.-1 mpn_mod_1_inv.-1
modun:
	./speed -s 1-30 -c mpn_mod_1_div.12345 mpn_mod_1_inv.12345


graph:
	./speed -s 1-5000 -f 1.02 -P graph mpn_mul_n mpn_sqr
	gnuplot graph.gnuplot
