## Process this file with automake to generate Makefile.in

# Copyright 1996, 1998, 1999, 2000, 2001, 2002, 2005, 2011 Free Software
# Foundation, Inc.
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


INCLUDES = -D__GMP_WITHIN_GMP -I$(top_srcdir) \
  -DOPERATION_`echo $* | sed 's/_$$//'`

OFILES = @mpn_objects@


# All possible mpn normal and optional function files are listed here, to
# get automake to generate ansi2knr rules for each.  Such rules will be
# ignored for any that are instead implemented with a .asm (or whatever) for
# a particular target.
#
nodist_EXTRA_libmpn_la_SOURCES =					    \
  add.c add_1.c add_n.c							    \
  addmul_1.c addmul_2.c addmul_3.c addmul_4.c addmul_5.c addmul_6.c	    \
  addmul_7.c addmul_8.c							    \
  and_n.c andn_n.c							    \
  cmp.c com.c copyd.c copyi.c						    \
  dive_1.c diveby3.c divis.c divrem.c divrem_1.c divrem_2.c		    \
  sbpi1_bdiv_qr.c sbpi1_bdiv_q.c					    \
  sbpi1_div_qr.c sbpi1_div_q.c sbpi1_divappr_q.c			    \
  dcpi1_bdiv_qr.c dcpi1_bdiv_q.c					    \
  dcpi1_div_qr.c dcpi1_div_q.c dcpi1_divappr_q.c			    \
  dump.c fib2_ui.c gcd.c						    \
  gcd_1.c gcdext.c get_d.c get_str.c					    \
  hamdist.c hgcd2.c hgcd.c invert_limb.c				    \
  ior_n.c iorn_n.c jacbase.c lshift.c					    \
  matrix22_mul.c mod_1.c mod_34lsub1.c mode1o.c				    \
  mod_1_1.c mod_1_2.c mod_1_3.c mod_1_4.c				    \
  mul.c mul_1.c mul_2.c mul_3.c mul_4.c mul_fft.c mul_n.c mul_basecase.c    \
  nussbaumer_mul.c							    \
  toom22_mul.c toom32_mul.c toom42_mul.c toom52_mul.c toom62_mul.c	    \
  toom33_mul.c toom43_mul.c toom53_mul.c toom63_mul.c			    \
  toom44_mul.c								    \
  toom6h_mul.c toom6_sqr.c toom8h_mul.c toom8_sqr.c			    \
  toom_couple_handling.c						    \
  toom2_sqr.c toom3_sqr.c toom4_sqr.c					    \
  toom_eval_dgr3_pm1.c toom_eval_dgr3_pm2.c				    \
  toom_eval_pm1.c toom_eval_pm1.c toom_eval_pm2exp.c toom_eval_pm2rexp.c    \
  toom_interpolate_5pts.c toom_interpolate_6pts.c toom_interpolate_7pts.c   \
  toom_interpolate_8pts.c toom_interpolate_12pts.c toom_interpolate_16pts.c \
  invertappr.c invert.c binvert.c mulmod_bnm1.c sqrmod_bnm1.c		    \
  mullo_n.c mullo_basecase.c nand_n.c neg.c nior_n.c perfsqr.c	    \
  popcount.c pre_divrem_1.c pre_mod_1.c pow_1.c random.c random2.c rshift.c \
  rootrem.c scan0.c scan1.c set_str.c			    \
  sqr_basecase.c sqr_diagonal.c						    \
  sqrtrem.c sub.c sub_1.c sub_n.c submul_1.c				    \
  tdiv_qr.c udiv_qrnnd.c udiv_w_sdiv.c xor_n.c xnor_n.c

noinst_LTLIBRARIES = libmpn.la
nodist_libmpn_la_SOURCES = fib_table.c mp_bases.c
libmpn_la_LIBADD = $(OFILES)
libmpn_la_DEPENDENCIES = $(OFILES)

TARG_DIST = a29k alpha arm clipper cray generic i960 ia64 lisp m68k m88k \
  minithres mips32 mips64 ns32k pa32 pa64 power powerpc32 powerpc64 pyr \
  s390_32 s390_64 sh sparc32 sparc64 vax x86 x86_64 z8000 z8000x

EXTRA_DIST = asm-defs.m4 cpp-ccas m4-ccas $(TARG_DIST)


# These are BUILT_SOURCES at the top-level, so normally they're built before
# recursing into this directory.
#
fib_table.c:
	cd ..; $(MAKE) $(AM_MAKEFLAGS) mpn/fib_table.c
mp_bases.c:
	cd ..; $(MAKE) $(AM_MAKEFLAGS) mpn/mp_bases.c
perfsqr.h:
	cd ..; $(MAKE) $(AM_MAKEFLAGS) mpn/perfsqr.h

tune-gcd-p: gcd.c
	$(COMPILE) -g -O1 -I $(top_srcdir)/tune -DTUNE_GCD_P=1 gcd.c -o tune-gcd-p -L ../.libs -L../tune/.libs -lspeed -lgmp -lm

include Makeasm.am
