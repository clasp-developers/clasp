/* Create tuned thresholds for various algorithms.

Copyright 1999, 2000, 2001, 2002, 2003, 2005 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
MA 02110-1301, USA. */


/* Usage: tuneup [-t] [-t] [-p precision]

   -t turns on some diagnostic traces, a second -t turns on more traces.

   Notes:

   The code here isn't a vision of loveliness, mainly because it's subject
   to ongoing changes according to new things wanting to be tuned, and
   practical requirements of systems tested.

   Sometimes running the program twice produces slightly different results.
   This is probably because there's so little separating algorithms near
   their crossover, and on that basis it should make little or no difference
   to the final speed of the relevant routines, but nothing has been done to
   check that carefully.

   Algorithm:

   The thresholds are determined as follows.  A crossover may not be a
   single size but rather a range where it oscillates between method A or
   method B faster.  If the threshold is set making B used where A is faster
   (or vice versa) that's bad.  Badness is the percentage time lost and
   total badness is the sum of this over all sizes measured.  The threshold
   is set to minimize total badness.

   Suppose, as sizes increase, method B becomes faster than method A.  The
   effect of the rule is that, as you look at increasing sizes, isolated
   points where B is faster are ignored, but when it's consistently faster,
   or faster on balance, then the threshold is set there.  The same result
   is obtained thinking in the other direction of A becoming faster at
   smaller sizes.

   In practice the thresholds tend to be chosen to bring on the next
   algorithm fairly quickly.

   This rule is attractive because it's got a basis in reason and is fairly
   easy to implement, but no work has been done to actually compare it in
   absolute terms to other possibilities.

   Implementation:

   In a normal library build the thresholds are constants.  To tune them
   selected objects are recompiled with the thresholds as global variables
   instead.  #define TUNE_PROGRAM_BUILD does this, with help from code at
   the end of gmp-impl.h, and rules in tune/Makefile.am.

   MUL_KARATSUBA_THRESHOLD for example uses a recompiled mpn_mul_n.  The
   threshold is set to "size+1" to avoid karatsuba, or to "size" to use one
   level, but recurse into the basecase.

   MUL_TOOM3_THRESHOLD makes use of the tuned MUL_KARATSUBA_THRESHOLD value.
   Other routines in turn will make use of both of those.  Naturally the
   dependants must be tuned first.

   In a couple of cases, like DIVEXACT_1_THRESHOLD, there's no recompiling,
   just a threshold based on comparing two routines (mpn_divrem_1 and
   mpn_divexact_1), and no further use of the value determined.

   Flags like USE_PREINV_MOD_1 or JACOBI_BASE_METHOD are even simpler, being
   just comparisons between certain routines on representative data.

   Shortcuts are applied when native (assembler) versions of routines exist.
   For instance a native mpn_sqr_basecase is assumed to be always faster
   than mpn_mul_basecase, with no measuring.

   No attempt is made to tune within assembler routines, for instance
   DIVREM_1_NORM_THRESHOLD.  An assembler mpn_divrem_1 is expected to be
   written and tuned all by hand.  Assembler routines that might have hard
   limits are recompiled though, to make them accept a bigger range of sizes
   than normal, eg. mpn_sqr_basecase to compare against mpn_kara_sqr_n.

   Limitations:

   The FFTs aren't subject to the same badness rule as the other thresholds,
   so each k is probably being brought on a touch early.  This isn't likely
   to make a difference, and the simpler probing means fewer tests.

*/

#define TUNE_PROGRAM_BUILD  1   /* for gmp-impl.h */

#include "config.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"

#include "tests.h"
#include "speed.h"

#if !HAVE_DECL_OPTARG
extern char *optarg;
extern int optind, opterr;
#endif


#define DEFAULT_MAX_SIZE   1000  /* limbs */

#if WANT_FFT
mp_size_t  option_fft_max_size = 50000;  /* limbs */
#else
mp_size_t  option_fft_max_size = 0;
#endif
int        option_trace = 0;
int        option_fft_trace = 0;
struct speed_params  s;

struct dat_t {
  mp_size_t  size;
  double     d;
} *dat = NULL;
int  ndat = 0;
int  allocdat = 0;

/* This is not defined if mpn_sqr_basecase doesn't declare a limit.  In that
   case use zero here, which for params.max_size means no limit.  */
#ifndef TUNE_SQR_KARATSUBA_MAX
#define TUNE_SQR_KARATSUBA_MAX  0
#endif

mp_size_t  mul_karatsuba_threshold      = MP_SIZE_T_MAX;
mp_size_t  mul_toom3_threshold          = MUL_TOOM3_THRESHOLD_LIMIT;
mp_size_t  mul_fft_threshold            = MP_SIZE_T_MAX;
mp_size_t  mul_fft_modf_threshold       = MP_SIZE_T_MAX;
mp_size_t  sqr_basecase_threshold       = MP_SIZE_T_MAX;
mp_size_t  sqr_karatsuba_threshold
  = (TUNE_SQR_KARATSUBA_MAX == 0 ? MP_SIZE_T_MAX : TUNE_SQR_KARATSUBA_MAX);
mp_size_t  sqr_toom3_threshold          = SQR_TOOM3_THRESHOLD_LIMIT;
mp_size_t  sqr_fft_threshold            = MP_SIZE_T_MAX;
mp_size_t  sqr_fft_modf_threshold       = MP_SIZE_T_MAX;
mp_size_t  mullow_basecase_threshold    = MP_SIZE_T_MAX;
mp_size_t  mullow_dc_threshold          = MP_SIZE_T_MAX;
mp_size_t  mullow_mul_n_threshold       = MP_SIZE_T_MAX;
mp_size_t  div_sb_preinv_threshold      = MP_SIZE_T_MAX;
mp_size_t  div_dc_threshold             = MP_SIZE_T_MAX;
mp_size_t  powm_threshold               = MP_SIZE_T_MAX;
mp_size_t  gcd_accel_threshold          = MP_SIZE_T_MAX;
mp_size_t  gcdext_threshold		= MP_SIZE_T_MAX;
mp_size_t  divrem_1_norm_threshold      = MP_SIZE_T_MAX;
mp_size_t  divrem_1_unnorm_threshold    = MP_SIZE_T_MAX;
mp_size_t  mod_1_norm_threshold         = MP_SIZE_T_MAX;
mp_size_t  mod_1_unnorm_threshold       = MP_SIZE_T_MAX;
mp_size_t  divrem_2_threshold           = MP_SIZE_T_MAX;
mp_size_t  get_str_dc_threshold         = MP_SIZE_T_MAX;
mp_size_t  get_str_precompute_threshold = MP_SIZE_T_MAX;
mp_size_t  set_str_threshold            = MP_SIZE_T_MAX;

mp_size_t  fft_modf_sqr_threshold = MP_SIZE_T_MAX;
mp_size_t  fft_modf_mul_threshold = MP_SIZE_T_MAX;

struct param_t {
  const char        *name;
  speed_function_t  function;
  speed_function_t  function2;
  double            step_factor;    /* how much to step sizes (rounded down) */
  double            function_fudge; /* multiplier for "function" speeds */
  int               stop_since_change;
  double            stop_factor;
  mp_size_t         min_size;
  int               min_is_always;
  mp_size_t         max_size;
  mp_size_t         check_size;
  mp_size_t         size_extra;

#define DATA_HIGH_LT_R  1
#define DATA_HIGH_GE_R  2
  int               data_high;

  int               noprint;
};


/* These are normally undefined when false, which suits "#if" fine.
   But give them zero values so they can be used in plain C "if"s.  */
#ifndef UDIV_PREINV_ALWAYS
#define UDIV_PREINV_ALWAYS 0
#endif
#ifndef HAVE_NATIVE_mpn_divexact_1
#define HAVE_NATIVE_mpn_divexact_1 0
#endif
#ifndef HAVE_NATIVE_mpn_divrem_1
#define HAVE_NATIVE_mpn_divrem_1 0
#endif
#ifndef HAVE_NATIVE_mpn_divrem_2
#define HAVE_NATIVE_mpn_divrem_2 0
#endif
#ifndef HAVE_NATIVE_mpn_mod_1
#define HAVE_NATIVE_mpn_mod_1 0
#endif
#ifndef HAVE_NATIVE_mpn_modexact_1_odd
#define HAVE_NATIVE_mpn_modexact_1_odd 0
#endif
#ifndef HAVE_NATIVE_mpn_preinv_divrem_1
#define HAVE_NATIVE_mpn_preinv_divrem_1 0
#endif
#ifndef HAVE_NATIVE_mpn_preinv_mod_1
#define HAVE_NATIVE_mpn_preinv_mod_1 0
#endif
#ifndef HAVE_NATIVE_mpn_sqr_basecase
#define HAVE_NATIVE_mpn_sqr_basecase 0
#endif


#define MAX3(a,b,c)  MAX (MAX (a, b), c)

mp_limb_t
randlimb_norm (void)
{
  mp_limb_t  n;
  mpn_random (&n, 1);
  n |= GMP_NUMB_HIGHBIT;
  return n;
}

#define GMP_NUMB_HALFMASK  ((CNST_LIMB(1) << (GMP_NUMB_BITS/2)) - 1)

mp_limb_t
randlimb_half (void)
{
  mp_limb_t  n;
  mpn_random (&n, 1);
  n &= GMP_NUMB_HALFMASK;
  n += (n==0);
  return n;
}


/* Add an entry to the end of the dat[] array, reallocing to make it bigger
   if necessary.  */
void
add_dat (mp_size_t size, double d)
{
#define ALLOCDAT_STEP  500

  ASSERT_ALWAYS (ndat <= allocdat);

  if (ndat == allocdat)
    {
      dat = (struct dat_t *) __gmp_allocate_or_reallocate
        (dat, allocdat * sizeof(dat[0]),
         (allocdat+ALLOCDAT_STEP) * sizeof(dat[0]));
      allocdat += ALLOCDAT_STEP;
    }

  dat[ndat].size = size;
  dat[ndat].d = d;
  ndat++;
}


/* Return the threshold size based on the data accumulated. */
mp_size_t
analyze_dat (int final)
{
  double  x, min_x;
  int     j, min_j;

  /* If the threshold is set at dat[0].size, any positive values are bad. */
  x = 0.0;
  for (j = 0; j < ndat; j++)
    if (dat[j].d > 0.0)
      x += dat[j].d;

  if (option_trace >= 2 && final)
    {
      printf ("\n");
      printf ("x is the sum of the badness from setting thresh at given size\n");
      printf ("  (minimum x is sought)\n");
      printf ("size=%ld  first x=%.4f\n", (long) dat[j].size, x);
    }

  min_x = x;
  min_j = 0;


  /* When stepping to the next dat[j].size, positive values are no longer
     bad (so subtracted), negative values become bad (so add the absolute
     value, meaning subtract). */
  for (j = 0; j < ndat; x -= dat[j].d, j++)
    {
      if (option_trace >= 2 && final)
        printf ("size=%ld  x=%.4f\n", (long) dat[j].size, x);

      if (x < min_x)
        {
          min_x = x;
          min_j = j;
        }
    }

  return min_j;
}


/* Measuring for recompiled mpn/generic/divrem_1.c and mpn/generic/mod_1.c */

mp_limb_t mpn_divrem_1_tune _PROTO ((mp_ptr qp, mp_size_t xsize,
                                    mp_srcptr ap, mp_size_t size,
                                    mp_limb_t d));
mp_limb_t mpn_mod_1_tune _PROTO ((mp_srcptr ap, mp_size_t size, mp_limb_t d));

double
speed_mpn_mod_1_tune (struct speed_params *s)
{
  SPEED_ROUTINE_MPN_MOD_1 (mpn_mod_1_tune);
}
double
speed_mpn_divrem_1_tune (struct speed_params *s)
{
  SPEED_ROUTINE_MPN_DIVREM_1 (mpn_divrem_1_tune);
}


double
tuneup_measure (speed_function_t fun,
                const struct param_t *param,
                struct speed_params *s)
{
  static struct param_t  dummy;
  double   t;
  TMP_DECL;

  if (! param)
    param = &dummy;

  s->size += param->size_extra;

  TMP_MARK;
  SPEED_TMP_ALLOC_LIMBS (s->xp, s->size, 0);
  SPEED_TMP_ALLOC_LIMBS (s->yp, s->size, 0);

  mpn_random (s->xp, s->size);
  mpn_random (s->yp, s->size);

  switch (param->data_high) {
  case DATA_HIGH_LT_R:
    s->xp[s->size-1] %= s->r;
    s->yp[s->size-1] %= s->r;
    break;
  case DATA_HIGH_GE_R:
    s->xp[s->size-1] |= s->r;
    s->yp[s->size-1] |= s->r;
    break;
  }

  t = speed_measure (fun, s);

  s->size -= param->size_extra;

  TMP_FREE;
  return t;
}


#define PRINT_WIDTH  28

void
print_define_start (const char *name)
{
  printf ("#define %-*s  ", PRINT_WIDTH, name);
  if (option_trace)
    printf ("...\n");
}

void
print_define_end_remark (const char *name, mp_size_t value, const char *remark)
{
  if (option_trace)
    printf ("#define %-*s  ", PRINT_WIDTH, name);

  if (value == MP_SIZE_T_MAX)
    printf ("MP_SIZE_T_MAX");
  else
    printf ("%5ld", (long) value);

  if (remark != NULL)
    printf ("  /* %s */", remark);
  printf ("\n");
}

void
print_define_end (const char *name, mp_size_t value)
{
  const char  *remark;
  if (value == MP_SIZE_T_MAX)
    remark = "never";
  else if (value == 0)
    remark = "always";
  else
    remark = NULL;
  print_define_end_remark (name, value, remark);
}

void
print_define (const char *name, mp_size_t value)
{
  print_define_start (name);
  print_define_end (name, value);
}

void
print_define_remark (const char *name, mp_size_t value, const char *remark)
{
  print_define_start (name);
  print_define_end_remark (name, value, remark);
}


void
one (mp_size_t *threshold, struct param_t *param)
{
  int  since_positive, since_thresh_change;
  int  thresh_idx, new_thresh_idx;

#define DEFAULT(x,n)  do { if (! (x))  (x) = (n); } while (0)

  DEFAULT (param->function_fudge, 1.0);
  DEFAULT (param->function2, param->function);
  DEFAULT (param->step_factor, 0.01);  /* small steps by default */
  DEFAULT (param->stop_since_change, 80);
  DEFAULT (param->stop_factor, 1.2);
  DEFAULT (param->min_size, 10);
  DEFAULT (param->max_size, DEFAULT_MAX_SIZE);

  if (param->check_size != 0)
    {
      double   t1, t2;
      s.size = param->check_size;

      *threshold = s.size+1;
      t1 = tuneup_measure (param->function, param, &s);

      *threshold = s.size;
      t2 = tuneup_measure (param->function2, param, &s);
      if (t1 == -1.0 || t2 == -1.0)
        {
          printf ("Oops, can't run both functions at size %ld\n",
                  (long) s.size);
          abort ();
        }
      t1 *= param->function_fudge;

      /* ask that t2 is at least 4% below t1 */
      if (t1 < t2*1.04)
        {
          if (option_trace)
            printf ("function2 never enough faster: t1=%.9f t2=%.9f\n", t1, t2);
          *threshold = MP_SIZE_T_MAX;
          if (! param->noprint)
            print_define (param->name, *threshold);
          return;
        }

      if (option_trace >= 2)
        printf ("function2 enough faster at size=%ld: t1=%.9f t2=%.9f\n",
                (long) s.size, t1, t2);
    }

  if (! param->noprint || option_trace)
    print_define_start (param->name);

  ndat = 0;
  since_positive = 0;
  since_thresh_change = 0;
  thresh_idx = 0;

  if (option_trace >= 2)
    {
      printf ("             algorithm-A  algorithm-B   ratio  possible\n");
      printf ("              (seconds)    (seconds)    diff    thresh\n");
    }

  for (s.size = param->min_size;
       s.size < param->max_size;
       s.size += MAX ((mp_size_t) floor (s.size * param->step_factor), 1))
    {
      double   ti, tiplus1, d;

      /* If there's a size limit and it's reached then it should still
         be sensible to analyze the data since we want the threshold put
         either at or near the limit.  */
      if (s.size >= param->max_size)
        {
          if (option_trace)
            printf ("Reached maximum size (%ld) without otherwise stopping\n",
                    (long) param->max_size);
          break;
        }

      /*
        FIXME: check minimum size requirements are met, possibly by just
        checking for the -1 returns from the speed functions.
      */

      /* using method A at this size */
      *threshold = s.size+1;
      ti = tuneup_measure (param->function, param, &s);
      if (ti == -1.0)
        abort ();
      ti *= param->function_fudge;

      /* using method B at this size */
      *threshold = s.size;
      tiplus1 = tuneup_measure (param->function2, param, &s);
      if (tiplus1 == -1.0)
        abort ();

      /* Calculate the fraction by which the one or the other routine is
         slower.  */
      if (tiplus1 >= ti)
        d = (tiplus1 - ti) / tiplus1;  /* negative */
      else
        d = (tiplus1 - ti) / ti;       /* positive */

      add_dat (s.size, d);

      new_thresh_idx = analyze_dat (0);

      if (option_trace >= 2)
        printf ("size=%ld  %.9f  %.9f  % .4f %c  %ld\n",
                (long) s.size, ti, tiplus1, d,
                ti > tiplus1 ? '#' : ' ',
                (long) dat[new_thresh_idx].size);

      /* Stop if the last time method i was faster was more than a
         certain number of measurements ago.  */
#define STOP_SINCE_POSITIVE  200
      if (d >= 0)
        since_positive = 0;
      else
        if (++since_positive > STOP_SINCE_POSITIVE)
          {
            if (option_trace >= 1)
              printf ("stopped due to since_positive (%d)\n",
                      STOP_SINCE_POSITIVE);
            break;
          }

      /* Stop if method A has become slower by a certain factor. */
      if (ti >= tiplus1 * param->stop_factor)
        {
          if (option_trace >= 1)
            printf ("stopped due to ti >= tiplus1 * factor (%.1f)\n",
                    param->stop_factor);
          break;
        }

      /* Stop if the threshold implied hasn't changed in a certain
         number of measurements.  (It's this condition that ususally
         stops the loop.) */
      if (thresh_idx != new_thresh_idx)
        since_thresh_change = 0, thresh_idx = new_thresh_idx;
      else
        if (++since_thresh_change > param->stop_since_change)
          {
            if (option_trace >= 1)
              printf ("stopped due to since_thresh_change (%d)\n",
                      param->stop_since_change);
            break;
          }

      /* Stop if the threshold implied is more than a certain number of
         measurements ago.  */
#define STOP_SINCE_AFTER   500
      if (ndat - thresh_idx > STOP_SINCE_AFTER)
        {
          if (option_trace >= 1)
            printf ("stopped due to ndat - thresh_idx > amount (%d)\n",
                    STOP_SINCE_AFTER);
          break;
        }

      /* Stop when the size limit is reached before the end of the
         crossover, but only show this as an error for >= the default max
         size.  FIXME: Maybe should make it a param choice whether this is
         an error.  */
      if (s.size >= param->max_size && param->max_size >= DEFAULT_MAX_SIZE)
        {
          fprintf (stderr, "%s\n", param->name);
          fprintf (stderr, "sizes %ld to %ld total %d measurements\n",
                   (long) dat[0].size, (long) dat[ndat-1].size, ndat);
          fprintf (stderr, "    max size reached before end of crossover\n");
          break;
        }
    }

  if (option_trace >= 1)
    printf ("sizes %ld to %ld total %d measurements\n",
            (long) dat[0].size, (long) dat[ndat-1].size, ndat);

  *threshold = dat[analyze_dat (1)].size;

  if (param->min_is_always)
    {
      if (*threshold == param->min_size)
        *threshold = 0;
    }

  if (! param->noprint || option_trace)
    print_define_end (param->name, *threshold);
}


/* Special probing for the fft thresholds.  The size restrictions on the
   FFTs mean the graph of time vs size has a step effect.  See this for
   example using

       ./speed -s 4096-16384 -t 128 -P foo mpn_mul_fft.8 mpn_mul_fft.9
       gnuplot foo.gnuplot

   The current approach is to compare routines at the midpoint of relevant
   steps.  Arguably a more sophisticated system of threshold data is wanted
   if this step effect remains. */

struct fft_param_t {
  const char        *table_name;
  const char        *threshold_name;
  const char        *modf_threshold_name;
  mp_size_t         *p_threshold;
  mp_size_t         *p_modf_threshold;
  mp_size_t         first_size;
  mp_size_t         max_size;
  speed_function_t  function;
  speed_function_t  mul_function;
  mp_size_t         sqr;
};


/* mpn_mul_fft requires pl a multiple of 2^k limbs, but with
   N=pl*BIT_PER_MP_LIMB it internally also pads out so N/2^k is a multiple
   of 2^(k-1) bits. */

mp_size_t
fft_step_size (int k)
{
  mp_size_t  step;

  step = MAX ((mp_size_t) 1 << (k-1), BITS_PER_MP_LIMB) / BITS_PER_MP_LIMB;
  step *= (mp_size_t) 1 << k;

  if (step <= 0)
    {
      printf ("Can't handle k=%d\n", k);
      abort ();
    }

  return step;
}

mp_size_t
fft_next_size (mp_size_t pl, int k)
{
  mp_size_t  m = fft_step_size (k);

/*    printf ("[k=%d %ld] %ld ->", k, m, pl); */

  if (pl == 0 || (pl & (m-1)) != 0)
    pl = (pl | (m-1)) + 1;

/*    printf (" %ld\n", pl); */
  return pl;
}

void
fft (struct fft_param_t *p)
{
  mp_size_t  size;
  int        i, k;

  for (i = 0; i < numberof (mpn_fft_table[p->sqr]); i++)
    mpn_fft_table[p->sqr][i] = MP_SIZE_T_MAX;

  *p->p_threshold = MP_SIZE_T_MAX;
  *p->p_modf_threshold = MP_SIZE_T_MAX;

  option_trace = MAX (option_trace, option_fft_trace);

  printf ("#define %s  {", p->table_name);
  if (option_trace >= 2)
    printf ("\n");

  k = FFT_FIRST_K;
  size = p->first_size;
  for (;;)
    {
      double  tk, tk1;

      size = fft_next_size (size+1, k+1);

      if (size >= p->max_size)
        break;
      if (k >= FFT_FIRST_K + numberof (mpn_fft_table[p->sqr]))
        break;

      /* compare k to k+1 in the middle of the current k+1 step */
      s.size = size + fft_step_size (k+1) / 2;
      s.r = k;
      tk = tuneup_measure (p->function, NULL, &s);
      if (tk == -1.0)
        abort ();

      s.r = k+1;
      tk1 = tuneup_measure (p->function, NULL, &s);
      if (tk1 == -1.0)
        abort ();

      if (option_trace >= 2)
        printf ("at %ld   size=%ld  k=%d  %.9f   k=%d %.9f\n",
                (long) size, (long) s.size, k, tk, k+1, tk1);

      /* declare the k+1 threshold as soon as it's faster at its midpoint */
      if (tk1 < tk)
        {
          mpn_fft_table[p->sqr][k-FFT_FIRST_K] = s.size;
          printf (" %ld,", (long) s.size);
          if (option_trace >= 2) printf ("\n");
          k++;
        }
    }

  mpn_fft_table[p->sqr][k-FFT_FIRST_K] = 0;
  printf (" 0 }\n");


  size = p->first_size;

  /* Declare an FFT faster than a plain toom3 etc multiplication found as
     soon as one faster measurement obtained.  A multiplication in the
     middle of the FFT step is tested.  */
  for (;;)
    {
      int     modf = (*p->p_modf_threshold == MP_SIZE_T_MAX);
      double  tk, tm;

      /* k=7 should be the first FFT which can beat toom3 on a full
         multiply, so jump to that threshold and save some probing after the
         modf threshold is found.  */
      if (!modf && size < mpn_fft_table[p->sqr][2])
        {
          size = mpn_fft_table[p->sqr][2];
          if (option_trace >= 2)
            printf ("jump to size=%ld\n", (long) size);
        }

      size = fft_next_size (size+1, mpn_fft_best_k (size, p->sqr));
      k = mpn_fft_best_k (size, p->sqr);

      if (size >= p->max_size)
        break;

      s.size = size + fft_step_size (k) / 2;
      s.r = k;
      tk = tuneup_measure (p->function, NULL, &s);
      if (tk == -1.0)
        abort ();

      if (!modf)  s.size /= 2;
      tm = tuneup_measure (p->mul_function, NULL, &s);
      if (tm == -1.0)
        abort ();

      if (option_trace >= 2)
        printf ("at %ld   size=%ld   k=%d  %.9f   size=%ld %s mul %.9f\n",
                (long) size,
                (long) size + fft_step_size (k) / 2, k, tk,
                (long) s.size, modf ? "modf" : "full", tm);

      if (tk < tm)
        {
          if (modf)
            {
              *p->p_modf_threshold = s.size;
              print_define (p->modf_threshold_name, *p->p_modf_threshold);
            }
          else
            {
              *p->p_threshold = s.size;
              print_define (p->threshold_name,      *p->p_threshold);
              break;
            }
        }
    }

}



/* Start karatsuba from 4, since the Cray t90 ieee code is much faster at 2,
   giving wrong results.  */
void
tune_mul (void)
{
  static struct param_t  param;

  param.function = speed_mpn_mul_n;

  param.name = "MUL_KARATSUBA_THRESHOLD";
  param.min_size = MAX (4, MPN_KARA_MUL_N_MINSIZE);
  param.max_size = MUL_KARATSUBA_THRESHOLD_LIMIT-1;
  one (&mul_karatsuba_threshold, &param);

  param.name = "MUL_TOOM3_THRESHOLD";
  param.min_size = MAX (mul_karatsuba_threshold, MPN_TOOM3_MUL_N_MINSIZE);
  param.max_size = MUL_TOOM3_THRESHOLD_LIMIT-1;
  one (&mul_toom3_threshold, &param);

  /* disabled until tuned */
  MUL_FFT_THRESHOLD = MP_SIZE_T_MAX;
}


/* This was written by the tuneup challenged tege.  Kevin, please delete
   this comment when you've reviewed/rewritten this.  :-) */
void
tune_mullow (void)
{
  static struct param_t  param;

  param.function = speed_mpn_mullow_n;

  param.name = "MULLOW_BASECASE_THRESHOLD";
  param.min_size = 3;
  param.min_is_always = 1;
  param.max_size = MULLOW_BASECASE_THRESHOLD_LIMIT-1;
  one (&mullow_basecase_threshold, &param);

  param.min_is_always = 0;	/* ??? */

  param.name = "MULLOW_DC_THRESHOLD";
  param.min_size = mul_karatsuba_threshold;
  param.max_size = 1000;
  one (&mullow_dc_threshold, &param);

  param.name = "MULLOW_MUL_N_THRESHOLD";
  param.min_size = mullow_dc_threshold;
  param.max_size = 2000;
  one (&mullow_mul_n_threshold, &param);

  /* disabled until tuned */
  MUL_FFT_THRESHOLD = MP_SIZE_T_MAX;
}


/* Start the basecase from 3, since 1 is a special case, and if mul_basecase
   is faster only at size==2 then we don't want to bother with extra code
   just for that.  Start karatsuba from 4 same as MUL above.  */

void
tune_sqr (void)
{
  /* disabled until tuned */
  SQR_FFT_THRESHOLD = MP_SIZE_T_MAX;

  if (HAVE_NATIVE_mpn_sqr_basecase)
    {
      print_define_remark ("SQR_BASECASE_THRESHOLD", 0, "always (native)");
      sqr_basecase_threshold = 0;
    }
  else
    {
      static struct param_t  param;
      param.name = "SQR_BASECASE_THRESHOLD";
      param.function = speed_mpn_sqr_n;
      param.min_size = 3;
      param.min_is_always = 1;
      param.max_size = TUNE_SQR_KARATSUBA_MAX;
      param.noprint = 1;
      one (&sqr_basecase_threshold, &param);
    }

  {
    static struct param_t  param;
    param.name = "SQR_KARATSUBA_THRESHOLD";
    param.function = speed_mpn_sqr_n;
    param.min_size = MAX (4, MPN_KARA_SQR_N_MINSIZE);
    param.max_size = TUNE_SQR_KARATSUBA_MAX;
    param.noprint = 1;
    one (&sqr_karatsuba_threshold, &param);

    if (! HAVE_NATIVE_mpn_sqr_basecase
        && sqr_karatsuba_threshold < sqr_basecase_threshold)
      {
        /* Karatsuba becomes faster than mul_basecase before
           sqr_basecase does.  Arrange for the expression
           "BELOW_THRESHOLD (un, SQR_KARATSUBA_THRESHOLD))" which
           selects mpn_sqr_basecase in mpn_sqr_n to be false, by setting
           SQR_KARATSUBA_THRESHOLD to zero, making
           SQR_BASECASE_THRESHOLD the karatsuba threshold.  */

        sqr_basecase_threshold = SQR_KARATSUBA_THRESHOLD;
        SQR_KARATSUBA_THRESHOLD = 0;

        print_define_remark ("SQR_BASECASE_THRESHOLD", sqr_basecase_threshold,
                             "karatsuba");
        print_define_remark ("SQR_KARATSUBA_THRESHOLD",SQR_KARATSUBA_THRESHOLD,
                             "never sqr_basecase");
      }
    else
      {
        if (! HAVE_NATIVE_mpn_sqr_basecase)
          print_define ("SQR_BASECASE_THRESHOLD", sqr_basecase_threshold);
        print_define ("SQR_KARATSUBA_THRESHOLD", SQR_KARATSUBA_THRESHOLD);
      }
  }

  {
    static struct param_t  param;
    param.name = "SQR_TOOM3_THRESHOLD";
    param.function = speed_mpn_sqr_n;
    param.min_size = MAX3 (MPN_TOOM3_SQR_N_MINSIZE,
                           SQR_KARATSUBA_THRESHOLD, SQR_BASECASE_THRESHOLD);
    param.max_size = SQR_TOOM3_THRESHOLD_LIMIT-1;
    one (&sqr_toom3_threshold, &param);
  }
}


void
tune_sb_preinv (void)
{
  static struct param_t  param;

  if (GMP_NAIL_BITS != 0)
    {
      DIV_SB_PREINV_THRESHOLD = MP_SIZE_T_MAX;
      print_define_remark ("DIV_SB_PREINV_THRESHOLD", MP_SIZE_T_MAX,
                           "no preinv with nails");
      return;
    }

  if (UDIV_PREINV_ALWAYS)
    {
      print_define_remark ("DIV_SB_PREINV_THRESHOLD", 0L, "preinv always");
      return;
    }

  param.check_size = 256;
  param.min_size = 3;
  param.min_is_always = 1;
  param.size_extra = 3;
  param.stop_factor = 2.0;
  param.name = "DIV_SB_PREINV_THRESHOLD";
  param.function = speed_mpn_sb_divrem_m3;
  one (&div_sb_preinv_threshold, &param);
}


void
tune_dc (void)
{
  static struct param_t  param;
  param.name = "DIV_DC_THRESHOLD";
  param.function = speed_mpn_dc_tdiv_qr;
  param.step_factor = 0.02;
  one (&div_dc_threshold, &param);
}


/* This is an indirect determination, based on a comparison between redc and
   mpz_mod.  A fudge factor of 1.04 is applied to redc, to represent
   additional overheads it gets in mpz_powm.

   stop_factor is 1.1 to hopefully help cray vector systems, where otherwise
   currently it hits the 1000 limb limit with only a factor of about 1.18
   (threshold should be around 650).  */

void
tune_powm (void)
{
  static struct param_t  param;
  param.name = "POWM_THRESHOLD";
  param.function = speed_redc;
  param.function2 = speed_mpz_mod;
  param.step_factor = 0.03;
  param.stop_factor = 1.1;
  param.function_fudge = 1.04;
  one (&powm_threshold, &param);
}

void
tune_gcd_accel (void)
{
  static struct param_t  param;
  param.name = "GCD_ACCEL_THRESHOLD";
  param.function = speed_mpn_gcd;
  param.min_size = 1;
  one (&gcd_accel_threshold, &param);
}


/* A comparison between the speed of a single limb step and a double limb
   step is made.  On a 32-bit limb the ratio is about 2.2 single steps to
   equal a double step, or on a 64-bit limb about 2.09.  (These were found
   from counting the steps on a 10000 limb gcdext.  */
void
tune_gcdext (void)
{
  static struct param_t  param;
  param.name = "GCDEXT_THRESHOLD";
  param.function = speed_mpn_gcdext_one_single;
  param.function2 = speed_mpn_gcdext_one_double;
  switch (BITS_PER_MP_LIMB) {
  case 32: param.function_fudge = 2.2; break;
  case 64: param.function_fudge = 2.09; break;
  default:
    printf ("Don't know GCDEXT_THERSHOLD factor for BITS_PER_MP_LIMB == %d\n",
            BITS_PER_MP_LIMB);
    abort ();
  }
  param.min_size = 5;
  param.min_is_always = 1;
  param.max_size = 300;
  param.check_size = 300;
  one (&gcdext_threshold, &param);
}


/* size_extra==1 reflects the fact that with high<divisor one division is
   always skipped.  Forcing high<divisor while testing ensures consistency
   while stepping through sizes, ie. that size-1 divides will be done each
   time.

   min_size==2 and min_is_always are used so that if plain division is only
   better at size==1 then don't bother including that code just for that
   case, instead go with preinv always and get a size saving.  */

#define DIV_1_PARAMS                    \
  param.check_size = 256;               \
  param.min_size = 2;                   \
  param.min_is_always = 1;              \
  param.data_high = DATA_HIGH_LT_R;     \
  param.size_extra = 1;                 \
  param.stop_factor = 2.0;


double (*tuned_speed_mpn_divrem_1) _PROTO ((struct speed_params *s));

void
tune_divrem_1 (void)
{
  /* plain version by default */
  tuned_speed_mpn_divrem_1 = speed_mpn_divrem_1;

  /* No support for tuning native assembler code, do that by hand and put
     the results in the .asm file, there's no need for such thresholds to
     appear in gmp-mparam.h.  */
  if (HAVE_NATIVE_mpn_divrem_1)
    return;

  if (GMP_NAIL_BITS != 0)
    {
      print_define_remark ("DIVREM_1_NORM_THRESHOLD", MP_SIZE_T_MAX,
                           "no preinv with nails");
      print_define_remark ("DIVREM_1_UNNORM_THRESHOLD", MP_SIZE_T_MAX,
                           "no preinv with nails");
      return;
    }

  if (UDIV_PREINV_ALWAYS)
    {
      print_define_remark ("DIVREM_1_NORM_THRESHOLD", 0L, "preinv always");
      print_define ("DIVREM_1_UNNORM_THRESHOLD", 0L);
      return;
    }

  tuned_speed_mpn_divrem_1 = speed_mpn_divrem_1_tune;

  /* Tune for the integer part of mpn_divrem_1.  This will very possibly be
     a bit out for the fractional part, but that's too bad, the integer part
     is more important. */
  {
    static struct param_t  param;
    param.name = "DIVREM_1_NORM_THRESHOLD";
    DIV_1_PARAMS;
    s.r = randlimb_norm ();
    param.function = speed_mpn_divrem_1_tune;
    one (&divrem_1_norm_threshold, &param);
  }
  {
    static struct param_t  param;
    param.name = "DIVREM_1_UNNORM_THRESHOLD";
    DIV_1_PARAMS;
    s.r = randlimb_half ();
    param.function = speed_mpn_divrem_1_tune;
    one (&divrem_1_unnorm_threshold, &param);
  }
}


double (*tuned_speed_mpn_mod_1) _PROTO ((struct speed_params *s));

void
tune_mod_1 (void)
{
  /* plain version by default */
  tuned_speed_mpn_mod_1 = speed_mpn_mod_1;

  /* No support for tuning native assembler code, do that by hand and put
     the results in the .asm file, there's no need for such thresholds to
     appear in gmp-mparam.h.  */
  if (HAVE_NATIVE_mpn_mod_1)
    return;

  if (GMP_NAIL_BITS != 0)
    {
      print_define_remark ("MOD_1_NORM_THRESHOLD", MP_SIZE_T_MAX,
                           "no preinv with nails");
      print_define_remark ("MOD_1_UNNORM_THRESHOLD", MP_SIZE_T_MAX,
                           "no preinv with nails");
      return;
    }

  if (UDIV_PREINV_ALWAYS)
    {
      print_define ("MOD_1_NORM_THRESHOLD", 0L);
      print_define ("MOD_1_UNNORM_THRESHOLD", 0L);
      return;
    }

  tuned_speed_mpn_mod_1 = speed_mpn_mod_1_tune;

  {
    static struct param_t  param;
    param.name = "MOD_1_NORM_THRESHOLD";
    DIV_1_PARAMS;
    s.r = randlimb_norm ();
    param.function = speed_mpn_mod_1_tune;
    one (&mod_1_norm_threshold, &param);
  }
  {
    static struct param_t  param;
    param.name = "MOD_1_UNNORM_THRESHOLD";
    DIV_1_PARAMS;
    s.r = randlimb_half ();
    param.function = speed_mpn_mod_1_tune;
    one (&mod_1_unnorm_threshold, &param);
  }
}


/* A non-zero DIVREM_1_UNNORM_THRESHOLD (or DIVREM_1_NORM_THRESHOLD) would
   imply that udiv_qrnnd_preinv is worth using, but it seems most
   straightforward to compare mpn_preinv_divrem_1 and mpn_divrem_1_div
   directly.  */

void
tune_preinv_divrem_1 (void)
{
  static struct param_t  param;
  speed_function_t  divrem_1;
  const char        *divrem_1_name;
  double            t1, t2;

  if (GMP_NAIL_BITS != 0)
    {
      print_define_remark ("USE_PREINV_DIVREM_1", 0, "no preinv with nails");
      return;
    }

  /* Any native version of mpn_preinv_divrem_1 is assumed to exist because
     it's faster than mpn_divrem_1.  */
  if (HAVE_NATIVE_mpn_preinv_divrem_1)
    {
      print_define_remark ("USE_PREINV_DIVREM_1", 1, "native");
      return;
    }

  /* If udiv_qrnnd_preinv is the only division method then of course
     mpn_preinv_divrem_1 should be used.  */
  if (UDIV_PREINV_ALWAYS)
    {
      print_define_remark ("USE_PREINV_DIVREM_1", 1, "preinv always");
      return;
    }

  /* If we've got an assembler version of mpn_divrem_1, then compare against
     that, not the mpn_divrem_1_div generic C.  */
  if (HAVE_NATIVE_mpn_divrem_1)
    {
      divrem_1 = speed_mpn_divrem_1;
      divrem_1_name = "mpn_divrem_1";
    }
  else
    {
      divrem_1 = speed_mpn_divrem_1_div;
      divrem_1_name = "mpn_divrem_1_div";
    }

  param.data_high = DATA_HIGH_LT_R; /* allow skip one division */
  s.size = 200;                     /* generous but not too big */
  /* Divisor, nonzero.  Unnormalized so as to exercise the shift!=0 case,
     since in general that's probably most common, though in fact for a
     64-bit limb mp_bases[10].big_base is normalized.  */
  s.r = urandom() & (GMP_NUMB_MASK >> 4);
  if (s.r == 0) s.r = 123;

  t1 = tuneup_measure (speed_mpn_preinv_divrem_1, &param, &s);
  t2 = tuneup_measure (divrem_1, &param, &s);
  if (t1 == -1.0 || t2 == -1.0)
    {
      printf ("Oops, can't measure mpn_preinv_divrem_1 and %s at %ld\n",
              divrem_1_name, (long) s.size);
      abort ();
    }
  if (option_trace >= 1)
    printf ("size=%ld, mpn_preinv_divrem_1 %.9f, %s %.9f\n",
            (long) s.size, t1, divrem_1_name, t2);

  print_define_remark ("USE_PREINV_DIVREM_1", (mp_size_t) (t1 < t2), NULL);
}


/* A non-zero MOD_1_UNNORM_THRESHOLD (or MOD_1_NORM_THRESHOLD) would imply
   that udiv_qrnnd_preinv is worth using, but it seems most straightforward
   to compare mpn_preinv_mod_1 and mpn_mod_1_div directly.  */

void
tune_preinv_mod_1 (void)
{
  static struct param_t  param;
  speed_function_t  mod_1;
  const char        *mod_1_name;
  double            t1, t2;

  /* Any native version of mpn_preinv_mod_1 is assumed to exist because it's
     faster than mpn_mod_1.  */
  if (HAVE_NATIVE_mpn_preinv_mod_1)
    {
      print_define_remark ("USE_PREINV_MOD_1", 1, "native");
      return;
    }

  if (GMP_NAIL_BITS != 0)
    {
      print_define_remark ("USE_PREINV_MOD_1", 0, "no preinv with nails");
      return;
    }

  /* If udiv_qrnnd_preinv is the only division method then of course
     mpn_preinv_mod_1 should be used.  */
  if (UDIV_PREINV_ALWAYS)
    {
      print_define_remark ("USE_PREINV_MOD_1", 1, "preinv always");
      return;
    }

  /* If we've got an assembler version of mpn_mod_1, then compare against
     that, not the mpn_mod_1_div generic C.  */
  if (HAVE_NATIVE_mpn_mod_1)
    {
      mod_1 = speed_mpn_mod_1;
      mod_1_name = "mpn_mod_1";
    }
  else
    {
      mod_1 = speed_mpn_mod_1_div;
      mod_1_name = "mpn_mod_1_div";
    }

  param.data_high = DATA_HIGH_LT_R; /* let mpn_mod_1 skip one division */
  s.size = 200;                     /* generous but not too big */
  s.r = randlimb_norm();            /* divisor */

  t1 = tuneup_measure (speed_mpn_preinv_mod_1, &param, &s);
  t2 = tuneup_measure (mod_1, &param, &s);
  if (t1 == -1.0 || t2 == -1.0)
    {
      printf ("Oops, can't measure mpn_preinv_mod_1 and %s at %ld\n",
              mod_1_name, (long) s.size);
      abort ();
    }
  if (option_trace >= 1)
    printf ("size=%ld, mpn_preinv_mod_1 %.9f, %s %.9f\n",
            (long) s.size, t1, mod_1_name, t2);

  print_define_remark ("USE_PREINV_MOD_1", (mp_size_t) (t1 < t2), NULL);
}


void
tune_divrem_2 (void)
{
  static struct param_t  param;

  /* No support for tuning native assembler code, do that by hand and put
     the results in the .asm file, and there's no need for such thresholds
     to appear in gmp-mparam.h.  */
  if (HAVE_NATIVE_mpn_divrem_2)
    return;

  if (GMP_NAIL_BITS != 0)
    {
      print_define_remark ("DIVREM_2_THRESHOLD", MP_SIZE_T_MAX,
                           "no preinv with nails");
      return;
    }

  if (UDIV_PREINV_ALWAYS)
    {
      print_define_remark ("DIVREM_2_THRESHOLD", 0L, "preinv always");
      return;
    }

  /* Tune for the integer part of mpn_divrem_2.  This will very possibly be
     a bit out for the fractional part, but that's too bad, the integer part
     is more important.

     min_size must be >=2 since nsize>=2 is required, but is set to 4 to save
     code space if plain division is better only at size==2 or size==3. */
  param.name = "DIVREM_2_THRESHOLD";
  param.check_size = 256;
  param.min_size = 4;
  param.min_is_always = 1;
  param.size_extra = 2;      /* does qsize==nsize-2 divisions */
  param.stop_factor = 2.0;

  s.r = randlimb_norm ();
  param.function = speed_mpn_divrem_2;
  one (&divrem_2_threshold, &param);
}


/* mpn_divexact_1 is vaguely expected to be used on smallish divisors, so
   tune for that.  Its speed can differ on odd or even divisor, so take an
   average threshold for the two.

   mpn_divrem_1 can vary with high<divisor or not, whereas mpn_divexact_1
   might not vary that way, but don't test this since high<divisor isn't
   expected to occur often with small divisors.  */

void
tune_divexact_1 (void)
{
  static struct param_t  param;
  mp_size_t  thresh[2], average;
  int        low, i;

  /* Any native mpn_divexact_1 is assumed to incorporate all the speed of a
     full mpn_divrem_1.  */
  if (HAVE_NATIVE_mpn_divexact_1)
    {
      print_define_remark ("DIVEXACT_1_THRESHOLD", 0, "always (native)");
      return;
    }

  ASSERT_ALWAYS (tuned_speed_mpn_divrem_1 != NULL);

  param.name = "DIVEXACT_1_THRESHOLD";
  param.data_high = DATA_HIGH_GE_R;
  param.check_size = 256;
  param.min_size = 2;
  param.stop_factor = 1.5;
  param.function  = tuned_speed_mpn_divrem_1;
  param.function2 = speed_mpn_divexact_1;
  param.noprint = 1;

  print_define_start (param.name);

  for (low = 0; low <= 1; low++)
    {
      s.r = randlimb_half();
      if (low == 0)
        s.r |= 1;
      else
        s.r &= ~CNST_LIMB(7);

      one (&thresh[low], &param);
      if (option_trace)
        printf ("low=%d thresh %ld\n", low, (long) thresh[low]);

      if (thresh[low] == MP_SIZE_T_MAX)
        {
          average = MP_SIZE_T_MAX;
          goto divexact_1_done;
        }
    }

  if (option_trace)
    {
      printf ("average of:");
      for (i = 0; i < numberof(thresh); i++)
        printf (" %ld", (long) thresh[i]);
      printf ("\n");
    }

  average = 0;
  for (i = 0; i < numberof(thresh); i++)
    average += thresh[i];
  average /= numberof(thresh);

  /* If divexact turns out to be better as early as 3 limbs, then use it
     always, so as to reduce code size and conditional jumps.  */
  if (average <= 3)
    average = 0;

 divexact_1_done:
  print_define_end (param.name, average);
}


/* The generic mpn_modexact_1_odd skips a divide step if high<divisor, the
   same as mpn_mod_1, but this might not be true of an assembler
   implementation.  The threshold used is an average based on data where a
   divide can be skipped and where it can't.

   If modexact turns out to be better as early as 3 limbs, then use it
   always, so as to reduce code size and conditional jumps.  */

void
tune_modexact_1_odd (void)
{
  static struct param_t  param;
  mp_size_t  thresh_lt, thresh_ge, average;

  /* Any native mpn_modexact_1_odd is assumed to incorporate all the speed
     of a full mpn_mod_1.  */
  if (HAVE_NATIVE_mpn_modexact_1_odd)
    {
      print_define_remark ("MODEXACT_1_ODD_THRESHOLD", 0, "always (native)");
      return;
    }

  ASSERT_ALWAYS (tuned_speed_mpn_mod_1 != NULL);

  param.name = "MODEXACT_1_ODD_THRESHOLD";
  param.check_size = 256;
  param.min_size = 2;
  param.stop_factor = 1.5;
  param.function  = tuned_speed_mpn_mod_1;
  param.function2 = speed_mpn_modexact_1c_odd;
  param.noprint = 1;
  s.r = randlimb_half () | 1;

  print_define_start (param.name);

  param.data_high = DATA_HIGH_LT_R;
  one (&thresh_lt, &param);
  if (option_trace)
    printf ("lt thresh %ld\n", (long) thresh_lt);

  average = thresh_lt;
  if (thresh_lt != MP_SIZE_T_MAX)
    {
      param.data_high = DATA_HIGH_GE_R;
      one (&thresh_ge, &param);
      if (option_trace)
        printf ("ge thresh %ld\n", (long) thresh_ge);

      if (thresh_ge != MP_SIZE_T_MAX)
        {
          average = (thresh_ge + thresh_lt) / 2;
          if (thresh_ge <= 3)
            average = 0;
        }
    }

  print_define_end (param.name, average);
}


void
tune_jacobi_base (void)
{
  static struct param_t  param;
  double   t1, t2, t3;
  int      method;

  s.size = BITS_PER_MP_LIMB * 3 / 4;

  t1 = tuneup_measure (speed_mpn_jacobi_base_1, &param, &s);
  if (option_trace >= 1)
    printf ("size=%ld, mpn_jacobi_base_1 %.9f\n", (long) s.size, t1);

  t2 = tuneup_measure (speed_mpn_jacobi_base_2, &param, &s);
  if (option_trace >= 1)
    printf ("size=%ld, mpn_jacobi_base_2 %.9f\n", (long) s.size, t2);

  t3 = tuneup_measure (speed_mpn_jacobi_base_3, &param, &s);
  if (option_trace >= 1)
    printf ("size=%ld, mpn_jacobi_base_3 %.9f\n", (long) s.size, t3);

  if (t1 == -1.0 || t2 == -1.0 || t3 == -1.0)
    {
      printf ("Oops, can't measure all mpn_jacobi_base methods at %ld\n",
              (long) s.size);
      abort ();
    }

  if (t1 < t2 && t1 < t3)
    method = 1;
  else if (t2 < t3)
    method = 2;
  else
    method = 3;

  print_define ("JACOBI_BASE_METHOD", method);
}


void
tune_get_str (void)
{
  /* Tune for decimal, it being most common.  Some rough testing suggests
     other bases are different, but not by very much.  */
  s.r = 10;
  {
    static struct param_t  param;
    GET_STR_PRECOMPUTE_THRESHOLD = 0;
    param.name = "GET_STR_DC_THRESHOLD";
    param.function = speed_mpn_get_str;
    param.min_size = 2;
    param.max_size = GET_STR_THRESHOLD_LIMIT;
    one (&get_str_dc_threshold, &param);
  }
  {
    static struct param_t  param;
    param.name = "GET_STR_PRECOMPUTE_THRESHOLD";
    param.function = speed_mpn_get_str;
    param.min_size = GET_STR_DC_THRESHOLD;
    param.max_size = GET_STR_THRESHOLD_LIMIT;
    one (&get_str_precompute_threshold, &param);
  }
}


void
tune_set_str (void)
{
  static struct param_t  param;

  s.r = 10;  /* decimal */
  param.step_factor = 0.1;
  param.name = "SET_STR_THRESHOLD";
  param.function = speed_mpn_set_str_basecase;
  param.function2 = speed_mpn_set_str_subquad;
  param.min_size = 100;
  param.max_size = 150000;
  one (&set_str_threshold, &param);
}


void
tune_fft_mul (void)
{
  static struct fft_param_t  param;

  if (option_fft_max_size == 0)
    return;

  param.table_name          = "MUL_FFT_TABLE";
  param.threshold_name      = "MUL_FFT_THRESHOLD";
  param.p_threshold         = &mul_fft_threshold;
  param.modf_threshold_name = "MUL_FFT_MODF_THRESHOLD";
  param.p_modf_threshold    = &mul_fft_modf_threshold;
  param.first_size          = MUL_TOOM3_THRESHOLD / 2;
  param.max_size            = option_fft_max_size;
  param.function            = speed_mpn_mul_fft;
  param.mul_function        = speed_mpn_mul_n;
  param.sqr = 0;
  fft (&param);
}


void
tune_fft_sqr (void)
{
  static struct fft_param_t  param;

  if (option_fft_max_size == 0)
    return;

  param.table_name          = "SQR_FFT_TABLE";
  param.threshold_name      = "SQR_FFT_THRESHOLD";
  param.p_threshold         = &sqr_fft_threshold;
  param.modf_threshold_name = "SQR_FFT_MODF_THRESHOLD";
  param.p_modf_threshold    = &sqr_fft_modf_threshold;
  param.first_size          = SQR_TOOM3_THRESHOLD / 2;
  param.max_size            = option_fft_max_size;
  param.function            = speed_mpn_mul_fft_sqr;
  param.mul_function        = speed_mpn_sqr_n;
  param.sqr = 0;
  fft (&param);
}


void
all (void)
{
  time_t  start_time, end_time;
  TMP_DECL;

  TMP_MARK;
  SPEED_TMP_ALLOC_LIMBS (s.xp_block, SPEED_BLOCK_SIZE, 0);
  SPEED_TMP_ALLOC_LIMBS (s.yp_block, SPEED_BLOCK_SIZE, 0);

  mpn_random (s.xp_block, SPEED_BLOCK_SIZE);
  mpn_random (s.yp_block, SPEED_BLOCK_SIZE);

  fprintf (stderr, "Parameters for %s\n", GMP_MPARAM_H_SUGGEST);

  speed_time_init ();
  fprintf (stderr, "Using: %s\n", speed_time_string);

  fprintf (stderr, "speed_precision %d", speed_precision);
  if (speed_unittime == 1.0)
    fprintf (stderr, ", speed_unittime 1 cycle");
  else
    fprintf (stderr, ", speed_unittime %.2e secs", speed_unittime);
  if (speed_cycletime == 1.0 || speed_cycletime == 0.0)
    fprintf (stderr, ", CPU freq unknown\n");
  else
    fprintf (stderr, ", CPU freq %.2f MHz\n", 1e-6/speed_cycletime);

  fprintf (stderr, "DEFAULT_MAX_SIZE %d, fft_max_size %ld\n",
           DEFAULT_MAX_SIZE, (long) option_fft_max_size);
  fprintf (stderr, "\n");

  time (&start_time);
  {
    struct tm  *tp;
    tp = localtime (&start_time);
    printf ("/* Generated by tuneup.c, %d-%02d-%02d, ",
            tp->tm_year+1900, tp->tm_mon+1, tp->tm_mday);

#ifdef __GNUC__
    /* gcc sub-minor version doesn't seem to come through as a define */
    printf ("gcc %d.%d */\n", __GNUC__, __GNUC_MINOR__);
#define PRINTED_COMPILER
#endif
#if defined (__SUNPRO_C)
    printf ("Sun C %d.%d */\n", __SUNPRO_C / 0x100, __SUNPRO_C % 0x100);
#define PRINTED_COMPILER
#endif
#if ! defined (__GNUC__) && defined (__sgi) && defined (_COMPILER_VERSION)
    /* gcc defines __sgi and _COMPILER_VERSION on irix 6, avoid that */
    printf ("MIPSpro C %d.%d.%d */\n",
	    _COMPILER_VERSION / 100,
	    _COMPILER_VERSION / 10 % 10,
	    _COMPILER_VERSION % 10);
#define PRINTED_COMPILER
#endif
#if defined (__DECC) && defined (__DECC_VER)
    printf ("DEC C %d */\n", __DECC_VER);
#define PRINTED_COMPILER
#endif
#if ! defined (PRINTED_COMPILER)
    printf ("system compiler */\n");
#endif
  }
  printf ("\n");

  tune_mul ();
  printf("\n");

  tune_sqr ();
  printf("\n");

  tune_mullow ();
  printf("\n");

  tune_sb_preinv ();
  tune_dc ();
  tune_powm ();
  printf("\n");

  tune_gcd_accel ();
  tune_gcdext ();
  tune_jacobi_base ();
  printf("\n");

  tune_divrem_1 ();
  tune_mod_1 ();
  tune_preinv_divrem_1 ();
  tune_preinv_mod_1 ();
  tune_divrem_2 ();
  tune_divexact_1 ();
  tune_modexact_1_odd ();
  printf("\n");

  tune_get_str ();
  tune_set_str ();
  printf("\n");

  tune_fft_mul ();
  printf("\n");

  tune_fft_sqr ();
  printf ("\n");

  time (&end_time);
  printf ("/* Tuneup completed successfully, took %ld seconds */\n",
          end_time - start_time);

  TMP_FREE;
}


int
main (int argc, char *argv[])
{
  int  opt;

  /* Unbuffered so if output is redirected to a file it isn't lost if the
     program is killed part way through.  */
  setbuf (stdout, NULL);
  setbuf (stderr, NULL);

  while ((opt = getopt(argc, argv, "f:o:p:t")) != EOF)
    {
      switch (opt) {
      case 'f':
        if (optarg[0] == 't')
          option_fft_trace = 2;
        else
          option_fft_max_size = atol (optarg);
        break;
      case 'o':
        speed_option_set (optarg);
        break;
      case 'p':
        speed_precision = atoi (optarg);
        break;
      case 't':
        option_trace++;
        break;
      case '?':
        exit(1);
      }
    }

  all ();
  exit (0);
}
