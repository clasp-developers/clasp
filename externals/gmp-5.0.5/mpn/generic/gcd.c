/* mpn/gcd.c: mpn_gcd for gcd of two odd integers.

Copyright 1991, 1993, 1994, 1995, 1996, 1997, 1998, 2000, 2001, 2002, 2003,
2004, 2005, 2008 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MP Library.  If not, see http://www.gnu.org/licenses/.  */

#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"

/* Uses the HGCD operation described in

     N. Möller, On Schönhage's algorithm and subquadratic integer gcd
     computation, Math. Comp. 77 (2008), 589-607.

  to reduce inputs until they are of size below GCD_DC_THRESHOLD, and
  then uses Lehmer's algorithm.
*/

/* Some reasonable choices are n / 2 (same as in hgcd), and p = (n +
 * 2)/3, which gives a balanced multiplication in
 * mpn_hgcd_matrix_adjust. However, p = 2 n/3 gives slightly better
 * performance. The matrix-vector multiplication is then
 * 4:1-unbalanced, with matrix elements of size n/6, and vector
 * elements of size p = 2n/3. */

/* From analysis of the theoretical running time, it appears that when
 * multiplication takes time O(n^alpha), p should be chosen so that
 * the ratio of the time for the mpn_hgcd call, and the time for the
 * multiplication in mpn_hgcd_matrix_adjust, is roughly 1/(alpha -
 * 1). */
#ifdef TUNE_GCD_P
#define P_TABLE_SIZE 10000
mp_size_t p_table[P_TABLE_SIZE];
#define CHOOSE_P(n) ( (n) < P_TABLE_SIZE ? p_table[n] : 2*(n)/3)
#else
#define CHOOSE_P(n) (2*(n) / 3)
#endif

mp_size_t
mpn_gcd (mp_ptr gp, mp_ptr up, mp_size_t usize, mp_ptr vp, mp_size_t n)
{
  mp_size_t talloc;
  mp_size_t scratch;
  mp_size_t matrix_scratch;

  mp_size_t gn;
  mp_ptr tp;
  TMP_DECL;

  /* FIXME: Check for small sizes first, before setting up temporary
     storage etc. */
  talloc = MPN_GCD_LEHMER_N_ITCH(n);

  /* For initial division */
  scratch = usize - n + 1;
  if (scratch > talloc)
    talloc = scratch;

#if TUNE_GCD_P
  if (CHOOSE_P (n) > 0)
#else
  if (ABOVE_THRESHOLD (n, GCD_DC_THRESHOLD))
#endif
    {
      mp_size_t hgcd_scratch;
      mp_size_t update_scratch;
      mp_size_t p = CHOOSE_P (n);
      mp_size_t scratch;
#if TUNE_GCD_P
      /* Worst case, since we don't guarantee that n - CHOOSE_P(n)
	 is increasing */
      matrix_scratch = MPN_HGCD_MATRIX_INIT_ITCH (n);
      hgcd_scratch = mpn_hgcd_itch (n);
      update_scratch = 2*(n - 1);
#else
      matrix_scratch = MPN_HGCD_MATRIX_INIT_ITCH (n - p);
      hgcd_scratch = mpn_hgcd_itch (n - p);
      update_scratch = p + n - 1;
#endif
      scratch = matrix_scratch + MAX(hgcd_scratch, update_scratch);
      if (scratch > talloc)
	talloc = scratch;
    }

  TMP_MARK;
  tp = TMP_ALLOC_LIMBS(talloc);

  if (usize > n)
    {
      mpn_tdiv_qr (tp, up, 0, up, usize, vp, n);

      if (mpn_zero_p (up, n))
	{
	  MPN_COPY (gp, vp, n);
	  TMP_FREE;
	  return n;
	}
    }

#if TUNE_GCD_P
  while (CHOOSE_P (n) > 0)
#else
  while (ABOVE_THRESHOLD (n, GCD_DC_THRESHOLD))
#endif
    {
      struct hgcd_matrix M;
      mp_size_t p = CHOOSE_P (n);
      mp_size_t matrix_scratch = MPN_HGCD_MATRIX_INIT_ITCH (n - p);
      mp_size_t nn;
      mpn_hgcd_matrix_init (&M, n - p, tp);
      nn = mpn_hgcd (up + p, vp + p, n - p, &M, tp + matrix_scratch);
      if (nn > 0)
	{
	  ASSERT (M.n <= (n - p - 1)/2);
	  ASSERT (M.n + p <= (p + n - 1) / 2);
	  /* Temporary storage 2 (p + M->n) <= p + n - 1. */
	  n = mpn_hgcd_matrix_adjust (&M, p + nn, up, vp, p, tp + matrix_scratch);
	}
      else
	{
	  /* Temporary storage n */
	  n = mpn_gcd_subdiv_step (gp, &gn, up, vp, n, tp);
	  if (n == 0)
	    {
	      TMP_FREE;
	      return gn;
	    }
	}
    }

  gn = mpn_gcd_lehmer_n (gp, up, vp, n, tp);
  TMP_FREE;
  return gn;
}

#ifdef TUNE_GCD_P
#include <stdio.h>
#include <string.h>
#include <time.h>
#include "speed.h"

static int
compare_double(const void *ap, const void *bp)
{
  double a = * (const double *) ap;
  double b = * (const double *) bp;

  if (a < b)
    return -1;
  else if (a > b)
    return 1;
  else
    return 0;
}

static double
median (double *v, size_t n)
{
  qsort(v, n, sizeof(*v), compare_double);

  return v[n/2];
}

#define TIME(res, code) do {				\
  double time_measurement[5];				\
  unsigned time_i;					\
							\
  for (time_i = 0; time_i < 5; time_i++)		\
    {							\
      speed_starttime();				\
      code;						\
      time_measurement[time_i] = speed_endtime();	\
    }							\
  res = median(time_measurement, 5);			\
} while (0)

int
main(int argc, char *argv)
{
  gmp_randstate_t rands;
  mp_size_t n;
  mp_ptr ap;
  mp_ptr bp;
  mp_ptr up;
  mp_ptr vp;
  mp_ptr gp;
  mp_ptr tp;
  TMP_DECL;

  /* Unbuffered so if output is redirected to a file it isn't lost if the
     program is killed part way through.  */
  setbuf (stdout, NULL);
  setbuf (stderr, NULL);

  gmp_randinit_default (rands);

  TMP_MARK;

  ap = TMP_ALLOC_LIMBS (P_TABLE_SIZE);
  bp = TMP_ALLOC_LIMBS (P_TABLE_SIZE);
  up = TMP_ALLOC_LIMBS (P_TABLE_SIZE);
  vp = TMP_ALLOC_LIMBS (P_TABLE_SIZE);
  gp = TMP_ALLOC_LIMBS (P_TABLE_SIZE);
  tp = TMP_ALLOC_LIMBS (MPN_GCD_LEHMER_N_ITCH (P_TABLE_SIZE));

  mpn_random (ap, P_TABLE_SIZE);
  mpn_random (bp, P_TABLE_SIZE);

  memset (p_table, 0, sizeof(p_table));

  for (n = 100; n++; n < P_TABLE_SIZE)
    {
      mp_size_t p;
      mp_size_t best_p;
      double best_time;
      double lehmer_time;

      if (ap[n-1] == 0)
	ap[n-1] = 1;

      if (bp[n-1] == 0)
	bp[n-1] = 1;

      p_table[n] = 0;
      TIME(lehmer_time, {
	  MPN_COPY (up, ap, n);
	  MPN_COPY (vp, bp, n);
	  mpn_gcd_lehmer_n (gp, up, vp, n, tp);
	});

      best_time = lehmer_time;
      best_p = 0;

      for (p = n * 0.48; p < n * 0.77; p++)
	{
	  double t;

	  p_table[n] = p;

	  TIME(t, {
	      MPN_COPY (up, ap, n);
	      MPN_COPY (vp, bp, n);
	      mpn_gcd (gp, up, n, vp, n);
	    });

	  if (t < best_time)
	    {
	      best_time = t;
	      best_p = p;
	    }
	}
      printf("%6d %6d %5.3g", n, best_p, (double) best_p / n);
      if (best_p > 0)
	{
	  double speedup = 100 * (lehmer_time - best_time) / lehmer_time;
	  printf(" %5.3g%%", speedup);
	  if (speedup < 1.0)
	    {
	      printf(" (ignored)");
	      best_p = 0;
	    }
	}
      printf("\n");

      p_table[n] = best_p;
    }
  TMP_FREE;
  gmp_randclear(rands);
  return 0;
}
#endif /* TUNE_GCD_P */
