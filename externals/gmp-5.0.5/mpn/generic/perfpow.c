/* mpn_perfect_power_p -- mpn perfect power detection.

   Contributed to the GNU project by Martin Boij.

Copyright 2009, 2010 Free Software Foundation, Inc.

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

#define SMALL 20
#define MEDIUM 100

/*
   Returns non-zero if {np,nn} == {xp,xn} ^ k.
   Algorithm:
       For s = 1, 2, 4, ..., s_max, compute the s least significant
       limbs of {xp,xn}^k. Stop if they don't match the s least
       significant limbs of {np,nn}.
*/
static int
pow_equals (mp_srcptr np, mp_size_t nn,
	    mp_srcptr xp,mp_size_t xn,
	    mp_limb_t k, mp_bitcnt_t f,
	    mp_ptr tp)
{
  mp_limb_t *tp2;
  mp_bitcnt_t y, z, count;
  mp_size_t i, bn;
  int ans;
  mp_limb_t h, l;
  TMP_DECL;

  ASSERT (nn > 1 || (nn == 1 && np[0] > 1));
  ASSERT (np[nn - 1] > 0);
  ASSERT (xn > 0);

  if (xn == 1 && xp[0] == 1)
    return 0;

  z = 1 + (nn >> 1);
  for (bn = 1; bn < z; bn <<= 1)
    {
      mpn_powlo (tp, xp, &k, 1, bn, tp + bn);
      if (mpn_cmp (tp, np, bn) != 0)
	return 0;
    }

  TMP_MARK;

  /* Final check. Estimate the size of {xp,xn}^k before computing
     the power with full precision.
     Optimization: It might pay off to make a more accurate estimation of
     the logarithm of {xp,xn}, rather than using the index of the MSB.
  */

  count_leading_zeros (count, xp[xn - 1]);
  y = xn * GMP_LIMB_BITS - count - 1;  /* msb_index (xp, xn) */

  umul_ppmm (h, l, k, y);
  h -= l == 0;  l--;	/* two-limb decrement */

  z = f - 1; /* msb_index (np, nn) */
  if (h == 0 && l <= z)
    {
      mp_limb_t size;
      size = l + k;
      ASSERT_ALWAYS (size >= k);

      y = 2 + size / GMP_LIMB_BITS;
      tp2 = TMP_ALLOC_LIMBS (y);

      i = mpn_pow_1 (tp, xp, xn, k, tp2);
      if (i == nn && mpn_cmp (tp, np, nn) == 0)
	ans = 1;
      else
	ans = 0;
    }
  else
    {
      ans = 0;
    }

  TMP_FREE;
  return ans;
}

/*
   Computes rp such that rp^k * yp = 1 (mod 2^b).
   Algorithm:
       Apply Hensel lifting repeatedly, each time
       doubling (approx.) the number of known bits in rp.
*/
static void
binv_root (mp_ptr rp, mp_srcptr yp,
	   mp_limb_t k, mp_size_t bn,
	   mp_bitcnt_t b, mp_ptr tp)
{
  mp_limb_t *tp2 = tp + bn, *tp3 = tp + 2 * bn, di, k2 = k + 1;
  mp_bitcnt_t order[GMP_LIMB_BITS * 2];
  int i, d = 0;

  ASSERT (bn > 0);
  ASSERT (b > 0);
  ASSERT ((k & 1) != 0);

  binvert_limb (di, k);

  rp[0] = 1;
  for (; b != 1; b = (b + 1) >> 1)
    order[d++] = b;

  for (i = d - 1; i >= 0; i--)
    {
      b = order[i];
      bn = 1 + (b - 1) / GMP_LIMB_BITS;

      mpn_mul_1 (tp, rp, bn, k2);

      mpn_powlo (tp2, rp, &k2, 1, bn, tp3);
      mpn_mullo_n (rp, yp, tp2, bn);

      mpn_sub_n (tp2, tp, rp, bn);
      mpn_pi1_bdiv_q_1 (rp, tp2, bn, k, di, 0);
      if ((b % GMP_LIMB_BITS) != 0)
	rp[(b - 1) / GMP_LIMB_BITS] &= (((mp_limb_t) 1) << (b % GMP_LIMB_BITS)) - 1;
    }
  return;
}

/*
   Computes rp such that rp^2 * yp = 1 (mod 2^{b+1}).
   Returns non-zero if such an integer rp exists.
*/
static int
binv_sqroot (mp_ptr rp, mp_srcptr yp,
	     mp_size_t bn, mp_bitcnt_t b,
	     mp_ptr tp)
{
  mp_limb_t k = 3, *tp2 = tp + bn, *tp3 = tp + (bn << 1);
  mp_bitcnt_t order[GMP_LIMB_BITS * 2];
  int i, d = 0;

  ASSERT (bn > 0);
  ASSERT (b > 0);

  rp[0] = 1;
  if (b == 1)
    {
      if ((yp[0] & 3) != 1)
	return 0;
    }
  else
    {
      if ((yp[0] & 7) != 1)
	return 0;

      for (; b != 2; b = (b + 2) >> 1)
	order[d++] = b;

      for (i = d - 1; i >= 0; i--)
	{
	  b = order[i];
	  bn = 1 + b / GMP_LIMB_BITS;

	  mpn_mul_1 (tp, rp, bn, k);

	  mpn_powlo (tp2, rp, &k, 1, bn, tp3);
	  mpn_mullo_n (rp, yp, tp2, bn);

#if HAVE_NATIVE_mpn_rsh1sub_n
	  mpn_rsh1sub_n (rp, tp, rp, bn);
#else
	  mpn_sub_n (tp2, tp, rp, bn);
	  mpn_rshift (rp, tp2, bn, 1);
#endif
	  rp[b / GMP_LIMB_BITS] &= (((mp_limb_t) 1) << (b % GMP_LIMB_BITS)) - 1;
	}
    }
  return 1;
}

/*
   Returns non-zero if {np,nn} is a kth power.
*/
static int
is_kth_power (mp_ptr rp, mp_srcptr np,
	      mp_limb_t k, mp_srcptr yp,
	      mp_size_t nn, mp_bitcnt_t f,
	      mp_ptr tp)
{
  mp_limb_t x, c;
  mp_bitcnt_t b;
  mp_size_t i, rn, xn;

  ASSERT (nn > 0);
  ASSERT (((k & 1) != 0) || (k == 2));
  ASSERT ((np[0] & 1) != 0);

  if (k == 2)
    {
      b = (f + 1) >> 1;
      rn = 1 + b / GMP_LIMB_BITS;
      if (binv_sqroot (rp, yp, rn, b, tp) != 0)
	{
	  xn = rn;
	  MPN_NORMALIZE (rp, xn);
	  if (pow_equals (np, nn, rp, xn, k, f, tp) != 0)
	    return 1;

	  /* Check if (2^b - rp)^2 == np */
	  c = 0;
	  for (i = 0; i < rn; i++)
	    {
	      x = rp[i];
	      rp[i] = -x - c;
	      c |= (x != 0);
	    }
	  rp[rn - 1] &= (((mp_limb_t) 1) << (b % GMP_LIMB_BITS)) - 1;
	  MPN_NORMALIZE (rp, rn);
	  if (pow_equals (np, nn, rp, rn, k, f, tp) != 0)
	    return 1;
	}
    }
  else
    {
      b = 1 + (f - 1) / k;
      rn = 1 + (b - 1) / GMP_LIMB_BITS;
      binv_root (rp, yp, k, rn, b, tp);
      MPN_NORMALIZE (rp, rn);
      if (pow_equals (np, nn, rp, rn, k, f, tp) != 0)
	return 1;
    }
  MPN_ZERO (rp, rn); /* Untrash rp */
  return 0;
}

static int
perfpow (mp_srcptr np, mp_size_t nn,
	 mp_limb_t ub, mp_limb_t g,
	 mp_bitcnt_t f, int neg)
{
  mp_limb_t *yp, *tp, k = 0, *rp1;
  int ans = 0;
  mp_bitcnt_t b;
  gmp_primesieve_t ps;
  TMP_DECL;

  ASSERT (nn > 0);
  ASSERT ((np[0] & 1) != 0);
  ASSERT (ub > 0);

  TMP_MARK;
  gmp_init_primesieve (&ps);
  b = (f + 3) >> 1;

  yp = TMP_ALLOC_LIMBS (nn);
  rp1 = TMP_ALLOC_LIMBS (nn);
  tp = TMP_ALLOC_LIMBS (5 * nn);	/* FIXME */
  MPN_ZERO (rp1, nn);

  mpn_binvert (yp, np, 1 + (b - 1) / GMP_LIMB_BITS, tp);
  if (b % GMP_LIMB_BITS)
    yp[(b - 1) / GMP_LIMB_BITS] &= (((mp_limb_t) 1) << (b % GMP_LIMB_BITS)) - 1;

  if (neg)
    gmp_nextprime (&ps);

  if (g > 0)
    {
      ub = MIN (ub, g + 1);
      while ((k = gmp_nextprime (&ps)) < ub)
	{
	  if ((g % k) == 0)
	    {
	      if (is_kth_power (rp1, np, k, yp, nn, f, tp) != 0)
		{
		  ans = 1;
		  goto ret;
		}
	    }
	}
    }
  else
    {
      while ((k = gmp_nextprime (&ps)) < ub)
	{
	  if (is_kth_power (rp1, np, k, yp, nn, f, tp) != 0)
	    {
	      ans = 1;
	      goto ret;
	    }
	}
    }
 ret:
  TMP_FREE;
  return ans;
}

static const unsigned short nrtrial[] = { 100, 500, 1000 };

/* Table of (log_{p_i} 2) values, where p_i is
   the (nrtrial[i] + 1)'th prime number.
*/
static const double logs[] = { 0.1099457228193620, 0.0847016403115322, 0.0772048195144415 };

int
mpn_perfect_power_p (mp_srcptr np, mp_size_t nn)
{
  mp_size_t ncn, s, pn, xn;
  mp_limb_t *nc, factor, g = 0;
  mp_limb_t exp, *prev, *next, d, l, r, c, *tp, cry;
  mp_bitcnt_t twos = 0, count;
  int ans, where = 0, neg = 0, trial;
  TMP_DECL;

  nc = (mp_ptr) np;

  if (nn < 0)
    {
      neg = 1;
      nn = -nn;
    }

  if (nn == 0 || (nn == 1 && np[0] == 1))
    return 1;

  TMP_MARK;

  ncn = nn;
  twos = mpn_scan1 (np, 0);
  if (twos > 0)
    {
      if (twos == 1)
	{
	  ans = 0;
	  goto ret;
	}
      s = twos / GMP_LIMB_BITS;
      if (s + 1 == nn && POW2_P (np[s]))
	{
	  ans = ! (neg && POW2_P (twos));
	  goto ret;
	}
      count = twos % GMP_LIMB_BITS;
      ncn = nn - s;
      nc = TMP_ALLOC_LIMBS (ncn);
      if (count > 0)
	{
	  mpn_rshift (nc, np + s, ncn, count);
	  ncn -= (nc[ncn - 1] == 0);
	}
      else
	{
	  MPN_COPY (nc, np + s, ncn);
	}
      g = twos;
    }

  if (ncn <= SMALL)
    trial = 0;
  else if (ncn <= MEDIUM)
    trial = 1;
  else
    trial = 2;

  factor = mpn_trialdiv (nc, ncn, nrtrial[trial], &where);

  if (factor != 0)
    {
      if (twos == 0)
	{
	  nc = TMP_ALLOC_LIMBS (ncn);
	  MPN_COPY (nc, np, ncn);
	}

      /* Remove factors found by trialdiv.
	 Optimization: Perhaps better to use
	 the strategy in mpz_remove ().
      */
      prev = TMP_ALLOC_LIMBS (ncn + 2);
      next = TMP_ALLOC_LIMBS (ncn + 2);
      tp = TMP_ALLOC_LIMBS (4 * ncn);

      do
	{
	  binvert_limb (d, factor);
	  prev[0] = d;
	  pn = 1;
	  exp = 1;
	  while (2 * pn - 1 <= ncn)
	    {
	      mpn_sqr (next, prev, pn);
	      xn = 2 * pn;
	      xn -= (next[xn - 1] == 0);

	      if (mpn_divisible_p (nc, ncn, next, xn) == 0)
		break;

	      exp <<= 1;
	      pn = xn;
	      MP_PTR_SWAP (next, prev);
	    }

	  /* Binary search for the exponent */
	  l = exp + 1;
	  r = 2 * exp - 1;
	  while (l <= r)
	    {
	      c = (l + r) >> 1;
	      if (c - exp > 1)
		{
		  xn = mpn_pow_1 (tp, &d, 1, c - exp, next);
		  if (pn + xn - 1 > ncn)
		    {
		      r = c - 1;
		      continue;
		    }
		  mpn_mul (next, prev, pn, tp, xn);
		  xn += pn;
		  xn -= (next[xn - 1] == 0);
		}
	      else
		{
		  cry = mpn_mul_1 (next, prev, pn, d);
		  next[pn] = cry;
		  xn = pn + (cry != 0);
		}

	      if (mpn_divisible_p (nc, ncn, next, xn) == 0)
		{
		  r = c - 1;
		}
	      else
		{
		  exp = c;
		  l = c + 1;
		  MP_PTR_SWAP (next, prev);
		  pn = xn;
		}
	    }

	  if (g == 0)
	    g = exp;
	  else
	    g = mpn_gcd_1 (&g, 1, exp);

	  if (g == 1)
	    {
	      ans = 0;
	      goto ret;
	    }

	  mpn_divexact (next, nc, ncn, prev, pn);
	  ncn = ncn - pn;
	  ncn += next[ncn] != 0;
	  MPN_COPY (nc, next, ncn);

	  if (ncn == 1 && nc[0] == 1)
	    {
	      ans = ! (neg && POW2_P (g));
	      goto ret;
	    }

	  factor = mpn_trialdiv (nc, ncn, nrtrial[trial], &where);
	}
      while (factor != 0);
    }

  count_leading_zeros (count, nc[ncn-1]);
  count = GMP_LIMB_BITS * ncn - count;   /* log (nc) + 1 */
  d = (mp_limb_t) (count * logs[trial] + 1e-9) + 1;
  ans = perfpow (nc, ncn, d, g, count, neg);

 ret:
  TMP_FREE;
  return ans;
}
