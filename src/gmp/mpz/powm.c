/* mpz_powm(res,base,exp,mod) -- Set RES to (base**exp) mod MOD.

Copyright 1991, 1993, 1994, 1996, 1997, 2000, 2001, 2002, 2005 Free Software
Foundation, Inc.  Contributed by Paul Zimmermann.

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


#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"
#ifdef BERKELEY_MP
#include "mp.h"
#endif


/* Set cp[] <- tp[]/R^n mod mp[].  Clobber tp[].
   mp[] is n limbs; tp[] is 2n limbs.  */
#if ! WANT_REDC_GLOBAL
static
#endif
void
redc (mp_ptr cp, mp_srcptr mp, mp_size_t n, mp_limb_t Nprim, mp_ptr tp)
{
  mp_limb_t cy;
  mp_limb_t q;
  mp_size_t j;

  ASSERT_MPN (tp, 2*n);

  for (j = 0; j < n; j++)
    {
      q = (tp[0] * Nprim) & GMP_NUMB_MASK;
      tp[0] = mpn_addmul_1 (tp, mp, n, q);
      tp++;
    }
  cy = mpn_add_n (cp, tp, tp - n, n);
  if (cy != 0)
    mpn_sub_n (cp, cp, mp, n);
}

/* Compute t = a mod m, a is defined by (ap,an), m is defined by (mp,mn), and
   t is defined by (tp,mn).  */
static void
reduce (mp_ptr tp, mp_srcptr ap, mp_size_t an, mp_srcptr mp, mp_size_t mn)
{
  mp_ptr qp;
  TMP_DECL;

  TMP_MARK;
  qp = TMP_ALLOC_LIMBS (an - mn + 1);

  mpn_tdiv_qr (qp, tp, 0L, ap, an, mp, mn);

  TMP_FREE;
}

#if REDUCE_EXPONENT
/* Return the group order of the ring mod m.  */
static mp_limb_t
phi (mp_limb_t t)
{
  mp_limb_t d, m, go;

  go = 1;

  if (t % 2 == 0)
    {
      t = t / 2;
      while (t % 2 == 0)
	{
	  go *= 2;
	  t = t / 2;
	}
    }
  for (d = 3;; d += 2)
    {
      m = d - 1;
      for (;;)
	{
	  unsigned long int q = t / d;
	  if (q < d)
	    {
	      if (t <= 1)
		return go;
	      if (t == d)
		return go * m;
	      return go * (t - 1);
	    }
	  if (t != q * d)
	    break;
	  go *= m;
	  m = d;
	  t = q;
	}
    }
}
#endif

/* average number of calls to redc for an exponent of n bits
   with the sliding window algorithm of base 2^k: the optimal is
   obtained for the value of k which minimizes 2^(k-1)+n/(k+1):

   n\k    4     5     6     7     8
   128    156*  159   171   200   261
   256    309   307*  316   343   403
   512    617   607*  610   632   688
   1024   1231  1204  1195* 1207  1256
   2048   2461  2399  2366  2360* 2396
   4096   4918  4787  4707  4665* 4670
*/


/* Use REDC instead of usual reduction for sizes < POWM_THRESHOLD.  In REDC
   each modular multiplication costs about 2*n^2 limbs operations, whereas
   using usual reduction it costs 3*K(n), where K(n) is the cost of a
   multiplication using Karatsuba, and a division is assumed to cost 2*K(n),
   for example using Burnikel-Ziegler's algorithm. This gives a theoretical
   threshold of a*SQR_KARATSUBA_THRESHOLD, with a=(3/2)^(1/(2-ln(3)/ln(2))) ~
   2.66.  */
/* For now, also disable REDC when MOD is even, as the inverse can't handle
   that.  At some point, we might want to make the code faster for that case,
   perhaps using CRR.  */

#ifndef POWM_THRESHOLD
#define POWM_THRESHOLD  ((8 * SQR_KARATSUBA_THRESHOLD) / 3)
#endif

#define HANDLE_NEGATIVE_EXPONENT 1
#undef REDUCE_EXPONENT

void
#ifndef BERKELEY_MP
mpz_powm (mpz_ptr r, mpz_srcptr b, mpz_srcptr e, mpz_srcptr m)
#else /* BERKELEY_MP */
pow (mpz_srcptr b, mpz_srcptr e, mpz_srcptr m, mpz_ptr r)
#endif /* BERKELEY_MP */
{
  mp_ptr xp, tp, qp, gp, this_gp;
  mp_srcptr bp, ep, mp;
  mp_size_t bn, es, en, mn, xn;
  mp_limb_t invm, c;
  unsigned long int enb;
  mp_size_t i, K, j, l, k;
  int m_zero_cnt, e_zero_cnt;
  int sh;
  int use_redc;
#if HANDLE_NEGATIVE_EXPONENT
  mpz_t new_b;
#endif
#if REDUCE_EXPONENT
  mpz_t new_e;
#endif
  TMP_DECL;

  mp = PTR(m);
  mn = ABSIZ (m);
  if (mn == 0)
    DIVIDE_BY_ZERO;

  TMP_MARK;

  es = SIZ (e);
  if (es <= 0)
    {
      if (es == 0)
	{
	  /* Exponent is zero, result is 1 mod m, i.e., 1 or 0 depending on if
	     m equals 1.  */
	  SIZ(r) = (mn == 1 && mp[0] == 1) ? 0 : 1;
	  PTR(r)[0] = 1;
	  TMP_FREE;	/* we haven't really allocated anything here */
	  return;
	}
#if HANDLE_NEGATIVE_EXPONENT
      MPZ_TMP_INIT (new_b, mn + 1);

      if (! mpz_invert (new_b, b, m))
	DIVIDE_BY_ZERO;
      b = new_b;
      es = -es;
#else
      DIVIDE_BY_ZERO;
#endif
    }
  en = es;

#if REDUCE_EXPONENT
  /* Reduce exponent by dividing it by phi(m) when m small.  */
  if (mn == 1 && mp[0] < 0x7fffffffL && en * GMP_NUMB_BITS > 150)
    {
      MPZ_TMP_INIT (new_e, 2);
      mpz_mod_ui (new_e, e, phi (mp[0]));
      e = new_e;
    }
#endif

  use_redc = mn < POWM_THRESHOLD && mp[0] % 2 != 0;
  if (use_redc)
    {
      /* invm = -1/m mod 2^BITS_PER_MP_LIMB, must have m odd */
      modlimb_invert (invm, mp[0]);
      invm = -invm;
    }
  else
    {
      /* Normalize m (i.e. make its most significant bit set) as required by
	 division functions below.  */
      count_leading_zeros (m_zero_cnt, mp[mn - 1]);
      m_zero_cnt -= GMP_NAIL_BITS;
      if (m_zero_cnt != 0)
	{
	  mp_ptr new_mp;
	  new_mp = TMP_ALLOC_LIMBS (mn);
	  mpn_lshift (new_mp, mp, mn, m_zero_cnt);
	  mp = new_mp;
	}
    }

  /* Determine optimal value of k, the number of exponent bits we look at
     at a time.  */
  count_leading_zeros (e_zero_cnt, PTR(e)[en - 1]);
  e_zero_cnt -= GMP_NAIL_BITS;
  enb = en * GMP_NUMB_BITS - e_zero_cnt; /* number of bits of exponent */
  k = 1;
  K = 2;
  while (2 * enb > K * (2 + k * (3 + k)))
    {
      k++;
      K *= 2;
      if (k == 10)			/* cap allocation */
	break;
    }

  tp = TMP_ALLOC_LIMBS (2 * mn);
  qp = TMP_ALLOC_LIMBS (mn + 1);

  gp = __GMP_ALLOCATE_FUNC_LIMBS (K / 2 * mn);

  /* Compute x*R^n where R=2^BITS_PER_MP_LIMB.  */
  bn = ABSIZ (b);
  bp = PTR(b);
  /* Handle |b| >= m by computing b mod m.  FIXME: It is not strictly necessary
     for speed or correctness to do this when b and m have the same number of
     limbs, perhaps remove mpn_cmp call.  */
  if (bn > mn || (bn == mn && mpn_cmp (bp, mp, mn) >= 0))
    {
      /* Reduce possibly huge base while moving it to gp[0].  Use a function
	 call to reduce, since we don't want the quotient allocation to
	 live until function return.  */
      if (use_redc)
	{
	  reduce (tp + mn, bp, bn, mp, mn);	/* b mod m */
	  MPN_ZERO (tp, mn);
	  mpn_tdiv_qr (qp, gp, 0L, tp, 2 * mn, mp, mn); /* unnormnalized! */
	}
      else
	{
	  reduce (gp, bp, bn, mp, mn);
	}
    }
  else
    {
      /* |b| < m.  We pad out operands to become mn limbs,  which simplifies
	 the rest of the function, but slows things down when the |b| << m.  */
      if (use_redc)
	{
	  MPN_ZERO (tp, mn);
	  MPN_COPY (tp + mn, bp, bn);
	  MPN_ZERO (tp + mn + bn, mn - bn);
	  mpn_tdiv_qr (qp, gp, 0L, tp, 2 * mn, mp, mn);
	}
      else
	{
	  MPN_COPY (gp, bp, bn);
	  MPN_ZERO (gp + bn, mn - bn);
	}
    }

  /* Compute xx^i for odd g < 2^i.  */

  xp = TMP_ALLOC_LIMBS (mn);
  mpn_sqr_n (tp, gp, mn);
  if (use_redc)
    redc (xp, mp, mn, invm, tp);		/* xx = x^2*R^n */
  else
    mpn_tdiv_qr (qp, xp, 0L, tp, 2 * mn, mp, mn);
  this_gp = gp;
  for (i = 1; i < K / 2; i++)
    {
      mpn_mul_n (tp, this_gp, xp, mn);
      this_gp += mn;
      if (use_redc)
	redc (this_gp, mp, mn, invm, tp);	/* g[i] = x^(2i+1)*R^n */
      else
	mpn_tdiv_qr (qp, this_gp, 0L, tp, 2 * mn, mp, mn);
    }

  /* Start the real stuff.  */
  ep = PTR (e);
  i = en - 1;				/* current index */
  c = ep[i];				/* current limb */
  sh = GMP_NUMB_BITS - e_zero_cnt;	/* significant bits in ep[i] */
  sh -= k;				/* index of lower bit of ep[i] to take into account */
  if (sh < 0)
    {					/* k-sh extra bits are needed */
      if (i > 0)
	{
	  i--;
	  c <<= (-sh);
	  sh += GMP_NUMB_BITS;
	  c |= ep[i] >> sh;
	}
    }
  else
    c >>= sh;

  for (j = 0; c % 2 == 0; j++)
    c >>= 1;

  MPN_COPY (xp, gp + mn * (c >> 1), mn);
  while (--j >= 0)
    {
      mpn_sqr_n (tp, xp, mn);
      if (use_redc)
	redc (xp, mp, mn, invm, tp);
      else
	mpn_tdiv_qr (qp, xp, 0L, tp, 2 * mn, mp, mn);
    }

  while (i > 0 || sh > 0)
    {
      c = ep[i];
      l = k;				/* number of bits treated */
      sh -= l;
      if (sh < 0)
	{
	  if (i > 0)
	    {
	      i--;
	      c <<= (-sh);
	      sh += GMP_NUMB_BITS;
	      c |= ep[i] >> sh;
	    }
	  else
	    {
	      l += sh;			/* last chunk of bits from e; l < k */
	    }
	}
      else
	c >>= sh;
      c &= ((mp_limb_t) 1 << l) - 1;

      /* This while loop implements the sliding window improvement--loop while
	 the most significant bit of c is zero, squaring xx as we go. */
      while ((c >> (l - 1)) == 0 && (i > 0 || sh > 0))
	{
	  mpn_sqr_n (tp, xp, mn);
	  if (use_redc)
	    redc (xp, mp, mn, invm, tp);
	  else
	    mpn_tdiv_qr (qp, xp, 0L, tp, 2 * mn, mp, mn);
	  if (sh != 0)
	    {
	      sh--;
	      c = (c << 1) + ((ep[i] >> sh) & 1);
	    }
	  else
	    {
	      i--;
	      sh = GMP_NUMB_BITS - 1;
	      c = (c << 1) + (ep[i] >> sh);
	    }
	}

      /* Replace xx by xx^(2^l)*x^c.  */
      if (c != 0)
	{
	  for (j = 0; c % 2 == 0; j++)
	    c >>= 1;

	  /* c0 = c * 2^j, i.e. xx^(2^l)*x^c = (A^(2^(l - j))*c)^(2^j) */
	  l -= j;
	  while (--l >= 0)
	    {
	      mpn_sqr_n (tp, xp, mn);
	      if (use_redc)
		redc (xp, mp, mn, invm, tp);
	      else
		mpn_tdiv_qr (qp, xp, 0L, tp, 2 * mn, mp, mn);
	    }
	  mpn_mul_n (tp, xp, gp + mn * (c >> 1), mn);
	  if (use_redc)
	    redc (xp, mp, mn, invm, tp);
	  else
	    mpn_tdiv_qr (qp, xp, 0L, tp, 2 * mn, mp, mn);
	}
      else
	j = l;				/* case c=0 */
      while (--j >= 0)
	{
	  mpn_sqr_n (tp, xp, mn);
	  if (use_redc)
	    redc (xp, mp, mn, invm, tp);
	  else
	    mpn_tdiv_qr (qp, xp, 0L, tp, 2 * mn, mp, mn);
	}
    }

  if (use_redc)
    {
      /* Convert back xx to xx/R^n.  */
      MPN_COPY (tp, xp, mn);
      MPN_ZERO (tp + mn, mn);
      redc (xp, mp, mn, invm, tp);
      if (mpn_cmp (xp, mp, mn) >= 0)
	mpn_sub_n (xp, xp, mp, mn);
    }
  else
    {
      if (m_zero_cnt != 0)
	{
	  mp_limb_t cy;
	  cy = mpn_lshift (tp, xp, mn, m_zero_cnt);
	  tp[mn] = cy;
	  mpn_tdiv_qr (qp, xp, 0L, tp, mn + (cy != 0), mp, mn);
	  mpn_rshift (xp, xp, mn, m_zero_cnt);
	}
    }
  xn = mn;
  MPN_NORMALIZE (xp, xn);

  if ((ep[0] & 1) && SIZ(b) < 0 && xn != 0)
    {
      mp = PTR(m);			/* want original, unnormalized m */
      mpn_sub (xp, mp, mn, xp, xn);
      xn = mn;
      MPN_NORMALIZE (xp, xn);
    }
  MPZ_REALLOC (r, xn);
  SIZ (r) = xn;
  MPN_COPY (PTR(r), xp, xn);

  __GMP_FREE_FUNC_LIMBS (gp, K / 2 * mn);
  TMP_FREE;
}
