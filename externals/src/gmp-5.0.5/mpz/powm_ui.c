/* mpz_powm_ui(res,base,exp,mod) -- Set RES to (base**exp) mod MOD.

Copyright 1991, 1993, 1994, 1996, 1997, 2000, 2001, 2002, 2005 Free Software
Foundation, Inc.

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

void
mpz_powm_ui (mpz_ptr r, mpz_srcptr b, unsigned long int el, mpz_srcptr m)
{
  mp_ptr xp, tp, qp, mp, bp;
  mp_size_t xn, tn, mn, bn;
  int m_zero_cnt;
  int c;
  mp_limb_t e;
  TMP_DECL;

  mp = PTR(m);
  mn = ABSIZ(m);
  if (mn == 0)
    DIVIDE_BY_ZERO;

  if (el == 0)
    {
      /* Exponent is zero, result is 1 mod MOD, i.e., 1 or 0
	 depending on if MOD equals 1.  */
      SIZ(r) = (mn == 1 && mp[0] == 1) ? 0 : 1;
      PTR(r)[0] = 1;
      return;
    }

  TMP_MARK;

  /* Normalize m (i.e. make its most significant bit set) as required by
     division functions below.  */
  count_leading_zeros (m_zero_cnt, mp[mn - 1]);
  m_zero_cnt -= GMP_NAIL_BITS;
  if (m_zero_cnt != 0)
    {
      mp_ptr new_mp = TMP_ALLOC_LIMBS (mn);
      mpn_lshift (new_mp, mp, mn, m_zero_cnt);
      mp = new_mp;
    }

  bn = ABSIZ(b);
  bp = PTR(b);
  if (bn > mn)
    {
      /* Reduce possibly huge base.  Use a function call to reduce, since we
	 don't want the quotient allocation to live until function return.  */
      mp_ptr new_bp = TMP_ALLOC_LIMBS (mn);
      reduce (new_bp, bp, bn, mp, mn);
      bp = new_bp;
      bn = mn;
      /* Canonicalize the base, since we are potentially going to multiply with
	 it quite a few times.  */
      MPN_NORMALIZE (bp, bn);
    }

  if (bn == 0)
    {
      SIZ(r) = 0;
      TMP_FREE;
      return;
    }

  tp = TMP_ALLOC_LIMBS (2 * mn + 1);
  xp = TMP_ALLOC_LIMBS (mn);

  qp = TMP_ALLOC_LIMBS (mn + 1);

  MPN_COPY (xp, bp, bn);
  xn = bn;

  e = el;
  count_leading_zeros (c, e);
  e = (e << c) << 1;		/* shift the exp bits to the left, lose msb */
  c = GMP_LIMB_BITS - 1 - c;

  /* Main loop. */

  /* If m is already normalized (high bit of high limb set), and b is the
     same size, but a bigger value, and e==1, then there's no modular
     reductions done and we can end up with a result out of range at the
     end. */
  if (c == 0)
    {
      if (xn == mn && mpn_cmp (xp, mp, mn) >= 0)
        mpn_sub_n (xp, xp, mp, mn);
      goto finishup;
    }

  while (c != 0)
    {
      mpn_sqr (tp, xp, xn);
      tn = 2 * xn; tn -= tp[tn - 1] == 0;
      if (tn < mn)
	{
	  MPN_COPY (xp, tp, tn);
	  xn = tn;
	}
      else
	{
	  mpn_tdiv_qr (qp, xp, 0L, tp, tn, mp, mn);
	  xn = mn;
	}

      if ((mp_limb_signed_t) e < 0)
	{
	  mpn_mul (tp, xp, xn, bp, bn);
	  tn = xn + bn; tn -= tp[tn - 1] == 0;
	  if (tn < mn)
	    {
	      MPN_COPY (xp, tp, tn);
	      xn = tn;
	    }
	  else
	    {
	      mpn_tdiv_qr (qp, xp, 0L, tp, tn, mp, mn);
	      xn = mn;
	    }
	}
      e <<= 1;
      c--;
    }

 finishup:
  /* We shifted m left m_zero_cnt steps.  Adjust the result by reducing
     it with the original MOD.  */
  if (m_zero_cnt != 0)
    {
      mp_limb_t cy;
      cy = mpn_lshift (tp, xp, xn, m_zero_cnt);
      tp[xn] = cy; xn += cy != 0;

      if (xn < mn)
	{
	  MPN_COPY (xp, tp, xn);
	}
      else
	{
	  mpn_tdiv_qr (qp, xp, 0L, tp, xn, mp, mn);
	  xn = mn;
	}
      mpn_rshift (xp, xp, xn, m_zero_cnt);
    }
  MPN_NORMALIZE (xp, xn);

  if ((el & 1) != 0 && SIZ(b) < 0 && xn != 0)
    {
      mp = PTR(m);			/* want original, unnormalized m */
      mpn_sub (xp, mp, mn, xp, xn);
      xn = mn;
      MPN_NORMALIZE (xp, xn);
    }
  MPZ_REALLOC (r, xn);
  SIZ (r) = xn;
  MPN_COPY (PTR(r), xp, xn);

  TMP_FREE;
}
