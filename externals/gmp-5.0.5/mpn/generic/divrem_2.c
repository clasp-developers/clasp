/* mpn_divrem_2 -- Divide natural numbers, producing both remainder and
   quotient.  The divisor is two limbs.

   THIS FILE CONTAINS INTERNAL FUNCTIONS WITH MUTABLE INTERFACES.  IT IS
   ONLY SAFE TO REACH THEM THROUGH DOCUMENTED INTERFACES.  IN FACT, IT IS
   ALMOST GUARANTEED THAT THEY'LL CHANGE OR DISAPPEAR IN A FUTURE GNU MP
   RELEASE.


Copyright 1993, 1994, 1995, 1996, 1999, 2000, 2001, 2002 Free Software
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


/* The size where udiv_qrnnd_preinv should be used rather than udiv_qrnnd,
   meaning the quotient size where that should happen, the quotient size
   being how many udiv divisions will be done.

   The default is to use preinv always, CPUs where this doesn't suit have
   tuned thresholds.  Note in particular that preinv should certainly be
   used if that's the only division available (USE_PREINV_ALWAYS).  */

#ifndef DIVREM_2_THRESHOLD
#define DIVREM_2_THRESHOLD  0
#endif


/* Divide num (NP/NSIZE) by den (DP/2) and write
   the NSIZE-2 least significant quotient limbs at QP
   and the 2 long remainder at NP.  If QEXTRA_LIMBS is
   non-zero, generate that many fraction bits and append them after the
   other quotient limbs.
   Return the most significant limb of the quotient, this is always 0 or 1.

   Preconditions:
   0. NSIZE >= 2.
   1. The most significant bit of the divisor must be set.
   2. QP must either not overlap with the input operands at all, or
      QP + 2 >= NP must hold true.  (This means that it's
      possible to put the quotient in the high part of NUM, right after the
      remainder in NUM.
   3. NSIZE >= 2, even if QEXTRA_LIMBS is non-zero.  */

mp_limb_t
mpn_divrem_2 (mp_ptr qp, mp_size_t qxn,
	      mp_ptr np, mp_size_t nn,
	      mp_srcptr dp)
{
  mp_limb_t most_significant_q_limb = 0;
  mp_size_t i;
  mp_limb_t n1, n0, n2;
  mp_limb_t d1, d0;
  mp_limb_t d1inv;
  int use_preinv;

  ASSERT (nn >= 2);
  ASSERT (qxn >= 0);
  ASSERT (dp[1] & GMP_NUMB_HIGHBIT);
  ASSERT (! MPN_OVERLAP_P (qp, nn-2+qxn, np, nn) || qp+2 >= np);
  ASSERT_MPN (np, nn);
  ASSERT_MPN (dp, 2);

  np += nn - 2;
  d1 = dp[1];
  d0 = dp[0];
  n1 = np[1];
  n0 = np[0];

  if (n1 >= d1 && (n1 > d1 || n0 >= d0))
    {
#if GMP_NAIL_BITS == 0
      sub_ddmmss (n1, n0, n1, n0, d1, d0);
#else
      n0 = n0 - d0;
      n1 = n1 - d1 - (n0 >> GMP_LIMB_BITS - 1);
      n0 &= GMP_NUMB_MASK;
#endif
      most_significant_q_limb = 1;
    }

  use_preinv = ABOVE_THRESHOLD (qxn + nn - 2, DIVREM_2_THRESHOLD);
  if (use_preinv)
    invert_limb (d1inv, d1);

  for (i = qxn + nn - 2 - 1; i >= 0; i--)
    {
      mp_limb_t q;
      mp_limb_t r;

      if (i >= qxn)
	np--;
      else
	np[0] = 0;

      if (n1 == d1)
	{
	  /* Q should be either 111..111 or 111..110.  Need special handling
	     of this rare case as normal division would give overflow.  */
	  q = GMP_NUMB_MASK;

	  r = (n0 + d1) & GMP_NUMB_MASK;
	  if (r < d1)	/* Carry in the addition? */
	    {
#if GMP_NAIL_BITS == 0
	      add_ssaaaa (n1, n0, r - d0, np[0], 0, d0);
#else
	      n0 = np[0] + d0;
	      n1 = (r - d0 + (n0 >> GMP_NUMB_BITS)) & GMP_NUMB_MASK;
	      n0 &= GMP_NUMB_MASK;
#endif
	      qp[i] = q;
	      continue;
	    }
	  n1 = d0 - (d0 != 0);
	  n0 = -d0 & GMP_NUMB_MASK;
	}
      else
	{
	  if (use_preinv)
	    udiv_qrnnd_preinv (q, r, n1, n0, d1, d1inv);
	  else
	    udiv_qrnnd (q, r, n1, n0 << GMP_NAIL_BITS, d1 << GMP_NAIL_BITS);
	  r >>= GMP_NAIL_BITS;
	  umul_ppmm (n1, n0, d0, q << GMP_NAIL_BITS);
	  n0 >>= GMP_NAIL_BITS;
	}

      n2 = np[0];

    q_test:
      if (n1 > r || (n1 == r && n0 > n2))
	{
	  /* The estimated Q was too large.  */
	  q--;

#if GMP_NAIL_BITS == 0
	  sub_ddmmss (n1, n0, n1, n0, 0, d0);
#else
	  n0 = n0 - d0;
	  n1 = n1 - (n0 >> GMP_LIMB_BITS - 1);
	  n0 &= GMP_NUMB_MASK;
#endif
	  r += d1;
	  if (r >= d1)	/* If not carry, test Q again.  */
	    goto q_test;
	}

      qp[i] = q;
#if GMP_NAIL_BITS == 0
      sub_ddmmss (n1, n0, r, n2, n1, n0);
#else
      n0 = n2 - n0;
      n1 = r - n1 - (n0 >> GMP_LIMB_BITS - 1);
      n0 &= GMP_NUMB_MASK;
#endif
    }
  np[1] = n1;
  np[0] = n0;

  return most_significant_q_limb;
}
