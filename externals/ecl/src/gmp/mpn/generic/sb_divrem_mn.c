/* mpn_sb_divrem_mn -- Divide natural numbers, producing both remainder and
   quotient.

   THE FUNCTIONS IN THIS FILE ARE INTERNAL FUNCTIONS WITH MUTABLE
   INTERFACES.  IT IS ONLY SAFE TO REACH THEM THROUGH DOCUMENTED INTERFACES.
   IN FACT, IT IS ALMOST GUARANTEED THAT THEY'LL CHANGE OR DISAPPEAR IN A
   FUTURE GNU MP RELEASE.


Copyright 1993, 1994, 1995, 1996, 2000, 2001, 2002 Free Software Foundation,
Inc.

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


/* The size where udiv_qrnnd_preinv should be used rather than udiv_qrnnd,
   meaning the quotient size where that should happen, the quotient size
   being how many udiv divisions will be done.

   The default is to use preinv always, CPUs where this doesn't suit have
   tuned thresholds.  Note in particular that preinv should certainly be
   used if that's the only division available (USE_PREINV_ALWAYS).  */

#ifndef DIV_SB_PREINV_THRESHOLD
#define DIV_SB_PREINV_THRESHOLD  0
#endif


/* Divide num (NP/NSIZE) by den (DP/DSIZE) and write
   the NSIZE-DSIZE least significant quotient limbs at QP
   and the DSIZE long remainder at NP.
   Return the most significant limb of the quotient, this is always 0 or 1.

   Preconditions:
   0. NSIZE >= DSIZE.
   1. The most significant bit of the divisor must be set.
   2. QP must either not overlap with the input operands at all, or
      QP + DSIZE >= NP must hold true.  (This means that it's
      possible to put the quotient in the high part of NUM, right after the
      remainder in NUM.
   3. NSIZE >= DSIZE.
   4. DSIZE > 2.  */


mp_limb_t
mpn_sb_divrem_mn (mp_ptr qp,
		  mp_ptr np, mp_size_t nn,
		  mp_srcptr dp, mp_size_t dn)
{
  mp_limb_t most_significant_q_limb = 0;
  mp_size_t qn = nn - dn;
  mp_size_t i;
  mp_limb_t dx, d1, n0;
  mp_limb_t dxinv;
  int use_preinv;

  ASSERT (dn > 2);
  ASSERT (nn >= dn);
  ASSERT (dp[dn-1] & GMP_NUMB_HIGHBIT);
  ASSERT (! MPN_OVERLAP_P (np, nn, dp, dn));
  ASSERT (! MPN_OVERLAP_P (qp, nn-dn, dp, dn));
  ASSERT (! MPN_OVERLAP_P (qp, nn-dn, np, nn) || qp+dn >= np);
  ASSERT_MPN (np, nn);
  ASSERT_MPN (dp, dn);

  np += qn;
  dx = dp[dn - 1];
  d1 = dp[dn - 2];
  n0 = np[dn - 1];

  if (n0 >= dx)
    {
      if (n0 > dx || mpn_cmp (np, dp, dn - 1) >= 0)
	{
	  mpn_sub_n (np, np, dp, dn);
	  most_significant_q_limb = 1;
	}
    }

  use_preinv = ABOVE_THRESHOLD (qn, DIV_SB_PREINV_THRESHOLD);
  if (use_preinv)
    invert_limb (dxinv, dx);

  for (i = qn - 1; i >= 0; i--)
    {
      mp_limb_t q;
      mp_limb_t nx;
      mp_limb_t cy_limb;

      nx = np[dn - 1];		/* FIXME: could get value from r1 */
      np--;

      if (nx == dx)
	{
	  /* This might over-estimate q, but it's probably not worth
	     the extra code here to find out.  */
	  q = GMP_NUMB_MASK;

#if 1
	  cy_limb = mpn_submul_1 (np, dp, dn, q);
#else
	  /* This should be faster on many machines */
	  cy_limb = mpn_sub_n (np + 1, np + 1, dp, dn);
	  cy = mpn_add_n (np, np, dp, dn);
	  np[dn] += cy;
#endif

	  if (nx != cy_limb)
	    {
	      mpn_add_n (np, np, dp, dn);
	      q--;
	    }

	  qp[i] = q;
	}
      else
	{
	  mp_limb_t rx, r1, r0, p1, p0;

	  /* "workaround" avoids a problem with gcc 2.7.2.3 i386 register usage
	     when np[dn-1] is used in an asm statement like umul_ppmm in
	     udiv_qrnnd_preinv.  The symptom is seg faults due to registers
	     being clobbered.  gcc 2.95 i386 doesn't have the problem. */
	  {
	    mp_limb_t  workaround = np[dn - 1];
	    if (CACHED_ABOVE_THRESHOLD (use_preinv, DIV_SB_PREINV_THRESHOLD))
	      udiv_qrnnd_preinv (q, r1, nx, workaround, dx, dxinv);
	    else
	      {
		udiv_qrnnd (q, r1, nx, workaround << GMP_NAIL_BITS,
			    dx << GMP_NAIL_BITS);
		r1 >>= GMP_NAIL_BITS;
	      }
	  }
	  umul_ppmm (p1, p0, d1, q << GMP_NAIL_BITS);
	  p0 >>= GMP_NAIL_BITS;

	  r0 = np[dn - 2];
	  rx = 0;
	  if (r1 < p1 || (r1 == p1 && r0 < p0))
	    {
	      p1 -= p0 < d1;
	      p0 = (p0 - d1) & GMP_NUMB_MASK;
	      q--;
	      r1 = (r1 + dx) & GMP_NUMB_MASK;
	      rx = r1 < dx;
	    }

	  p1 += r0 < p0;	/* cannot carry! */
	  rx -= r1 < p1;	/* may become 11..1 if q is still too large */
	  r1 = (r1 - p1) & GMP_NUMB_MASK;
	  r0 = (r0 - p0) & GMP_NUMB_MASK;

	  cy_limb = mpn_submul_1 (np, dp, dn - 2, q);

	  /* Check if we've over-estimated q, and adjust as needed.  */
	  {
	    mp_limb_t cy1, cy2;
	    cy1 = r0 < cy_limb;
	    r0 = (r0 - cy_limb) & GMP_NUMB_MASK;
	    cy2 = r1 < cy1;
	    r1 -= cy1;
	    np[dn - 1] = r1;
	    np[dn - 2] = r0;
	    if (cy2 != rx)
	      {
		mpn_add_n (np, np, dp, dn);
		q--;
	      }
	  }
	  qp[i] = q;
	}
    }

  /* ______ ______ ______
    |__rx__|__r1__|__r0__|		partial remainder
	    ______ ______
	 - |__p1__|__p0__|		partial product to subtract
	    ______ ______
	 - |______|cylimb|

     rx is -1, 0 or 1.  If rx=1, then q is correct (it should match
     carry out).  If rx=-1 then q is too large.  If rx=0, then q might
     be too large, but it is most likely correct.
  */

  return most_significant_q_limb;
}
