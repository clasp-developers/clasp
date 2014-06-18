/* mpn_rootrem(rootp,remp,ap,an,nth) -- Compute the nth root of {ap,an}, and
   store the truncated integer part at rootp and the remainder at remp.

   THE FUNCTIONS IN THIS FILE ARE INTERNAL FUNCTIONS WITH MUTABLE
   INTERFACES.  IT IS ONLY SAFE TO REACH THEM THROUGH DOCUMENTED INTERFACES.
   IN FACT, IT IS ALMOST GUARANTEED THAT THEY'LL CHANGE OR DISAPPEAR IN A
   FUTURE GNU MP RELEASE.


Copyright 2002, 2005 Free Software Foundation, Inc.

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
along with the GNU MP Library; see the file COPYING.LIB.  If not, write
to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA. */

/*
  We use Newton's method to compute the root of a:

           n
  f(x) := x  - a


            n - 1
  f'(x) := x      n


                                       n-1            n-1           n-1
                                x - a/x            a/x   - x     a/x   + (n-1)x
  new x = x - f(x)/f'(x) =  x - ----------  =  x + ---------  =  --------------
                                     n                 n                n

*/


#include <stdio.h>
#include <stdlib.h>

#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"

mp_size_t
mpn_rootrem (mp_ptr rootp, mp_ptr remp,
	     mp_srcptr up, mp_size_t un, mp_limb_t nth)
{
  mp_ptr pp, qp, xp;
  mp_size_t pn, xn, qn;
  unsigned long int unb, xnb, bit;
  unsigned int cnt;
  mp_size_t i;
  unsigned long int n_valid_bits, adj;
  TMP_DECL;

  TMP_MARK;

  /* The extra factor 1.585 = log(3)/log(2) here is for the worst case
     overestimate of the root, i.e., when the code rounds a root that is
     2+epsilon to 3, and then powers this to a potentially huge power.  We
     could generalize the code for detecting root=1 a few lines below to deal
     with xnb <= k, for some small k.  For example, when xnb <= 2, meaning
     the root should be 1, 2, or 3, we could replace this factor by the much
     smaller log(5)/log(4).  */

#define PP_ALLOC (2 + (mp_size_t) (un*1.585))
  pp = TMP_ALLOC_LIMBS (PP_ALLOC);

  count_leading_zeros (cnt, up[un - 1]);
  unb = un * GMP_NUMB_BITS - cnt + GMP_NAIL_BITS;

  xnb = (unb - 1) / nth + 1;
  if (xnb == 1)
    {
      if (remp == NULL)
	remp = pp;
      mpn_sub_1 (remp, up, un, (mp_limb_t) 1);
      MPN_NORMALIZE (remp, un);
      rootp[0] = 1;
      TMP_FREE;
      return un;
    }

  xn = (xnb + GMP_NUMB_BITS - 1) / GMP_NUMB_BITS;

  qp = TMP_ALLOC_LIMBS (PP_ALLOC);
  xp = TMP_ALLOC_LIMBS (xn + 1);

  /* Set initial root to only ones.  This is an overestimate of the actual root
     by less than a factor of 2.  */
  for (i = 0; i < xn; i++)
    xp[i] = GMP_NUMB_MAX;
  xp[xnb / GMP_NUMB_BITS] = ((mp_limb_t) 1 << (xnb % GMP_NUMB_BITS)) - 1;

  /* Improve the initial approximation, one bit at a time.  Keep the
     approximations >= root(U,nth).  */
  bit = xnb - 2;
  n_valid_bits = 0;
  for (i = 0; (nth >> i) != 0; i++)
    {
      mp_limb_t xl = xp[bit / GMP_NUMB_BITS];
      xp[bit / GMP_NUMB_BITS] = xl ^ (mp_limb_t) 1 << bit % GMP_NUMB_BITS;
      pn = mpn_pow_1 (pp, xp, xn, nth, qp);
      ASSERT_ALWAYS (pn < PP_ALLOC);
      /* If the new root approximation is too small, restore old value.  */
      if (! (un < pn || (un == pn && mpn_cmp (up, pp, pn) < 0)))
	xp[bit / GMP_NUMB_BITS] = xl;		/* restore old value */
      n_valid_bits += 1;
      if (bit == 0)
	goto done;
      bit--;
    }

  adj = n_valid_bits - 1;

  /* Newton loop.  Converges downwards towards root(U,nth).  Currently we use
     full precision from iteration 1.  Clearly, we should use just n_valid_bits
     of precision in each step, and thus save most of the computations.  */
  while (n_valid_bits <= xnb)
    {
      mp_limb_t cy;

      pn = mpn_pow_1 (pp, xp, xn, nth - 1, qp);
      ASSERT_ALWAYS (pn < PP_ALLOC);
      qp[xn - 1] = 0;		/* pad quotient to make it always xn limbs */
      mpn_tdiv_qr (qp, pp, (mp_size_t) 0, up, un, pp, pn); /* junk remainder */
      cy = mpn_addmul_1 (qp, xp, xn, nth - 1);
      if (un - pn == xn)
	{
	  cy += qp[xn];
	  if (cy == nth)
	    {
	      for (i = xn - 1; i >= 0; i--)
		qp[i] = GMP_NUMB_MAX;
	      cy = nth - 1;
	    }
	}

      qp[xn] = cy;
      qn = xn + (cy != 0);

      mpn_divrem_1 (xp, (mp_size_t) 0, qp, qn, nth);
      n_valid_bits = n_valid_bits * 2 - adj;
    }

  /* The computed result might be one unit too large.  Adjust as necessary.  */
 done:
  pn = mpn_pow_1 (pp, xp, xn, nth, qp);
  ASSERT_ALWAYS (pn < PP_ALLOC);
  if (un < pn || (un == pn && mpn_cmp (up, pp, pn) < 0))
    {
      mpn_decr_u (xp, 1);
      pn = mpn_pow_1 (pp, xp, xn, nth, qp);
      ASSERT_ALWAYS (pn < PP_ALLOC);

      ASSERT_ALWAYS (! (un < pn || (un == pn && mpn_cmp (up, pp, pn) < 0)));
    }

  if (remp == NULL)
    remp = pp;
  mpn_sub (remp, up, un, pp, pn);
  MPN_NORMALIZE (remp, un);
  MPN_COPY (rootp, xp, xn);
  TMP_FREE;
  return un;
}
