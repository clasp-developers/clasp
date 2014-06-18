/* mpn_mul -- Multiply two natural numbers.

   THE HELPER FUNCTIONS IN THIS FILE (meaning everything except mpn_mul)
   ARE INTERNAL FUNCTIONS WITH MUTABLE INTERFACES.  IT IS ONLY SAFE TO REACH
   THEM THROUGH DOCUMENTED INTERFACES.  IN FACT, IT IS ALMOST GUARANTEED
   THAT THEY'LL CHANGE OR DISAPPEAR IN A FUTURE GNU MP RELEASE.


Copyright 1991, 1993, 1994, 1996, 1997, 1999, 2000, 2001, 2002, 2003, 2005
Free Software Foundation, Inc.

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


#ifndef MUL_BASECASE_MAX_UN
#define MUL_BASECASE_MAX_UN 500
#endif

/* Multiply the natural numbers u (pointed to by UP, with UN limbs) and v
   (pointed to by VP, with VN limbs), and store the result at PRODP.  The
   result is UN + VN limbs.  Return the most significant limb of the result.

   NOTE: The space pointed to by PRODP is overwritten before finished with U
   and V, so overlap is an error.

   Argument constraints:
   1. UN >= VN.
   2. PRODP != UP and PRODP != VP, i.e. the destination must be distinct from
      the multiplier and the multiplicand.  */

mp_limb_t
mpn_mul (mp_ptr prodp,
	 mp_srcptr up, mp_size_t un,
	 mp_srcptr vp, mp_size_t vn)
{
  mp_size_t l;
  mp_limb_t c;

  ASSERT (un >= vn);
  ASSERT (vn >= 1);
  ASSERT (! MPN_OVERLAP_P (prodp, un+vn, up, un));
  ASSERT (! MPN_OVERLAP_P (prodp, un+vn, vp, vn));

  if (up == vp && un == vn)
    {
      mpn_sqr_n (prodp, up, un);
      return prodp[2 * un - 1];
    }

  if (vn < MUL_KARATSUBA_THRESHOLD)
    { /* plain schoolbook multiplication */
      if (un <= MUL_BASECASE_MAX_UN)
	mpn_mul_basecase (prodp, up, un, vp, vn);
      else
	{
	  /* We have un >> MUL_BASECASE_MAX_UN > vn.  For better memory
	     locality, split up[] into MUL_BASECASE_MAX_UN pieces and multiply
	     these pieces with the vp[] operand.  After each such partial
	     multiplication (but the last) we copy the most significant vn
	     limbs into a temporary buffer since that part would otherwise be
	     overwritten by the next multiplication.  After the next
	     multiplication, we add it back.  This illustrates the situation:

                                                    -->vn<--
                                                      |  |<------- un ------->|
                                                         _____________________|
                                                        X                    /|
                                                      /XX__________________/  |
                                    _____________________                     |
                                   X                    /                     |
                                 /XX__________________/                       |
               _____________________                                          |
              /                    /                                          |
            /____________________/                                            |
	    ==================================================================

	    The parts marked with X are the parts whose sums are copied into
	    the temporary buffer.  */

	  mp_limb_t tp[MUL_KARATSUBA_THRESHOLD_LIMIT];
	  mp_limb_t cy;
          ASSERT (MUL_KARATSUBA_THRESHOLD <= MUL_KARATSUBA_THRESHOLD_LIMIT);

	  mpn_mul_basecase (prodp, up, MUL_BASECASE_MAX_UN, vp, vn);
	  prodp += MUL_BASECASE_MAX_UN;
	  MPN_COPY (tp, prodp, vn);		/* preserve high triangle */
	  up += MUL_BASECASE_MAX_UN;
	  un -= MUL_BASECASE_MAX_UN;
	  while (un > MUL_BASECASE_MAX_UN)
	    {
	      mpn_mul_basecase (prodp, up, MUL_BASECASE_MAX_UN, vp, vn);
	      cy = mpn_add_n (prodp, prodp, tp, vn); /* add back preserved triangle */
	      mpn_incr_u (prodp + vn, cy);		/* safe? */
	      prodp += MUL_BASECASE_MAX_UN;
	      MPN_COPY (tp, prodp, vn);		/* preserve high triangle */
	      up += MUL_BASECASE_MAX_UN;
	      un -= MUL_BASECASE_MAX_UN;
	    }
	  if (un > vn)
	    {
	      mpn_mul_basecase (prodp, up, un, vp, vn);
	    }
	  else
	    {
	      ASSERT_ALWAYS (un > 0);
	      mpn_mul_basecase (prodp, vp, vn, up, un);
	    }
	  cy = mpn_add_n (prodp, prodp, tp, vn); /* add back preserved triangle */
	  mpn_incr_u (prodp + vn, cy);		/* safe? */
	}
      return prodp[un + vn - 1];
    }

  if (ABOVE_THRESHOLD (vn, MUL_FFT_THRESHOLD))
    {
      mpn_mul_fft_full (prodp, up, un, vp, vn);
      return prodp[un + vn - 1];
    }

  mpn_mul_n (prodp, up, vp, vn);
  if (un != vn)
    { mp_limb_t t;
      mp_ptr ws;
      TMP_DECL;
      TMP_MARK;

      prodp += vn;
      l = vn;
      up += vn;
      un -= vn;

      if (un < vn)
	{
	  /* Swap u's and v's. */
	  MPN_SRCPTR_SWAP (up,un, vp,vn);
	}

      ws = TMP_ALLOC_LIMBS ((vn >= MUL_KARATSUBA_THRESHOLD ? vn : un) + vn);

      t = 0;
      while (vn >= MUL_KARATSUBA_THRESHOLD)
	{
	  mpn_mul_n (ws, up, vp, vn);
	  if (l <= 2*vn)
	    {
	      t += mpn_add_n (prodp, prodp, ws, l);
	      if (l != 2*vn)
		{
		  t = mpn_add_1 (prodp + l, ws + l, 2*vn - l, t);
		  l = 2*vn;
		}
	    }
	  else
	    {
	      c = mpn_add_n (prodp, prodp, ws, 2*vn);
	      t += mpn_add_1 (prodp + 2*vn, prodp + 2*vn, l - 2*vn, c);
	    }
	  prodp += vn;
	  l -= vn;
	  up += vn;
	  un -= vn;
	  if (un < vn)
	    {
	      /* Swap u's and v's. */
	      MPN_SRCPTR_SWAP (up,un, vp,vn);
	    }
	}

      if (vn != 0)
	{
	  mpn_mul_basecase (ws, up, un, vp, vn);
	  if (l <= un + vn)
	    {
	      t += mpn_add_n (prodp, prodp, ws, l);
	      if (l != un + vn)
		t = mpn_add_1 (prodp + l, ws + l, un + vn - l, t);
	    }
	  else
	    {
	      c = mpn_add_n (prodp, prodp, ws, un + vn);
	      t += mpn_add_1 (prodp + un + vn, prodp + un + vn, l - un - vn, c);
	    }
	}

      TMP_FREE;
    }
  return prodp[un + vn - 1];
}
