/* gcdext_subdiv_step.c.

   THE FUNCTIONS IN THIS FILE ARE INTERNAL WITH MUTABLE INTERFACES.  IT IS ONLY
   SAFE TO REACH THEM THROUGH DOCUMENTED INTERFACES.  IN FACT, IT IS ALMOST
   GUARANTEED THAT THEY'LL CHANGE OR DISAPPEAR IN A FUTURE GNU MP RELEASE.

Copyright 2003, 2004, 2005, 2008, 2009 Free Software Foundation, Inc.

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

/* Used when mpn_hgcd or mpn_hgcd2 has failed. Then either one of a or
   b is small, or the difference is small. Perform one subtraction
   followed by one division. If the gcd is found, stores it in gp and
   *gn, and returns zero. Otherwise, compute the reduced a and b,
   return the new size, and cofactors. */

/* Temporary storage: Needs n limbs for the quotient, at qp. tp must
   point to an area large enough for the resulting cofactor, plus one
   limb extra. All in all, 2N + 1 if N is a bound for both inputs and
   outputs. */
mp_size_t
mpn_gcdext_subdiv_step (mp_ptr gp, mp_size_t *gn, mp_ptr up, mp_size_t *usizep,
			mp_ptr ap, mp_ptr bp, mp_size_t n,
			mp_ptr u0, mp_ptr u1, mp_size_t *unp,
			mp_ptr qp, mp_ptr tp)
{
  mp_size_t an, bn, un;
  mp_size_t qn;
  mp_size_t u0n;

  int swapped;

  an = bn = n;

  ASSERT (an > 0);
  ASSERT (ap[an-1] > 0 || bp[an-1] > 0);

  MPN_NORMALIZE (ap, an);
  MPN_NORMALIZE (bp, bn);

  un = *unp;

  swapped = 0;

  if (UNLIKELY (an == 0))
    {
    return_b:
      MPN_COPY (gp, bp, bn);
      *gn = bn;

      MPN_NORMALIZE (u0, un);
      MPN_COPY (up, u0, un);

      *usizep = swapped ? un : -un;

      return 0;
    }
  else if (UNLIKELY (bn == 0))
    {
      MPN_COPY (gp, ap, an);
      *gn = an;

      MPN_NORMALIZE (u1, un);
      MPN_COPY (up, u1, un);

      *usizep = swapped ? -un : un;

      return 0;
    }

  /* Arrange so that a > b, subtract an -= bn, and maintain
     normalization. */
  if (an < bn)
    {
      MPN_PTR_SWAP (ap, an, bp, bn);
      MP_PTR_SWAP (u0, u1);
      swapped ^= 1;
    }
  else if (an == bn)
    {
      int c;
      MPN_CMP (c, ap, bp, an);
      if (UNLIKELY (c == 0))
	{
	  MPN_COPY (gp, ap, an);
	  *gn = an;

	  /* Must return the smallest cofactor, +u1 or -u0 */
	  MPN_CMP (c, u0, u1, un);
	  ASSERT (c != 0 || (un == 1 && u0[0] == 1 && u1[0] == 1));

	  if (c < 0)
	    {
	      MPN_NORMALIZE (u0, un);
	      MPN_COPY (up, u0, un);
	      swapped ^= 1;
	    }
	  else
	    {
	      MPN_NORMALIZE_NOT_ZERO (u1, un);
	      MPN_COPY (up, u1, un);
	    }

	  *usizep = swapped ? -un : un;
	  return 0;
	}
      else if (c < 0)
	{
	  MP_PTR_SWAP (ap, bp);
	  MP_PTR_SWAP (u0, u1);
	  swapped ^= 1;
	}
    }
  /* Reduce a -= b, u1 += u0 */
  ASSERT_NOCARRY (mpn_sub (ap, ap, an, bp, bn));
  MPN_NORMALIZE (ap, an);
  ASSERT (an > 0);

  u1[un] = mpn_add_n (u1, u1, u0, un);
  un += (u1[un] > 0);

  /* Arrange so that a > b, and divide a = q b + r */
  if (an < bn)
    {
      MPN_PTR_SWAP (ap, an, bp, bn);
      MP_PTR_SWAP (u0, u1);
      swapped ^= 1;
    }
  else if (an == bn)
    {
      int c;
      MPN_CMP (c, ap, bp, an);
      if (UNLIKELY (c == 0))
	goto return_b;
      else if (c < 0)
	{
	  MP_PTR_SWAP (ap, bp);
	  MP_PTR_SWAP (u0, u1);
	  swapped ^= 1;
	}
    }

  /* Reduce a -= q b, u1 += q u0 */
  qn = an - bn + 1;
  mpn_tdiv_qr (qp, ap, 0, ap, an, bp, bn);

  if (mpn_zero_p (ap, bn))
    goto return_b;

  n = bn;

  /* Update u1 += q u0 */
  u0n = un;
  MPN_NORMALIZE (u0, u0n);

  if (u0n > 0)
    {
      qn -= (qp[qn - 1] == 0);

      if (qn > u0n)
	mpn_mul (tp, qp, qn, u0, u0n);
      else
	mpn_mul (tp, u0, u0n, qp, qn);

      if (qn + u0n > un)
	{
	  mp_size_t u1n = un;
	  un = qn + u0n;
	  un -= (tp[un-1] == 0);
	  u1[un] = mpn_add (u1, tp, un, u1, u1n);
	}
      else
	{
	  u1[un] = mpn_add (u1, u1, un, tp, qn + u0n);
	}

      un += (u1[un] > 0);
    }

  *unp = un;
  return n;
}
