/* gcd_subdiv_step.c.

   THE FUNCTIONS IN THIS FILE ARE INTERNAL WITH MUTABLE INTERFACES.  IT IS ONLY
   SAFE TO REACH THEM THROUGH DOCUMENTED INTERFACES.  IN FACT, IT IS ALMOST
   GUARANTEED THAT THEY'LL CHANGE OR DISAPPEAR IN A FUTURE GNU MP RELEASE.

Copyright 2003, 2004, 2005, 2008 Free Software Foundation, Inc.

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
   *gn, and returns zero. Otherwise, compute the reduced a and b, and
   return the new size. */

/* FIXME: Check when the smaller number is a single limb, and invoke
 * mpn_gcd_1. */
mp_size_t
mpn_gcd_subdiv_step (mp_ptr gp, mp_size_t *gn,
		     mp_ptr ap, mp_ptr bp, mp_size_t n, mp_ptr tp)
{
  mp_size_t an, bn;

  ASSERT (n > 0);
  ASSERT (ap[n-1] > 0 || bp[n-1] > 0);

  an = bn = n;
  MPN_NORMALIZE (ap, an);
  MPN_NORMALIZE (bp, bn);

  if (UNLIKELY (an == 0))
    {
    return_b:
      MPN_COPY (gp, bp, bn);
      *gn = bn;
      return 0;
    }
  else if (UNLIKELY (bn == 0))
    {
    return_a:
      MPN_COPY (gp, ap, an);
      *gn = an;
      return 0;
    }

  /* Arrange so that a > b, subtract an -= bn, and maintain
     normalization. */
  if (an < bn)
    MPN_PTR_SWAP (ap, an, bp, bn);
  else if (an == bn)
    {
      int c;
      MPN_CMP (c, ap, bp, an);
      if (UNLIKELY (c == 0))
	goto return_a;
      else if (c < 0)
	MP_PTR_SWAP (ap, bp);
    }

  ASSERT_NOCARRY (mpn_sub (ap, ap, an, bp, bn));
  MPN_NORMALIZE (ap, an);
  ASSERT (an > 0);

  /* Arrange so that a > b, and divide a = q b + r */
  /* FIXME: an < bn happens when we have cancellation. If that is the
     common case, then we could reverse the roles of a and b to avoid
     the swap. */
  if (an < bn)
    MPN_PTR_SWAP (ap, an, bp, bn);
  else if (an == bn)
    {
      int c;
      MPN_CMP (c, ap, bp, an);
      if (UNLIKELY (c == 0))
	goto return_a;
      else if (c < 0)
	MP_PTR_SWAP (ap, bp);
    }

  mpn_tdiv_qr (tp, ap, 0, ap, an, bp, bn);

  if (mpn_zero_p (ap, bn))
    goto return_b;

  return bn;
}
