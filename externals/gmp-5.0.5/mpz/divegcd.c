/* mpz_divexact_gcd -- exact division optimized for GCDs.

   THE FUNCTIONS IN THIS FILE ARE FOR INTERNAL USE AND ARE ALMOST CERTAIN TO
   BE SUBJECT TO INCOMPATIBLE CHANGES IN FUTURE GNU MP RELEASES.

Copyright 2000, 2005 Free Software Foundation, Inc.

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


/* Set q to a/d, expecting d to be from a GCD and therefore usually small.

   The distribution of GCDs of random numbers can be found in Knuth volume 2
   section 4.5.2 theorem D.

            GCD     chance
             1       60.8%
            2^k      20.2%     (1<=k<32)
           3*2^k      9.0%     (1<=k<32)
           other     10.1%

   Only the low limb is examined for optimizations, since GCDs bigger than
   2^32 (or 2^64) will occur very infrequently.

   Future: This could change to an mpn_divexact_gcd, possibly partly
   inlined, if/when the relevant mpq functions change to an mpn based
   implementation.  */


static void
mpz_divexact_by3 (mpz_ptr q, mpz_srcptr a)
{
  mp_size_t  size = SIZ(a);
  if (size == 0)
    {
      SIZ(q) = 0;
      return;
    }
  else
    {
      mp_size_t  abs_size = ABS(size);
      mp_ptr     qp;

      MPZ_REALLOC (q, abs_size);

      qp = PTR(q);
      mpn_divexact_by3 (qp, PTR(a), abs_size);

      abs_size -= (qp[abs_size-1] == 0);
      SIZ(q) = (size>0 ? abs_size : -abs_size);
    }
}

void
mpz_divexact_gcd (mpz_ptr q, mpz_srcptr a, mpz_srcptr d)
{
  ASSERT (mpz_sgn (d) > 0);

  if (SIZ(d) == 1)
    {
      mp_limb_t  dl = PTR(d)[0];
      int        twos;

      if (dl == 1)
        {
          if (q != a)
            mpz_set (q, a);
          return;
        }
      if (dl == 3)
        {
          mpz_divexact_by3 (q, a);
          return;
        }

      count_trailing_zeros (twos, dl);
      dl >>= twos;

      if (dl == 1)
        {
          mpz_tdiv_q_2exp (q, a, twos);
          return;
        }
      if (dl == 3)
        {
          mpz_tdiv_q_2exp (q, a, twos);
          mpz_divexact_by3 (q, q);
          return;
        }
    }

  mpz_divexact (q, a, d);
}
