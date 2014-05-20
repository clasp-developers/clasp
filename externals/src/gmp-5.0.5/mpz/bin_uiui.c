/* mpz_bin_uiui - compute n over k.

Copyright 1998, 1999, 2000, 2001, 2002, 2003, 2006 Free Software Foundation,
Inc.

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


/* Enhancement: It ought to be possible to calculate the size of the final
   result in advance, to a rough approximation at least, and use it to do
   just one realloc.  Stirling's approximation n! ~= sqrt(2*pi*n)*(n/e)^n
   (Knuth section 1.2.5) might be of use.  */

/* "inc" in the main loop allocates a chunk more space if not already
   enough, so as to avoid repeated reallocs.  The final step on the other
   hand requires only one more limb.  */
#define MULDIV(inc)                                                     \
  do {                                                                  \
    ASSERT (rsize <= ralloc);                                           \
                                                                        \
    if (rsize == ralloc)                                                \
      {                                                                 \
        mp_size_t  new_ralloc = ralloc + (inc);                         \
        rp = __GMP_REALLOCATE_FUNC_LIMBS (rp, ralloc, new_ralloc);      \
        ralloc = new_ralloc;                                            \
      }                                                                 \
                                                                        \
    rp[rsize] = mpn_mul_1 (rp, rp, rsize, nacc);                        \
    MPN_DIVREM_OR_DIVEXACT_1 (rp, rp, rsize+1, kacc);                   \
    rsize += (rp[rsize] != 0);                                          \
                                                                        \
} while (0)

void
mpz_bin_uiui (mpz_ptr r, unsigned long int n, unsigned long int k)
{
  unsigned long int  i, j;
  mp_limb_t          nacc, kacc;
  unsigned long int  cnt;
  mp_size_t          rsize, ralloc;
  mp_ptr             rp;

  /* bin(n,k) = 0 if k>n. */
  if (n < k)
    {
      SIZ(r) = 0;
      return;
    }

  rp = PTR(r);

  /* Rewrite bin(n,k) as bin(n,n-k) if that is smaller. */
  k = MIN (k, n-k);

  /* bin(n,0) = 1 */
  if (k == 0)
    {
      SIZ(r) = 1;
      rp[0] = 1;
      return;
    }

  j = n - k + 1;
  rp[0] = j;
  rsize = 1;
  ralloc = ALLOC(r);

  /* Initialize accumulators.  */
  nacc = 1;
  kacc = 1;

  for (i = 2; i <= k; i++)
    {
      mp_limb_t n1, n0;

      /* Remove common 2 factors.  */
      cnt = ((nacc | kacc) & 1) ^ 1;
      nacc >>= cnt;
      kacc >>= cnt;

      j++;
      /* Accumulate next multiples.  */
      umul_ppmm (n1, n0, nacc, (mp_limb_t) j << GMP_NAIL_BITS);
      n0 >>= GMP_NAIL_BITS;
      if (n1 == 0)
        {
          /* Save new products in accumulators to keep accumulating.  */
          nacc = n0;
          kacc = kacc * i;
        }
      else
        {
          /* Accumulator overflow.  Perform bignum step.  */
          MULDIV (32);
          nacc = j;
          kacc = i;
        }
    }

  /* Take care of whatever is left in accumulators.  */
  MULDIV (1);

  ALLOC(r) = ralloc;
  SIZ(r) = rsize;
  PTR(r) = rp;
}
