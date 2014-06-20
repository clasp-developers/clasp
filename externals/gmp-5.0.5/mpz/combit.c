/* mpz_combit -- complement a specified bit.

Copyright 2002, 2003 Free Software Foundation, Inc.

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

void
mpz_combit (mpz_ptr d, mp_bitcnt_t bit_index)
{
  mp_size_t dsize = ABSIZ(d);
  mp_ptr dp = LIMBS(d);

  mp_size_t limb_index = bit_index / GMP_NUMB_BITS;
  mp_limb_t bit = ((mp_limb_t) 1 << (bit_index % GMP_NUMB_BITS));

  if (limb_index >= dsize)
    {
      MPZ_REALLOC(d, limb_index + 1);
      dp = LIMBS(d);

      MPN_ZERO(dp + dsize, limb_index + 1 - dsize);
      dsize = limb_index + 1;
    }

  if (SIZ(d) >= 0)
    {
      dp[limb_index] ^= bit;
      MPN_NORMALIZE (dp, dsize);
      SIZ(d) = dsize;
    }
  else
    {
      mp_limb_t x = -dp[limb_index];
      mp_size_t i;

      /* non-zero limb below us means ones-complement */
      for (i = limb_index-1; i >= 0; i--)
	if (dp[i] != 0)
	  {
	    x--;  /* change twos comp to ones comp */
	    break;
	  }

      if (x & bit)
	{
	  mp_limb_t  c;

	  /* Clearing the bit increases the magitude. We might need a carry. */
	  MPZ_REALLOC(d, dsize + 1);
	  dp = LIMBS(d);

	  __GMPN_ADD_1 (c, dp+limb_index, dp+limb_index,
			dsize - limb_index, bit);
	  dp[dsize] = c;
	  dsize += c;
	}
      else
	/* Setting the bit decreases the magnitude */
	mpn_sub_1(dp+limb_index, dp+limb_index, dsize + limb_index, bit);

      MPN_NORMALIZE (dp, dsize);
      SIZ(d) = -dsize;
    }
}
