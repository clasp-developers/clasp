/* mpz_clrbit -- clear a specified bit.

Copyright 1991, 1993, 1994, 1995, 2001, 2002 Free Software Foundation, Inc.

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
mpz_clrbit (mpz_ptr d, mp_bitcnt_t bit_index)
{
  mp_size_t dsize = d->_mp_size;
  mp_ptr dp = d->_mp_d;
  mp_size_t limb_index;

  limb_index = bit_index / GMP_NUMB_BITS;
  if (dsize >= 0)
    {
      if (limb_index < dsize)
	{
          mp_limb_t  dlimb;
          dlimb = dp[limb_index];
          dlimb &= ~((mp_limb_t) 1 << (bit_index % GMP_NUMB_BITS));
          dp[limb_index] = dlimb;

          if (UNLIKELY (dlimb == 0 && limb_index == dsize-1))
            {
              /* high limb became zero, must normalize */
              do {
                dsize--;
              } while (dsize > 0 && dp[dsize-1] == 0);
              d->_mp_size = dsize;
            }
	}
      else
	;
    }
  else
    {
      mp_size_t zero_bound;

      /* Simulate two's complement arithmetic, i.e. simulate
	 1. Set OP = ~(OP - 1) [with infinitely many leading ones].
	 2. clear the bit.
	 3. Set OP = ~OP + 1.  */

      dsize = -dsize;

      /* No upper bound on this loop, we're sure there's a non-zero limb
	 sooner ot later.  */
      for (zero_bound = 0; ; zero_bound++)
	if (dp[zero_bound] != 0)
	  break;

      if (limb_index > zero_bound)
	{
	  if (limb_index < dsize)
	    dp[limb_index] |= (mp_limb_t) 1 << (bit_index % GMP_NUMB_BITS);
	  else
	    {
	      /* Ugh.  The bit should be cleared outside of the end of the
		 number.  We have to increase the size of the number.  */
	      if (UNLIKELY (d->_mp_alloc < limb_index + 1))
                dp = _mpz_realloc (d, limb_index + 1);

	      MPN_ZERO (dp + dsize, limb_index - dsize);
	      dp[limb_index] = (mp_limb_t) 1 << (bit_index % GMP_NUMB_BITS);
	      d->_mp_size = -(limb_index + 1);
	    }
	}
      else if (limb_index == zero_bound)
	{
	  dp[limb_index] = ((((dp[limb_index] - 1)
			      | ((mp_limb_t) 1 << (bit_index % GMP_NUMB_BITS))) + 1)
			    & GMP_NUMB_MASK);
	  if (dp[limb_index] == 0)
	    {
	      mp_size_t i;
	      for (i = limb_index + 1; i < dsize; i++)
		{
		  dp[i] = (dp[i] + 1) & GMP_NUMB_MASK;
		  if (dp[i] != 0)
		    goto fin;
		}
	      /* We got carry all way out beyond the end of D.  Increase
		 its size (and allocation if necessary).  */
	      dsize++;
	      if (UNLIKELY (d->_mp_alloc < dsize))
                dp = _mpz_realloc (d, dsize);

	      dp[i] = 1;
	      d->_mp_size = -dsize;
	    fin:;
	    }
	}
      else
	;
    }
}
