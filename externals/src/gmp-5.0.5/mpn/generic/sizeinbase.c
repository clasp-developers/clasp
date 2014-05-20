/* mpn_sizeinbase -- approximation to chars required for an mpn.

   THE FUNCTIONS IN THIS FILE ARE FOR INTERNAL USE ONLY.  THEY'RE ALMOST
   CERTAIN TO BE SUBJECT TO INCOMPATIBLE CHANGES OR DISAPPEAR COMPLETELY IN
   FUTURE GNU MP RELEASES.

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
#include "longlong.h"


/* Same as mpz_sizeinbase, meaning exact for power-of-2 bases, and either
   exact or 1 too big for other bases.  */

size_t
mpn_sizeinbase (mp_srcptr xp, mp_size_t xsize, int base)
{
  int lb_base, cnt;
  mp_size_t totbits;

  ASSERT (xsize >= 0);
  ASSERT (base >= 2);
  ASSERT (base < numberof (mp_bases));

  /* Special case for X == 0.  */
  if (xsize == 0)
    return 1;

  /* Calculate the total number of significant bits of X.  */
  count_leading_zeros (cnt, xp[xsize-1]);
  totbits = xsize * GMP_LIMB_BITS - cnt;

  if (POW2_P (base))
    {
      /* Special case for powers of 2, giving exact result.  */
      lb_base = mp_bases[base].big_base;
      return (totbits + lb_base - 1) / lb_base;
    }
  else
    return (size_t) (totbits * mp_bases[base].chars_per_bit_exactly) + 1;
}
