/* mpz_root(root, u, nth) --  Set ROOT to floor(U^(1/nth)).
   Return an indication if the result is exact.

Copyright 1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.

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

#include <stdio.h>  /* for NULL */
#include "gmp.h"
#include "gmp-impl.h"

int
mpz_root (mpz_ptr root, mpz_srcptr u, unsigned long int nth)
{
  mp_ptr rootp, up;
  mp_size_t us, un, rootn, remn;

  up = PTR(u);
  us = SIZ(u);

  /* even roots of negatives provoke an exception */
  if (us < 0 && (nth & 1) == 0)
    SQRT_OF_NEGATIVE;

  /* root extraction interpreted as c^(1/nth) means a zeroth root should
     provoke a divide by zero, do this even if c==0 */
  if (nth == 0)
    DIVIDE_BY_ZERO;

  if (us == 0)
    {
      if (root != NULL)
	SIZ(root) = 0;
      return 1;			/* exact result */
    }

  un = ABS (us);
  rootn = (un - 1) / nth + 1;

  if (root != NULL)
    {
      rootp = MPZ_REALLOC (root, rootn);
      up = PTR(u);
    }
  else
    {
      rootp = __GMP_ALLOCATE_FUNC_LIMBS (rootn);
    }

  if (nth == 1)
    {
      MPN_COPY (rootp, up, un);
      remn = 0;
    }
  else
    {
      remn = mpn_rootrem (rootp, NULL, up, un, (mp_limb_t) nth);
    }

  if (root != NULL)
    SIZ(root) = us >= 0 ? rootn : -rootn;
  else
    __GMP_FREE_FUNC_LIMBS (rootp, rootn);

  return remn == 0;
}
