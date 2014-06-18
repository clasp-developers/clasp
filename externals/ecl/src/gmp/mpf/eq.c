/* mpf_eq -- Compare two floats up to a specified bit #.

Copyright 1993, 1995, 1996, 2001, 2002 Free Software Foundation, Inc.

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

int
mpf_eq (mpf_srcptr u, mpf_srcptr v, unsigned long int n_bits)
{
  mp_srcptr up, vp;
  mp_size_t usize, vsize, size, i;
  mp_exp_t uexp, vexp;

  uexp = u->_mp_exp;
  vexp = v->_mp_exp;

  usize = u->_mp_size;
  vsize = v->_mp_size;

  /* 1. Are the signs different?  */
  if ((usize ^ vsize) >= 0)
    {
      /* U and V are both non-negative or both negative.  */
      if (usize == 0)
	return vsize == 0;
      if (vsize == 0)
	return 0;

      /* Fall out.  */
    }
  else
    {
      /* Either U or V is negative, but not both.  */
      return 0;
    }

  /* U and V have the same sign and are both non-zero.  */

  /* 2. Are the exponents different?  */
  if (uexp > vexp)
    return 0;			/* ??? handle (uexp = vexp + 1)   */
  if (vexp > uexp)
    return 0;			/* ??? handle (vexp = uexp + 1)   */

  usize = ABS (usize);
  vsize = ABS (vsize);

  up = u->_mp_d;
  vp = v->_mp_d;

  /* Ignore zeroes at the low end of U and V.  */
  while (up[0] == 0)
    {
      up++;
      usize--;
    }
  while (vp[0] == 0)
    {
      vp++;
      vsize--;
    }

  if (usize > vsize)
    {
      if (vsize * GMP_NUMB_BITS < n_bits)
	return 0;		/* surely too different */
      size = vsize;
    }
  else if (vsize > usize)
    {
      if (usize * GMP_NUMB_BITS < n_bits)
	return 0;		/* surely too different */
      size = usize;
    }
  else
    {
      size = usize;
    }

  if (size > (n_bits + GMP_NUMB_BITS - 1) / GMP_NUMB_BITS)
    size = (n_bits + GMP_NUMB_BITS - 1) / GMP_NUMB_BITS;

  up += usize - size;
  vp += vsize - size;

  for (i = size - 1; i >= 0; i--)
    {
      if (up[i] != vp[i])
	return 0;
    }

  return 1;
}
