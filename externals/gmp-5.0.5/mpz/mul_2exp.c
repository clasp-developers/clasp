/* mpz_mul_2exp -- Multiply a bignum by 2**CNT

Copyright 1991, 1993, 1994, 1996, 2001, 2002 Free Software Foundation, Inc.

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
mpz_mul_2exp (mpz_ptr w, mpz_srcptr u, mp_bitcnt_t cnt)
{
  mp_size_t usize = u->_mp_size;
  mp_size_t abs_usize = ABS (usize);
  mp_size_t wsize;
  mp_size_t limb_cnt;
  mp_ptr wp;
  mp_limb_t wlimb;

  if (usize == 0)
    {
      w->_mp_size = 0;
      return;
    }

  limb_cnt = cnt / GMP_NUMB_BITS;
  wsize = abs_usize + limb_cnt + 1;
  if (w->_mp_alloc < wsize)
    _mpz_realloc (w, wsize);

  wp = w->_mp_d;
  wsize = abs_usize + limb_cnt;

  cnt %= GMP_NUMB_BITS;
  if (cnt != 0)
    {
      wlimb = mpn_lshift (wp + limb_cnt, u->_mp_d, abs_usize, cnt);
      if (wlimb != 0)
	{
	  wp[wsize] = wlimb;
	  wsize++;
	}
    }
  else
    {
      MPN_COPY_DECR (wp + limb_cnt, u->_mp_d, abs_usize);
    }

  /* Zero all whole limbs at low end.  Do it here and not before calling
     mpn_lshift, not to lose for U == W.  */
  MPN_ZERO (wp, limb_cnt);

  w->_mp_size = usize >= 0 ? wsize : -wsize;
}
