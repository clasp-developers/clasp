/* mpz_com(mpz_ptr dst, mpz_ptr src) -- Assign the bit-complemented value of
   SRC to DST.

Copyright 1991, 1993, 1994, 1996, 2001, 2003 Free Software Foundation, Inc.

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
mpz_com (mpz_ptr dst, mpz_srcptr src)
{
  mp_size_t size = src->_mp_size;
  mp_srcptr src_ptr;
  mp_ptr dst_ptr;

  if (size >= 0)
    {
      /* As with infinite precision: one's complement, two's complement.
	 But this can be simplified using the identity -x = ~x + 1.
	 So we're going to compute (~~x) + 1 = x + 1!  */

      if (dst->_mp_alloc < size + 1)
	_mpz_realloc (dst, size + 1);

      src_ptr = src->_mp_d;
      dst_ptr = dst->_mp_d;

      if (UNLIKELY (size == 0))
	{
	  /* special case, as mpn_add_1 wants size!=0 */
	  dst_ptr[0] = 1;
	  dst->_mp_size = -1;
	  return;
	}

      {
	mp_limb_t cy;

	cy = mpn_add_1 (dst_ptr, src_ptr, size, (mp_limb_t) 1);
	if (cy)
	  {
	    dst_ptr[size] = cy;
	    size++;
	  }
      }

      /* Store a negative size, to indicate ones-extension.  */
      dst->_mp_size = -size;
    }
  else
    {
      /* As with infinite precision: two's complement, then one's complement.
	 But that can be simplified using the identity -x = ~(x - 1).
	 So we're going to compute ~~(x - 1) = x - 1!  */
      size = -size;

      if (dst->_mp_alloc < size)
	_mpz_realloc (dst, size);

      src_ptr = src->_mp_d;
      dst_ptr = dst->_mp_d;

      mpn_sub_1 (dst_ptr, src_ptr, size, (mp_limb_t) 1);
      size -= dst_ptr[size - 1] == 0;

      /* Store a positive size, to indicate zero-extension.  */
      dst->_mp_size = size;
    }
}
