/* mpz_init_set_si(dest,val) -- Make a new multiple precision in DEST and
   assign VAL to the new number.

Copyright 1991, 1993, 1994, 1995, 2000, 2001, 2002 Free Software Foundation,
Inc.

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

void
mpz_init_set_si (mpz_ptr dest, signed long int val)
{
  mp_size_t size;
  mp_limb_t vl;

  dest->_mp_alloc = 1;
  dest->_mp_d = (mp_ptr) (*__gmp_allocate_func) (BYTES_PER_MP_LIMB);

  vl = (mp_limb_t) (unsigned long int) (val >= 0 ? val : -val);

  dest->_mp_d[0] = vl & GMP_NUMB_MASK;
  size = vl != 0;

#if GMP_NAIL_BITS != 0
  if (vl > GMP_NUMB_MAX)
    {
      MPZ_REALLOC (dest, 2);
      dest->_mp_d[1] = vl >> GMP_NUMB_BITS;
      size = 2;
    }
#endif

  dest->_mp_size = val >= 0 ? size : -size;
}
