/* mpz_init2 -- initialize mpz, with requested size in bits.

Copyright 2001, 2002 Free Software Foundation, Inc.

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
mpz_init2 (mpz_ptr x, unsigned long bits)
{
  mp_size_t  limbs;
  limbs = (bits + GMP_NUMB_BITS-1) / GMP_NUMB_BITS;
  limbs = MAX (limbs, 1);
  SIZ(x) = 0;
  ALLOC(x) = limbs;
  PTR(x) = __GMP_ALLOCATE_FUNC_LIMBS (limbs);

#ifdef __CHECKER__
  /* let the low limb look initialized, for the benefit of mpz_get_ui etc */
  PTR(x)[0] = 0;
#endif
}
