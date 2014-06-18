/* IA-64 mpn_submul_1 -- Multiply a limb vector with a limb and subtract
   the result from a second limb vector.

Copyright 2000, 2001, 2002 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published
by the Free Software Foundation; either version 2.1 of the License, or (at
your option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MP Library; see the file COPYING.LIB.  If not, write
to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.

This code runs at about 9 cycles/limb on the Itanium.  That's far from the peak
execution speed of the Itanium pipeline.  With well-tuned assembly code we
should be able to get to around 3 cycles/limb.  See README.  */


#include "gmp.h"
#include "gmp-impl.h"

mp_limb_t
mpn_submul_1 (mp_ptr rp, mp_srcptr s1p, mp_size_t n, mp_limb_t s2d)
{
  mp_ptr tp;
  mp_limb_t cy;
  TMP_DECL;

  TMP_MARK;
  tp = TMP_ALLOC_LIMBS (n);
  cy = mpn_mul_1 (tp, s1p, n, s2d);
  cy += mpn_sub_n (rp, rp, tp, n);
  TMP_FREE;
  return cy;
}
