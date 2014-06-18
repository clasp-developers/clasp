/* double mpf_get_d (mpf_t src) -- return SRC truncated to a double.

Copyright 1996, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

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

double
mpf_get_d (mpf_srcptr src)
{
  mp_size_t  size, abs_size;
  long       exp;

  size = SIZ (src);
  if (UNLIKELY (size == 0))
    return 0.0;

  abs_size = ABS (size);
  exp = (EXP (src) - abs_size) * GMP_NUMB_BITS;
  return mpn_get_d (PTR (src), abs_size, size, exp);
}
