/* mpq_set_den(dest,den) -- Set the denominator of DEST from DEN.

Copyright 1991, 1994, 1995, 1996, 2000, 2001 Free Software Foundation, Inc.

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
mpq_set_den (MP_RAT *dest, const MP_INT *den)
{
  mp_size_t size = den->_mp_size;
  mp_size_t abs_size = ABS (size);

  if (dest->_mp_den._mp_alloc < abs_size)
    _mpz_realloc (&(dest->_mp_den), abs_size);

  MPN_COPY (dest->_mp_den._mp_d, den->_mp_d, abs_size);
  dest->_mp_den._mp_size = size;
}
