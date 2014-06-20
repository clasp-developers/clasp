/* mpq_neg -- negate a rational.

Copyright 2000, 2001 Free Software Foundation, Inc.

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

#define __GMP_FORCE_mpq_neg 1

#include "gmp.h"
#include "gmp-impl.h"


void
mpq_neg (mpq_ptr dst, mpq_srcptr src)
{
  mp_size_t  num_size = src->_mp_num._mp_size;

  if (src != dst)
    {
      mp_size_t  num_abs_size = ABS(num_size);
      mp_size_t  den_size = src->_mp_den._mp_size;

      MPZ_REALLOC (mpq_numref(dst), num_abs_size);
      MPZ_REALLOC (mpq_denref(dst), den_size);

      MPN_COPY (dst->_mp_num._mp_d, src->_mp_num._mp_d, num_abs_size);
      MPN_COPY (dst->_mp_den._mp_d, src->_mp_den._mp_d, den_size);

      dst->_mp_den._mp_size = den_size;
    }

  dst->_mp_num._mp_size = -num_size;
}
