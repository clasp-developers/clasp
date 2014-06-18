/* mpq_set_si(dest,ulong_num,ulong_den) -- Set DEST to the retional number
   ULONG_NUM/ULONG_DEN.

Copyright 1991, 1994, 1995, 2001, 2003 Free Software Foundation, Inc.

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
mpq_set_si (MP_RAT *dest, signed long int num, unsigned long int den)
{
  unsigned long int abs_num;

  if (GMP_NUMB_BITS < BITS_PER_ULONG)
    {
      if (num == 0)  /* Canonicalize 0/d to 0/1.  */
        den = 1;
      mpz_set_si (mpq_numref (dest), num);
      mpz_set_ui (mpq_denref (dest), den);
      return;
    }

  abs_num = ABS (num);

  if (num == 0)
    {
      /* Canonicalize 0/d to 0/1.  */
      den = 1;
      dest->_mp_num._mp_size = 0;
    }
  else
    {
      dest->_mp_num._mp_d[0] = abs_num;
      dest->_mp_num._mp_size = num > 0 ? 1 : -1;
    }

  dest->_mp_den._mp_d[0] = den;
  dest->_mp_den._mp_size = (den != 0);
}
