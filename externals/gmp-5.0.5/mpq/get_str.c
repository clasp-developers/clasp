/* mpq_get_str -- mpq to string conversion.

Copyright 2001, 2002, 2006 Free Software Foundation, Inc.

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

#include <stdio.h>
#include <string.h>
#include "gmp.h"
#include "gmp-impl.h"

char *
mpq_get_str (char *str, int base, mpq_srcptr q)
{
  size_t  str_alloc, len;

  ASSERT (ABS(base) >= 2);
  ASSERT (ABS(base) <= 62);

  str_alloc = 0;
  if (str == NULL)
    {
      /* This is an overestimate since we don't bother checking how much of
         the high limbs of num and den are used.  +2 for rounding up the
         chars per bit of num and den.  +3 for sign, slash and '\0'.  */
      str_alloc = ((size_t) ((ABS (q->_mp_num._mp_size) + q->_mp_den._mp_size)
                             * GMP_LIMB_BITS
                             * mp_bases[ABS(base)].chars_per_bit_exactly))
                   + 5;
      str = (char *) (*__gmp_allocate_func) (str_alloc);
    }

  mpz_get_str (str, base, mpq_numref(q));
  len = strlen (str);
  if (! MPZ_EQUAL_1_P (mpq_denref (q)))
    {
      str[len++] = '/';
      mpz_get_str (str+len, base, mpq_denref(q));
      len += strlen (str+len);
    }

  ASSERT (len == strlen(str));
  ASSERT (str_alloc == 0 || len+1 <= str_alloc);
  ASSERT (len+1 <=  /* size recommended to applications */
          mpz_sizeinbase (mpq_numref(q), ABS(base)) +
          mpz_sizeinbase (mpq_denref(q), ABS(base)) + 3);

  if (str_alloc != 0)
    __GMP_REALLOCATE_FUNC_MAYBE_TYPE (str, str_alloc, len+1, char);

  return str;
}
