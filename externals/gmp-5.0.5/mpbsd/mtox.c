/* mtox -- Convert OPERAND to hexadecimal and return a malloc'ed string
   with the result of the conversion.

Copyright 1991, 1994, 2000, 2001, 2002 Free Software Foundation, Inc.

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

#include <string.h>
#include "mp.h"
#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"

char *
mtox (const MINT *x)
{
  mp_size_t xsize = x->_mp_size;
  mp_ptr    xp;
  mp_size_t xsign;
  unsigned char *str, *s;
  size_t str_size, alloc_size, i;

  xsign = xsize;
  if (xsize < 0)
    xsize = -xsize;

  /* digits, plus '\0', plus possible '-', for an exact size */
  xp = x->_mp_d;
  MPN_SIZEINBASE_16 (alloc_size, xp, xsize);
  alloc_size += 1 + (xsign < 0);

  str = (unsigned char *) (*__gmp_allocate_func) (alloc_size);
  s = str;

  if (xsign < 0)
    *s++ = '-';

  str_size = mpn_get_str (s, 16, xp, xsize);
  ASSERT (str_size <= alloc_size - (xsign < 0));
  ASSERT (str_size == 1 || *s != 0);

  for (i = 0; i < str_size; i++)
    s[i] = "0123456789abcdef"[s[i]];
  s[str_size] = 0;

  ASSERT (strlen (str) + 1 == alloc_size);
  return (char *) str;
}
