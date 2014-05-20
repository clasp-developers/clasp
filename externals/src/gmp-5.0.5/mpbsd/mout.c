/* mout(MINT) -- Do decimal output of MINT to standard output.

Copyright 1991, 1994, 1996, 2000, 2001, 2002, 2005 Free Software Foundation,
Inc.

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
#include "mp.h"
#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"

void
mout (const MINT *x)
{
  mp_ptr xp;
  mp_srcptr x_ptr;
  mp_size_t x_size;
  unsigned char *str;
  size_t str_size;
  int i;
  TMP_DECL;

  x_size = x->_mp_size;
  if (x_size == 0)
    {
      fputc ('0', stdout);
      fputc ('\n', stdout);
      return;
    }
  if (x_size < 0)
    {
      fputc ('-', stdout);
      x_size = -x_size;
    }

  TMP_MARK;
  x_ptr = x->_mp_d;
  MPN_SIZEINBASE (str_size, x_ptr, x_size, 10);
  str_size += 2;
  str = (unsigned char *) TMP_ALLOC (str_size);

  /* mpn_get_str clobbers its argument */
  xp = TMP_ALLOC_LIMBS (x_size);
  MPN_COPY (xp, x_ptr, x_size);

  str_size = mpn_get_str (str, 10, xp, x_size);

  /* mpn_get_str might make a leading zero, skip it.  */
  str_size -= (*str == 0);
  str += (*str == 0);
  ASSERT (*str != 0);

  /* Translate to printable chars.  */
  for (i = 0; i < str_size; i++)
    str[i] = "0123456789"[str[i]];
  str[str_size] = 0;

  str_size = strlen ((char *) str);
  if (str_size % 10 != 0)
    {
      fwrite (str, 1, str_size % 10, stdout);
      str += str_size % 10;
      str_size -= str_size % 10;
      if (str_size != 0)
	fputc (' ', stdout);
    }
  for (i = 0; i < str_size; i += 10)
    {
      fwrite (str, 1, 10, stdout);
      str += 10;
      if (i + 10 < str_size)
	fputc (' ', stdout);
    }
  fputc ('\n', stdout);
  TMP_FREE;
}
