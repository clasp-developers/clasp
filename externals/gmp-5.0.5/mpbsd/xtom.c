/* xtom -- convert a hexadecimal string to a MINT, and return a pointer to
   the MINT.

Copyright 1991, 1994, 1995, 1996, 2000, 2001, 2002, 2005 Free Software
Foundation, Inc.

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
#include <ctype.h>
#include "mp.h"
#include "gmp.h"
#include "gmp-impl.h"

extern const unsigned char __gmp_digit_value_tab[];
#define digit_value __gmp_digit_value_tab

MINT *
xtom (const char *str)
{
  size_t str_size;
  char *s, *begs;
  size_t i;
  mp_size_t xsize;
  int c;
  int negative;
  MINT *x = (MINT *) (*__gmp_allocate_func) (sizeof (MINT));
  TMP_DECL;

  /* Skip whitespace.  */
  do
    c = (unsigned char) *str++;
  while (isspace (c));

  negative = 0;
  if (c == '-')
    {
      negative = 1;
      c = (unsigned char) *str++;
    }

  if (digit_value[c] >= 16)
    return 0;			/* error if no digits */

  TMP_MARK;
  str_size = strlen (str - 1);
  s = begs = (char *) TMP_ALLOC (str_size + 1);

  for (i = 0; i < str_size; i++)
    {
      if (!isspace (c))
	{
	  int dig = digit_value[c];
	  if (dig >= 16)
	    {
	      TMP_FREE;
	      return 0;
	    }
	  *s++ = dig;
	}
      c = (unsigned char) *str++;
    }

  str_size = s - begs;

  xsize = str_size / mp_bases[16].chars_per_limb + 1;
  x->_mp_alloc = xsize;
  x->_mp_d = (mp_ptr) (*__gmp_allocate_func) (xsize * BYTES_PER_MP_LIMB);

  xsize = mpn_set_str (x->_mp_d, (unsigned char *) begs, str_size, 16);
  x->_mp_size = negative ? -xsize : xsize;

  TMP_FREE;
  return x;
}
