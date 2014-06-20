/* min(MINT) -- Do decimal input from standard input and store result in
   MINT.

Copyright 1991, 1994, 1996, 2000, 2001 Free Software Foundation, Inc.

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
#include <ctype.h>
#include "mp.h"
#include "gmp.h"
#include "gmp-impl.h"

extern const unsigned char __gmp_digit_value_tab[];
#define digit_value_tab __gmp_digit_value_tab

void
min (MINT *dest)
{
  char *str;
  size_t alloc_size, str_size;
  int c;
  int negative;
  mp_size_t dest_size;
  const unsigned char *digit_value;

  digit_value = digit_value_tab;

  alloc_size = 100;
  str = (char *) (*__gmp_allocate_func) (alloc_size);
  str_size = 0;

  /* Skip whitespace.  */
  do
    c = getc (stdin);
  while (isspace (c));

  negative = 0;
  if (c == '-')
    {
      negative = 1;
      c = getc (stdin);
    }

  if (c == EOF || digit_value[c] >= 10)
    return;			/* error if no digits */

  do
    {
      int dig;
      dig = digit_value[c];
      if (dig >= 10)
	break;
      if (str_size >= alloc_size)
	{
	  size_t old_alloc_size = alloc_size;
	  alloc_size = alloc_size * 3 / 2;
	  str = (char *) (*__gmp_reallocate_func) (str, old_alloc_size, alloc_size);
	}
      str[str_size++] = dig;
      c = getc (stdin);
    }
  while (c != EOF);

  ungetc (c, stdin);

  dest_size = str_size / mp_bases[10].chars_per_limb + 1;
  if (dest->_mp_alloc < dest_size)
    _mp_realloc (dest, dest_size);

  dest_size = mpn_set_str (dest->_mp_d, (unsigned char *) str, str_size, 10);
  dest->_mp_size = negative ? -dest_size : dest_size;

  (*__gmp_free_func) (str, alloc_size);
  return;
}
