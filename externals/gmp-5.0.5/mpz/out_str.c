/* mpz_out_str(stream, base, integer) -- Output to STREAM the multi prec.
   integer INTEGER in base BASE.

Copyright 1991, 1993, 1994, 1996, 2001, 2005 Free Software Foundation, Inc.

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
#include "gmp.h"
#include "gmp-impl.h"

size_t
mpz_out_str (FILE *stream, int base, mpz_srcptr x)
{
  mp_ptr xp;
  mp_size_t x_size = x->_mp_size;
  unsigned char *str;
  size_t str_size;
  size_t i;
  size_t written;
  char *num_to_text;
  TMP_DECL;

  if (stream == 0)
    stream = stdout;

  if (base >= 0)
    {
      num_to_text = "0123456789abcdefghijklmnopqrstuvwxyz";
      if (base == 0)
	base = 10;
      else if (base > 36)
	{
	  num_to_text = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
	  if (base > 62)
	    return 0;
	}
    }
  else
    {
      base = -base;
      num_to_text = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    }

  if (x_size == 0)
    {
      fputc ('0', stream);
      return ferror (stream) ? 0 : 1;
    }

  written = 0;

  if (x_size < 0)
    {
      fputc ('-', stream);
      x_size = -x_size;
      written = 1;
    }

  TMP_MARK;
  str_size = ((size_t) (x_size * GMP_LIMB_BITS
			* mp_bases[base].chars_per_bit_exactly)) + 3;
  str = (unsigned char *) TMP_ALLOC (str_size);

  /* Move the number to convert into temporary space, since mpn_get_str
     clobbers its argument + needs one extra high limb....  */
  xp = TMP_ALLOC_LIMBS (x_size + 1);
  MPN_COPY (xp, x->_mp_d, x_size);

  str_size = mpn_get_str (str, base, xp, x_size);

  /* mpn_get_str might make some leading zeros.  Skip them.  */
  while (*str == 0)
    {
      str_size--;
      str++;
    }

  /* Translate to printable chars.  */
  for (i = 0; i < str_size; i++)
    str[i] = num_to_text[str[i]];
  str[str_size] = 0;

  {
    size_t fwret;
    fwret = fwrite ((char *) str, 1, str_size, stream);
    written += fwret;
  }

  TMP_FREE;
  return ferror (stream) ? 0 : written;
}
