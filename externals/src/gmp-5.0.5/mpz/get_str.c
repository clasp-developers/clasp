/* mpz_get_str (string, base, mp_src) -- Convert the multiple precision
   number MP_SRC to a string STRING of base BASE.  If STRING is NULL
   allocate space for the result.  In any case, return a pointer to the
   result.  If STRING is not NULL, the caller must ensure enough space is
   available to store the result.

Copyright 1991, 1993, 1994, 1996, 2000, 2001, 2002, 2005 Free Software
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

#include <string.h> /* for strlen */
#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"

char *
mpz_get_str (char *res_str, int base, mpz_srcptr x)
{
  mp_ptr xp;
  mp_size_t x_size = x->_mp_size;
  char *str;
  char *return_str;
  size_t str_size;
  size_t alloc_size = 0;
  char *num_to_text;
  int i;
  TMP_DECL;

  if (base >= 0)
    {
      num_to_text = "0123456789abcdefghijklmnopqrstuvwxyz";
      if (base == 0)
	base = 10;
      else if (base > 36)
	{
	  num_to_text = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
	  if (base > 62)
	    return NULL;
	}
    }
  else
    {
      base = -base;
      num_to_text = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    }

  /* allocate string for the user if necessary */
  if (res_str == NULL)
    {
      /* digits, null terminator, possible minus sign */
      MPN_SIZEINBASE (alloc_size, PTR(x), ABS(x_size), base);
      alloc_size += 1 + (x_size<0);
      res_str = (char *) (*__gmp_allocate_func) (alloc_size);
    }
  return_str = res_str;

  if (x_size < 0)
    {
      *res_str++ = '-';
      x_size = -x_size;
    }

  /* mpn_get_str clobbers its input on non power-of-2 bases */
  TMP_MARK;
  xp = x->_mp_d;
  if (! POW2_P (base))
    {
      xp = TMP_ALLOC_LIMBS (x_size + 1);  /* +1 in case x_size==0 */
      MPN_COPY (xp, x->_mp_d, x_size);
    }

  str_size = mpn_get_str ((unsigned char *) res_str, base, xp, x_size);
  ASSERT (alloc_size == 0 || str_size <= alloc_size - (SIZ(x) < 0));

  /* might have a leading zero, skip it */
  str = res_str;
  if (*res_str == 0 && str_size != 1)
    {
      str_size--;
      str++;
      ASSERT (*str != 0);  /* at most one leading zero */
    }

  /* Convert result to printable chars, and move down if there was a leading
     zero.  */
  for (i = 0; i < str_size; i++)
    res_str[i] = num_to_text[(int) str[i]];
  res_str[str_size] = 0;

  TMP_FREE;

  /* if allocated then resize down to the actual space required */
  if (alloc_size != 0)
    {
      size_t  actual_size = str_size + 1 + (res_str - return_str);
      ASSERT (actual_size == strlen (return_str) + 1);
      __GMP_REALLOCATE_FUNC_MAYBE_TYPE (return_str, alloc_size, actual_size,
                                        char);
    }
  return return_str;
}
