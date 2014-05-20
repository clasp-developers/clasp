/* Test conversion using mpz_get_str and mpz_set_str.

Copyright 1993, 1994, 1996, 1999, 2000, 2001, 2002 Free Software Foundation,
Inc.

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

#include <stdio.h>
#include <stdlib.h>
#include <string.h> /* for strlen */

#include "gmp.h"
#include "gmp-impl.h"
#include "tests.h"

void debug_mp _PROTO ((mpz_t, int));

int
main (int argc, char **argv)
{
  mpz_t op1, op2;
  mp_size_t size;
  int i;
  int reps = 10000;
  char *str, *buf;
  int base;
  gmp_randstate_ptr rands;
  mpz_t bs;
  unsigned long bsi, size_range;

  tests_start ();
  rands = RANDS;

  mpz_init (bs);

  if (argc == 2)
     reps = atoi (argv[1]);

  mpz_init (op1);
  mpz_init (op2);

  for (i = 0; i < reps; i++)
    {
      /* 1. Generate random mpz_t and convert to a string and back to mpz_t
	 again.  */
      mpz_urandomb (bs, rands, 32);
      size_range = mpz_get_ui (bs) % 12 + 2;	/* 2..13 */
      mpz_urandomb (bs, rands, size_range);	/* 3..8191 bits */
      size = mpz_get_ui (bs);
      mpz_rrandomb (op1, rands, size);

      mpz_urandomb (bs, rands, 1);
      bsi = mpz_get_ui (bs);
      if ((bsi & 1) != 0)
	mpz_neg (op1, op1);

      mpz_urandomb (bs, rands, 32);
      bsi = mpz_get_ui (bs);
      base = bsi % 62 + 1;
      if (base == 1)
	base = 0;

      str = mpz_get_str ((char *) 0, base, op1);
      mpz_set_str_or_abort (op2, str, base);

      if (mpz_cmp (op1, op2))
	{
	  fprintf (stderr, "ERROR, op1 and op2 different in test %d\n", i);
	  fprintf (stderr, "str  = %s\n", str);
	  fprintf (stderr, "base = %d\n", base);
	  fprintf (stderr, "op1  = "); debug_mp (op1, -16);
	  fprintf (stderr, "op2  = "); debug_mp (op2, -16);
	  abort ();
	}

      (*__gmp_free_func) (str, strlen (str) + 1);

#if 0
      /* 2. Generate random string and convert to mpz_t and back to a string
	 again.  */
      mpz_urandomb (bs, rands, 32);
      size_range = mpz_get_ui (bs) % 10 + 2;	/* 2..11 */
      mpz_urandomb (bs, rands, size_range);	/* 3..2047 bits */
      len = mpz_get_ui (bs);
      buf = (*__gmp_allocate_func) (len + 1);
      string_urandomb (buf, len, base);
      mpz_set_str_or_abort (op1, buf, base);
      str = mpz_get_str ((char *) 0, base, op1);

      if (strcmp (str, buf) != 0)
	{
	  fprintf (stderr, "ERROR, str and buf different\n");
	  fprintf (stderr, "str  = %s\n", str);
	  fprintf (stderr, "buf  = %s\n", buf);
	  fprintf (stderr, "base = %d\n", base);
	  fprintf (stderr, "op1  = "); debug_mp (op1, -16);
	  abort ();
	}

      (*__gmp_free_func) (buf, len + 1);
      (*__gmp_free_func) (str, strlen (str) + 1);
#endif
    }

  mpz_clear (bs);
  mpz_clear (op1);
  mpz_clear (op2);

  tests_end ();
  exit (0);
}

void
debug_mp (mpz_t x, int base)
{
  mpz_out_str (stderr, base, x); fputc ('\n', stderr);
}
