/* Test mpz_set_str.

Copyright 2001 Free Software Foundation, Inc.

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
#include <stdlib.h>
#include "gmp.h"
#include "gmp-impl.h"
#include "tests.h"


void
check_one (mpz_srcptr want, int base, const char *str)
{
  mpz_t   got;

  MPZ_CHECK_FORMAT (want);
  mp_trace_base = (base == 0 ? 16 : base);

  mpz_init (got);

  if (mpz_set_str (got, str, base) != 0)
    {
      printf ("mpz_set_str unexpectedly failed\n");
      printf ("  base %d\n", base);
      printf ("  str  \"%s\"\n", str);
      abort ();
    }
  MPZ_CHECK_FORMAT (got);

  if (mpz_cmp (got, want) != 0)
    {
      printf ("mpz_set_str wrong\n");
      printf ("  base %d\n", base);
      printf ("  str  \"%s\"\n", str);
      mpz_trace ("got ", got);
      mpz_trace ("want", want);
      abort ();
    }

  mpz_clear (got);
}

void
check_samples (void)
{
  mpz_t  z;

  mpz_init (z);

  mpz_set_ui (z, 0L);
  check_one (z, 0, "0 ");
  check_one (z, 0, "0    ");
  check_one (z, 10, "0 ");
  check_one (z, 10, "0    ");
  check_one (z, 10, "0000000    ");

  mpz_set_ui (z, 123L);
  check_one (z, 0, "123 ");
  check_one (z, 0, "123    ");
  check_one (z, 10, "123 ");
  check_one (z, 10, "123    ");
  check_one (z, 0, " 123 ");
  check_one (z, 0, "  123    ");
  check_one (z, 10, "  0000123 ");
  check_one (z, 10, "  123    ");

  mpz_clear (z);
}

int
main (void)
{
  tests_start ();

  check_samples ();

  tests_end ();
  exit (0);
}
