/* Test mtox.

Copyright 2002 Free Software Foundation, Inc.

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

#include <string.h>		/* for strcmp, strlen */
#include <stdlib.h>		/* for abort */
#include <stdio.h>
#include "gmp.h"
#include "gmp-impl.h"
#include "mp.h"
#include "tests.h"


void
check_random (void)
{
  mpz_t  z;
  int    i;
  char   *got, *want;
  gmp_randstate_ptr  rands = RANDS;

  mpz_init (z);

  for (i = 0; i < 1000; i++)
    {
      mpz_erandomb (z, rands, 6 * GMP_LIMB_BITS);
      got = mtox (z);
      want = mpz_get_str (NULL, 16, z);
      if (strcmp (got, want) != 0)
        {
          printf ("mtox wrong result\n");
          printf ("  got  \"%s\"\n", got);
          printf ("  want \"%s\"\n", want);
          abort ();
        }
      (*__gmp_free_func) (got, strlen (got) + 1);
      (*__gmp_free_func) (want, strlen (want) + 1);
    }

  mpz_clear (z);
}

void
check_mem (void)
{
  MINT  *m;
  char  *s;

  m = itom (0);
  s = mtox (m);
  if (! tests_memory_valid (s))
    {
      printf ("Skipping t-mtox, cannot test libgmp and libmp memory together\n");
      exit (0);
    }
  mfree (m);
  (*__gmp_free_func) (s, strlen (s) + 1);
}


int
main (void)
{
  tests_start ();

  check_mem ();
  check_random ();

  tests_end ();
  exit (0);
}
