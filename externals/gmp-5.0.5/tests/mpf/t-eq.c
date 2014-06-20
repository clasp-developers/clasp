/* Test mpf_eq.

Copyright 2009 Free Software Foundation, Inc.

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

#define SZ (2 * sizeof(mp_limb_t))

void insert_random_low_zero_limbs (mpf_t, gmp_randstate_ptr);
void dump_abort (mpf_t, mpf_t, int, int, int, int, int, long);
void hexdump (mpf_t);

int
main (int argc, char **argv)
{
  unsigned long test, reps = 10000;
  mpf_t a, b, x;
  gmp_randstate_ptr rands;
  mpz_t ds;
  int hibits, lshift1, lshift2;
  int xtra;

#define HIBITS 10
#define LSHIFT1 10
#define LSHIFT2 10

  if (argc > 1)
    reps = strtol (argv[1], 0, 0);

  tests_start ();

  rands = RANDS;

  mpf_set_default_prec ((1 << HIBITS) + (1 << LSHIFT1) + (1 << LSHIFT2));

  mpz_init (ds);
  mpf_inits (a, b, x, NULL);

  for (test = 0; test < reps; test++)
    {
      mpz_urandomb (ds, rands, HIBITS);
      hibits = mpz_get_ui (ds) + 1;
      mpz_urandomb (ds, rands, hibits);
      mpz_setbit (ds, hibits  - 1);	/* make sure msb is set */
      mpf_set_z (a, ds);
      mpf_set_z (b, ds);

      mpz_urandomb (ds, rands, LSHIFT1);
      lshift1 = mpz_get_ui (ds);
      mpf_mul_2exp (a, a, lshift1 + 1);
      mpf_mul_2exp (b, b, lshift1 + 1);
      mpf_add_ui (a, a, 1);	/* make a one-bit difference */

      mpz_urandomb (ds, rands, LSHIFT2);
      lshift2 = mpz_get_ui (ds);
      mpf_mul_2exp (a, a, lshift2);
      mpf_mul_2exp (b, b, lshift2);
      mpz_urandomb (ds, rands, lshift2);
      mpf_set_z (x, ds);
      mpf_add (a, a, x);
      mpf_add (b, b, x);

      insert_random_low_zero_limbs (a, rands);
      insert_random_low_zero_limbs (b, rands);

      if (mpf_eq (a, b, lshift1 + hibits) == 0)
	{
	  dump_abort (a, b, lshift1 + hibits, lshift1, lshift2, hibits, 1, test);
	}
      for (xtra = 1; xtra < 100; xtra++)
	if (mpf_eq (a, b, lshift1 + hibits + xtra) != 0)
	  {
	    dump_abort (a, b, lshift1 + hibits + xtra, lshift1, lshift2, hibits, 0, test);
	  }
    }

  mpf_clears (a, b, x, NULL);
  mpz_clear (ds);
  tests_end ();
  exit (0);
}

void
insert_random_low_zero_limbs (mpf_t x, gmp_randstate_ptr rands)
{
  mp_size_t max = PREC(x) - SIZ(x);
  mp_size_t s;
  mpz_t ds; mpz_init (ds);
  mpz_urandomb (ds, rands, 32);
  s = mpz_get_ui (ds) % (max + 1);
  MPN_COPY_DECR (PTR(x) + s, PTR(x), SIZ(x));
  MPN_ZERO (PTR(x), s);
  SIZ(x) += s;
  mpz_clear (ds);
}

void
dump_abort (mpf_t a, mpf_t b, int cmp_prec, int lshift1, int lshift2, int hibits, int want, long test)
{
  printf ("ERROR in test %ld\n", test);
  printf ("want %d got %d from mpf_eq\n", want, 1-want);
  printf ("cmp_prec = %d\n", cmp_prec);
  printf ("lshift1 = %d\n", lshift1);
  printf ("lshift2 = %d\n", lshift2);
  printf ("hibits = %d\n", hibits);
  hexdump (a); puts ("");
  hexdump (b); puts ("");
  abort ();
}

void
hexdump (mpf_t x)
{
  mp_size_t i;
  for (i = ABSIZ(x) - 1; i >= 0; i--)
    {
      gmp_printf ("%0*MX", SZ, PTR(x)[i]);
      if (i != 0)
	printf (" ");
    }
}
