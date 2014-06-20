/* Factoring with Pollard's rho method.

Copyright 1995, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2005, 2009
Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see http://www.gnu.org/licenses/.  */


#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "gmp.h"

int flag_verbose = 0;

static unsigned add[] = {4, 2, 4, 2, 4, 6, 2, 6};

void
factor_using_division (mpz_t t, unsigned int limit)
{
  mpz_t q, r;
  unsigned long int f;
  int ai;
  unsigned *addv = add;
  unsigned int failures;

  if (flag_verbose > 0)
    {
      printf ("[trial division (%u)] ", limit);
      fflush (stdout);
    }

  mpz_init (q);
  mpz_init (r);

  f = mpz_scan1 (t, 0);
  mpz_div_2exp (t, t, f);
  while (f)
    {
      printf ("2 ");
      fflush (stdout);
      --f;
    }

  for (;;)
    {
      mpz_tdiv_qr_ui (q, r, t, 3);
      if (mpz_cmp_ui (r, 0) != 0)
	break;
      mpz_set (t, q);
      printf ("3 ");
      fflush (stdout);
    }

  for (;;)
    {
      mpz_tdiv_qr_ui (q, r, t, 5);
      if (mpz_cmp_ui (r, 0) != 0)
	break;
      mpz_set (t, q);
      printf ("5 ");
      fflush (stdout);
    }

  failures = 0;
  f = 7;
  ai = 0;
  while (mpz_cmp_ui (t, 1) != 0)
    {
      mpz_tdiv_qr_ui (q, r, t, f);
      if (mpz_cmp_ui (r, 0) != 0)
	{
	  f += addv[ai];
	  if (mpz_cmp_ui (q, f) < 0)
	    break;
	  ai = (ai + 1) & 7;
	  failures++;
	  if (failures > limit)
	    break;
	}
      else
	{
	  mpz_swap (t, q);
	  printf ("%lu ", f);
	  fflush (stdout);
	  failures = 0;
	}
    }

  mpz_clears (q, r, NULL);
}

void
factor_using_division_2kp (mpz_t t, unsigned int limit, unsigned long p)
{
  mpz_t r;
  mpz_t f;
  unsigned int k;

  if (flag_verbose > 0)
    {
      printf ("[trial division (%u)] ", limit);
      fflush (stdout);
    }

  mpz_init (r);
  mpz_init_set_ui (f, 2 * p);
  mpz_add_ui (f, f, 1);
  for (k = 1; k < limit; k++)
    {
      mpz_tdiv_r (r, t, f);
      while (mpz_cmp_ui (r, 0) == 0)
	{
	  mpz_tdiv_q (t, t, f);
	  mpz_tdiv_r (r, t, f);
	  mpz_out_str (stdout, 10, f);
	  fflush (stdout);
	  fputc (' ', stdout);
	}
      mpz_add_ui (f, f, 2 * p);
    }

  mpz_clears (f, r, NULL);
}

void
factor_using_pollard_rho (mpz_t n, unsigned long a, unsigned long p)
{
  mpz_t x, x1, y, P;
  mpz_t t1, t2;
  unsigned long long k, l, i;

  if (flag_verbose > 0)
    {
      printf ("[pollard-rho (%lu)] ", a);
      fflush (stdout);
    }

  mpz_inits (t1, t2, NULL);
  mpz_init_set_si (y, 2);
  mpz_init_set_si (x, 2);
  mpz_init_set_si (x1, 2);
  mpz_init_set_ui (P, 1);
  k = 1;
  l = 1;

  while (mpz_cmp_ui (n, 1) != 0)
    {
      for (;;)
	{
	  do
	    {
	      if (p != 0)
		{
		  mpz_powm_ui (x, x, p, n);
		  mpz_add_ui (x, x, a);
		}
	      else
		{
		  mpz_mul (t1, x, x);
		  mpz_mod (x, t1, n);
		  mpz_add_ui (x, x, a);
		}

	      mpz_sub (t1, x1, x);
	      mpz_mul (t2, P, t1);
	      mpz_mod (P, t2, n);

	      if (k % 32 == 1)
		{
		  mpz_gcd (t1, P, n);
		  if (mpz_cmp_ui (t1, 1) != 0)
		    goto factor_found;
		  mpz_set (y, x);
		}
	    }
	  while (--k != 0);

	  mpz_gcd (t1, P, n);
	  if (mpz_cmp_ui (t1, 1) != 0)
	    goto factor_found;

	  mpz_set (x1, x);
	  k = l;
	  l = 2 * l;
	  for (i = 0; i < k; i++)
	    {
	      if (p != 0)
		{
		  mpz_powm_ui (x, x, p, n);
		  mpz_add_ui (x, x, a);
		}
	      else
		{
		  mpz_mul (t1, x, x);
		  mpz_mod (x, t1, n);
		  mpz_add_ui (x, x, a);
		}
	    }
	  mpz_set (y, x);
	}

    factor_found:
      do
	{
	  if (p != 0)
	    {
	      mpz_powm_ui (y, y, p, n); mpz_add_ui (y, y, a);
	    }
	  else
	    {
	      mpz_mul (t1, y, y);
	      mpz_mod (y, t1, n);
	      mpz_add_ui (y, y, a);
	    }
	  mpz_sub (t1, x1, y);
	  mpz_gcd (t1, t1, n);
	}
      while (mpz_cmp_ui (t1, 1) == 0);

      mpz_divexact (n, n, t1);	/* divide by t1, before t1 is overwritten */

      if (!mpz_probab_prime_p (t1, 25))
	{
	  do
	    {
	      mp_limb_t a_limb;
	      mpn_random (&a_limb, (mp_size_t) 1);
	      a = a_limb;
	    }
	  while (a == 0);

	  if (flag_verbose > 0)
	    {
	      printf ("[composite factor--restarting pollard-rho] ");
	      fflush (stdout);
	    }
	  factor_using_pollard_rho (t1, a, p);
	}
      else
	{
	  mpz_out_str (stdout, 10, t1);
	  fflush (stdout);
	  fputc (' ', stdout);
	}
      mpz_mod (x, x, n);
      mpz_mod (x1, x1, n);
      mpz_mod (y, y, n);
      if (mpz_probab_prime_p (n, 25))
	{
	  mpz_out_str (stdout, 10, n);
	  fflush (stdout);
	  fputc (' ', stdout);
	  break;
	}
    }

  mpz_clears (P, t2, t1, x1, x, y, NULL);
}

void
factor (mpz_t t, unsigned long p)
{
  unsigned int division_limit;

  if (mpz_sgn (t) == 0)
    return;

  /* Set the trial division limit according the size of t.  */
  division_limit = mpz_sizeinbase (t, 2);
  if (division_limit > 1000)
    division_limit = 1000 * 1000;
  else
    division_limit = division_limit * division_limit;

  if (p != 0)
    factor_using_division_2kp (t, division_limit / 10, p);
  else
    factor_using_division (t, division_limit);

  if (mpz_cmp_ui (t, 1) != 0)
    {
      if (flag_verbose > 0)
	{
	  printf ("[is number prime?] ");
	  fflush (stdout);
	}
      if (mpz_probab_prime_p (t, 25))
	mpz_out_str (stdout, 10, t);
      else
	factor_using_pollard_rho (t, 1L, p);
    }
}

int
main (int argc, char *argv[])
{
  mpz_t t;
  unsigned long p;
  int i;

  if (argc > 1 && !strcmp (argv[1], "-v"))
    {
      flag_verbose = 1;
      argv++;
      argc--;
    }
  if (argc > 1 && !strcmp (argv[1], "-q"))
    {
      flag_verbose = -1;
      argv++;
      argc--;
    }

  mpz_init (t);
  if (argc > 1)
    {
      p = 0;
      for (i = 1; i < argc; i++)
	{
	  if (!strncmp (argv[i], "-Mp", 3))
	    {
	      p = atoi (argv[i] + 3);
	      mpz_set_ui (t, 1);
	      mpz_mul_2exp (t, t, p);
	      mpz_sub_ui (t, t, 1);
	    }
	  else if (!strncmp (argv[i], "-2kp", 4))
	    {
	      p = atoi (argv[i] + 4);
	      continue;
	    }
	  else
	    {
	      mpz_set_str (t, argv[i], 0);
	    }

	  if (mpz_cmp_ui (t, 0) == 0)
	    puts ("-");
	  else
	    {
	      factor (t, p);
	      puts ("");
	    }
	}
    }
  else
    {
      for (;;)
	{
	  mpz_inp_str (t, stdin, 0);
	  if (feof (stdin))
	    break;
	  if (flag_verbose >= 0)
	    {
	      mpz_out_str (stdout, 10, t); printf (" = ");
	    }
	  factor (t, 0);
	  puts ("");
	}
    }

  exit (0);
}
