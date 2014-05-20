/* Exercise mpz_bin_ui and mpz_bin_uiui.

Copyright 2000, 2001 Free Software Foundation, Inc.

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
try_mpz_bin_ui (mpz_srcptr want, mpz_srcptr n, unsigned long k)
{
  mpz_t  got;

  mpz_init (got);
  mpz_bin_ui (got, n, k);
  MPZ_CHECK_FORMAT (got);
  if (mpz_cmp (got, want) != 0)
    {
      printf ("mpz_bin_ui wrong\n");
      printf ("  n="); mpz_out_str (stdout, 10, n); printf ("\n");
      printf ("  k=%lu\n", k);
      printf ("  got="); mpz_out_str (stdout, 10, got); printf ("\n");
      printf ("  want="); mpz_out_str (stdout, 10, want); printf ("\n");
      abort();
    }
  mpz_clear (got);
}


void
try_mpz_bin_uiui (mpz_srcptr want, unsigned long n, unsigned long k)
{
  mpz_t  got;

  mpz_init (got);
  mpz_bin_uiui (got, n, k);
  MPZ_CHECK_FORMAT (got);
  if (mpz_cmp (got, want) != 0)
    {
      printf ("mpz_bin_uiui wrong\n");
      printf ("  n=%lu\n", n);
      printf ("  k=%lu\n", k);
      printf ("  got="); mpz_out_str (stdout, 10, got); printf ("\n");
      printf ("  want="); mpz_out_str (stdout, 10, want); printf ("\n");
      abort();
    }
  mpz_clear (got);
}


void
samples (void)
{
  static const struct {
    const char     *n;
    unsigned long  k;
    const char     *want;
  } data[] = {

    {   "0",  0, "1"   },
    {   "0",  1, "0"   },
    {   "0",  2, "0"   },
    {   "0",  3, "0"   },
    {   "0",  4, "0"   },
    {   "0", 123456, "0" },

    {   "1",  0, "1"   },
    {   "1",  1, "1"   },
    {   "1",  2, "0"   },
    {   "1",  3, "0"   },
    {   "1",  4, "0"   },
    {   "1", 123456, "0" },

    {   "2",  0, "1"   },
    {   "2",  1, "2"   },
    {   "2",  2, "1"   },
    {   "2",  3, "0"   },
    {   "2",  4, "0"   },
    {   "2", 123456, "0" },

    {   "3",  0, "1"   },
    {   "3",  1, "3"   },
    {   "3",  2, "3"   },
    {   "3",  3, "1"   },
    {   "3",  4, "0"   },
    {   "3",  5, "0"   },
    {   "3", 123456, "0" },

    {   "4",  0, "1"   },
    {   "4",  1, "4"   },
    {   "4",  2, "6"   },
    {   "4",  3, "4"   },
    {   "4",  4, "1"   },
    {   "4",  5, "0"   },
    {   "4",  6, "0"   },
    {   "4", 123456, "0" },

    {   "10",  0, "1"   },
    {   "10",  1, "10"  },
    {   "10",  2, "45"  },
    {   "10",  3, "120" },
    {   "10",  4, "210" },
    {   "10",  5, "252" },
    {   "10",  6, "210" },
    {   "10",  7, "120" },
    {   "10",  8, "45"  },
    {   "10",  9, "10"  },
    {   "10", 10, "1"   },
    {   "10", 11,     "0" },
    {   "10", 12,     "0" },
    {   "10", 123456, "0" },

    /* negatives, using bin(-n,k)=bin(n+k-1,k) */
    {   "-1",  0,  "1"  },
    {   "-1",  1, "-1"  },
    {   "-1",  2,  "1"  },
    {   "-1",  3, "-1"  },
    {   "-1",  4,  "1"  },

    {   "-2",  0,  "1"  },
    {   "-2",  1, "-2"  },
    {   "-2",  2,  "3"  },
    {   "-2",  3, "-4"  },
    {   "-2",  4,  "5"  },
    {   "-2",  5, "-6"  },
    {   "-2",  6,  "7"  },

    {   "-3",  0,   "1"  },
    {   "-3",  1,  "-3"  },
    {   "-3",  2,   "6"  },
    {   "-3",  3, "-10"  },
    {   "-3",  4,  "15"  },
    {   "-3",  5, "-21"  },
    {   "-3",  6,  "28"  },

    {   "40", 20,  "137846528820" },
    {   "60", 30,  "118264581564861424" },
  };

  mpz_t  n, want;
  int    i;

  mpz_init (n);
  mpz_init (want);

  for (i = 0; i < numberof (data); i++)
    {
      mpz_set_str_or_abort (n, data[i].n, 0);
      mpz_set_str_or_abort (want, data[i].want, 0);

      try_mpz_bin_ui (want, n, data[i].k);

      if (mpz_fits_ulong_p (n))
	try_mpz_bin_uiui (want, mpz_get_ui (n), data[i].k);
    }

  mpz_clear (n);
  mpz_clear (want);
}


/* Test some bin(2k,k) cases.  This produces some biggish numbers to
   exercise the limb accumulating code.  */
void
twos (void)
{
  mpz_t          n, want;
  unsigned long  k;

  mpz_init (n);
  mpz_init (want);

  mpz_set_ui (want, (unsigned long) 2);
  for (k = 1; k < 200; k++)
    {
      mpz_set_ui (n, 2*k);
      try_mpz_bin_ui (want, n, k);

      try_mpz_bin_uiui (want, 2*k, k);

      mpz_mul_ui (want, want, 2*(2*k+1));
      mpz_fdiv_q_ui (want, want, k+1);
    }

  mpz_clear (n);
  mpz_clear (want);
}


int
main (void)
{
  tests_start ();

  samples ();
  twos ();

  tests_end ();
  exit (0);
}
