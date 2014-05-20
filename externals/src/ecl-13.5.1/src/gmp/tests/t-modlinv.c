/* Test modlimb_invert.

Copyright 2000, 2001, 2002, 2003 Free Software Foundation, Inc.

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
#include <string.h>
#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"
#include "tests.h"


void
one (mp_limb_t n)
{
  mp_limb_t  inv, prod;

  modlimb_invert (inv, n);
  prod = (inv * n) & GMP_NUMB_MASK;
  if (prod != 1)
    {
      printf ("modlimb_invert wrong\n");
      mp_limb_trace ("  n       ", n);
      mp_limb_trace ("  got     ", inv);
      mp_limb_trace ("  product ", prod);
      abort ();
    }
}

void
some (void)
{
  int  i;
  for (i = 0; i < 10000; i++)
    one (refmpn_random_limb () | 1);
}

void
all (void)
{
  mp_limb_t  n;

  n = 1;
  do {
    one (n);
    n += 2;
  } while (n != 1);
}


int
main (int argc, char *argv[])
{
  tests_start ();

  if (argc >= 2 && strcmp (argv[1], "-a") == 0)
    {
      /* it's feasible to run all values on a 32-bit limb, but not a 64-bit */
      all ();
    }
  else
    {
      some ();
    }

  tests_end ();
  exit (0);
}
