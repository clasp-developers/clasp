/* Test mpf_set.

Copyright 2004 Free Software Foundation, Inc.

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
check_reuse (void)
{
  /* Try mpf_set(f,f) when f is bigger than prec.  In the past this had
     resulted in an MPN_COPY with invalid operand overlap. */
  mpf_t  f;
  mp_size_t      limbs = 20;
  unsigned long  bits = limbs * GMP_NUMB_BITS;
  mpf_init2 (f, bits);
  refmpf_fill (f, limbs, GMP_NUMB_MAX);
  mpf_set_prec_raw (f, bits / 2);
  mpf_set (f, f);
  MPF_CHECK_FORMAT (f);
  mpf_set_prec_raw (f, bits);
  mpf_clear (f);
}

int
main (void)
{
  tests_start ();

  check_reuse ();

  tests_end ();
  exit (0);
}
