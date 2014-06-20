/* mpz_divexact -- finds quotient when known that quot * den == num && den != 0.

Contributed to the GNU project by Niels Möller.

Copyright 1991, 1993, 1994, 1995, 1996, 1997, 1998, 2000, 2001, 2002, 2005,
2006, 2007, 2009 Free Software Foundation, Inc.

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


#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"

void
mpz_divexact (mpz_ptr quot, mpz_srcptr num, mpz_srcptr den)
{
  mp_ptr qp;
  mp_size_t qn;
  mp_srcptr np, dp;
  mp_size_t nn, dn;
  TMP_DECL;

#if WANT_ASSERT
  {
    mpz_t  rem;
    mpz_init (rem);
    mpz_tdiv_r (rem, num, den);
    ASSERT (SIZ(rem) == 0);
    mpz_clear (rem);
  }
#endif

  nn = ABSIZ (num);
  dn = ABSIZ (den);

  qn = nn - dn + 1;
  MPZ_REALLOC (quot, qn);

  if (nn < dn)
    {
      /* This special case avoids segfaults below when the function is
	 incorrectly called with |N| < |D|, N != 0.  It also handles the
	 well-defined case N = 0.  */
      SIZ(quot) = 0;
      return;
    }

  TMP_MARK;

  qp = PTR(quot);

  if (quot == num || quot == den)
    qp = TMP_ALLOC_LIMBS (qn);

  np = PTR(num);
  dp = PTR(den);

  mpn_divexact (qp, np, nn, dp, dn);
  MPN_NORMALIZE (qp, qn);

  SIZ(quot) = (SIZ(num) ^ SIZ(den)) >= 0 ? qn : -qn;

  if (qp != PTR(quot))
    MPN_COPY (PTR(quot), qp, qn);

  TMP_FREE;
}
