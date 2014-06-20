/* mpz_tdiv_qr(quot,rem,dividend,divisor) -- Set QUOT to DIVIDEND/DIVISOR,
   and REM to DIVIDEND mod DIVISOR.

Copyright 1991, 1993, 1994, 2000, 2001, 2005 Free Software Foundation, Inc.

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
#ifdef BERKELEY_MP
#include "mp.h"
#endif

void
#ifndef BERKELEY_MP
mpz_tdiv_qr (mpz_ptr quot, mpz_ptr rem, mpz_srcptr num, mpz_srcptr den)
#else /* BERKELEY_MP */
mdiv (mpz_srcptr num, mpz_srcptr den, mpz_ptr quot, mpz_ptr rem)
#endif /* BERKELEY_MP */
{
  mp_size_t ql;
  mp_size_t ns, ds, nl, dl;
  mp_ptr np, dp, qp, rp;
  TMP_DECL;

  ns = SIZ (num);
  ds = SIZ (den);
  nl = ABS (ns);
  dl = ABS (ds);
  ql = nl - dl + 1;

  if (dl == 0)
    DIVIDE_BY_ZERO;

  MPZ_REALLOC (rem, dl);

  if (ql <= 0)
    {
      if (num != rem)
	{
	  mp_ptr np, rp;
	  np = PTR (num);
	  rp = PTR (rem);
	  MPN_COPY (rp, np, nl);
	  SIZ (rem) = SIZ (num);
	}
      /* This needs to follow the assignment to rem, in case the
	 numerator and quotient are the same.  */
      SIZ (quot) = 0;
      return;
    }

  MPZ_REALLOC (quot, ql);

  TMP_MARK;
  qp = PTR (quot);
  rp = PTR (rem);
  np = PTR (num);
  dp = PTR (den);

  /* FIXME: We should think about how to handle the temporary allocation.
     Perhaps mpn_tdiv_qr should handle it, since it anyway often needs to
     allocate temp space.  */

  /* Copy denominator to temporary space if it overlaps with the quotient
     or remainder.  */
  if (dp == rp || dp == qp)
    {
      mp_ptr tp;
      tp = TMP_ALLOC_LIMBS (dl);
      MPN_COPY (tp, dp, dl);
      dp = tp;
    }
  /* Copy numerator to temporary space if it overlaps with the quotient or
     remainder.  */
  if (np == rp || np == qp)
    {
      mp_ptr tp;
      tp = TMP_ALLOC_LIMBS (nl);
      MPN_COPY (tp, np, nl);
      np = tp;
    }

  mpn_tdiv_qr (qp, rp, 0L, np, nl, dp, dl);

  ql -=  qp[ql - 1] == 0;
  MPN_NORMALIZE (rp, dl);

  SIZ (quot) = (ns ^ ds) >= 0 ? ql : -ql;
  SIZ (rem) = ns >= 0 ? dl : -dl;
  TMP_FREE;
}
