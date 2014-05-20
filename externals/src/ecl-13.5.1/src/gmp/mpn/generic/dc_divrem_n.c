/* mpn_dc_divrem_n and auxilliary routines.

   THE FUNCTIONS IN THIS FILE ARE INTERNAL FUNCTIONS WITH MUTABLE
   INTERFACES.  IT IS ONLY SAFE TO REACH THEM THROUGH DOCUMENTED INTERFACES.
   IN FACT, IT IS ALMOST GUARANTEED THAT THEY'LL CHANGE OR DISAPPEAR IN A
   FUTURE GNU MP RELEASE.


Copyright 2000, 2001, 2002, 2004, 2005 Free Software Foundation, Inc.
Contributed by Paul Zimmermann.

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

#include "gmp.h"
#include "gmp-impl.h"

/*
[1] Fast Recursive Division, by Christoph Burnikel and Joachim Ziegler,
    Technical report MPI-I-98-1-022, october 1998.
    http://www.mpi-sb.mpg.de/~ziegler/TechRep.ps.gz
*/

static mp_limb_t mpn_dc_div_3_by_2
  _PROTO ((mp_ptr qp, mp_ptr np, mp_srcptr dp, mp_size_t n, mp_ptr scratch));
static mp_limb_t mpn_dc_div_2_by_1
  _PROTO ((mp_ptr qp, mp_ptr np, mp_srcptr dp, mp_size_t n, mp_ptr scratch));

/* mpn_dc_divrem_n - Implements algorithm of page 8 in [1]: divides (np,2n)
   by (dp,n) and puts the quotient in (qp,n), the remainder in (np,n).
   Returns most significant limb of the quotient, which is 0 or 1.
   Requires that the most significant bit of the divisor is set.  */

mp_limb_t
mpn_dc_divrem_n (mp_ptr qp, mp_ptr np, mp_srcptr dp, mp_size_t n)
{
  mp_limb_t ret;
  mp_ptr scratch;
  TMP_DECL;
  TMP_MARK;

  scratch = TMP_ALLOC_LIMBS (n);
  ret = mpn_dc_div_2_by_1 (qp, np, dp, n, scratch);

  TMP_FREE;
  return ret;
}

static mp_limb_t
mpn_dc_div_2_by_1 (mp_ptr qp, mp_ptr np, mp_srcptr dp, mp_size_t n, mp_ptr scratch)
{
  mp_limb_t qhl, cc;
  mp_size_t n2 = n/2;

  if (n % 2 != 0)
    {
      mp_ptr qp1 = qp + 1;
      qhl = mpn_dc_div_3_by_2 (qp1 + n2, np + 2 + n2, dp + 1, n2, scratch);
      qhl += mpn_add_1 (qp1 + n2, qp1 + n2, n2,
			mpn_dc_div_3_by_2 (qp1, np + 2, dp + 1, n2, scratch));

      cc = mpn_submul_1 (np + 1, qp1, n - 1, dp[0]);
      cc = mpn_sub_1 (np + n, np + n, 1, cc);
      if (qhl != 0)
	cc += mpn_sub_1 (np + n, np + n, 1, dp[0]);
      while (cc != 0)
	{
	  qhl -= mpn_sub_1 (qp1, qp1, n - 1, (mp_limb_t) 1);
	  cc -= mpn_add_n (np + 1, np + 1, dp, n);
	}
      qhl += mpn_add_1 (qp1, qp1, n - 1,
			mpn_sb_divrem_mn (qp, np, n + 1, dp, n));
    }
  else
    {
      qhl = mpn_dc_div_3_by_2 (qp + n2, np + n2, dp, n2, scratch);
      qhl += mpn_add_1 (qp + n2, qp + n2, n2,
			mpn_dc_div_3_by_2 (qp, np, dp, n2, scratch));
    }
  return qhl;
}


/* divides (np, 3n) by (dp, 2n) and puts the quotient in (qp, n),
   the remainder in (np, 2n) */

static mp_limb_t
mpn_dc_div_3_by_2 (mp_ptr qp, mp_ptr np, mp_srcptr dp, mp_size_t n, mp_ptr scratch)
{
  mp_size_t twon = n + n;
  mp_limb_t qhl, cc;

  if (n < DIV_DC_THRESHOLD)
    qhl = mpn_sb_divrem_mn (qp, np + n, twon, dp + n, n);
  else
    qhl = mpn_dc_div_2_by_1 (qp, np + n, dp + n, n, scratch);

  mpn_mul_n (scratch, qp, dp, n);
  cc = mpn_sub_n (np, np, scratch, twon);

  if (qhl != 0)
    cc += mpn_sub_n (np + n, np + n, dp, n);
  while (cc != 0)
    {
      qhl -= mpn_sub_1 (qp, qp, n, (mp_limb_t) 1);
      cc -= mpn_add_n (np, np, dp, twon);
    }
  return qhl;
}
