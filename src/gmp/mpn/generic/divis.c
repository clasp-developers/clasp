/* mpn_divisible_p -- mpn by mpn divisibility test

   THE FUNCTIONS IN THIS FILE ARE FOR INTERNAL USE ONLY.  THEY'RE ALMOST
   CERTAIN TO BE SUBJECT TO INCOMPATIBLE CHANGES OR DISAPPEAR COMPLETELY IN
   FUTURE GNU MP RELEASES.

Copyright 2001, 2002, 2005 Free Software Foundation, Inc.

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
#include "longlong.h"


/* Determine whether {ap,asize} is divisible by {dp,dsize}.  Must have both
   operands normalized, meaning high limbs non-zero, except that asize==0 is
   allowed.

   There usually won't be many low zero bits on d, but the checks for this
   are fast and might pick up a few operand combinations, in particular they
   might reduce d to fit the single-limb mod_1/modexact_1 code.

   Future:

   This is currently not much faster than the user doing an mpz_tdiv_r
   and testing for a zero remainder, but hopefully it can be improved.

   mpn_bdivmod is one possibility, but it only trades udiv_qrnnd's for
   multiplies, it won't save crossproducts the way it can in mpz_divexact.
   Definitely worthwhile on small operands for most processors, but a
   sub-quadratic version will be wanted before it can be used on all sizes.

   Getting the remainder limb by limb would make an early exit possible on
   finding a non-zero.  This would probably have to be bdivmod style so
   there's no addback, but it would need a multi-precision inverse and so
   might be slower than the plain method (on small sizes at least).

   When d must be normalized (shifted to high bit set), it's possible to
   just append a low zero limb to "a" rather than bit-shifting as
   mpn_tdiv_qr does internally, so long as it's already been checked that a
   has at least as many trailing zeros bits as d.  Or equivalently, pass
   qxn==1 to mpn_tdiv_qr, if/when it accepts that.

   When called from mpz_congruent_p, {ap,asize} is a temporary which can be
   destroyed.  Maybe it'd be possible to get into mpn_tdiv_qr at a lower
   level to save copying it, or maybe that function could accept rp==ap.

   Could use __attribute__ ((regparm (2))) on i386, so the parameters
   wouldn't need extra stack when called from mpz_divisible_p, but a
   pre-release gcc 3 didn't generate particularly good register juggling in
   that case, so this isn't done for now.  */

int
mpn_divisible_p (mp_srcptr ap, mp_size_t asize,
		 mp_srcptr dp, mp_size_t dsize)
{
  mp_limb_t  alow, dlow, dmask;
  mp_ptr     qp, rp;
  mp_size_t  i;
  TMP_DECL;

  ASSERT (asize >= 0);
  ASSERT (asize == 0 || ap[asize-1] != 0);
  ASSERT (dsize >= 1);
  ASSERT (dp[dsize-1] != 0);
  ASSERT_MPN (ap, asize);
  ASSERT_MPN (dp, dsize);

  /* When a<d only a==0 is divisible.
     Notice this test covers all cases of asize==0. */
  if (asize < dsize)
    return (asize == 0);

  /* Strip low zero limbs from d, requiring a==0 on those. */
  for (;;)
    {
      alow = *ap;
      dlow = *dp;

      if (dlow != 0)
	break;

      if (alow != 0)
	return 0;  /* a has fewer low zero limbs than d, so not divisible */

      /* a!=0 and d!=0 so won't get to size==0 */
      asize--; ASSERT (asize >= 1);
      dsize--; ASSERT (dsize >= 1);
      ap++;
      dp++;
    }

  /* a must have at least as many low zero bits as d */
  dmask = LOW_ZEROS_MASK (dlow);
  if ((alow & dmask) != 0)
    return 0;

  if (dsize == 1)
    {
      if (BELOW_THRESHOLD (asize, MODEXACT_1_ODD_THRESHOLD))
	return mpn_mod_1 (ap, asize, dlow) == 0;

      if ((dlow & 1) == 0)
	{
	  unsigned  twos;
	  count_trailing_zeros (twos, dlow);
	  dlow >>= twos;
	}
      return mpn_modexact_1_odd (ap, asize, dlow) == 0;
    }

  if (dsize == 2)
    {
      mp_limb_t  dsecond = dp[1];
      if (dsecond <= dmask)
	{
	  unsigned  twos;
	  count_trailing_zeros (twos, dlow);
	  dlow = (dlow >> twos) | (dsecond << (GMP_NUMB_BITS-twos));
          ASSERT_LIMB (dlow);
	  return MPN_MOD_OR_MODEXACT_1_ODD (ap, asize, dlow) == 0;
	}
    }

  TMP_MARK;

  rp = TMP_ALLOC_LIMBS (asize+1);
  qp = rp + dsize;

  mpn_tdiv_qr (qp, rp, (mp_size_t) 0, ap, asize, dp, dsize);

  /* test for {rp,dsize} zero or non-zero */
  i = 0;
  do
    {
      if (rp[i] != 0)
	{
	  TMP_FREE;
	  return 0;
	}
    }
  while (++i < dsize);

  TMP_FREE;
  return 1;
}
