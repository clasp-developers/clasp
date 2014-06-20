/* mpz_jacobi, mpz_legendre, mpz_kronecker -- mpz/mpz Jacobi symbols.

Copyright 2000, 2001, 2002, 2005 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify it
under the terms of the GNU Lesser General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License
for more details.

You should have received a copy of the GNU Lesser General Public License along
with the GNU MP Library.  If not, see http://www.gnu.org/licenses/.  */

#include <stdio.h>
#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"


/* Change this to "#define TRACE(x) x" for some traces. */
#define TRACE(x)


#define MPN_RSHIFT_OR_COPY(dst,src,size,shift)                  \
  do {                                                          \
    if ((shift) != 0)                                           \
      {                                                         \
        ASSERT_NOCARRY (mpn_rshift (dst, src, size, shift));    \
        (size) -= ((dst)[(size)-1] == 0);                       \
      }                                                         \
    else                                                        \
      MPN_COPY (dst, src, size);                                \
  } while (0)


/* This code does triple duty as mpz_jacobi, mpz_legendre and mpz_kronecker.

   mpz_jacobi could assume b is odd, but the improvements from that seem
   small compared to other operations, and anything significant should be
   checked at run-time since we'd like odd b to go fast in mpz_kronecker
   too.

   mpz_legendre could assume b is an odd prime, but knowing this doesn't
   present any obvious benefits.  Result 0 wouldn't arise (unless "a" is a
   multiple of b), but the checking for that takes little time compared to
   other operations.

   The main loop is just a simple binary GCD with the jacobi symbol result
   tracked during the reduction.

   The special cases for a or b fitting in one limb let mod_1 or modexact_1
   get used, without any copying, and end up just as efficient as the mixed
   precision mpz_kronecker_ui etc.

   When tdiv_qr is called it's not necessary to make "a" odd or make a
   working copy of it, but tdiv_qr is going to be pretty slow so it's not
   worth bothering trying to save anything for that case.

   Enhancements:

   mpn_bdiv_qr should be used instead of mpn_tdiv_qr.

   Some sort of multi-step algorithm should be used.  The current subtract
   and shift for every bit is very inefficient.  Lehmer (per current gcdext)
   would need some low bits included in its calculation to apply the sign
   change for reciprocity.  Binary Lehmer keeps low bits to strip twos
   anyway, so might be better suited.  Maybe the accelerated GCD style k-ary
   reduction would work, if sign changes due to the extra factors it
   introduces can be accounted for (or maybe they can be ignored).  */


int
mpz_jacobi (mpz_srcptr a, mpz_srcptr b)
{
  mp_srcptr  asrcp, bsrcp;
  mp_size_t  asize, bsize;
  mp_ptr     ap, bp;
  mp_limb_t  alow, blow, ahigh, bhigh, asecond, bsecond;
  unsigned   atwos, btwos;
  int        result_bit1;
  TMP_DECL;

  TRACE (printf ("start asize=%d bsize=%d\n", SIZ(a), SIZ(b));
         mpz_trace (" a", a);
         mpz_trace (" b", b));

  asize = SIZ(a);
  asrcp = PTR(a);
  alow = asrcp[0];

  bsize = SIZ(b);
  if (bsize == 0)
    return JACOBI_LS0 (alow, asize);  /* (a/0) */

  bsrcp = PTR(b);
  blow = bsrcp[0];

  if (asize == 0)
    return JACOBI_0LS (blow, bsize);  /* (0/b) */

  /* (even/even)=0 */
  if (((alow | blow) & 1) == 0)
    return 0;

  /* account for effect of sign of b, then ignore it */
  result_bit1 = JACOBI_BSGN_SS_BIT1 (asize, bsize);
  bsize = ABS (bsize);

  /* low zero limbs on b can be discarded */
  JACOBI_STRIP_LOW_ZEROS (result_bit1, alow, bsrcp, bsize, blow);

  count_trailing_zeros (btwos, blow);
  TRACE (printf ("b twos %u\n", btwos));

  /* establish shifted blow */
  blow >>= btwos;
  if (bsize > 1)
    {
      bsecond = bsrcp[1];
      if (btwos != 0)
        blow |= (bsecond << (GMP_NUMB_BITS - btwos)) & GMP_NUMB_MASK;
    }

  /* account for effect of sign of a, then ignore it */
  result_bit1 ^= JACOBI_ASGN_SU_BIT1 (asize, blow);
  asize = ABS (asize);

  if (bsize == 1 || (bsize == 2 && (bsecond >> btwos) == 0))
    {
      /* special case one limb b, use modexact and no copying */

      /* (a/2)=(2/a) with a odd, and if b is even then a is odd here */
      result_bit1 ^= JACOBI_TWOS_U_BIT1 (btwos, alow);

      if (blow == 1)   /* (a/1)=1 always */
        return JACOBI_BIT1_TO_PN (result_bit1);

      JACOBI_MOD_OR_MODEXACT_1_ODD (result_bit1, alow, asrcp, asize, blow);
      TRACE (printf ("base (%lu/%lu) with %d\n",
                     alow, blow, JACOBI_BIT1_TO_PN (result_bit1)));
      return mpn_jacobi_base (alow, blow, result_bit1);
    }

  /* Discard low zero limbs of a.  Usually there won't be anything to
     strip, hence not bothering with it for the bsize==1 case.  */
  JACOBI_STRIP_LOW_ZEROS (result_bit1, blow, asrcp, asize, alow);

  count_trailing_zeros (atwos, alow);
  TRACE (printf ("a twos %u\n", atwos));
  result_bit1 ^= JACOBI_TWOS_U_BIT1 (atwos, blow);

  /* establish shifted alow */
  alow >>= atwos;
  if (asize > 1)
    {
      asecond = asrcp[1];
      if (atwos != 0)
        alow |= (asecond << (GMP_NUMB_BITS - atwos)) & GMP_NUMB_MASK;
    }

  /* (a/2)=(2/a) with a odd */
  result_bit1 ^= JACOBI_TWOS_U_BIT1 (btwos, alow);

  if (asize == 1 || (asize == 2 && (asecond >> atwos) == 0))
    {
      /* another special case with modexact and no copying */

      if (alow == 1)  /* (1/b)=1 always */
        return JACOBI_BIT1_TO_PN (result_bit1);

      /* b still has its twos, so cancel out their effect */
      result_bit1 ^= JACOBI_TWOS_U_BIT1 (btwos, alow);

      result_bit1 ^= JACOBI_RECIP_UU_BIT1 (alow, blow);  /* now (b/a) */
      JACOBI_MOD_OR_MODEXACT_1_ODD (result_bit1, blow, bsrcp, bsize, alow);
      TRACE (printf ("base (%lu/%lu) with %d\n",
                     blow, alow, JACOBI_BIT1_TO_PN (result_bit1)));
      return mpn_jacobi_base (blow, alow, result_bit1);
    }


  TMP_MARK;
  TMP_ALLOC_LIMBS_2 (ap, asize, bp, bsize);

  MPN_RSHIFT_OR_COPY (ap, asrcp, asize, atwos);
  ASSERT (alow == ap[0]);
  TRACE (mpn_trace ("stripped a", ap, asize));

  MPN_RSHIFT_OR_COPY (bp, bsrcp, bsize, btwos);
  ASSERT (blow == bp[0]);
  TRACE (mpn_trace ("stripped b", bp, bsize));

  /* swap if necessary to make a longer than b */
  if (asize < bsize)
    {
      TRACE (printf ("swap\n"));
      MPN_PTR_SWAP (ap,asize, bp,bsize);
      MP_LIMB_T_SWAP (alow, blow);
      result_bit1 ^= JACOBI_RECIP_UU_BIT1 (alow, blow);
    }

  /* If a is bigger than b then reduce to a mod b.
     Division is much faster than chipping away at "a" bit-by-bit. */
  if (asize > bsize)
    {
      mp_ptr  rp, qp;

      TRACE (printf ("tdiv_qr asize=%ld bsize=%ld\n", asize, bsize));

      TMP_ALLOC_LIMBS_2 (rp, bsize, qp, asize-bsize+1);
      mpn_tdiv_qr (qp, rp, (mp_size_t) 0, ap, asize, bp, bsize);
      ap = rp;
      asize = bsize;
      MPN_NORMALIZE (ap, asize);

      TRACE (printf ("tdiv_qr asize=%ld bsize=%ld\n", asize, bsize);
             mpn_trace (" a", ap, asize);
             mpn_trace (" b", bp, bsize));

      if (asize == 0)  /* (0/b)=0 for b!=1 */
        goto zero;

      alow = ap[0];
      goto strip_a;
    }

  for (;;)
    {
      ASSERT (asize >= 1);         /* a,b non-empty */
      ASSERT (bsize >= 1);
      ASSERT (ap[asize-1] != 0);   /* a,b normalized (and hence non-zero) */
      ASSERT (bp[bsize-1] != 0);
      ASSERT (alow == ap[0]);      /* low limb copies should be correct */
      ASSERT (blow == bp[0]);
      ASSERT (alow & 1);           /* a,b odd */
      ASSERT (blow & 1);

      TRACE (printf ("top asize=%ld bsize=%ld\n", asize, bsize);
             mpn_trace (" a", ap, asize);
             mpn_trace (" b", bp, bsize));

      /* swap if necessary to make a>=b, applying reciprocity
         high limbs are almost always enough to tell which is bigger */
      if (asize < bsize
          || (asize == bsize
              && ((ahigh=ap[asize-1]) < (bhigh=bp[asize-1])
                  || (ahigh == bhigh
                      && mpn_cmp (ap, bp, asize-1) < 0))))
        {
          TRACE (printf ("swap\n"));
          MPN_PTR_SWAP (ap,asize, bp,bsize);
          MP_LIMB_T_SWAP (alow, blow);
          result_bit1 ^= JACOBI_RECIP_UU_BIT1 (alow, blow);
        }

      if (asize == 1)
        break;

      /* a = a-b */
      ASSERT (asize >= bsize);
      ASSERT_NOCARRY (mpn_sub (ap, ap, asize, bp, bsize));
      MPN_NORMALIZE (ap, asize);
      alow = ap[0];

      /* (0/b)=0 for b!=1.  b!=1 when a==0 because otherwise would have had
         a==1 which is asize==1 and would have exited above.  */
      if (asize == 0)
        goto zero;

    strip_a:
      /* low zero limbs on a can be discarded */
      JACOBI_STRIP_LOW_ZEROS (result_bit1, blow, ap, asize, alow);

      if ((alow & 1) == 0)
        {
          /* factors of 2 from a */
          unsigned  twos;
          count_trailing_zeros (twos, alow);
          TRACE (printf ("twos %u\n", twos));
          result_bit1 ^= JACOBI_TWOS_U_BIT1 (twos, blow);
          ASSERT_NOCARRY (mpn_rshift (ap, ap, asize, twos));
          asize -= (ap[asize-1] == 0);
          alow = ap[0];
        }
    }

  ASSERT (asize == 1 && bsize == 1);  /* just alow and blow left */
  TMP_FREE;

  /* (1/b)=1 always (in this case have b==1 because a>=b) */
  if (alow == 1)
    return JACOBI_BIT1_TO_PN (result_bit1);

  /* swap with reciprocity and do (b/a) */
  result_bit1 ^= JACOBI_RECIP_UU_BIT1 (alow, blow);
  TRACE (printf ("base (%lu/%lu) with %d\n",
                 blow, alow, JACOBI_BIT1_TO_PN (result_bit1)));
  return mpn_jacobi_base (blow, alow, result_bit1);

 zero:
  TMP_FREE;
  return 0;
}
