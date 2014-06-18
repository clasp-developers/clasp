/* mpn_gcdext -- Extended Greatest Common Divisor.

Copyright 1996, 1998, 2000, 2001, 2002 Free Software Foundation, Inc.

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
along with the GNU MP Library; see the file COPYING.LIB.  If not, write
to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA. */

#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"

#ifndef GCDEXT_THRESHOLD
#define GCDEXT_THRESHOLD 17
#endif

#ifndef EXTEND
#define EXTEND 1
#endif

#if STAT
int arr[GMP_LIMB_BITS + 1];
#endif


/* mpn_gcdext (GP, SP, SSIZE, UP, USIZE, VP, VSIZE)

   Compute the extended GCD of {UP,USIZE} and {VP,VSIZE} and store the
   greatest common divisor at GP (unless it is 0), and the first cofactor at
   SP.  Write the size of the cofactor through the pointer SSIZE.  Return the
   size of the value at GP.  Note that SP might be a negative number; this is
   denoted by storing the negative of the size through SSIZE.

   {UP,USIZE} and {VP,VSIZE} are both clobbered.

   The space allocation for all four areas needs to be USIZE+1.

   Preconditions: 1) U >= V.
		  2) V > 0.  */

/* We use Lehmer's algorithm.  The idea is to extract the most significant
   bits of the operands, and compute the continued fraction for them.  We then
   apply the gathered cofactors to the full operands.

   Idea 1: After we have performed a full division, don't shift operands back,
	   but instead account for the extra factors-of-2 thus introduced.
   Idea 2: Simple generalization to use divide-and-conquer would give us an
	   algorithm that runs faster than O(n^2).
   Idea 3: The input numbers need less space as the computation progresses,
	   while the s0 and s1 variables need more space.  To save memory, we
	   could make them share space, and have the latter variables grow
	   into the former.
   Idea 4: We should not do double-limb arithmetic from the start.  Instead,
	   do things in single-limb arithmetic until the quotients differ,
	   and then switch to double-limb arithmetic.  */


/* One-limb division optimized for small quotients.  */
static mp_limb_t
div1 (mp_limb_t n0, mp_limb_t d0)
{
  if ((mp_limb_signed_t) n0 < 0)
    {
      mp_limb_t q;
      int cnt;
      for (cnt = 1; (mp_limb_signed_t) d0 >= 0; cnt++)
	{
	  d0 = d0 << 1;
	}

      q = 0;
      while (cnt)
	{
	  q <<= 1;
	  if (n0 >= d0)
	    {
	      n0 = n0 - d0;
	      q |= 1;
	    }
	  d0 = d0 >> 1;
	  cnt--;
	}

      return q;
    }
  else
    {
      mp_limb_t q;
      int cnt;
      for (cnt = 0; n0 >= d0; cnt++)
	{
	  d0 = d0 << 1;
	}

      q = 0;
      while (cnt)
	{
	  d0 = d0 >> 1;
	  q <<= 1;
	  if (n0 >= d0)
	    {
	      n0 = n0 - d0;
	      q |= 1;
	    }
	  cnt--;
	}

      return q;
    }
}

/* Two-limb division optimized for small quotients.  */
static mp_limb_t
div2 (mp_limb_t n1, mp_limb_t n0, mp_limb_t d1, mp_limb_t d0)
{
  if ((mp_limb_signed_t) n1 < 0)
    {
      mp_limb_t q;
      int cnt;
      for (cnt = 1; (mp_limb_signed_t) d1 >= 0; cnt++)
	{
	  d1 = (d1 << 1) | (d0 >> (GMP_LIMB_BITS - 1));
	  d0 = d0 << 1;
	}

      q = 0;
      while (cnt)
	{
	  q <<= 1;
	  if (n1 > d1 || (n1 == d1 && n0 >= d0))
	    {
	      sub_ddmmss (n1, n0, n1, n0, d1, d0);
	      q |= 1;
	    }
	  d0 = (d1 << (GMP_LIMB_BITS - 1)) | (d0 >> 1);
	  d1 = d1 >> 1;
	  cnt--;
	}

      return q;
    }
  else
    {
      mp_limb_t q;
      int cnt;
      for (cnt = 0; n1 > d1 || (n1 == d1 && n0 >= d0); cnt++)
	{
	  d1 = (d1 << 1) | (d0 >> (GMP_LIMB_BITS - 1));
	  d0 = d0 << 1;
	}

      q = 0;
      while (cnt)
	{
	  d0 = (d1 << (GMP_LIMB_BITS - 1)) | (d0 >> 1);
	  d1 = d1 >> 1;
	  q <<= 1;
	  if (n1 > d1 || (n1 == d1 && n0 >= d0))
	    {
	      sub_ddmmss (n1, n0, n1, n0, d1, d0);
	      q |= 1;
	    }
	  cnt--;
	}

      return q;
    }
}

mp_size_t
#if EXTEND
mpn_gcdext (mp_ptr gp, mp_ptr s0p, mp_size_t *s0size,
	    mp_ptr up, mp_size_t size, mp_ptr vp, mp_size_t vsize)
#else
mpn_gcd (mp_ptr gp,
	 mp_ptr up, mp_size_t size, mp_ptr vp, mp_size_t vsize)
#endif
{
  mp_limb_t A, B, C, D;
  int cnt;
  mp_ptr tp, wp;
#if RECORD
  mp_limb_t max = 0;
#endif
#if EXTEND
  mp_ptr s1p;
  mp_ptr orig_s0p = s0p;
  mp_size_t ssize;
  int sign = 1;
#endif
  int use_double_flag;
  TMP_DECL;

  ASSERT (size >= vsize);
  ASSERT (vsize >= 1);
  ASSERT (up[size-1] != 0);
  ASSERT (vp[vsize-1] != 0);
  ASSERT (! MPN_OVERLAP_P (up, size+1, vp, vsize+1));
#if EXTEND
  ASSERT (! MPN_OVERLAP_P (s0p, size, up, size+1));
  ASSERT (! MPN_OVERLAP_P (s0p, size, vp, vsize+1));
#endif
  ASSERT (MPN_SAME_OR_SEPARATE_P (gp, up, size));
  ASSERT (MPN_SAME_OR_SEPARATE2_P (gp, size, vp, vsize));

  TMP_MARK;

  tp = (mp_ptr) TMP_ALLOC ((size + 1) * BYTES_PER_MP_LIMB);
  wp = (mp_ptr) TMP_ALLOC ((size + 1) * BYTES_PER_MP_LIMB);
#if EXTEND
  s1p = (mp_ptr) TMP_ALLOC ((size + 1) * BYTES_PER_MP_LIMB);

#if ! WANT_GCDEXT_ONE_STEP
  MPN_ZERO (s0p, size);
  MPN_ZERO (s1p, size);
#endif

  s0p[0] = 1;
  s1p[0] = 0;
  ssize = 1;
#endif

  if (size > vsize)
    {
      mpn_tdiv_qr (tp, up, (mp_size_t) 0, up, size, vp, vsize);

#if EXTEND
      /* This is really what it boils down to in this case... */
      s0p[0] = 0;
      s1p[0] = 1;
      sign = -sign;
#endif
      size = vsize;
      MP_PTR_SWAP (up, vp);
    }

  use_double_flag = ABOVE_THRESHOLD (size, GCDEXT_THRESHOLD);

  for (;;)
    {
      mp_limb_t asign;
      /* Figure out exact size of V.  */
      vsize = size;
      MPN_NORMALIZE (vp, vsize);
      if (vsize <= 1)
	break;

      if (use_double_flag)
	{
	  mp_limb_t uh, vh, ul, vl;
	  /* Let UH,UL be the most significant limbs of U, and let VH,VL be
	     the corresponding bits from V.  */
	  uh = up[size - 1];
	  vh = vp[size - 1];
	  ul = up[size - 2];
	  vl = vp[size - 2];
	  count_leading_zeros (cnt, uh);
#if GMP_NAIL_BITS == 0
	  if (cnt != 0)
	    {
	      uh = (uh << cnt) | (ul >> (GMP_LIMB_BITS - cnt));
	      vh = (vh << cnt) | (vl >> (GMP_LIMB_BITS - cnt));
	      vl <<= cnt;
	      ul <<= cnt;
	      if (size >= 3)
		{
		  ul |= (up[size - 3] >> (GMP_LIMB_BITS - cnt));
		  vl |= (vp[size - 3] >> (GMP_LIMB_BITS - cnt));
		}
	    }
#else
	  uh = uh << cnt;
	  vh = vh << cnt;
	  if (cnt < GMP_NUMB_BITS)
	    {			     /* GMP_NAIL_BITS <= cnt < GMP_NUMB_BITS */
	      uh |= ul >> (GMP_NUMB_BITS - cnt);
	      vh |= vl >> (GMP_NUMB_BITS - cnt);
	      ul <<= cnt + GMP_NAIL_BITS;
	      vl <<= cnt + GMP_NAIL_BITS;
	      if (size >= 3)
		{
		  if (cnt + GMP_NAIL_BITS > GMP_NUMB_BITS)
		    {
		      ul |= up[size - 3] << cnt + GMP_NAIL_BITS - GMP_NUMB_BITS;
		      vl |= vp[size - 3] << cnt + GMP_NAIL_BITS - GMP_NUMB_BITS;
		      if (size >= 4)
			{
			  ul |= up[size - 4] >> 2 * GMP_NUMB_BITS - GMP_NAIL_BITS - cnt;
			  vl |= vp[size - 4] >> 2 * GMP_NUMB_BITS - GMP_NAIL_BITS - cnt;
			}
		    }
		  else
		    {
		      ul |= up[size - 3] >> (GMP_LIMB_BITS - cnt - 2 * GMP_NAIL_BITS);
		      vl |= vp[size - 3] >> (GMP_LIMB_BITS - cnt - 2 * GMP_NAIL_BITS);
		    }
		}
	    }
	  else
	    {			  /* GMP_NUMB_BITS <= cnt <= GMP_LIMB_BITS-1 */
	      uh |= ul << cnt - GMP_NUMB_BITS;	/* 0 <= c <= GMP_NAIL_BITS-1 */
	      vh |= vl << cnt - GMP_NUMB_BITS;
	      if (size >= 3)
		{
		  if (cnt - GMP_NUMB_BITS != 0)
		    {				/* uh/vh need yet more bits! */
		      uh |= up[size - 3] >> 2 * GMP_NUMB_BITS - cnt;
		      vh |= vp[size - 3] >> 2 * GMP_NUMB_BITS - cnt;
		      ul = up[size - 3] << cnt + GMP_NAIL_BITS - GMP_NUMB_BITS;
		      vl = vp[size - 3] << cnt + GMP_NAIL_BITS - GMP_NUMB_BITS;
		      if (size >= 4)
			{
			  ul |= up[size - 4] >> 2 * GMP_NUMB_BITS - GMP_NAIL_BITS - cnt;
			  vl |= vp[size - 4] >> 2 * GMP_NUMB_BITS - GMP_NAIL_BITS - cnt;
			}
		    }
		  else
		    {
		      ul = up[size - 3] << GMP_LIMB_BITS - cnt;
		      vl = vp[size - 3] << GMP_LIMB_BITS - cnt;
		      if (size >= 4)
			{
			  ul |= up[size - 4] >> GMP_NUMB_BITS - (GMP_LIMB_BITS - cnt);
			  vl |= vp[size - 4] >> GMP_NUMB_BITS - (GMP_LIMB_BITS - cnt);
			}
		    }
		}
	      else
		{
		  ul = 0;
		  vl = 0;
		}
	    }
#endif

	  A = 1;
	  B = 0;
	  C = 0;
	  D = 1;

	  asign = 0;
	  for (;;)
	    {
	      mp_limb_t Tac, Tbd;
	      mp_limb_t q1, q2;
	      mp_limb_t nh, nl, dh, dl;
	      mp_limb_t t1, t0;
	      mp_limb_t Th, Tl;

	      sub_ddmmss (dh, dl, vh, vl, 0, C);
	      if (dh == 0)
		break;
	      add_ssaaaa (nh, nl, uh, ul, 0, A);
	      q1 = div2 (nh, nl, dh, dl);

	      add_ssaaaa (dh, dl, vh, vl, 0, D);
	      if (dh == 0)
		break;
	      sub_ddmmss (nh, nl, uh, ul, 0, B);
	      q2 = div2 (nh, nl, dh, dl);

	      if (q1 != q2)
		break;

	      Tac = A + q1 * C;
	      if (GMP_NAIL_BITS != 0 && Tac > GMP_NUMB_MAX)
		break;
	      Tbd = B + q1 * D;
	      if (GMP_NAIL_BITS != 0 && Tbd > GMP_NUMB_MAX)
		break;
	      A = C;
	      C = Tac;
	      B = D;
	      D = Tbd;
	      umul_ppmm (t1, t0, q1, vl);
	      t1 += q1 * vh;
	      sub_ddmmss (Th, Tl, uh, ul, t1, t0);
	      uh = vh, ul = vl;
	      vh = Th, vl = Tl;

	      asign = ~asign;

	      add_ssaaaa (dh, dl, vh, vl, 0, C);
/*	      if (dh == 0)	should never happen
		break;	       */
	      sub_ddmmss (nh, nl, uh, ul, 0, A);
	      q1 = div2 (nh, nl, dh, dl);

	      sub_ddmmss (dh, dl, vh, vl, 0, D);
	      if (dh == 0)
		break;
	      add_ssaaaa (nh, nl, uh, ul, 0, B);
	      q2 = div2 (nh, nl, dh, dl);

	      if (q1 != q2)
		break;

	      Tac = A + q1 * C;
	      if (GMP_NAIL_BITS != 0 && Tac > GMP_NUMB_MAX)
		break;
	      Tbd = B + q1 * D;
	      if (GMP_NAIL_BITS != 0 && Tbd > GMP_NUMB_MAX)
		break;
	      A = C;
	      C = Tac;
	      B = D;
	      D = Tbd;
	      umul_ppmm (t1, t0, q1, vl);
	      t1 += q1 * vh;
	      sub_ddmmss (Th, Tl, uh, ul, t1, t0);
	      uh = vh, ul = vl;
	      vh = Th, vl = Tl;

	      asign = ~asign;
	    }
#if EXTEND
	  if (asign)
	    sign = -sign;
#endif
	}
      else /* Same, but using single-limb calculations.  */
	{
	  mp_limb_t uh, vh;
	  /* Make UH be the most significant limb of U, and make VH be
	     corresponding bits from V.  */
	  uh = up[size - 1];
	  vh = vp[size - 1];
	  count_leading_zeros (cnt, uh);
#if GMP_NAIL_BITS == 0
	  if (cnt != 0)
	    {
	      uh = (uh << cnt) | (up[size - 2] >> (GMP_LIMB_BITS - cnt));
	      vh = (vh << cnt) | (vp[size - 2] >> (GMP_LIMB_BITS - cnt));
	    }
#else
	  uh <<= cnt;
	  vh <<= cnt;
	  if (cnt < GMP_NUMB_BITS)
	    {
	      uh |= up[size - 2] >> (GMP_NUMB_BITS - cnt);
	      vh |= vp[size - 2] >> (GMP_NUMB_BITS - cnt);
	    }
	  else
	    {
	      uh |= up[size - 2] << cnt - GMP_NUMB_BITS;
	      vh |= vp[size - 2] << cnt - GMP_NUMB_BITS;
	      if (size >= 3)
		{
		  uh |= up[size - 3] >> 2 * GMP_NUMB_BITS - cnt;
		  vh |= vp[size - 3] >> 2 * GMP_NUMB_BITS - cnt;
		}
	    }
#endif

	  A = 1;
	  B = 0;
	  C = 0;
	  D = 1;

	  asign = 0;
	  for (;;)
	    {
	      mp_limb_t q, T;
	      if (vh - C == 0 || vh + D == 0)
		break;

	      q = (uh + A) / (vh - C);
	      if (q != (uh - B) / (vh + D))
		break;

	      T = A + q * C;
	      A = C;
	      C = T;
	      T = B + q * D;
	      B = D;
	      D = T;
	      T = uh - q * vh;
	      uh = vh;
	      vh = T;

	      asign = ~asign;

	      if (vh - D == 0)
		break;

	      q = (uh - A) / (vh + C);
	      if (q != (uh + B) / (vh - D))
		break;

	      T = A + q * C;
	      A = C;
	      C = T;
	      T = B + q * D;
	      B = D;
	      D = T;
	      T = uh - q * vh;
	      uh = vh;
	      vh = T;

	      asign = ~asign;
	    }
#if EXTEND
	  if (asign)
	    sign = -sign;
#endif
	}

#if RECORD
      max = MAX (A, max);  max = MAX (B, max);
      max = MAX (C, max);  max = MAX (D, max);
#endif

      if (B == 0)
	{
	  /* This is quite rare.  I.e., optimize something else!  */

	  mpn_tdiv_qr (wp, up, (mp_size_t) 0, up, size, vp, vsize);

#if EXTEND
	  MPN_COPY (tp, s0p, ssize);
	  {
	    mp_size_t qsize;
	    mp_size_t i;

	    qsize = size - vsize + 1; /* size of stored quotient from division */
	    MPN_ZERO (s1p + ssize, qsize); /* zero s1 too */

	    for (i = 0; i < qsize; i++)
	      {
		mp_limb_t cy;
		cy = mpn_addmul_1 (tp + i, s1p, ssize, wp[i]);
		tp[ssize + i] = cy;
	      }

	    ssize += qsize;
	    ssize -= tp[ssize - 1] == 0;
	  }

	  sign = -sign;
	  MP_PTR_SWAP (s0p, s1p);
	  MP_PTR_SWAP (s1p, tp);
#endif
	  size = vsize;
	  MP_PTR_SWAP (up, vp);
	}
      else
	{
#if EXTEND
	  mp_size_t tsize, wsize;
#endif
	  /* T = U*A + V*B
	     W = U*C + V*D
	     U = T
	     V = W	   */

#if STAT
	  { mp_limb_t x; x = A | B | C | D; count_leading_zeros (cnt, x);
	  arr[GMP_LIMB_BITS - cnt]++; }
#endif
	  if (A == 0)
	    {
	      /* B == 1 and C == 1 (D is arbitrary) */
	      mp_limb_t cy;
	      MPN_COPY (tp, vp, size);
	      MPN_COPY (wp, up, size);
	      mpn_submul_1 (wp, vp, size, D);
	      MP_PTR_SWAP (tp, up);
	      MP_PTR_SWAP (wp, vp);
#if EXTEND
	      MPN_COPY (tp, s1p, ssize);
	      tsize = ssize;
	      tp[ssize] = 0;	/* must zero since wp might spill below */
	      MPN_COPY (wp, s0p, ssize);
	      cy = mpn_addmul_1 (wp, s1p, ssize, D);
	      wp[ssize] = cy;
	      wsize = ssize + (cy != 0);
	      MP_PTR_SWAP (tp, s0p);
	      MP_PTR_SWAP (wp, s1p);
	      ssize = MAX (wsize, tsize);
#endif
	    }
	  else
	    {
	      mp_limb_t cy, cy1, cy2;

	      if (asign)
		{
		  mpn_mul_1 (tp, vp, size, B);
		  mpn_submul_1 (tp, up, size, A);
		  mpn_mul_1 (wp, up, size, C);
		  mpn_submul_1 (wp, vp, size, D);
		}
	      else
		{
		  mpn_mul_1 (tp, up, size, A);
		  mpn_submul_1 (tp, vp, size, B);
		  mpn_mul_1 (wp, vp, size, D);
		  mpn_submul_1 (wp, up, size, C);
		}
	      MP_PTR_SWAP (tp, up);
	      MP_PTR_SWAP (wp, vp);
#if EXTEND
	      /* Compute new s0 */
	      cy1 = mpn_mul_1 (tp, s0p, ssize, A);
	      cy2 = mpn_addmul_1 (tp, s1p, ssize, B);
	      cy = cy1 + cy2;
	      tp[ssize] = cy & GMP_NUMB_MASK;
	      tsize = ssize + (cy != 0);
#if GMP_NAIL_BITS == 0
	      if (cy < cy1)
#else
	      if (cy > GMP_NUMB_MAX)
#endif
		{
		  tp[tsize] = 1;
		  wp[tsize] = 0;
		  tsize++;
		  /* This happens just for nails, since we get more work done
		     per numb there.  */
		}

	      /* Compute new s1 */
	      cy1 = mpn_mul_1 (wp, s1p, ssize, D);
	      cy2 = mpn_addmul_1 (wp, s0p, ssize, C);
	      cy = cy1 + cy2;
	      wp[ssize] = cy & GMP_NUMB_MASK;
	      wsize = ssize + (cy != 0);
#if GMP_NAIL_BITS == 0
	      if (cy < cy1)
#else
	      if (cy > GMP_NUMB_MAX)
#endif
		{
		  wp[wsize] = 1;
		  if (wsize >= tsize)
		    tp[wsize] = 0;
		  wsize++;
		}

	      MP_PTR_SWAP (tp, s0p);
	      MP_PTR_SWAP (wp, s1p);
	      ssize = MAX (wsize, tsize);
#endif
	    }
	  size -= up[size - 1] == 0;
#if GMP_NAIL_BITS != 0
	  size -= up[size - 1] == 0;
#endif
	}

#if WANT_GCDEXT_ONE_STEP
      TMP_FREE;
      return 0;
#endif
    }

#if RECORD
  printf ("max: %lx\n", max);
#endif

#if STAT
 {int i; for (i = 0; i <= GMP_LIMB_BITS; i++) printf ("%d:%d\n", i, arr[i]);}
#endif

  if (vsize == 0)
    {
      if (gp != up && gp != 0)
	MPN_COPY (gp, up, size);
#if EXTEND
      MPN_NORMALIZE (s0p, ssize);
      if (orig_s0p != s0p)
	MPN_COPY (orig_s0p, s0p, ssize);
      *s0size = sign >= 0 ? ssize : -ssize;
#endif
      TMP_FREE;
      return size;
    }
  else
    {
      mp_limb_t vl, ul, t;
#if EXTEND
      mp_size_t qsize, i;
#endif
      vl = vp[0];
#if EXTEND
      t = mpn_divmod_1 (wp, up, size, vl);

      MPN_COPY (tp, s0p, ssize);

      qsize = size - (wp[size - 1] == 0); /* size of quotient from division */
      if (ssize < qsize)
	{
	  MPN_ZERO (tp + ssize, qsize - ssize);
	  MPN_ZERO (s1p + ssize, qsize); /* zero s1 too */
	  for (i = 0; i < ssize; i++)
	    {
	      mp_limb_t cy;
	      cy = mpn_addmul_1 (tp + i, wp, qsize, s1p[i]);
	      tp[qsize + i] = cy;
	    }
	}
      else
	{
	  MPN_ZERO (s1p + ssize, qsize); /* zero s1 too */
	  for (i = 0; i < qsize; i++)
	    {
	      mp_limb_t cy;
	      cy = mpn_addmul_1 (tp + i, s1p, ssize, wp[i]);
	      tp[ssize + i] = cy;
	    }
	}
      ssize += qsize;
      ssize -= tp[ssize - 1] == 0;

      sign = -sign;
      MP_PTR_SWAP (s0p, s1p);
      MP_PTR_SWAP (s1p, tp);
#else
      t = mpn_mod_1 (up, size, vl);
#endif
      ul = vl;
      vl = t;
      while (vl != 0)
	{
	  mp_limb_t t;
#if EXTEND
	  mp_limb_t q;
	  q = ul / vl;
	  t = ul - q * vl;

	  MPN_COPY (tp, s0p, ssize);

	  MPN_ZERO (s1p + ssize, 1); /* zero s1 too */

	  {
	    mp_limb_t cy;
	    cy = mpn_addmul_1 (tp, s1p, ssize, q);
	    tp[ssize] = cy;
	  }

	  ssize += 1;
	  ssize -= tp[ssize - 1] == 0;

	  sign = -sign;
	  MP_PTR_SWAP (s0p, s1p);
	  MP_PTR_SWAP (s1p, tp);
#else
	  t = ul % vl;
#endif
	  ul = vl;
	  vl = t;
	}
      if (gp != 0)
	gp[0] = ul;
#if EXTEND
      MPN_NORMALIZE (s0p, ssize);
      if (orig_s0p != s0p)
	MPN_COPY (orig_s0p, s0p, ssize);
      *s0size = sign >= 0 ? ssize : -ssize;
#endif
      TMP_FREE;
      return 1;
    }
}
