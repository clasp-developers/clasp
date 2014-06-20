/* hgcd.c.

   THE FUNCTIONS IN THIS FILE ARE INTERNAL WITH MUTABLE INTERFACES.  IT IS ONLY
   SAFE TO REACH THEM THROUGH DOCUMENTED INTERFACES.  IN FACT, IT IS ALMOST
   GUARANTEED THAT THEY'LL CHANGE OR DISAPPEAR IN A FUTURE GNU MP RELEASE.

Copyright 2003, 2004, 2005, 2008 Free Software Foundation, Inc.

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

/* For input of size n, matrix elements are of size at most ceil(n/2)
   - 1, but we need two limbs extra. */
void
mpn_hgcd_matrix_init (struct hgcd_matrix *M, mp_size_t n, mp_ptr p)
{
  mp_size_t s = (n+1)/2 + 1;
  M->alloc = s;
  M->n = 1;
  MPN_ZERO (p, 4 * s);
  M->p[0][0] = p;
  M->p[0][1] = p + s;
  M->p[1][0] = p + 2 * s;
  M->p[1][1] = p + 3 * s;

  M->p[0][0][0] = M->p[1][1][0] = 1;
}

/* Updated column COL, adding in column (1-COL). */
static void
hgcd_matrix_update_1 (struct hgcd_matrix *M, unsigned col)
{
  mp_limb_t c0, c1;
  ASSERT (col < 2);

  c0 = mpn_add_n (M->p[0][col], M->p[0][0], M->p[0][1], M->n);
  c1 = mpn_add_n (M->p[1][col], M->p[1][0], M->p[1][1], M->n);

  M->p[0][col][M->n] = c0;
  M->p[1][col][M->n] = c1;

  M->n += (c0 | c1) != 0;
  ASSERT (M->n < M->alloc);
}

/* Updated column COL, adding in column Q * (1-COL). Temporary
 * storage: qn + n <= M->alloc, where n is the size of the largest
 * element in column 1 - COL. */
static void
hgcd_matrix_update_q (struct hgcd_matrix *M, mp_srcptr qp, mp_size_t qn,
		      unsigned col, mp_ptr tp)
{
  ASSERT (col < 2);

  if (qn == 1)
    {
      mp_limb_t q = qp[0];
      mp_limb_t c0, c1;

      c0 = mpn_addmul_1 (M->p[0][col], M->p[0][1-col], M->n, q);
      c1 = mpn_addmul_1 (M->p[1][col], M->p[1][1-col], M->n, q);

      M->p[0][col][M->n] = c0;
      M->p[1][col][M->n] = c1;

      M->n += (c0 | c1) != 0;
    }
  else
    {
      unsigned row;

      /* Carries for the unlikely case that we get both high words
	 from the multiplication and carries from the addition. */
      mp_limb_t c[2];
      mp_size_t n;

      /* The matrix will not necessarily grow in size by qn, so we
	 need normalization in order not to overflow M. */

      for (n = M->n; n + qn > M->n; n--)
	{
	  ASSERT (n > 0);
	  if (M->p[0][1-col][n-1] > 0 || M->p[1][1-col][n-1] > 0)
	    break;
	}

      ASSERT (qn + n <= M->alloc);

      for (row = 0; row < 2; row++)
	{
	  if (qn <= n)
	    mpn_mul (tp, M->p[row][1-col], n, qp, qn);
	  else
	    mpn_mul (tp, qp, qn, M->p[row][1-col], n);

	  ASSERT (n + qn >= M->n);
	  c[row] = mpn_add (M->p[row][col], tp, n + qn, M->p[row][col], M->n);
	}
      if (c[0] | c[1])
	{
	  M->n = n + qn + 1;
	  M->p[0][col][M->n - 1] = c[0];
	  M->p[1][col][M->n - 1] = c[1];
	}
      else
	{
	  n += qn;
	  n -= (M->p[0][col][n-1] | M->p[1][col][n-1]) == 0;
	  if (n > M->n)
	    M->n = n;
	}
    }

  ASSERT (M->n < M->alloc);
}

/* Multiply M by M1 from the right. Since the M1 elements fit in
   GMP_NUMB_BITS - 1 bits, M grows by at most one limb. Needs
   temporary space M->n */
static void
hgcd_matrix_mul_1 (struct hgcd_matrix *M, const struct hgcd_matrix1 *M1,
		   mp_ptr tp)
{
  mp_size_t n0, n1;

  /* Could avoid copy by some swapping of pointers. */
  MPN_COPY (tp, M->p[0][0], M->n);
  n0 = mpn_hgcd_mul_matrix1_vector (M1, M->p[0][0], tp, M->p[0][1], M->n);
  MPN_COPY (tp, M->p[1][0], M->n);
  n1 = mpn_hgcd_mul_matrix1_vector (M1, M->p[1][0], tp, M->p[1][1], M->n);

  /* Depends on zero initialization */
  M->n = MAX(n0, n1);
  ASSERT (M->n < M->alloc);
}

/* Perform a few steps, using some of mpn_hgcd2, subtraction and
   division. Reduces the size by almost one limb or more, but never
   below the given size s. Return new size for a and b, or 0 if no
   more steps are possible.

   If hgcd2 succeds, needs temporary space for hgcd_matrix_mul_1, M->n
   limbs, and hgcd_mul_matrix1_inverse_vector, n limbs. If hgcd2
   fails, needs space for the quotient, qn <= n - s + 1 limbs, for and
   hgcd_matrix_update_q, qn + (size of the appropriate column of M) <=
   resulting size of $.

   If N is the input size to the calling hgcd, then s = floor(N/2) +
   1, M->n < N, qn + matrix size <= n - s + 1 + n - s = 2 (n - s) + 1
   < N, so N is sufficient.
*/

static mp_size_t
hgcd_step (mp_size_t n, mp_ptr ap, mp_ptr bp, mp_size_t s,
	   struct hgcd_matrix *M, mp_ptr tp)
{
  struct hgcd_matrix1 M1;
  mp_limb_t mask;
  mp_limb_t ah, al, bh, bl;
  mp_size_t an, bn, qn;
  int col;

  ASSERT (n > s);

  mask = ap[n-1] | bp[n-1];
  ASSERT (mask > 0);

  if (n == s + 1)
    {
      if (mask < 4)
	goto subtract;

      ah = ap[n-1]; al = ap[n-2];
      bh = bp[n-1]; bl = bp[n-2];
    }
  else if (mask & GMP_NUMB_HIGHBIT)
    {
      ah = ap[n-1]; al = ap[n-2];
      bh = bp[n-1]; bl = bp[n-2];
    }
  else
    {
      int shift;

      count_leading_zeros (shift, mask);
      ah = MPN_EXTRACT_NUMB (shift, ap[n-1], ap[n-2]);
      al = MPN_EXTRACT_NUMB (shift, ap[n-2], ap[n-3]);
      bh = MPN_EXTRACT_NUMB (shift, bp[n-1], bp[n-2]);
      bl = MPN_EXTRACT_NUMB (shift, bp[n-2], bp[n-3]);
    }

  /* Try an mpn_hgcd2 step */
  if (mpn_hgcd2 (ah, al, bh, bl, &M1))
    {
      /* Multiply M <- M * M1 */
      hgcd_matrix_mul_1 (M, &M1, tp);

      /* Can't swap inputs, so we need to copy. */
      MPN_COPY (tp, ap, n);
      /* Multiply M1^{-1} (a;b) */
      return mpn_hgcd_mul_matrix1_inverse_vector (&M1, ap, tp, bp, n);
    }

 subtract:
  /* There are two ways in which mpn_hgcd2 can fail. Either one of ah and
     bh was too small, or ah, bh were (almost) equal. Perform one
     subtraction step (for possible cancellation of high limbs),
     followed by one division. */

  /* Since we must ensure that #(a-b) > s, we handle cancellation of
     high limbs explicitly up front. (FIXME: Or is it better to just
     subtract, normalize, and use an addition to undo if it turns out
     the the difference is too small?) */
  for (an = n; an > s; an--)
    if (ap[an-1] != bp[an-1])
      break;

  if (an == s)
    return 0;

  /* Maintain a > b. When needed, swap a and b, and let col keep track
     of how to update M. */
  if (ap[an-1] > bp[an-1])
    {
      /* a is largest. In the subtraction step, we need to update
	 column 1 of M */
      col = 1;
    }
  else
    {
      MP_PTR_SWAP (ap, bp);
      col = 0;
    }

  bn = n;
  MPN_NORMALIZE (bp, bn);
  if (bn <= s)
    return 0;

  /* We have #a, #b > s. When is it possible that #(a-b) < s? For
     cancellation to happen, the numbers must be of the form

       a = x + 1, 0,            ..., 0,            al
       b = x    , GMP_NUMB_MAX, ..., GMP_NUMB_MAX, bl

     where al, bl denotes the least significant k limbs. If al < bl,
     then #(a-b) < k, and if also high(al) != 0, high(bl) != GMP_NUMB_MAX,
     then #(a-b) = k. If al >= bl, then #(a-b) = k + 1. */

  if (ap[an-1] == bp[an-1] + 1)
    {
      mp_size_t k;
      int c;
      for (k = an-1; k > s; k--)
	if (ap[k-1] != 0 || bp[k-1] != GMP_NUMB_MAX)
	  break;

      MPN_CMP (c, ap, bp, k);
      if (c < 0)
	{
	  mp_limb_t cy;

	  /* The limbs from k and up are cancelled. */
	  if (k == s)
	    return 0;
	  cy = mpn_sub_n (ap, ap, bp, k);
	  ASSERT (cy == 1);
	  an = k;
	}
      else
	{
	  ASSERT_NOCARRY (mpn_sub_n (ap, ap, bp, k));
	  ap[k] = 1;
	  an = k + 1;
	}
    }
  else
    ASSERT_NOCARRY (mpn_sub_n (ap, ap, bp, an));

  ASSERT (an > s);
  ASSERT (ap[an-1] > 0);
  ASSERT (bn > s);
  ASSERT (bp[bn-1] > 0);

  hgcd_matrix_update_1 (M, col);

  if (an < bn)
    {
      MPN_PTR_SWAP (ap, an, bp, bn);
      col ^= 1;
    }
  else if (an == bn)
    {
      int c;
      MPN_CMP (c, ap, bp, an);
      if (c < 0)
	{
	  MP_PTR_SWAP (ap, bp);
	  col ^= 1;
	}
    }

  /* Divide a / b. */
  qn = an + 1 - bn;

  /* FIXME: We could use an approximate division, that may return a
     too small quotient, and only guarantee that the size of r is
     almost the size of b. FIXME: Let ap and remainder overlap. */
  mpn_tdiv_qr (tp, ap, 0, ap, an, bp, bn);
  qn -= (tp[qn -1] == 0);

  /* Normalize remainder */
  an = bn;
  for ( ; an > s; an--)
    if (ap[an-1] > 0)
      break;

  if (an <= s)
    {
      /* Quotient is too large */
      mp_limb_t cy;

      cy = mpn_add (ap, bp, bn, ap, an);

      if (cy > 0)
	{
	  ASSERT (bn < n);
	  ap[bn] = cy;
	  bp[bn] = 0;
	  bn++;
	}

      MPN_DECR_U (tp, qn, 1);
      qn -= (tp[qn-1] == 0);
    }

  if (qn > 0)
    hgcd_matrix_update_q (M, tp, qn, col, tp + qn);

  return bn;
}

/* Reduces a,b until |a-b| fits in n/2 + 1 limbs. Constructs matrix M
   with elements of size at most (n+1)/2 - 1. Returns new size of a,
   b, or zero if no reduction is possible. */
mp_size_t
mpn_hgcd_lehmer (mp_ptr ap, mp_ptr bp, mp_size_t n,
		 struct hgcd_matrix *M, mp_ptr tp)
{
  mp_size_t s = n/2 + 1;
  mp_size_t nn;

  ASSERT (n > s);
  ASSERT (ap[n-1] > 0 || bp[n-1] > 0);

  nn = hgcd_step (n, ap, bp, s, M, tp);
  if (!nn)
    return 0;

  for (;;)
    {
      n = nn;
      ASSERT (n > s);
      nn = hgcd_step (n, ap, bp, s, M, tp);
      if (!nn )
	return n;
    }
}

/* Multiply M by M1 from the right. Needs 3*(M->n + M1->n) + 5 limbs
   of temporary storage (see mpn_matrix22_mul_itch). */
void
mpn_hgcd_matrix_mul (struct hgcd_matrix *M, const struct hgcd_matrix *M1,
		     mp_ptr tp)
{
  mp_size_t n;

  /* About the new size of M:s elements. Since M1's diagonal elements
     are > 0, no element can decrease. The new elements are of size
     M->n + M1->n, one limb more or less. The computation of the
     matrix product produces elements of size M->n + M1->n + 1. But
     the true size, after normalization, may be three limbs smaller.

     The reason that the product has normalized size >= M->n + M1->n -
     2 is subtle. It depends on the fact that M and M1 can be factored
     as products of (1,1; 0,1) and (1,0; 1,1), and that we can't have
     M ending with a large power and M1 starting with a large power of
     the same matrix. */

  /* FIXME: Strassen multiplication gives only a small speedup. In FFT
     multiplication range, this function could be sped up quite a lot
     using invariance. */
  ASSERT (M->n + M1->n < M->alloc);

  ASSERT ((M->p[0][0][M->n-1] | M->p[0][1][M->n-1]
	   | M->p[1][0][M->n-1] | M->p[1][1][M->n-1]) > 0);

  ASSERT ((M1->p[0][0][M1->n-1] | M1->p[0][1][M1->n-1]
	   | M1->p[1][0][M1->n-1] | M1->p[1][1][M1->n-1]) > 0);

  mpn_matrix22_mul (M->p[0][0], M->p[0][1],
		    M->p[1][0], M->p[1][1], M->n,
		    M1->p[0][0], M1->p[0][1],
		    M1->p[1][0], M1->p[1][1], M1->n, tp);

  /* Index of last potentially non-zero limb, size is one greater. */
  n = M->n + M1->n;

  n -= ((M->p[0][0][n] | M->p[0][1][n] | M->p[1][0][n] | M->p[1][1][n]) == 0);
  n -= ((M->p[0][0][n] | M->p[0][1][n] | M->p[1][0][n] | M->p[1][1][n]) == 0);
  n -= ((M->p[0][0][n] | M->p[0][1][n] | M->p[1][0][n] | M->p[1][1][n]) == 0);

  ASSERT ((M->p[0][0][n] | M->p[0][1][n] | M->p[1][0][n] | M->p[1][1][n]) > 0);

  M->n = n + 1;
}

/* Multiplies the least significant p limbs of (a;b) by M^-1.
   Temporary space needed: 2 * (p + M->n)*/
mp_size_t
mpn_hgcd_matrix_adjust (struct hgcd_matrix *M,
			mp_size_t n, mp_ptr ap, mp_ptr bp,
			mp_size_t p, mp_ptr tp)
{
  /* M^-1 (a;b) = (r11, -r01; -r10, r00) (a ; b)
     = (r11 a - r01 b; - r10 a + r00 b */

  mp_ptr t0 = tp;
  mp_ptr t1 = tp + p + M->n;
  mp_limb_t ah, bh;
  mp_limb_t cy;

  ASSERT (p + M->n  < n);

  /* First compute the two values depending on a, before overwriting a */

  if (M->n >= p)
    {
      mpn_mul (t0, M->p[1][1], M->n, ap, p);
      mpn_mul (t1, M->p[1][0], M->n, ap, p);
    }
  else
    {
      mpn_mul (t0, ap, p, M->p[1][1], M->n);
      mpn_mul (t1, ap, p, M->p[1][0], M->n);
    }

  /* Update a */
  MPN_COPY (ap, t0, p);
  ah = mpn_add (ap + p, ap + p, n - p, t0 + p, M->n);

  if (M->n >= p)
    mpn_mul (t0, M->p[0][1], M->n, bp, p);
  else
    mpn_mul (t0, bp, p, M->p[0][1], M->n);

  cy = mpn_sub (ap, ap, n, t0, p + M->n);
  ASSERT (cy <= ah);
  ah -= cy;

  /* Update b */
  if (M->n >= p)
    mpn_mul (t0, M->p[0][0], M->n, bp, p);
  else
    mpn_mul (t0, bp, p, M->p[0][0], M->n);

  MPN_COPY (bp, t0, p);
  bh = mpn_add (bp + p, bp + p, n - p, t0 + p, M->n);
  cy = mpn_sub (bp, bp, n, t1, p + M->n);
  ASSERT (cy <= bh);
  bh -= cy;

  if (ah > 0 || bh > 0)
    {
      ap[n] = ah;
      bp[n] = bh;
      n++;
    }
  else
    {
      /* The subtraction can reduce the size by at most one limb. */
      if (ap[n-1] == 0 && bp[n-1] == 0)
	n--;
    }
  ASSERT (ap[n-1] > 0 || bp[n-1] > 0);
  return n;
}

/* Size analysis for hgcd:

   For the recursive calls, we have n1 <= ceil(n / 2). Then the
   storage need is determined by the storage for the recursive call
   computing M1, and hgcd_matrix_adjust and hgcd_matrix_mul calls that use M1
   (after this, the storage needed for M1 can be recycled).

   Let S(r) denote the required storage. For M1 we need 4 * (ceil(n1/2) + 1)
   = 4 * (ceil(n/4) + 1), for the hgcd_matrix_adjust call, we need n + 2,
   and for the hgcd_matrix_mul, we may need 3 ceil(n/2) + 8. In total,
   4 * ceil(n/4) + 3 ceil(n/2) + 12 <= 10 ceil(n/4) + 12.

   For the recursive call, we need S(n1) = S(ceil(n/2)).

   S(n) <= 10*ceil(n/4) + 12 + S(ceil(n/2))
	<= 10*(ceil(n/4) + ... + ceil(n/2^(1+k))) + 12k + S(ceil(n/2^k))
	<= 10*(2 ceil(n/4) + k) + 12k + S(ceil(n/2^k))
	<= 20 ceil(n/4) + 22k + S(ceil(n/2^k))
*/

mp_size_t
mpn_hgcd_itch (mp_size_t n)
{
  unsigned k;
  int count;
  mp_size_t nscaled;

  if (BELOW_THRESHOLD (n, HGCD_THRESHOLD))
    return MPN_HGCD_LEHMER_ITCH (n);

  /* Get the recursion depth. */
  nscaled = (n - 1) / (HGCD_THRESHOLD - 1);
  count_leading_zeros (count, nscaled);
  k = GMP_LIMB_BITS - count;

  return 20 * ((n+3) / 4) + 22 * k
    + MPN_HGCD_LEHMER_ITCH (HGCD_THRESHOLD);
}

/* Reduces a,b until |a-b| fits in n/2 + 1 limbs. Constructs matrix M
   with elements of size at most (n+1)/2 - 1. Returns new size of a,
   b, or zero if no reduction is possible. */

mp_size_t
mpn_hgcd (mp_ptr ap, mp_ptr bp, mp_size_t n,
	  struct hgcd_matrix *M, mp_ptr tp)
{
  mp_size_t s = n/2 + 1;
  mp_size_t n2 = (3*n)/4 + 1;

  mp_size_t p, nn;
  int success = 0;

  if (n <= s)
    /* Happens when n <= 2, a fairly uninteresting case but exercised
       by the random inputs of the testsuite. */
    return 0;

  ASSERT ((ap[n-1] | bp[n-1]) > 0);

  ASSERT ((n+1)/2 - 1 < M->alloc);

  if (BELOW_THRESHOLD (n, HGCD_THRESHOLD))
    return mpn_hgcd_lehmer (ap, bp, n, M, tp);

  p = n/2;
  nn = mpn_hgcd (ap + p, bp + p, n - p, M, tp);
  if (nn > 0)
    {
      /* Needs 2*(p + M->n) <= 2*(floor(n/2) + ceil(n/2) - 1)
	 = 2 (n - 1) */
      n = mpn_hgcd_matrix_adjust (M, p + nn, ap, bp, p, tp);
      success = 1;
    }
  while (n > n2)
    {
      /* Needs n + 1 storage */
      nn = hgcd_step (n, ap, bp, s, M, tp);
      if (!nn)
	return success ? n : 0;
      n = nn;
      success = 1;
    }

  if (n > s + 2)
    {
      struct hgcd_matrix M1;
      mp_size_t scratch;

      p = 2*s - n + 1;
      scratch = MPN_HGCD_MATRIX_INIT_ITCH (n-p);

      mpn_hgcd_matrix_init(&M1, n - p, tp);
      nn = mpn_hgcd (ap + p, bp + p, n - p, &M1, tp + scratch);
      if (nn > 0)
	{
	  /* We always have max(M) > 2^{-(GMP_NUMB_BITS + 1)} max(M1) */
	  ASSERT (M->n + 2 >= M1.n);

	  /* Furthermore, assume M ends with a quotient (1, q; 0, 1),
	     then either q or q + 1 is a correct quotient, and M1 will
	     start with either (1, 0; 1, 1) or (2, 1; 1, 1). This
	     rules out the case that the size of M * M1 is much
	     smaller than the expected M->n + M1->n. */

	  ASSERT (M->n + M1.n < M->alloc);

	  /* Needs 2 (p + M->n) <= 2 (2*s - n2 + 1 + n2 - s - 1)
	     = 2*s <= 2*(floor(n/2) + 1) <= n + 2. */
	  n = mpn_hgcd_matrix_adjust (&M1, p + nn, ap, bp, p, tp + scratch);

	  /* We need a bound for of M->n + M1.n. Let n be the original
	     input size. Then

	       ceil(n/2) - 1 >= size of product >= M.n + M1.n - 2

	     and it follows that

	       M.n + M1.n <= ceil(n/2) + 1

	     Then 3*(M.n + M1.n) + 5 <= 3 * ceil(n/2) + 8 is the
	     amount of needed scratch space. */
	  mpn_hgcd_matrix_mul (M, &M1, tp + scratch);
	  success = 1;
	}
    }

  /* This really is the base case */
  for (;;)
    {
      /* Needs s+3 < n */
      nn = hgcd_step (n, ap, bp, s, M, tp);
      if (!nn)
	return success ? n : 0;

      n = nn;
      success = 1;
    }
}
