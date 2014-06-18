/* An implementation in GMP of Scho"nhage's fast multiplication algorithm
   modulo 2^N+1, by Paul Zimmermann, INRIA Lorraine, February 1998.

   Revised July 2002 and January 2003, Paul Zimmermann.

   THE CONTENTS OF THIS FILE ARE FOR INTERNAL USE AND THE FUNCTIONS HAVE
   MUTABLE INTERFACES.  IT IS ONLY SAFE TO REACH THEM THROUGH DOCUMENTED
   INTERFACES.  IT IS ALMOST GUARANTEED THAT THEY'LL CHANGE OR DISAPPEAR IN
   A FUTURE GNU MP RELEASE.

Copyright 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005 Free Software
Foundation, Inc.

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


/* References:

   Schnelle Multiplikation grosser Zahlen, by Arnold Scho"nhage and Volker
   Strassen, Computing 7, p. 281-292, 1971.

   Asymptotically fast algorithms for the numerical multiplication
   and division of polynomials with complex coefficients, by Arnold Scho"nhage,
   Computer Algebra, EUROCAM'82, LNCS 144, p. 3-15, 1982.

   Tapes versus Pointers, a study in implementing fast algorithms,
   by Arnold Scho"nhage, Bulletin of the EATCS, 30, p. 23-32, 1986.

   See also http://www.loria.fr/~zimmerma/bignum


   Future:

   It might be possible to avoid a small number of MPN_COPYs by using a
   rotating temporary or two.

   Multiplications of unequal sized operands can be done with this code, but
   it needs a tighter test for identifying squaring (same sizes as well as
   same pointers).  */

#include <stdio.h>
#include "gmp.h"
#include "gmp-impl.h"

#ifdef WANT_ADDSUB
#include "generic/addsub_n.c"
#define HAVE_NATIVE_mpn_addsub_n 1
#endif

/* Change this to "#define TRACE(x) x" for some traces. */
#define TRACE(x)



FFT_TABLE_ATTRS mp_size_t mpn_fft_table[2][MPN_FFT_TABLE_SIZE] = {
  MUL_FFT_TABLE,
  SQR_FFT_TABLE
};


static int mpn_mul_fft_internal
_PROTO ((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t, int, int, mp_ptr *, mp_ptr *,
	 mp_ptr, mp_ptr, mp_size_t, mp_size_t, mp_size_t, int **, mp_ptr,
	 int));


/* Find the best k to use for a mod 2^(m*GMP_NUMB_BITS)+1 FFT for m >= n.
   sqr==0 if for a multiply, sqr==1 for a square.
   Don't declare it static since it is needed by tuneup.
*/
int
mpn_fft_best_k (mp_size_t n, int sqr)
{
  int i;

  for (i = 0; mpn_fft_table[sqr][i] != 0; i++)
    if (n < mpn_fft_table[sqr][i])
      return i + FFT_FIRST_K;

  /* treat 4*last as one further entry */
  if (i == 0 || n < 4 * mpn_fft_table[sqr][i - 1])
    return i + FFT_FIRST_K;
  else
    return i + FFT_FIRST_K + 1;
}


/* Returns smallest possible number of limbs >= pl for a fft of size 2^k,
   i.e. smallest multiple of 2^k >= pl.

   Don't declare static: needed by tuneup.
*/

mp_size_t
mpn_fft_next_size (mp_size_t pl, int k)
{
  pl = 1 + ((pl - 1) >> k); /* ceil (pl/2^k) */
  return pl << k;
}


static void
mpn_fft_initl (int **l, int k)
{
  int i, j, K;
  int *li;

  l[0][0] = 0;
  for (i = 1, K = 1; i <= k; i++, K *= 2)
    {
      li = l[i];
      for (j = 0; j < K ; j++)
	{
	  li[j] = 2 * l[i - 1][j];
	  li[K + j] = 1 + li[j];
	}
    }
}

/*
  shift {up, n} of cnt bits to the left, store the complemented result
  in {rp, n}, and output the shifted bits (not complemented).
  Same as:
     cc = mpn_lshift (rp, up, n, cnt);
     mpn_com_n (rp, rp, n);
     return cc;

  Assumes n >= 1, 1 < cnt < GMP_NUMB_BITS, rp >= up.

  Copied from mpn/generic/lshift.c.
*/
static mp_limb_t
mpn_lshift_com (mp_ptr rp, mp_srcptr up, mp_size_t n, unsigned int cnt)
{
  mp_limb_t high_limb, low_limb;
  unsigned int tnc;
  mp_size_t i;
  mp_limb_t retval;

  up += n;
  rp += n;

  tnc = GMP_NUMB_BITS - cnt;
  low_limb = *--up;
  retval = low_limb >> tnc;
  high_limb = (low_limb << cnt);

  for (i = n - 1; i != 0; i--)
    {
      low_limb = *--up;
      *--rp = (~(high_limb | (low_limb >> tnc))) & GMP_NUMB_MASK;
      high_limb = low_limb << cnt;
    }
  *--rp = (~high_limb) & GMP_NUMB_MASK;

  return retval;
}

/* r <- a*2^e mod 2^(n*GMP_NUMB_BITS)+1 with a = {a, n+1}
   Assumes a is semi-normalized, i.e. a[n] <= 1.
   r and a must have n+1 limbs, and not overlap.
*/
static void
mpn_fft_mul_2exp_modF (mp_ptr r, mp_srcptr a, int e, mp_size_t n)
{
  int d, sh, negate;
  mp_limb_t cc = 0, rd;

  d = e % (n * GMP_NUMB_BITS);	/* 2^e = (+/-) 2^d */
  negate = (e / (n * GMP_NUMB_BITS)) % 2;
  sh = d % GMP_NUMB_BITS;
  d /= GMP_NUMB_BITS;

  if (negate)
    {
      /* r[0..d-1]  <-- lshift(a[n-d]..a[n-1], sh)
	 r[d..n-1]  <-- -lshift(a[0]..a[n-d-1],  sh) */
      if (sh)
	{
	  /* no out shift below since a[n] <= 1 */
	  mpn_lshift (r, a + n - d, d + 1, sh);
	  rd = r[d];
	  cc = mpn_lshift_com (r + d, a, n - d, sh);
	}
      else
	{
	  MPN_COPY (r, a + n - d, d);
	  rd = a[n];
	  mpn_com_n (r + d, a, n - d);
	}

      /* add cc to r[0], and add rd to r[d] */

      /* now add 1 in r[d], subtract 1 in r[n], i.e. add 1 in r[0] */

      r[n] = 0;
      /* cc < 2^sh <= 2^(GMP_NUMB_BITS-1) thus no overflow here */
      cc ++; /* warning: don't put this in the mpn_incr_u call,
		since it may be a macro and evaluate its arg. two times */
      mpn_incr_u (r, cc);

      rd ++;
      /* rd might overflow when sh=GMP_NUMB_BITS-1 */
      cc = (rd == 0) ? CNST_LIMB(1) : rd;
      r = r + d + (rd == 0);
      mpn_incr_u (r, cc);

      return;
    }

  /* if negate=0,
        r[0..d-1]  <-- -lshift(a[n-d]..a[n-1], sh)
        r[d..n-1]  <-- lshift(a[0]..a[n-d-1],  sh)
  */
  if (sh != 0)
    {
      /* no out bits below since a[n] <= 1 */
      mpn_lshift_com (r, a + n - d, d + 1, sh);
      rd = ~r[d];
      /* {r, d+1} = {a+n-d, d+1} << sh */
      cc = mpn_lshift (r + d, a, n - d, sh); /* {r+d, n-d} = {a, n-d}<<sh */
    }
  else
    {
      /* r[d] is not used below, but we save a test for d=0 */
      mpn_com_n (r, a + n - d, d + 1);
      rd = a[n];
      MPN_COPY (r + d, a, n - d);
    }

  /* now complement {r, d}, subtract cc from r[0], subtract rd from r[d] */

  /* if d=0 we just have r[0]=a[n] << sh */
  if (d)
    {
      /* now add 1 in r[0], subtract 1 in r[d] */
      if (cc-- == 0) /* then add 1 to r[0] */
	cc = mpn_add_1 (r, r, n, CNST_LIMB(1));
      cc = mpn_sub_1 (r, r, d, cc) + CNST_LIMB(1);
      /* add 1 to cc instead of rd since rd might overflow */
    }

  /* now subtract cc and rd from r[d..n] */

  r[n] = -mpn_sub_1 (r + d, r + d, n - d, cc);
  r[n] -= mpn_sub_1 (r + d, r + d, n - d, rd);
  if (r[n] & GMP_LIMB_HIGHBIT)
    r[n] = mpn_add_1 (r, r, n, CNST_LIMB(1));
}


/* r <- a+b mod 2^(n*GMP_NUMB_BITS)+1.
   Assumes a and b are semi-normalized.
*/
static inline void
mpn_fft_add_modF (mp_ptr r, mp_srcptr a, mp_srcptr b, int n)
{
  r[n] = a[n] + b[n] + mpn_add_n (r, a, b, n);
  /* now 0 <= r[n] <= 3 */
  if (r[n] > 1) /* subtract r[n]-1 at both ends */
    {
      mp_limb_t c = r[n] - CNST_LIMB(1);
      r[n] = CNST_LIMB(1); /* r[n] - c = 1 */
      MPN_DECR_U (r, n + 1, c);
    }
}


/* r <- a-b mod 2^(n*GMP_NUMB_BITS)+1.
   Assumes a and b are semi-normalized.
*/
static inline void
mpn_fft_sub_modF (mp_ptr r, mp_srcptr a, mp_srcptr b, int n)
{
  mp_limb_t c;

  c = a[n] - b[n] - mpn_sub_n (r, a, b, n); /* -2 <= c <= 1 */
  /* r[n] <- c */
  r[n] = (c & GMP_LIMB_HIGHBIT) ? mpn_add_1 (r, r, n, -c) : c;
}


/* input: A[0] ... A[inc*(K-1)] are residues mod 2^N+1 where
	  N=n*GMP_NUMB_BITS, and 2^omega is a primitive root mod 2^N+1
   output: A[inc*l[k][i]] <- \sum (2^omega)^(ij) A[inc*j] mod 2^N+1 */

static void
mpn_fft_fft_sqr (mp_ptr *Ap, mp_size_t K, int **ll,
		 mp_size_t omega, mp_size_t n, mp_size_t inc, mp_ptr tp)
{
  if (K == 2)
    {
      mp_limb_t cy;
#if HAVE_NATIVE_mpn_addsub_n
      cy = mpn_addsub_n (Ap[0], Ap[inc], Ap[0], Ap[inc], n + 1) & 1;
#else
      MPN_COPY (tp, Ap[0], n + 1);
      mpn_add_n (Ap[0], Ap[0], Ap[inc], n + 1);
      cy = mpn_sub_n (Ap[inc], tp, Ap[inc], n + 1);
#endif
      if (Ap[0][n] > CNST_LIMB(1)) /* can be 2 or 3 */
	Ap[0][n] = CNST_LIMB(1) - mpn_sub_1 (Ap[0], Ap[0], n, Ap[0][n] - CNST_LIMB(1));
      if (cy) /* Ap[inc][n] can be -1 or -2 */
	Ap[inc][n] = mpn_add_1 (Ap[inc], Ap[inc], n, ~Ap[inc][n] + CNST_LIMB(1));
    }
  else
    {
      int j, inc2 = 2 * inc;
      int *lk = *ll;
      mp_ptr tmp;
      TMP_DECL;

      TMP_MARK;
      tmp = TMP_ALLOC_LIMBS (n + 1);
      mpn_fft_fft_sqr (Ap, K/2,ll-1,2 * omega,n,inc2, tp);
      mpn_fft_fft_sqr (Ap+inc, K/2,ll-1,2 * omega,n,inc2, tp);
      /* A[2*j*inc]   <- A[2*j*inc] + omega^l[k][2*j*inc] A[(2j+1)inc]
	 A[(2j+1)inc] <- A[2*j*inc] + omega^l[k][(2j+1)inc] A[(2j+1)inc] */
      for (j = 0; j < K / 2; j++, lk += 2, Ap += 2 * inc)
	{
	  /* Ap[inc] <- Ap[0] + Ap[inc] * 2^(lk[1] * omega)
	     Ap[0]   <- Ap[0] + Ap[inc] * 2^(lk[0] * omega) */
	  mpn_fft_mul_2exp_modF (tp,  Ap[inc], lk[1] * omega, n);
	  mpn_fft_mul_2exp_modF (tmp, Ap[inc], lk[0] * omega, n);
	  mpn_fft_add_modF	(Ap[inc], Ap[0], tp, n);
	  mpn_fft_add_modF	(Ap[0], Ap[0], tmp, n);
	}
      TMP_FREE;
    }
}

/* A1 <- A0 + A1 * 2^e1
   A0 <- A0 + A1 * 2^e0
   Same for B0, B1.
   t0 and t1 must have space for (n+1) limbs each.
   We have 0 <= e0, e1 < 2*n*GMP_NUMB_BITS.
*/
static inline void
mpn_fft_butterfly (mp_ptr A0, mp_ptr A1, mp_ptr B0, mp_ptr B1, int e0, int e1,
		   mp_ptr t0, mp_ptr t1, mp_size_t n)
{
  int dif;

  ASSERT (e0 != e1);
  dif = (e1 - e0) % (n * GMP_NUMB_BITS);

  mpn_fft_mul_2exp_modF (t0, A1, e0, n);
  if (dif)
    {
      mpn_fft_mul_2exp_modF (t1, A1, e1, n);
      mpn_fft_add_modF (A1, A0, t1, n);
    }
  else
    mpn_fft_sub_modF (A1, A0, t0, n);
  mpn_fft_add_modF   (A0, A0, t0, n);
  mpn_fft_mul_2exp_modF (t0, B1, e0, n);
  if (dif)
    {
      mpn_fft_mul_2exp_modF (t1, B1, e1, n);
      mpn_fft_add_modF (B1, B0, t1, n);
    }
  else
    mpn_fft_sub_modF (B1, B0, t0, n);
  mpn_fft_add_modF   (B0, B0, t0, n);
}


/* input: A[0] ... A[inc*(K-1)] are residues mod 2^N+1 where
	  N=n*GMP_NUMB_BITS, and 2^omega is a primitive root mod 2^N+1
   output: A[inc*l[k][i]] <- \sum (2^omega)^(ij) A[inc*j] mod 2^N+1
   tp must have space for 2*(n+1) limbs.
*/

static void
mpn_fft_fft (mp_ptr *Ap, mp_ptr *Bp, mp_size_t K, int **ll,
	     mp_size_t omega, mp_size_t n, mp_size_t inc, mp_ptr tp)
{
  if (K == 2)
    {
      mp_limb_t ca, cb;
#if HAVE_NATIVE_mpn_addsub_n
      ca = mpn_addsub_n (Ap[0], Ap[inc], Ap[0], Ap[inc], n + 1) & 1;
      cb = mpn_addsub_n (Bp[0], Bp[inc], Bp[0], Bp[inc], n + 1) & 1;
#else
      MPN_COPY (tp, Ap[0], n + 1);
      mpn_add_n (Ap[0], Ap[0], Ap[inc], n + 1);
      ca = mpn_sub_n (Ap[inc], tp, Ap[inc], n + 1);
      MPN_COPY (tp, Bp[0], n + 1);
      mpn_add_n (Bp[0], Bp[0], Bp[inc], n + 1);
      cb = mpn_sub_n (Bp[inc], tp, Bp[inc], n + 1);
#endif
      if (Ap[0][n] > CNST_LIMB(1)) /* can be 2 or 3 */
	Ap[0][n] = CNST_LIMB(1) - mpn_sub_1 (Ap[0], Ap[0], n, Ap[0][n] - CNST_LIMB(1));
      if (ca) /* Ap[inc][n] can be -1 or -2 */
	Ap[inc][n] = mpn_add_1 (Ap[inc], Ap[inc], n, ~Ap[inc][n] + CNST_LIMB(1));
      if (Bp[0][n] > CNST_LIMB(1)) /* can be 2 or 3 */
	Bp[0][n] = CNST_LIMB(1) - mpn_sub_1 (Bp[0], Bp[0], n, Bp[0][n] - CNST_LIMB(1));
      if (cb) /* Bp[inc][n] can be -1 or -2 */
	Bp[inc][n] = mpn_add_1 (Bp[inc], Bp[inc], n, ~Bp[inc][n] + CNST_LIMB(1));
    }
  else
    {
      int j, inc2=2 * inc;
      int *lk = *ll;
      mp_ptr tmp = tp + (n + 1);
      mp_size_t N = 2 * n * GMP_NUMB_BITS;

      mpn_fft_fft (Ap, Bp, K / 2, ll - 1, 2 * omega, n, inc2, tp);
      mpn_fft_fft (Ap + inc, Bp + inc, K / 2, ll - 1, 2 * omega, n, inc2, tp);
      /* A[2*j*inc]   <- A[2*j*inc] + omega^l[k][2*j*inc] A[(2j+1)inc]
	 A[(2j+1)inc] <- A[2*j*inc] + omega^l[k][(2j+1)inc] A[(2j+1)inc] */
      for (j = 0; j < K / 2; j++, lk += 2, Ap += 2 * inc, Bp += 2 * inc)
	mpn_fft_butterfly (Ap[0], Ap[inc], Bp[0], Bp[inc], (lk[0] * omega) % N,
			   (lk[1] * omega) % N, tmp, tp, n);
    }
}


/* Given ap[0..n] with ap[n]<=1, reduce it modulo 2^(n*GMP_NUMB_BITS)+1,
   by subtracting that modulus if necessary.

   If ap[0..n] is exactly 2^(n*GMP_NUMB_BITS) then mpn_sub_1 produces a
   borrow and the limbs must be zeroed out again.  This will occur very
   infrequently.  */

static inline void
mpn_fft_normalize (mp_ptr ap, mp_size_t n)
{
  if (ap[n] != 0)
    {
      ap[n] = mpn_sub_1 (ap, ap, n, CNST_LIMB(1));
      if (ap[n] != 0)
	MPN_ZERO (ap, n);
    }
}

/* a[i] <- a[i]*b[i] mod 2^(n*GMP_NUMB_BITS)+1 for 0 <= i < K */
static void
mpn_fft_mul_modF_K (mp_ptr *ap, mp_ptr *bp, mp_size_t n, int K)
{
  int i;
  int sqr = (ap == bp);
  TMP_DECL;

  TMP_MARK;

  if (n >= (sqr ? SQR_FFT_MODF_THRESHOLD : MUL_FFT_MODF_THRESHOLD))
    {
      int k, K2, nprime2, Nprime2, M2, maxLK, l, Mp2;
      int **_fft_l;
      mp_ptr *Ap, *Bp, A, B, T;

      k = mpn_fft_best_k (n, sqr);
      K2 = 1 << k;
      ASSERT_ALWAYS(n % K2 == 0);
      maxLK = (K2 > GMP_NUMB_BITS) ? K2 : GMP_NUMB_BITS;
      M2 = n * GMP_NUMB_BITS / K2;
      l = n / K2;
      Nprime2 = ((2 * M2 + k + 2 + maxLK) / maxLK) * maxLK;
      /* Nprime2 = ceil((2*M2+k+3)/maxLK)*maxLK*/
      nprime2 = Nprime2 / GMP_NUMB_BITS;

      /* we should ensure that nprime2 is a multiple of the next K */
      if (nprime2 >= (sqr ? SQR_FFT_MODF_THRESHOLD : MUL_FFT_MODF_THRESHOLD))
	{
	  unsigned long K3;
	  while (nprime2 % (K3 = 1 << mpn_fft_best_k (nprime2, sqr)))
	    {
	      nprime2 = ((nprime2 + K3 - 1) / K3) * K3;
	      Nprime2 = nprime2 * BITS_PER_MP_LIMB;
	      /* warning: since nprime2 changed, K3 may change too! */
	    }
	  ASSERT(nprime2 % K3 == 0);
	}
      ASSERT_ALWAYS(nprime2 < n); /* otherwise we'll loop */

      Mp2 = Nprime2 / K2;

      Ap = TMP_ALLOC_MP_PTRS (K2);
      Bp = TMP_ALLOC_MP_PTRS (K2);
      A = TMP_ALLOC_LIMBS (2 * K2 * (nprime2 + 1));
      T = TMP_ALLOC_LIMBS (2 * (nprime2 + 1));
      B = A + K2 * (nprime2 + 1);
      _fft_l = TMP_ALLOC_TYPE (k + 1, int *);
      for (i = 0; i <= k; i++)
	_fft_l[i] = TMP_ALLOC_TYPE (1<<i, int);
      mpn_fft_initl (_fft_l, k);

      TRACE (printf ("recurse: %dx%d limbs -> %d times %dx%d (%1.2f)\n", n,
		    n, K2, nprime2, nprime2, 2.0*(double)n/nprime2/K2));
      for (i = 0; i < K; i++, ap++, bp++)
	{
	  mpn_fft_normalize (*ap, n);
	  if (!sqr)
	    mpn_fft_normalize (*bp, n);
	  mpn_mul_fft_internal (*ap, *ap, *bp, n, k, K2, Ap, Bp, A, B, nprime2,
			       l, Mp2, _fft_l, T, 1);
	}
    }
  else
    {
      mp_ptr a, b, tp, tpn;
      mp_limb_t cc;
      int n2 = 2 * n;
      tp = TMP_ALLOC_LIMBS (n2);
      tpn = tp + n;
      TRACE (printf ("  mpn_mul_n %d of %d limbs\n", K, n));
      for (i = 0; i < K; i++)
	{
	  a = *ap++;
	  b = *bp++;
	  if (sqr)
	    mpn_sqr_n (tp, a, n);
	  else
	    mpn_mul_n (tp, b, a, n);
	  if (a[n] != 0)
	    cc = mpn_add_n (tpn, tpn, b, n);
	  else
	    cc = 0;
	  if (b[n] != 0)
	    cc += mpn_add_n (tpn, tpn, a, n) + a[n];
	  if (cc != 0)
	    {
	      cc = mpn_add_1 (tp, tp, n2, cc);
	      ASSERT_NOCARRY (mpn_add_1 (tp, tp, n2, cc));
	    }
	  a[n] = mpn_sub_n (a, tp, tpn, n) && mpn_add_1 (a, a, n, CNST_LIMB(1));
	}
    }
  TMP_FREE;
}


/* input: A^[l[k][0]] A^[l[k][1]] ... A^[l[k][K-1]]
   output: K*A[0] K*A[K-1] ... K*A[1].
   Assumes the Ap[] are pseudo-normalized, i.e. 0 <= Ap[][n] <= 1.
   This condition is also fulfilled at exit.
*/

static void
mpn_fft_fftinv (mp_ptr *Ap, int K, mp_size_t omega, mp_size_t n, mp_ptr tp)
{
  if (K == 2)
    {
      mp_limb_t cy;
#if HAVE_NATIVE_mpn_addsub_n
      cy = mpn_addsub_n (Ap[0], Ap[1], Ap[0], Ap[1], n + 1) & 1;
#else
      MPN_COPY (tp, Ap[0], n + 1);
      mpn_add_n (Ap[0], Ap[0], Ap[1], n + 1);
      cy = mpn_sub_n (Ap[1], tp, Ap[1], n + 1);
#endif
      if (Ap[0][n] > CNST_LIMB(1)) /* can be 2 or 3 */
	Ap[0][n] = CNST_LIMB(1) - mpn_sub_1 (Ap[0], Ap[0], n, Ap[0][n] - CNST_LIMB(1));
      if (cy) /* Ap[1][n] can be -1 or -2 */
	Ap[1][n] = mpn_add_1 (Ap[1], Ap[1], n, ~Ap[1][n] + CNST_LIMB(1));
    }
  else
    {
      int j, K2 = K / 2;
      mp_ptr *Bp = Ap + K2, tmp;
      TMP_DECL;

      TMP_MARK;
      tmp = TMP_ALLOC_LIMBS (n + 1);
      mpn_fft_fftinv (Ap, K2, 2 * omega, n, tp);
      mpn_fft_fftinv (Bp, K2, 2 * omega, n, tp);
      /* A[j]     <- A[j] + omega^j A[j+K/2]
	 A[j+K/2] <- A[j] + omega^(j+K/2) A[j+K/2] */
      for (j = 0; j < K2; j++, Ap++, Bp++)
	{
	  /* Bp[0] <- Ap[0] + Bp[0] * 2^((j + K2) * omega)
	     Ap[0] <- Ap[0] + Bp[0] * 2^(j * omega)
	  */
	  mpn_fft_mul_2exp_modF (tp,  Bp[0], (j + K2) * omega, n);
	  mpn_fft_mul_2exp_modF (tmp, Bp[0], j * omega, n);
	  mpn_fft_add_modF	(Bp[0], Ap[0], tp, n);
	  mpn_fft_add_modF	(Ap[0], Ap[0], tmp, n);
	}
      TMP_FREE;
    }
}


/* A <- A/2^k mod 2^(n*GMP_NUMB_BITS)+1 */
static void
mpn_fft_div_2exp_modF (mp_ptr r, mp_srcptr a, int k, mp_size_t n)
{
  int i;

  ASSERT (r != a);
  i = 2 * n * GMP_NUMB_BITS;
  i = (i - k) % i;
  mpn_fft_mul_2exp_modF (r, a, i, n);
  /* 1/2^k = 2^(2nL-k) mod 2^(n*GMP_NUMB_BITS)+1 */
  /* normalize so that R < 2^(n*GMP_NUMB_BITS)+1 */
  mpn_fft_normalize (r, n);
}


/* {rp,n} <- {ap,an} mod 2^(n*GMP_NUMB_BITS)+1, n <= an <= 3*n.
   Returns carry out, i.e. 1 iff {ap,an} = -1 mod 2^(n*GMP_NUMB_BITS)+1,
   then {rp,n}=0.
*/
static int
mpn_fft_norm_modF (mp_ptr rp, mp_size_t n, mp_ptr ap, mp_size_t an)
{
  mp_size_t l;
  long int m;
  mp_limb_t cc;
  int rpn;

  ASSERT ((n <= an) && (an <= 3 * n));
  m = an - 2 * n;
  if (m > 0)
    {
      l = n;
      /* add {ap, m} and {ap+2n, m} in {rp, m} */
      cc = mpn_add_n (rp, ap, ap + 2 * n, m);
      /* copy {ap+m, n-m} to {rp+m, n-m} */
      rpn = mpn_add_1 (rp + m, ap + m, n - m, cc);
    }
  else
    {
      l = an - n; /* l <= n */
      MPN_COPY (rp, ap, n);
      rpn = 0;
    }

  /* remains to subtract {ap+n, l} from {rp, n+1} */
  cc = mpn_sub_n (rp, rp, ap + n, l);
  rpn -= mpn_sub_1 (rp + l, rp + l, n - l, cc);
  if (rpn < 0) /* necessarily rpn = -1 */
    rpn = mpn_add_1 (rp, rp, n, CNST_LIMB(1));
  return rpn;
}

/* store in A[0..nprime] the first M bits from {n, nl},
   in A[nprime+1..] the following M bits, ...
   Assumes M is a multiple of GMP_NUMB_BITS (M = l * GMP_NUMB_BITS).
   T must have space for at least (nprime + 1) limbs.
   We must have nl <= 2*K*l.
*/
static void
mpn_mul_fft_decompose (mp_ptr A, mp_ptr *Ap, int K, int nprime, mp_srcptr n,
		       mp_size_t nl, int l, int Mp, mp_ptr T)
{
  int i, j;
  mp_ptr tmp;
  mp_size_t Kl = K * l;
  TMP_DECL;
  TMP_MARK;

  if (nl > Kl) /* normalize {n, nl} mod 2^(Kl*GMP_NUMB_BITS)+1 */
    {
      mp_size_t dif = nl - Kl;
      mp_limb_signed_t cy;

      tmp = TMP_ALLOC_LIMBS(Kl + 1);

      if (dif > Kl)
	{
	  int subp = 0;

	  cy = mpn_sub_n (tmp, n, n + Kl, Kl);
	  n += 2 * Kl;
	  dif -= Kl;

	  /* now dif > 0 */
	  while (dif > Kl)
	    {
	      if (subp)
		cy += mpn_sub_n (tmp, tmp, n, Kl);
	      else
		cy -= mpn_add_n (tmp, tmp, n, Kl);
	      subp ^= 1;
	      n += Kl;
	      dif -= Kl;
	    }
	  /* now dif <= Kl */
	  if (subp)
	    cy += mpn_sub (tmp, tmp, Kl, n, dif);
	  else
	    cy -= mpn_add (tmp, tmp, Kl, n, dif);
	  if (cy >= 0)
	    cy = mpn_add_1 (tmp, tmp, Kl, cy);
	  else
	    cy = mpn_sub_1 (tmp, tmp, Kl, -cy);
	}
      else /* dif <= Kl, i.e. nl <= 2 * Kl */
	{
	  cy = mpn_sub (tmp, n, Kl, n + Kl, dif);
	  cy = mpn_add_1 (tmp, tmp, Kl, cy);
	}
      tmp[Kl] = cy;
      nl = Kl + 1;
      n = tmp;
    }
  for (i = 0; i < K; i++)
    {
      Ap[i] = A;
      /* store the next M bits of n into A[0..nprime] */
      if (nl > 0) /* nl is the number of remaining limbs */
	{
	  j = (l <= nl && i < K - 1) ? l : nl; /* store j next limbs */
	  nl -= j;
	  MPN_COPY (T, n, j);
	  MPN_ZERO (T + j, nprime + 1 - j);
	  n += l;
	  mpn_fft_mul_2exp_modF (A, T, i * Mp, nprime);
	}
      else
	MPN_ZERO (A, nprime + 1);
      A += nprime + 1;
    }
  ASSERT_ALWAYS (nl == 0);
  TMP_FREE;
}

/* op <- n*m mod 2^N+1 with fft of size 2^k where N=pl*GMP_NUMB_BITS
   n and m have respectively nl and ml limbs
   op must have space for pl+1 limbs if rec=1 (and pl limbs if rec=0).
   One must have pl = mpn_fft_next_size (pl, k).
   T must have space for 2 * (nprime + 1) limbs.

   If rec=0, then store only the pl low bits of the result, and return
   the out carry.
*/

static int
mpn_mul_fft_internal (mp_ptr op, mp_srcptr n, mp_srcptr m, mp_size_t pl,
		      int k, int K,
		      mp_ptr *Ap, mp_ptr *Bp,
		      mp_ptr A, mp_ptr B,
		      mp_size_t nprime, mp_size_t l, mp_size_t Mp,
		      int **_fft_l,
		      mp_ptr T, int rec)
{
  int i, sqr, pla, lo, sh, j;
  mp_ptr p;

  sqr = n == m;

  TRACE (printf ("pl=%d k=%d K=%d np=%d l=%d Mp=%d rec=%d sqr=%d\n",
		 pl,k,K,nprime,l,Mp,rec,sqr));

  /* decomposition of inputs into arrays Ap[i] and Bp[i] */
  if (rec)
    {
      mpn_mul_fft_decompose (A, Ap, K, nprime, n, K * l + 1, l, Mp, T);
      if (!sqr)
	mpn_mul_fft_decompose (B, Bp, K, nprime, m, K * l + 1, l, Mp, T);
    }

  /* direct fft's */
  if (sqr)
    mpn_fft_fft_sqr (Ap, K, _fft_l + k, 2 * Mp, nprime, 1, T);
  else
    mpn_fft_fft (Ap, Bp, K, _fft_l + k, 2 * Mp, nprime, 1, T);

  /* term to term multiplications */
  mpn_fft_mul_modF_K (Ap, (sqr) ? Ap : Bp, nprime, K);

  /* inverse fft's */
  mpn_fft_fftinv (Ap, K, 2 * Mp, nprime, T);

  /* division of terms after inverse fft */
  Bp[0] = T + nprime + 1;
  mpn_fft_div_2exp_modF (Bp[0], Ap[0], k, nprime);
  for (i = 1; i < K; i++)
    {
      Bp[i] = Ap[i-1];
      mpn_fft_div_2exp_modF (Bp[i], Ap[i], k + ((K - i) % K) * Mp, nprime);
    }

  /* addition of terms in result p */
  MPN_ZERO (T, nprime + 1);
  pla = l * (K - 1) + nprime + 1; /* number of required limbs for p */
  p = B; /* B has K*(n' + 1) limbs, which is >= pla, i.e. enough */
  MPN_ZERO (p, pla);
  sqr = 0; /* will accumulate the (signed) carry at p[pla] */
  for (i = K - 1, lo = l * i + nprime,sh = l * i; i >= 0; i--,lo -= l,sh -= l)
    {
      mp_ptr n = p + sh;

      j = (K - i) % K;
      if (mpn_add_n (n, n, Bp[j], nprime + 1))
	sqr += mpn_add_1 (n + nprime + 1, n + nprime + 1,
			  pla - sh - nprime - 1, CNST_LIMB(1));
      T[2 * l] = i + 1; /* T = (i + 1)*2^(2*M) */
      if (mpn_cmp (Bp[j], T, nprime + 1) > 0)
	{ /* subtract 2^N'+1 */
	  sqr -= mpn_sub_1 (n, n, pla - sh, CNST_LIMB(1));
	  sqr -= mpn_sub_1 (p + lo, p + lo, pla - lo, CNST_LIMB(1));
	}
    }
    if (sqr == -1)
      {
	if ((sqr = mpn_add_1 (p + pla - pl,p + pla - pl, pl, CNST_LIMB(1))))
	  {
	    /* p[pla-pl]...p[pla-1] are all zero */
	    mpn_sub_1 (p + pla - pl - 1, p + pla - pl - 1, pl + 1, CNST_LIMB(1));
	    mpn_sub_1 (p + pla - 1, p + pla - 1, 1, CNST_LIMB(1));
	  }
      }
    else if (sqr == 1)
      {
	if (pla >= 2 * pl)
	  {
	    while ((sqr = mpn_add_1 (p + pla - 2 * pl, p + pla - 2 * pl, 2 * pl, (mp_limb_t) sqr)))
	      ;
	  }
	else
	  {
	    sqr = mpn_sub_1 (p + pla - pl, p + pla - pl, pl, (mp_limb_t) sqr);
	    ASSERT (sqr == 0);
	  }
      }
    else
      ASSERT (sqr == 0);

    /* here p < 2^(2M) [K 2^(M(K-1)) + (K-1) 2^(M(K-2)) + ... ]
       < K 2^(2M) [2^(M(K-1)) + 2^(M(K-2)) + ... ]
       < K 2^(2M) 2^(M(K-1))*2 = 2^(M*K+M+k+1) */
    i = mpn_fft_norm_modF (op, pl, p, pla);
    if (rec) /* store the carry out */
      op[pl] = i;

    return i;
}

/* return the lcm of a and 2^k */
static unsigned long int
mpn_mul_fft_lcm (unsigned long int a, unsigned int k)
{
  unsigned long int l = 1;

  while ((a % 2 == 0) && (k > 0))
    {
      a >>= 1;
      k --;
      l <<= 1;
    }
  return (l * a) << k;
}

int
mpn_mul_fft (mp_ptr op, mp_size_t pl,
	     mp_srcptr n, mp_size_t nl,
	     mp_srcptr m, mp_size_t ml,
	     int k)
{
  int K, maxLK, i;
  mp_size_t N, Nprime, nprime, M, Mp, l;
  mp_ptr *Ap, *Bp, A, T, B;
  int **_fft_l;
  int sqr = (n == m && nl == ml);
  TMP_DECL;

  TRACE (printf ("\nmpn_mul_fft pl=%ld nl=%ld ml=%ld k=%d\n", pl, nl, ml, k));
  ASSERT_ALWAYS (mpn_fft_next_size (pl, k) == pl);

  TMP_MARK;
  N = pl * GMP_NUMB_BITS;
  _fft_l = TMP_ALLOC_TYPE (k + 1, int *);
  for (i = 0; i <= k; i++)
    _fft_l[i] = TMP_ALLOC_TYPE (1 << i, int);
  mpn_fft_initl (_fft_l, k);
  K = 1 << k;
  M = N / K;	/* N = 2^k M */
  l = 1 + (M - 1) / GMP_NUMB_BITS;
  maxLK = mpn_mul_fft_lcm ((unsigned long) GMP_NUMB_BITS, k); /* lcm (GMP_NUMB_BITS, 2^k) */

  Nprime = ((2 * M + k + 2 + maxLK) / maxLK) * maxLK;
  /* Nprime = ceil((2*M+k+3)/maxLK)*maxLK; */
  nprime = Nprime / GMP_NUMB_BITS;
  TRACE (printf ("N=%d K=%d, M=%d, l=%d, maxLK=%d, Np=%d, np=%d\n",
		 N, K, M, l, maxLK, Nprime, nprime));
  /* we should ensure that recursively, nprime is a multiple of the next K */
  if (nprime >= (sqr ? SQR_FFT_MODF_THRESHOLD : MUL_FFT_MODF_THRESHOLD))
    {
      unsigned long K2;
      while (nprime % (K2 = 1 << mpn_fft_best_k (nprime, sqr)))
	{
	  nprime = ((nprime + K2 - 1) / K2) * K2;
	  Nprime = nprime * BITS_PER_MP_LIMB;
	  /* warning: since nprime changed, K2 may change too! */
	}
      TRACE (printf ("new maxLK=%d, Np=%d, np=%d\n", maxLK, Nprime, nprime));
      ASSERT(nprime % K2 == 0);
    }
  ASSERT_ALWAYS (nprime < pl); /* otherwise we'll loop */

  T = TMP_ALLOC_LIMBS (2 * (nprime + 1));
  Mp = Nprime / K;

  TRACE (printf ("%dx%d limbs -> %d times %dx%d limbs (%1.2f)\n",
		pl, pl, K, nprime, nprime, 2.0 * (double) N / Nprime / K);
	 printf ("   temp space %ld\n", 2 * K * (nprime + 1)));

  A = __GMP_ALLOCATE_FUNC_LIMBS (2 * K * (nprime + 1));
  B = A + K * (nprime + 1);
  Ap = TMP_ALLOC_MP_PTRS (K);
  Bp = TMP_ALLOC_MP_PTRS (K);

  /* special decomposition for main call */
  /* nl is the number of significant limbs in n */
  mpn_mul_fft_decompose (A, Ap, K, nprime, n, nl, l, Mp, T);
  if (n != m)
    mpn_mul_fft_decompose (B, Bp, K, nprime, m, ml, l, Mp, T);

  i = mpn_mul_fft_internal (op, n, m, pl, k, K, Ap, Bp, A, B, nprime, l, Mp, _fft_l, T, 0);

  TMP_FREE;
  __GMP_FREE_FUNC_LIMBS (A, 2 * K * (nprime + 1));

  return i;
}

/* multiply {n, nl} by {m, ml}, and put the result in {op, nl+ml} */
void
mpn_mul_fft_full (mp_ptr op,
		  mp_srcptr n, mp_size_t nl,
		  mp_srcptr m, mp_size_t ml)
{
  mp_ptr pad_op;
  mp_size_t pl, pl2, pl3, l;
  int k2, k3;
  int sqr = (n == m && nl == ml);
  int cc, c2, oldcc;

  pl = nl + ml; /* total number of limbs of the result */

  /* perform a fft mod 2^(2N)+1 and one mod 2^(3N)+1.
     We must have pl3 = 3/2 * pl2, with pl2 a multiple of 2^k2, and
     pl3 a multiple of 2^k3. Since k3 >= k2, both are multiples of 2^k2,
     and pl2 must be an even multiple of 2^k2. Thus (pl2,pl3) =
     (2*j*2^k2,3*j*2^k2), which works for 3*j <= pl/2^k2 <= 5*j.
     We need that consecutive intervals overlap, i.e. 5*j >= 3*(j+1),
     which requires j>=2. Thus this scheme requires pl >= 6 * 2^FFT_FIRST_K. */

  /*  ASSERT_ALWAYS(pl >= 6 * (1 << FFT_FIRST_K)); */

  pl2 = (2 * pl - 1) / 5; /* ceil (2pl/5) - 1 */
  do
    {
      pl2 ++;
      k2 = mpn_fft_best_k (pl2, sqr); /* best fft size for pl2 limbs */
      pl2 = mpn_fft_next_size (pl2, k2);
      pl3 = 3 * pl2 / 2; /* since k>=FFT_FIRST_K=4, pl2 is a multiple of 2^4,
			    thus pl2 / 2 is exact */
      /*      k3 = mpn_fft_best_k (pl3, sqr); */
      k3 = k2;
    }
  while (mpn_fft_next_size (pl3, k3) != pl3);

  TRACE (printf ("mpn_mul_fft_full nl=%ld ml=%ld -> pl2=%ld pl3=%ld k=%d\n",
		 nl, ml, pl2, pl3, k2));

  ASSERT_ALWAYS(pl3 <= pl);
  mpn_mul_fft (op, pl3, n, nl, m, ml, k3);     /* mu */
  pad_op = __GMP_ALLOCATE_FUNC_LIMBS (pl2);
  mpn_mul_fft (pad_op, pl2, n, nl, m, ml, k2); /* lambda */
  cc = mpn_sub_n (pad_op, pad_op, op, pl2);    /* lambda - low(mu) */
  /* 0 <= cc <= 1 */
  l = pl3 - pl2; /* l = pl2 / 2 since pl3 = 3/2 * pl2 */
  c2 = mpn_add_n (pad_op, pad_op, op + pl2, l);
  cc = mpn_add_1 (pad_op + l, pad_op + l, l, (mp_limb_t) c2) - cc; /* -1 <= cc <= 1 */
  if (cc < 0)
    cc = mpn_add_1 (pad_op, pad_op, pl2, (mp_limb_t) -cc);
  /* 0 <= cc <= 1 */
  /* now lambda-mu = {pad_op, pl2} - cc mod 2^(pl2*GMP_NUMB_BITS)+1 */
  oldcc = cc;
#if HAVE_NATIVE_mpn_addsub_n
  c2 = mpn_addsub_n (pad_op + l, pad_op, pad_op, pad_op + l, l);
  /* c2 & 1 is the borrow, c2 & 2 is the carry */
  cc += c2 >> 1; /* carry out from high <- low + high */
  c2 = c2 & 1; /* borrow out from low <- low - high */
#else
  {
    mp_ptr tmp;
    TMP_DECL;

    TMP_MARK;
    tmp = TMP_ALLOC_LIMBS (l);
    MPN_COPY (tmp, pad_op, l);
    c2 = mpn_sub_n (pad_op,      pad_op, pad_op + l, l);
    cc += mpn_add_n (pad_op + l, tmp,    pad_op + l, l);
    TMP_FREE;
  }
#endif
  c2 += oldcc;
  /* first normalize {pad_op, pl2} before dividing by 2: c2 is the borrow
     at pad_op + l, cc is the carry at pad_op + pl2 */
  /* 0 <= cc <= 2 */
  cc -= mpn_sub_1 (pad_op + l, pad_op + l, l, (mp_limb_t) c2);
  /* -1 <= cc <= 2 */
  if (cc > 0)
    cc = -mpn_sub_1 (pad_op, pad_op, pl2, (mp_limb_t) cc);
  /* now -1 <= cc <= 0 */
  if (cc < 0)
    cc = mpn_add_1 (pad_op, pad_op, pl2, (mp_limb_t) -cc);
  /* now {pad_op, pl2} is normalized, with 0 <= cc <= 1 */
  if (pad_op[0] & 1) /* if odd, add 2^(pl2*GMP_NUMB_BITS)+1 */
    cc += 1 + mpn_add_1 (pad_op, pad_op, pl2, CNST_LIMB(1));
  /* now 0 <= cc <= 2, but cc=2 cannot occur since it would give a carry
     out below */
  mpn_rshift (pad_op, pad_op, pl2, 1); /* divide by two */
  if (cc) /* then cc=1 */
    pad_op [pl2 - 1] |= (mp_limb_t) 1 << (GMP_NUMB_BITS - 1);
  /* now {pad_op,pl2}-cc = (lambda-mu)/(1-2^(l*GMP_NUMB_BITS))
     mod 2^(pl2*GMP_NUMB_BITS) + 1 */
  c2 = mpn_add_n (op, op, pad_op, pl2); /* no need to add cc (is 0) */
  /* since pl2+pl3 >= pl, necessary the extra limbs (including cc) are zero */
  MPN_COPY (op + pl3, pad_op, pl - pl3);
  ASSERT_MPN_ZERO_P (pad_op + pl - pl3, pl2 + pl3 - pl);
  __GMP_FREE_FUNC_LIMBS (pad_op, pl2);
  /* since the final result has at most pl limbs, no carry out below */
  mpn_add_1 (op + pl2, op + pl2, pl - pl2, (mp_limb_t) c2);
}
