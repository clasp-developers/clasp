/* Test mpz_cmp, mpz_mul.

Copyright 1991, 1993, 1994, 1996, 1997, 2000, 2001, 2002, 2003, 2004 Free
Software Foundation, Inc.

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

#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"
#include "tests.h"

void debug_mp _PROTO ((mpz_t));
static void ref_mpn_mul _PROTO ((mp_ptr,mp_srcptr,mp_size_t,mp_srcptr,mp_size_t));
static void ref_mpz_mul _PROTO ((mpz_t, const mpz_t, const mpz_t));
void dump_abort _PROTO ((int, char *, mpz_t, mpz_t, mpz_t, mpz_t));

#define FFT_MIN_BITSIZE 100000

char *extra_fft;

void
one (int i, mpz_t multiplicand, mpz_t multiplier)
{
  mpz_t product, ref_product;
  mpz_t quotient;

  mpz_init (product);
  mpz_init (ref_product);
  mpz_init (quotient);

  /* Test plain multiplication comparing results against reference code.  */
  mpz_mul (product, multiplier, multiplicand);
  ref_mpz_mul (ref_product, multiplier, multiplicand);
  if (mpz_cmp (product, ref_product))
    dump_abort (i, "incorrect plain product",
		multiplier, multiplicand, product, ref_product);

  /* Test squaring, comparing results against plain multiplication  */
  mpz_mul (product, multiplier, multiplier);
  mpz_set (multiplicand, multiplier);
  mpz_mul (ref_product, multiplier, multiplicand);
  if (mpz_cmp (product, ref_product))
    dump_abort (i, "incorrect square product",
		multiplier, multiplier, product, ref_product);

  mpz_clear (product);
  mpz_clear (ref_product);
  mpz_clear (quotient);
}

int
main (int argc, char **argv)
{
  mpz_t op1, op2;
  int i;

  gmp_randstate_ptr rands;
  mpz_t bs;
  unsigned long bsi, size_range, fsize_range;

  tests_start ();
  rands = RANDS;

  extra_fft = getenv ("GMP_CHECK_FFT");

  mpz_init (bs);
  mpz_init (op1);
  mpz_init (op2);

  fsize_range = 4 << 8;		/* a fraction 1/256 of size_range */
  for (i = 0; fsize_range >> 8 < (extra_fft ? 27 : 22); i++)
    {
      size_range = fsize_range >> 8;
      fsize_range = fsize_range * 33 / 32;

      mpz_urandomb (bs, rands, size_range);
      mpz_rrandomb (op1, rands, mpz_get_ui (bs));
      mpz_urandomb (bs, rands, size_range);
      mpz_rrandomb (op2, rands, mpz_get_ui (bs));

      mpz_urandomb (bs, rands, 4);
      bsi = mpz_get_ui (bs);
      if ((bsi & 0x3) == 0)
	mpz_neg (op1, op1);
      if ((bsi & 0xC) == 0)
	mpz_neg (op2, op2);

      /* printf ("%d %d\n", SIZ (op1), SIZ (op2)); */
      one (i, op2, op1);
    }

  if (extra_fft)
    for (i = -50; i < 0; i++)
      {
	mpz_urandomb (bs, rands, 32);
	size_range = mpz_get_ui (bs) % 27;

	mpz_urandomb (bs, rands, size_range);
	mpz_rrandomb (op1, rands, mpz_get_ui (bs) + FFT_MIN_BITSIZE);
	mpz_urandomb (bs, rands, size_range);
	mpz_rrandomb (op2, rands, mpz_get_ui (bs) + FFT_MIN_BITSIZE);

	/* printf ("%d: %d %d\n", i, SIZ (op1), SIZ (op2)); */
	fflush (stdout);
	one (-1, op2, op1);
      }

  mpz_clear (bs);
  mpz_clear (op1);
  mpz_clear (op2);

  tests_end ();
  exit (0);
}

static void
ref_mpz_mul (mpz_t w, const mpz_t u, const mpz_t v)
{
  mp_size_t usize = u->_mp_size;
  mp_size_t vsize = v->_mp_size;
  mp_size_t wsize;
  mp_size_t sign_product;
  mp_ptr up, vp;
  mp_ptr wp;
  mp_size_t talloc;

  sign_product = usize ^ vsize;
  usize = ABS (usize);
  vsize = ABS (vsize);

  if (usize == 0 || vsize == 0)
    {
      SIZ (w) = 0;
      return;
    }

  talloc = usize + vsize;

  up = u->_mp_d;
  vp = v->_mp_d;

  wp = __GMP_ALLOCATE_FUNC_LIMBS (talloc);

  if (usize > vsize)
    ref_mpn_mul (wp, up, usize, vp, vsize);
  else
    ref_mpn_mul (wp, vp, vsize, up, usize);
  wsize = usize + vsize;
  wsize -= wp[wsize - 1] == 0;
  MPZ_REALLOC (w, wsize);
  MPN_COPY (PTR(w), wp, wsize);

  SIZ(w) = sign_product < 0 ? -wsize : wsize;
  __GMP_FREE_FUNC_LIMBS (wp, talloc);
}

static void mul_basecase __GMP_PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t));

#define TOOM3_THRESHOLD (MAX (MUL_TOOM3_THRESHOLD, SQR_TOOM3_THRESHOLD))
#define FFT_THRESHOLD (MAX (MUL_FFT_THRESHOLD, SQR_FFT_THRESHOLD))

static void
ref_mpn_mul (mp_ptr wp, mp_srcptr up, mp_size_t un, mp_srcptr vp, mp_size_t vn)
{
  mp_ptr tp;
  mp_size_t tn;
  mp_limb_t cy;

  if (vn < TOOM3_THRESHOLD)
    {
      /* In the mpn_mul_basecase and mpn_kara_mul_n range, use our own
	 mul_basecase.  */
      if (vn != 0)
	mul_basecase (wp, up, un, vp, vn);
      else
	MPN_ZERO (wp, un);
      return;
    }

  if (vn < FFT_THRESHOLD)
    {
      /* In the mpn_toom3_mul_n range, use mpn_kara_mul_n.  */
      tn = 2 * vn + MPN_KARA_MUL_N_TSIZE (vn);
      tp = __GMP_ALLOCATE_FUNC_LIMBS (tn);
      mpn_kara_mul_n (tp, up, vp, vn, tp + 2 * vn);
    }
  else
    {
      /* Finally, for the largest operands, use mpn_toom3_mul_n.  */
      /* The "- 63 + 255" tweaks the allocation to allow for huge operands.
	 See the definition of this macro in gmp-impl.h to understand this.  */
      tn = 2 * vn + MPN_TOOM3_MUL_N_TSIZE (vn) - 63 + 255;
      tp = __GMP_ALLOCATE_FUNC_LIMBS (tn);
      mpn_toom3_mul_n (tp, up, vp, vn, tp + 2 * vn);
    }

  if (un != vn)
    {
      if (un - vn < vn)
	ref_mpn_mul (wp + vn, vp, vn, up + vn, un - vn);
      else
	ref_mpn_mul (wp + vn, up + vn, un - vn, vp, vn);

      MPN_COPY (wp, tp, vn);
      cy = mpn_add_n (wp + vn, wp + vn, tp + vn, vn);
      mpn_incr_u (wp + 2 * vn, cy);
    }
  else
    {
      MPN_COPY (wp, tp, 2 * vn);
    }

  __GMP_FREE_FUNC_LIMBS (tp, tn);
}

static void
mul_basecase (mp_ptr wp, mp_srcptr up, mp_size_t un, mp_srcptr vp, mp_size_t vn)
{
  mp_size_t i, j;
  mp_limb_t prod_low, prod_high;
  mp_limb_t cy_dig;
  mp_limb_t v_limb;

  /* Multiply by the first limb in V separately, as the result can
     be stored (not added) to PROD.  We also avoid a loop for zeroing.  */
  v_limb = vp[0];
  cy_dig = 0;
  for (j = un; j > 0; j--)
    {
      mp_limb_t u_limb, w_limb;
      u_limb = *up++;
      umul_ppmm (prod_high, prod_low, u_limb, v_limb << GMP_NAIL_BITS);
      add_ssaaaa (cy_dig, w_limb, prod_high, prod_low, 0, cy_dig << GMP_NAIL_BITS);
      *wp++ = w_limb >> GMP_NAIL_BITS;
    }

  *wp++ = cy_dig;
  wp -= un;
  up -= un;

  /* For each iteration in the outer loop, multiply one limb from
     U with one limb from V, and add it to PROD.  */
  for (i = 1; i < vn; i++)
    {
      v_limb = vp[i];
      cy_dig = 0;

      for (j = un; j > 0; j--)
	{
	  mp_limb_t u_limb, w_limb;
	  u_limb = *up++;
	  umul_ppmm (prod_high, prod_low, u_limb, v_limb << GMP_NAIL_BITS);
	  w_limb = *wp;
	  add_ssaaaa (prod_high, prod_low, prod_high, prod_low, 0, w_limb << GMP_NAIL_BITS);
	  prod_low >>= GMP_NAIL_BITS;
	  prod_low += cy_dig;
#if GMP_NAIL_BITS == 0
	  cy_dig = prod_high + (prod_low < cy_dig);
#else
	  cy_dig = prod_high;
	  cy_dig += prod_low >> GMP_NUMB_BITS;
#endif
	  *wp++ = prod_low & GMP_NUMB_MASK;
	}

      *wp++ = cy_dig;
      wp -= un;
      up -= un;
    }
}

void
dump_abort (int i, char *s,
            mpz_t op1, mpz_t op2, mpz_t product, mpz_t ref_product)
{
  fprintf (stderr, "ERROR: %s in test %d\n", s, i);
  fprintf (stderr, "op1          = "); debug_mp (op1);
  fprintf (stderr, "op2          = "); debug_mp (op2);
  fprintf (stderr, "    product  = "); debug_mp (product);
  fprintf (stderr, "ref_product  = "); debug_mp (ref_product);
  abort();
}

void
debug_mp (mpz_t x)
{
  size_t siz = mpz_sizeinbase (x, 16);

  if (siz > 65)
    {
      mpz_t q;
      mpz_init (q);
      mpz_tdiv_q_2exp (q, x, 4 * (mpz_sizeinbase (x, 16) - 25));
      gmp_fprintf (stderr, "%ZX...", q);
      mpz_tdiv_r_2exp (q, x, 4 * 25);
      gmp_fprintf (stderr, "%025ZX [%d]\n", q, (int) siz);
      mpz_clear (q);
    }
  else
    {
      gmp_fprintf (stderr, "%ZX\n", x);
    }
}
