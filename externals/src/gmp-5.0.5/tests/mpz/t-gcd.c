/* Test mpz_gcd, mpz_gcdext, and mpz_gcd_ui.

Copyright 1991, 1993, 1994, 1996, 1997, 2000, 2001, 2002, 2003, 2004, 2005,
2008, 2009 Free Software Foundation, Inc.

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

void one_test __GMP_PROTO ((mpz_t, mpz_t, mpz_t, int));
void debug_mp __GMP_PROTO ((mpz_t, int));

static int gcdext_valid_p __GMP_PROTO ((const mpz_t a, const mpz_t b, const mpz_t g, const mpz_t s));

void
check_data (void)
{
  static const struct {
    const char *a;
    const char *b;
    const char *want;
  } data[] = {
    /* This tickled a bug in gmp 4.1.2 mpn/x86/k6/gcd_finda.asm. */
    { "0x3FFC000007FFFFFFFFFF00000000003F83FFFFFFFFFFFFFFF80000000000000001",
      "0x1FFE0007FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC000000000000000000000001",
      "5" }
  };

  mpz_t  a, b, got, want;
  int    i;

  mpz_init (a);
  mpz_init (b);
  mpz_init (got);
  mpz_init (want);

  for (i = 0; i < numberof (data); i++)
    {
      mpz_set_str_or_abort (a, data[i].a, 0);
      mpz_set_str_or_abort (b, data[i].b, 0);
      mpz_set_str_or_abort (want, data[i].want, 0);
      mpz_gcd (got, a, b);
      MPZ_CHECK_FORMAT (got);
      if (mpz_cmp (got, want) != 0)
	{
	  printf    ("mpz_gcd wrong on data[%d]\n", i);
	  printf    (" a  %s\n", data[i].a);
	  printf    (" b  %s\n", data[i].b);
	  mpz_trace (" a", a);
	  mpz_trace (" b", b);
	  mpz_trace (" want", want);
	  mpz_trace (" got ", got);
	  abort ();
	}
    }

  mpz_clear (a);
  mpz_clear (b);
  mpz_clear (got);
  mpz_clear (want);
}

/* Keep one_test's variables global, so that we don't need
   to reinitialize them for each test.  */
mpz_t gcd1, gcd2, s, t, temp1, temp2, temp3;

#if GCD_DC_THRESHOLD > GCDEXT_DC_THRESHOLD
#define MAX_SCHOENHAGE_THRESHOLD GCD_DC_THRESHOLD
#else
#define MAX_SCHOENHAGE_THRESHOLD GCDEXT_DC_THRESHOLD
#endif

/* Define this to make all operands be large enough for Schoenhage gcd
   to be used.  */
#ifndef WHACK_SCHOENHAGE
#define WHACK_SCHOENHAGE 0
#endif

#if WHACK_SCHOENHAGE
#define MIN_OPERAND_BITSIZE (MAX_SCHOENHAGE_THRESHOLD * GMP_NUMB_BITS)
#else
#define MIN_OPERAND_BITSIZE 1
#endif

int
main (int argc, char **argv)
{
  mpz_t op1, op2, ref;
  int i, j, chain_len;
  gmp_randstate_ptr rands;
  mpz_t bs;
  unsigned long bsi, size_range;
  int reps = 200;

  tests_start ();
  TESTS_REPS (reps, argv, argc);

  rands = RANDS;

  check_data ();

  mpz_init (bs);
  mpz_init (op1);
  mpz_init (op2);
  mpz_init (ref);
  mpz_init (gcd1);
  mpz_init (gcd2);
  mpz_init (temp1);
  mpz_init (temp2);
  mpz_init (temp3);
  mpz_init (s);
  mpz_init (t);

  /* Testcase to exercise the u0 == u1 case in mpn_gcdext_lehmer_n. */
  mpz_set_ui (op2, GMP_NUMB_MAX);
  mpz_mul_2exp (op1, op2, 100);
  mpz_add (op1, op1, op2);
  mpz_mul_ui (op2, op2, 2);
  one_test (op1, op2, NULL, -1);

#if 0
  mpz_set_str (op1, "4da8e405e0d2f70d6d679d3de08a5100a81ec2cff40f97b313ae75e1183f1df2b244e194ebb02a4ece50d943640a301f0f6cc7f539117b783c3f3a3f91649f8a00d2e1444d52722810562bce02fccdbbc8fe3276646e306e723dd3b", 16);
  mpz_set_str (op2, "76429e12e4fdd8929d89c21657097fbac09d1dc08cf7f1323a34e78ca34226e1a7a29b86fee0fa7fe2cc2a183d46d50df1fe7029590974ad7da77605f35f902cb8b9b8d22dd881eaae5919675d49a337145a029c3b33fc2b0", 16);
  one_test (op1, op2, NULL, -1);
#endif

  for (i = 0; i < reps; i++)
    {
      /* Generate plain operands with unknown gcd.  These types of operands
	 have proven to trigger certain bugs in development versions of the
	 gcd code.  The "hgcd->row[3].rsize > M" ASSERT is not triggered by
	 the division chain code below, but that is most likely just a result
	 of that other ASSERTs are triggered before it.  */

      mpz_urandomb (bs, rands, 32);
      size_range = mpz_get_ui (bs) % 17 + 2;

      mpz_urandomb (bs, rands, size_range);
      mpz_rrandomb (op1, rands, mpz_get_ui (bs) + MIN_OPERAND_BITSIZE);
      mpz_urandomb (bs, rands, size_range);
      mpz_rrandomb (op2, rands, mpz_get_ui (bs) + MIN_OPERAND_BITSIZE);

      mpz_urandomb (bs, rands, 8);
      bsi = mpz_get_ui (bs);

      if ((bsi & 0x3c) == 4)
	mpz_mul (op1, op1, op2);	/* make op1 a multiple of op2 */
      else if ((bsi & 0x3c) == 8)
	mpz_mul (op2, op1, op2);	/* make op2 a multiple of op1 */

      if ((bsi & 1) != 0)
	mpz_neg (op1, op1);
      if ((bsi & 2) != 0)
	mpz_neg (op2, op2);

      one_test (op1, op2, NULL, i);

      /* Generate a division chain backwards, allowing otherwise unlikely huge
	 quotients.  */

      mpz_set_ui (op1, 0);
      mpz_urandomb (bs, rands, 32);
      mpz_urandomb (bs, rands, mpz_get_ui (bs) % 16 + 1);
      mpz_rrandomb (op2, rands, mpz_get_ui (bs));
      mpz_add_ui (op2, op2, 1);
      mpz_set (ref, op2);

#if WHACK_SCHOENHAGE
      chain_len = 1000000;
#else
      mpz_urandomb (bs, rands, 32);
      chain_len = mpz_get_ui (bs) % (GMP_NUMB_BITS * MAX_SCHOENHAGE_THRESHOLD / 256);
#endif

      for (j = 0; j < chain_len; j++)
	{
	  mpz_urandomb (bs, rands, 32);
	  mpz_urandomb (bs, rands, mpz_get_ui (bs) % 12 + 1);
	  mpz_rrandomb (temp2, rands, mpz_get_ui (bs) + 1);
	  mpz_add_ui (temp2, temp2, 1);
	  mpz_mul (temp1, op2, temp2);
	  mpz_add (op1, op1, temp1);

	  /* Don't generate overly huge operands.  */
	  if (SIZ (op1) > 3 * MAX_SCHOENHAGE_THRESHOLD)
	    break;

	  mpz_urandomb (bs, rands, 32);
	  mpz_urandomb (bs, rands, mpz_get_ui (bs) % 12 + 1);
	  mpz_rrandomb (temp2, rands, mpz_get_ui (bs) + 1);
	  mpz_add_ui (temp2, temp2, 1);
	  mpz_mul (temp1, op1, temp2);
	  mpz_add (op2, op2, temp1);

	  /* Don't generate overly huge operands.  */
	  if (SIZ (op2) > 3 * MAX_SCHOENHAGE_THRESHOLD)
	    break;
	}
      one_test (op1, op2, ref, i);
    }

  mpz_clear (bs);
  mpz_clear (op1);
  mpz_clear (op2);
  mpz_clear (ref);
  mpz_clear (gcd1);
  mpz_clear (gcd2);
  mpz_clear (temp1);
  mpz_clear (temp2);
  mpz_clear (temp3);
  mpz_clear (s);
  mpz_clear (t);

  tests_end ();
  exit (0);
}

void
debug_mp (mpz_t x, int base)
{
  mpz_out_str (stderr, base, x); fputc ('\n', stderr);
}

void
one_test (mpz_t op1, mpz_t op2, mpz_t ref, int i)
{
  /*
  printf ("%ld %ld %ld\n", SIZ (op1), SIZ (op2), SIZ (ref));
  fflush (stdout);
  */

  /*
  fprintf (stderr, "op1=");  debug_mp (op1, -16);
  fprintf (stderr, "op2=");  debug_mp (op2, -16);
  */

  mpz_gcdext (gcd1, s, NULL, op1, op2);
  MPZ_CHECK_FORMAT (gcd1);
  MPZ_CHECK_FORMAT (s);

  if (ref && mpz_cmp (ref, gcd1) != 0)
    {
      fprintf (stderr, "ERROR in test %d\n", i);
      fprintf (stderr, "mpz_gcdext returned incorrect result\n");
      fprintf (stderr, "op1=");                 debug_mp (op1, -16);
      fprintf (stderr, "op2=");                 debug_mp (op2, -16);
      fprintf (stderr, "expected result:\n");   debug_mp (ref, -16);
      fprintf (stderr, "mpz_gcdext returns:\n");debug_mp (gcd1, -16);
      abort ();
    }

  if (!gcdext_valid_p(op1, op2, gcd1, s))
    {
      fprintf (stderr, "ERROR in test %d\n", i);
      fprintf (stderr, "mpz_gcdext returned invalid result\n");
      fprintf (stderr, "op1=");                 debug_mp (op1, -16);
      fprintf (stderr, "op2=");                 debug_mp (op2, -16);
      fprintf (stderr, "mpz_gcdext returns:\n");debug_mp (gcd1, -16);
      fprintf (stderr, "s=");                   debug_mp (s, -16);
      abort ();
    }

  mpz_gcd (gcd2, op1, op2);
  MPZ_CHECK_FORMAT (gcd2);

  if (mpz_cmp (gcd2, gcd1) != 0)
    {
      fprintf (stderr, "ERROR in test %d\n", i);
      fprintf (stderr, "mpz_gcd returned incorrect result\n");
      fprintf (stderr, "op1=");                 debug_mp (op1, -16);
      fprintf (stderr, "op2=");                 debug_mp (op2, -16);
      fprintf (stderr, "expected result:\n");   debug_mp (gcd1, -16);
      fprintf (stderr, "mpz_gcd returns:\n");   debug_mp (gcd2, -16);
      abort ();
    }

  /* This should probably move to t-gcd_ui.c */
  if (mpz_fits_ulong_p (op1) || mpz_fits_ulong_p (op2))
    {
      if (mpz_fits_ulong_p (op1))
	mpz_gcd_ui (gcd2, op2, mpz_get_ui (op1));
      else
	mpz_gcd_ui (gcd2, op1, mpz_get_ui (op2));
      if (mpz_cmp (gcd2, gcd1))
	{
	  fprintf (stderr, "ERROR in test %d\n", i);
	  fprintf (stderr, "mpz_gcd_ui returned incorrect result\n");
	  fprintf (stderr, "op1=");                 debug_mp (op1, -16);
	  fprintf (stderr, "op2=");                 debug_mp (op2, -16);
	  fprintf (stderr, "expected result:\n");   debug_mp (gcd1, -16);
	  fprintf (stderr, "mpz_gcd_ui returns:\n");   debug_mp (gcd2, -16);
	  abort ();
	}
    }

  mpz_gcdext (gcd2, temp1, temp2, op1, op2);
  MPZ_CHECK_FORMAT (gcd2);
  MPZ_CHECK_FORMAT (temp1);
  MPZ_CHECK_FORMAT (temp2);

  mpz_mul (temp1, temp1, op1);
  mpz_mul (temp2, temp2, op2);
  mpz_add (temp1, temp1, temp2);

  if (mpz_cmp (gcd1, gcd2) != 0
      || mpz_cmp (gcd2, temp1) != 0)
    {
      fprintf (stderr, "ERROR in test %d\n", i);
      fprintf (stderr, "mpz_gcdext returned incorrect result\n");
      fprintf (stderr, "op1=");                 debug_mp (op1, -16);
      fprintf (stderr, "op2=");                 debug_mp (op2, -16);
      fprintf (stderr, "expected result:\n");   debug_mp (gcd1, -16);
      fprintf (stderr, "mpz_gcdext returns:\n");debug_mp (gcd2, -16);
      abort ();
    }
}

/* Called when g is supposed to be gcd(a,b), and g = s a + t b, for some t.
   Uses temp1, temp2 and temp3. */
static int
gcdext_valid_p (const mpz_t a, const mpz_t b, const mpz_t g, const mpz_t s)
{
  /* It's not clear that gcd(0,0) is well defined, but we allow it and require that
     gcd(0,0) = 0. */
  if (mpz_sgn (g) < 0)
    return 0;

  if (mpz_sgn (a) == 0)
    {
      /* Must have g == abs (b). Any value for s is in some sense "correct",
	 but it makes sense to require that s == 0. */
      return mpz_cmpabs (g, b) == 0 && mpz_sgn (s) == 0;
    }
  else if (mpz_sgn (b) == 0)
    {
      /* Must have g == abs (a), s == sign (a) */
      return mpz_cmpabs (g, a) == 0 && mpz_cmp_si (s, mpz_sgn (a)) == 0;
    }

  if (mpz_sgn (g) <= 0)
    return 0;

  mpz_tdiv_qr (temp1, temp3, a, g);
  if (mpz_sgn (temp3) != 0)
    return 0;

  mpz_tdiv_qr (temp2, temp3, b, g);
  if (mpz_sgn (temp3) != 0)
    return 0;

  /* Require that 2 |s| < |b/g|, or |s| == 1. */
  if (mpz_cmpabs_ui (s, 1) > 0)
    {
      mpz_mul_2exp (temp3, s, 1);
      if (mpz_cmpabs (temp3, temp2) > 0)
	return 0;
    }

  /* Compute the other cofactor. */
  mpz_mul(temp2, s, a);
  mpz_sub(temp2, g, temp2);
  mpz_tdiv_qr(temp2, temp3, temp2, b);

  if (mpz_sgn (temp3) != 0)
    return 0;

  /* Require that 2 |t| < |a/g| or |t| == 1*/
  if (mpz_cmpabs_ui (temp2, 1) > 0)
    {
      mpz_mul_2exp (temp2, temp2, 1);
      if (mpz_cmpabs (temp2, temp1) > 0)
	return 0;
    }
  return 1;
}
