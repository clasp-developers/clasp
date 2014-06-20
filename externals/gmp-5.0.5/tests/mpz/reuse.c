/* Test that routines allow reusing a source variable as destination.

   Test all relevant functions except:
	mpz_bin_ui
	mpz_nextprime
	mpz_mul_si
	mpz_addmul_ui (should this really allow a+=a*c?)

Copyright 1996, 1999, 2000, 2001, 2002, 2009 Free Software Foundation, Inc.

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
#include <string.h>

#include "gmp.h"
#include "gmp-impl.h"
#include "tests.h"

#if __GMP_LIBGMP_DLL

/* FIXME: When linking to a DLL libgmp, mpz_add etc can't be used as
   initializers for global variables because they're effectively global
   variables (function pointers) themselves.  Perhaps calling a test
   function successively with mpz_add etc would be better.  */

int
main (void)
{
  printf ("Test suppressed for windows DLL\n");
  exit (0);
}


#else /* ! DLL_EXPORT */

void dump __GMP_PROTO ((char *, mpz_t, mpz_t, mpz_t));

typedef void (*dss_func) __GMP_PROTO ((mpz_ptr, mpz_srcptr, mpz_srcptr));
typedef void (*dsi_func) __GMP_PROTO ((mpz_ptr, mpz_srcptr, unsigned long int));
typedef unsigned long int (*dsi_div_func) __GMP_PROTO ((mpz_ptr, mpz_srcptr, unsigned long int));
typedef unsigned long int (*ddsi_div_func) __GMP_PROTO ((mpz_ptr, mpz_ptr, mpz_srcptr, unsigned long int));
typedef void (*ddss_div_func) __GMP_PROTO ((mpz_ptr, mpz_ptr, mpz_srcptr, mpz_srcptr));
typedef void (*ds_func) __GMP_PROTO ((mpz_ptr, mpz_srcptr));


void
mpz_xinvert (mpz_ptr r, mpz_srcptr a, mpz_srcptr b)
{
  int res;
  res = mpz_invert (r, a, b);
  if (res == 0)
    mpz_set_ui (r, 0);
}

dss_func dss_funcs[] =
{
  mpz_add, mpz_sub, mpz_mul,
  mpz_cdiv_q, mpz_cdiv_r, mpz_fdiv_q, mpz_fdiv_r, mpz_tdiv_q, mpz_tdiv_r,
  mpz_xinvert,
  mpz_gcd, mpz_lcm, mpz_and, mpz_ior, mpz_xor
};
char *dss_func_names[] =
{
  "mpz_add", "mpz_sub", "mpz_mul",
  "mpz_cdiv_q", "mpz_cdiv_r", "mpz_fdiv_q", "mpz_fdiv_r", "mpz_tdiv_q", "mpz_tdiv_r",
  "mpz_xinvert",
  "mpz_gcd", "mpz_lcm", "mpz_and", "mpz_ior", "mpz_xor"
};
char dss_func_division[] = {0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0};

dsi_func dsi_funcs[] =
{
  /* Don't change order here without changing the code in main(). */
  mpz_add_ui, mpz_mul_ui, mpz_sub_ui,
  mpz_fdiv_q_2exp, mpz_fdiv_r_2exp,
  mpz_cdiv_q_2exp, mpz_cdiv_r_2exp,
  mpz_tdiv_q_2exp, mpz_tdiv_r_2exp,
  mpz_mul_2exp,
  mpz_pow_ui
};
char *dsi_func_names[] =
{
  "mpz_add_ui", "mpz_mul_ui", "mpz_sub_ui",
  "mpz_fdiv_q_2exp", "mpz_fdiv_r_2exp",
  "mpz_cdiv_q_2exp", "mpz_cdiv_r_2exp",
  "mpz_tdiv_q_2exp", "mpz_tdiv_r_2exp",
  "mpz_mul_2exp",
  "mpz_pow_ui"
};

dsi_div_func dsi_div_funcs[] =
{
  mpz_cdiv_q_ui, mpz_cdiv_r_ui,
  mpz_fdiv_q_ui, mpz_fdiv_r_ui,
  mpz_tdiv_q_ui, mpz_tdiv_r_ui
};
char *dsi_div_func_names[] =
{
  "mpz_cdiv_q_ui", "mpz_cdiv_r_ui",
  "mpz_fdiv_q_ui", "mpz_fdiv_r_ui",
  "mpz_tdiv_q_ui", "mpz_tdiv_r_ui"
};

ddsi_div_func ddsi_div_funcs[] =
{
  mpz_cdiv_qr_ui,
  mpz_fdiv_qr_ui,
  mpz_tdiv_qr_ui
};
char *ddsi_div_func_names[] =
{
  "mpz_cdiv_qr_ui",
  "mpz_fdiv_qr_ui",
  "mpz_tdiv_qr_ui"
};

ddss_div_func ddss_div_funcs[] =
{
  mpz_cdiv_qr,
  mpz_fdiv_qr,
  mpz_tdiv_qr
};
char *ddss_div_func_names[] =
{
  "mpz_cdiv_qr",
  "mpz_fdiv_qr",
  "mpz_tdiv_qr"
};

ds_func ds_funcs[] =
{
  mpz_abs, mpz_com, mpz_neg, mpz_sqrt
};
char *ds_func_names[] =
{
  "mpz_abs", "mpz_com", "mpz_neg", "mpz_sqrt"
};


/* Really use `defined (__STDC__)' here; we want it to be true for Sun C */
#if defined (__STDC__) || defined (__cplusplus)
#define FAIL(class,indx,op1,op2,op3) \
  do {									\
  class##_funcs[indx] = 0;						\
  dump (class##_func_names[indx], op1, op2, op3);			\
  failures++;								\
  } while (0)
#define FAIL2(fname,op1,op2,op3) \
  do {									\
  dump (#fname, op1, op2, op3);						\
  failures++;								\
  } while (0)
#else
#define FAIL(class,indx,op1,op2,op3) \
  do {									\
  class/**/_funcs[indx] = 0;						\
  dump (class/**/_func_names[indx], op1, op2, op3);			\
  failures++;								\
  } while (0)
#define FAIL2(fname,op1,op2,op3) \
  do {									\
  dump ("fname", op1, op2, op3);					\
  failures++;								\
  } while (0)
#endif



int
main (int argc, char **argv)
{
  int i;
  int pass, reps = 100;
  mpz_t in1, in2, in3;
  unsigned long int in2i;
  mp_size_t size;
  mpz_t res1, res2, res3;
  mpz_t ref1, ref2, ref3;
  mpz_t t;
  unsigned long int r1, r2;
  long failures = 0;
  gmp_randstate_ptr rands;
  mpz_t bs;
  unsigned long bsi, size_range;

  tests_start ();
  TESTS_REPS (reps, argv, argc);

  rands = RANDS;

  mpz_init (bs);

  mpz_init (in1);
  mpz_init (in2);
  mpz_init (in3);
  mpz_init (ref1);
  mpz_init (ref2);
  mpz_init (ref3);
  mpz_init (res1);
  mpz_init (res2);
  mpz_init (res3);
  mpz_init (t);

  for (pass = 1; pass <= reps; pass++)
    {
      mpz_urandomb (bs, rands, 32);
      size_range = mpz_get_ui (bs) % 17 + 2;

      mpz_urandomb (bs, rands, size_range);
      size = mpz_get_ui (bs);
      mpz_rrandomb (in1, rands, size);

      mpz_urandomb (bs, rands, size_range);
      size = mpz_get_ui (bs);
      mpz_rrandomb (in2, rands, size);

      mpz_urandomb (bs, rands, size_range);
      size = mpz_get_ui (bs);
      mpz_rrandomb (in3, rands, size);

      mpz_urandomb (bs, rands, 3);
      bsi = mpz_get_ui (bs);
      if ((bsi & 1) != 0)
	mpz_neg (in1, in1);
      if ((bsi & 1) != 0)
	mpz_neg (in2, in2);
      if ((bsi & 1) != 0)
	mpz_neg (in3, in3);

      for (i = 0; i < sizeof (dss_funcs) / sizeof (dss_func); i++)
	{
	  if (dss_funcs[i] == 0)
	    continue;
	  if (dss_func_division[i] && mpz_sgn (in2) == 0)
	    continue;

	  (dss_funcs[i]) (ref1, in1, in2);
	  MPZ_CHECK_FORMAT (ref1);

	  mpz_set (res1, in1);
	  (dss_funcs[i]) (res1, res1, in2);
	  MPZ_CHECK_FORMAT (res1);
	  if (mpz_cmp (ref1, res1) != 0)
	    FAIL (dss, i, in1, in2, NULL);

	  mpz_set (res1, in2);
	  (dss_funcs[i]) (res1, in1, res1);
	  MPZ_CHECK_FORMAT (res1);
	  if (mpz_cmp (ref1, res1) != 0)
	    FAIL (dss, i, in1, in2, NULL);
	}

      for (i = 0; i < sizeof (ddss_div_funcs) / sizeof (ddss_div_func); i++)
	{
	  if (ddss_div_funcs[i] == 0)
	    continue;
	  if (mpz_sgn (in2) == 0)
	    continue;

	  (ddss_div_funcs[i]) (ref1, ref2, in1, in2);
	  MPZ_CHECK_FORMAT (ref1);
	  MPZ_CHECK_FORMAT (ref2);

	  mpz_set (res1, in1);
	  (ddss_div_funcs[i]) (res1, res2, res1, in2);
	  MPZ_CHECK_FORMAT (res1);
	  MPZ_CHECK_FORMAT (res2);
	  if (mpz_cmp (ref1, res1) != 0 || mpz_cmp (ref2, res2) != 0)
	    FAIL (ddss_div, i, in1, in2, NULL);

	  mpz_set (res2, in1);
	  (ddss_div_funcs[i]) (res1, res2, res2, in2);
	  MPZ_CHECK_FORMAT (res1);
	  MPZ_CHECK_FORMAT (res2);
	  if (mpz_cmp (ref1, res1) != 0 || mpz_cmp (ref2, res2) != 0)
	    FAIL (ddss_div, i, in1, in2, NULL);

	  mpz_set (res1, in2);
	  (ddss_div_funcs[i]) (res1, res2, in1, res1);
	  MPZ_CHECK_FORMAT (res1);
	  MPZ_CHECK_FORMAT (res2);
	  if (mpz_cmp (ref1, res1) != 0 || mpz_cmp (ref2, res2) != 0)
	    FAIL (ddss_div, i, in1, in2, NULL);

	  mpz_set (res2, in2);
	  (ddss_div_funcs[i]) (res1, res2, in1, res2);
	  MPZ_CHECK_FORMAT (res1);
	  MPZ_CHECK_FORMAT (res2);
	  if (mpz_cmp (ref1, res1) != 0 || mpz_cmp (ref2, res2) != 0)
	    FAIL (ddss_div, i, in1, in2, NULL);
	}

      for (i = 0; i < sizeof (ds_funcs) / sizeof (ds_func); i++)
	{
	  if (ds_funcs[i] == 0)
	    continue;
	  if (strcmp (ds_func_names[i], "mpz_sqrt") == 0
	      && mpz_sgn (in1) < 0)
	    continue;

	  (ds_funcs[i]) (ref1, in1);
	  MPZ_CHECK_FORMAT (ref1);

	  mpz_set (res1, in1);
	  (ds_funcs[i]) (res1, res1);
	  MPZ_CHECK_FORMAT (res1);
	  if (mpz_cmp (ref1, res1) != 0)
	    FAIL (ds, i, in1, in2, NULL);
	}

      in2i = mpz_get_ui (in2);

      for (i = 0; i < sizeof (dsi_funcs) / sizeof (dsi_func); i++)
	{
	  if (dsi_funcs[i] == 0)
	    continue;
	  if (strcmp (dsi_func_names[i], "mpz_fdiv_q_2exp") == 0)
	    /* Limit exponent to something reasonable for the division
	       functions.  Without this, we'd  normally shift things off
	       the end and just generate the trivial values 1, 0, -1.  */
	    in2i %= 0x1000;
	  if (strcmp (dsi_func_names[i], "mpz_mul_2exp") == 0)
	    /* Limit exponent more for mpz_mul_2exp to save time.  */
	    in2i %= 0x100;
	  if (strcmp (dsi_func_names[i], "mpz_pow_ui") == 0)
	    /* Limit exponent yet more for mpz_pow_ui to save time.  */
	    in2i %= 0x10;

	  (dsi_funcs[i]) (ref1, in1, in2i);
	  MPZ_CHECK_FORMAT (ref1);

	  mpz_set (res1, in1);
	  (dsi_funcs[i]) (res1, res1, in2i);
	  MPZ_CHECK_FORMAT (res1);
	  if (mpz_cmp (ref1, res1) != 0)
	    FAIL (dsi, i, in1, in2, NULL);
	}

      if (in2i != 0)	  /* Don't divide by 0.  */
	{
	  for (i = 0; i < sizeof (dsi_div_funcs) / sizeof (dsi_div_funcs); i++)
	    {
	      r1 = (dsi_div_funcs[i]) (ref1, in1, in2i);
	      MPZ_CHECK_FORMAT (ref1);

	      mpz_set (res1, in1);
	      r2 = (dsi_div_funcs[i]) (res1, res1, in2i);
	      MPZ_CHECK_FORMAT (res1);
	      if (mpz_cmp (ref1, res1) != 0 || r1 != r2)
		FAIL (dsi_div, i, in1, in2, NULL);
	    }

	  for (i = 0; i < sizeof (ddsi_div_funcs) / sizeof (ddsi_div_funcs); i++)
	    {
	      r1 = (ddsi_div_funcs[i]) (ref1, ref2, in1, in2i);
	      MPZ_CHECK_FORMAT (ref1);

	      mpz_set (res1, in1);
	      r2 = (ddsi_div_funcs[i]) (res1, res2, res1, in2i);
	      MPZ_CHECK_FORMAT (res1);
	      if (mpz_cmp (ref1, res1) != 0 || mpz_cmp (ref2, res2) != 0 || r1 != r2)
		FAIL (ddsi_div, i, in1, in2, NULL);

	      mpz_set (res2, in1);
	      (ddsi_div_funcs[i]) (res1, res2, res2, in2i);
	      MPZ_CHECK_FORMAT (res1);
	      if (mpz_cmp (ref1, res1) != 0 || mpz_cmp (ref2, res2) != 0 || r1 != r2)
		FAIL (ddsi_div, i, in1, in2, NULL);
	    }
	}

      if (mpz_sgn (in1) >= 0)
	{
	  mpz_sqrtrem (ref1, ref2, in1);
	  MPZ_CHECK_FORMAT (ref1);
	  MPZ_CHECK_FORMAT (ref2);

	  mpz_set (res1, in1);
	  mpz_sqrtrem (res1, res2, res1);
	  MPZ_CHECK_FORMAT (res1);
	  MPZ_CHECK_FORMAT (res2);
	  if (mpz_cmp (ref1, res1) != 0 || mpz_cmp (ref2, res2) != 0)
	    FAIL2 (mpz_sqrtrem, in1, NULL, NULL);

	  mpz_set (res2, in1);
	  mpz_sqrtrem (res1, res2, res2);
	  MPZ_CHECK_FORMAT (res1);
	  MPZ_CHECK_FORMAT (res2);
	  if (mpz_cmp (ref1, res1) != 0 || mpz_cmp (ref2, res2) != 0)
	    FAIL2 (mpz_sqrtrem, in1, NULL, NULL);
	}

      if (mpz_sgn (in1) >= 0)
	{
	  mpz_root (ref1, in1, in2i % 0x1000 + 1);
	  MPZ_CHECK_FORMAT (ref1);

	  mpz_set (res1, in1);
	  mpz_root (res1, res1, in2i % 0x1000 + 1);
	  MPZ_CHECK_FORMAT (res1);
	  if (mpz_cmp (ref1, res1) != 0)
	    FAIL2 (mpz_root, in1, in2, NULL);
	}

      if (mpz_sgn (in1) >= 0)
	{
	  mpz_rootrem (ref1, ref2, in1, in2i % 0x1000 + 1);
	  MPZ_CHECK_FORMAT (ref1);
	  MPZ_CHECK_FORMAT (ref2);

	  mpz_set (res1, in1);
	  mpz_rootrem (res1, res2, res1, in2i % 0x1000 + 1);
	  MPZ_CHECK_FORMAT (res1);
	  MPZ_CHECK_FORMAT (res2);
	  if (mpz_cmp (ref1, res1) != 0 || mpz_cmp (ref2, res2) != 0)
	    FAIL2 (mpz_rootrem, in1, in2, NULL);

	  mpz_set (res2, in1);
	  mpz_rootrem (res1, res2, res2, in2i % 0x1000 + 1);
	  MPZ_CHECK_FORMAT (res1);
	  MPZ_CHECK_FORMAT (res2);
	  if (mpz_cmp (ref1, res1) != 0 || mpz_cmp (ref2, res2) != 0)
	    FAIL2 (mpz_rootrem, in1, in2, NULL);
	}

      if (pass < reps / 2)	/* run fewer tests since gcdext lots of time */
	{
	  mpz_gcdext (ref1, ref2, ref3, in1, in2);
	  MPZ_CHECK_FORMAT (ref1);
	  MPZ_CHECK_FORMAT (ref2);
	  MPZ_CHECK_FORMAT (ref3);

	  mpz_set (res1, in1);
	  mpz_gcdext (res1, res2, res3, res1, in2);
	  MPZ_CHECK_FORMAT (res1);
	  MPZ_CHECK_FORMAT (res2);
	  MPZ_CHECK_FORMAT (res3);
	  if (mpz_cmp (ref1, res1) != 0 || mpz_cmp (ref2, res2) != 0
	      || mpz_cmp (ref3, res3) != 0)
	    FAIL2 (mpz_gcdext, in1, in2, NULL);

	  mpz_set (res2, in1);
	  mpz_gcdext (res1, res2, res3, res2, in2);
	  MPZ_CHECK_FORMAT (res1);
	  MPZ_CHECK_FORMAT (res2);
	  MPZ_CHECK_FORMAT (res3);
	  if (mpz_cmp (ref1, res1) != 0 || mpz_cmp (ref2, res2) != 0
	      || mpz_cmp (ref3, res3) != 0)
	    FAIL2 (mpz_gcdext, in1, in2, NULL);

	  mpz_set (res3, in1);
	  mpz_gcdext (res1, res2, res3, res3, in2);
	  MPZ_CHECK_FORMAT (res1);
	  MPZ_CHECK_FORMAT (res2);
	  MPZ_CHECK_FORMAT (res3);
	  if (mpz_cmp (ref1, res1) != 0 || mpz_cmp (ref2, res2) != 0
	      || mpz_cmp (ref3, res3) != 0)
	    FAIL2 (mpz_gcdext, in1, in2, NULL);

	  mpz_set (res1, in2);
	  mpz_gcdext (res1, res2, res3, in1, res1);
	  MPZ_CHECK_FORMAT (res1);
	  MPZ_CHECK_FORMAT (res2);
	  MPZ_CHECK_FORMAT (res3);
	  if (mpz_cmp (ref1, res1) != 0 || mpz_cmp (ref2, res2) != 0
	      || mpz_cmp (ref3, res3) != 0)
	    FAIL2 (mpz_gcdext, in1, in2, NULL);

	  mpz_set (res2, in2);
	  mpz_gcdext (res1, res2, res3, in1, res2);
	  MPZ_CHECK_FORMAT (res1);
	  MPZ_CHECK_FORMAT (res2);
	  MPZ_CHECK_FORMAT (res3);
	  if (mpz_cmp (ref1, res1) != 0 || mpz_cmp (ref2, res2) != 0
	      || mpz_cmp (ref3, res3) != 0)
	    FAIL2 (mpz_gcdext, in1, in2, NULL);

	  mpz_set (res3, in2);
	  mpz_gcdext (res1, res2, res3, in1, res3);
	  MPZ_CHECK_FORMAT (res1);
	  MPZ_CHECK_FORMAT (res2);
	  MPZ_CHECK_FORMAT (res3);
	  if (mpz_cmp (ref1, res1) != 0 || mpz_cmp (ref2, res2) != 0
	      || mpz_cmp (ref3, res3) != 0)
	    FAIL2 (mpz_gcdext, in1, in2, NULL);

	  mpz_set (res1, in1);
	  mpz_gcdext (res1, res2, NULL, res1, in2);
	  MPZ_CHECK_FORMAT (res1);
	  MPZ_CHECK_FORMAT (res2);
	  if (mpz_cmp (ref1, res1) != 0 || mpz_cmp (ref2, res2) != 0
	      || mpz_cmp (ref3, res3) != 0)
	    FAIL2 (mpz_gcdext, in1, in2, NULL);

	  mpz_set (res2, in1);
	  mpz_gcdext (res1, res2, NULL, res2, in2);
	  MPZ_CHECK_FORMAT (res1);
	  MPZ_CHECK_FORMAT (res2);
	  if (mpz_cmp (ref1, res1) != 0 || mpz_cmp (ref2, res2) != 0
	      || mpz_cmp (ref3, res3) != 0)
	    FAIL2 (mpz_gcdext, in1, in2, NULL);

	  mpz_set (res1, in2);
	  mpz_gcdext (res1, res2, NULL, in1, res1);
	  MPZ_CHECK_FORMAT (res1);
	  MPZ_CHECK_FORMAT (res2);
	  if (mpz_cmp (ref1, res1) != 0 || mpz_cmp (ref2, res2) != 0
	      || mpz_cmp (ref3, res3) != 0)
	    FAIL2 (mpz_gcdext, in1, in2, NULL);

	  mpz_set (res2, in2);
	  mpz_gcdext (res1, res2, NULL, in1, res2);
	  MPZ_CHECK_FORMAT (res1);
	  MPZ_CHECK_FORMAT (res2);
	  if (mpz_cmp (ref1, res1) != 0 || mpz_cmp (ref2, res2) != 0
	      || mpz_cmp (ref3, res3) != 0)
	    FAIL2 (mpz_gcdext, in1, in2, NULL);
	}

      /* Don't run mpz_powm for huge exponents or when undefined.  */
      if (mpz_sizeinbase (in2, 2) < 250 && mpz_sgn (in3) != 0
	  && (mpz_sgn (in2) >= 0 || mpz_invert (t, in1, in3)))
	{
	  mpz_powm (ref1, in1, in2, in3);
	  MPZ_CHECK_FORMAT (ref1);

	  mpz_set (res1, in1);
	  mpz_powm (res1, res1, in2, in3);
	  MPZ_CHECK_FORMAT (res1);
	  if (mpz_cmp (ref1, res1) != 0)
	    FAIL2 (mpz_powm, in1, in2, in3);

	  mpz_set (res1, in2);
	  mpz_powm (res1, in1, res1, in3);
	  MPZ_CHECK_FORMAT (res1);
	  if (mpz_cmp (ref1, res1) != 0)
	    FAIL2 (mpz_powm, in1, in2, in3);

	  mpz_set (res1, in3);
	  mpz_powm (res1, in1, in2, res1);
	  MPZ_CHECK_FORMAT (res1);
	  if (mpz_cmp (ref1, res1) != 0)
	    FAIL2 (mpz_powm, in1, in2, in3);
	}

      /* Don't run mpz_powm_ui when undefined.  */
      if (mpz_sgn (in3) != 0)
	{
	  mpz_powm_ui (ref1, in1, in2i, in3);
	  MPZ_CHECK_FORMAT (ref1);

	  mpz_set (res1, in1);
	  mpz_powm_ui (res1, res1, in2i, in3);
	  MPZ_CHECK_FORMAT (res1);
	  if (mpz_cmp (ref1, res1) != 0)
	    FAIL2 (mpz_powm_ui, in1, in2, in3);

	  mpz_set (res1, in3);
	  mpz_powm_ui (res1, in1, in2i, res1);
	  MPZ_CHECK_FORMAT (res1);
	  if (mpz_cmp (ref1, res1) != 0)
	    FAIL2 (mpz_powm_ui, in1, in2, in3);
	}

      {
	r1 = mpz_gcd_ui (ref1, in1, in2i);
	MPZ_CHECK_FORMAT (ref1);

	mpz_set (res1, in1);
	r2 = mpz_gcd_ui (res1, res1, in2i);
	MPZ_CHECK_FORMAT (res1);
	if (mpz_cmp (ref1, res1) != 0)
	  FAIL2 (mpz_gcd_ui, in1, in2, NULL);
      }

      if (mpz_cmp_ui (in2, 1L) > 0 && mpz_sgn (in1) != 0)
	{
	  /* Test mpz_remove */
	  mpz_remove (ref1, in1, in2);
	  MPZ_CHECK_FORMAT (ref1);

	  mpz_set (res1, in1);
	  mpz_remove (res1, res1, in2);
	  MPZ_CHECK_FORMAT (res1);
	  if (mpz_cmp (ref1, res1) != 0)
	    FAIL2 (mpz_remove, in1, in2, NULL);

	  mpz_set (res1, in2);
	  mpz_remove (res1, in1, res1);
	  MPZ_CHECK_FORMAT (res1);
	  if (mpz_cmp (ref1, res1) != 0)
	    FAIL2 (mpz_remove, in1, in2, NULL);
	}

      if (mpz_sgn (in2) != 0)
	{
	  /* Test mpz_divexact */
	  mpz_mul (t, in1, in2);
	  mpz_divexact (ref1, t, in2);
	  MPZ_CHECK_FORMAT (ref1);

	  mpz_set (res1, t);
	  mpz_divexact (res1, res1, in2);
	  MPZ_CHECK_FORMAT (res1);
	  if (mpz_cmp (ref1, res1) != 0)
	    FAIL2 (mpz_divexact, t, in2, NULL);

	  mpz_set (res1, in2);
	  mpz_divexact (res1, t, res1);
	  MPZ_CHECK_FORMAT (res1);
	  if (mpz_cmp (ref1, res1) != 0)
	    FAIL2 (mpz_divexact, t, in2, NULL);
	}

      if (mpz_sgn (in2) > 0)
	{
	  /* Test mpz_divexact_gcd, same as mpz_divexact */
	  mpz_mul (t, in1, in2);
	  mpz_divexact_gcd (ref1, t, in2);
	  MPZ_CHECK_FORMAT (ref1);

	  mpz_set (res1, t);
	  mpz_divexact_gcd (res1, res1, in2);
	  MPZ_CHECK_FORMAT (res1);
	  if (mpz_cmp (ref1, res1) != 0)
	    FAIL2 (mpz_divexact_gcd, t, in2, NULL);

	  mpz_set (res1, in2);
	  mpz_divexact_gcd (res1, t, res1);
	  MPZ_CHECK_FORMAT (res1);
	  if (mpz_cmp (ref1, res1) != 0)
	    FAIL2 (mpz_divexact_gcd, t, in2, NULL);
	}
    }

  if (failures != 0)
    {
      fprintf (stderr, "mpz/reuse: %ld error%s\n", failures, "s" + (failures == 1));
      exit (1);
    }

  mpz_clear (bs);
  mpz_clear (in1);
  mpz_clear (in2);
  mpz_clear (in3);
  mpz_clear (ref1);
  mpz_clear (ref2);
  mpz_clear (ref3);
  mpz_clear (res1);
  mpz_clear (res2);
  mpz_clear (res3);
  mpz_clear (t);

  tests_end ();
  exit (0);
}

void
dump (char *name, mpz_t in1, mpz_t in2, mpz_t in3)
{
  printf ("failure in %s (", name);
  mpz_out_str (stdout, -16, in1);
  if (in2 != NULL)
    {
      printf (" ");
      mpz_out_str (stdout, -16, in2);
    }
  if (in3 != NULL)
    {
      printf (" ");
      mpz_out_str (stdout, -16, in3);
    }
  printf (")\n");
}

#endif /* ! DLL_EXPORT */
