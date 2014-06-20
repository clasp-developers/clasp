/* Exercise mpz_*_kronecker_*() and mpz_jacobi() functions.

Copyright 1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

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


/* With no arguments the various Kronecker/Jacobi symbol routines are
   checked against some test data and a lot of derived data.

   To check the test data against PARI-GP, run

	   t-jac -p | gp -q

   It takes a while because the output from "t-jac -p" is big.


   Enhancements:

   More big test cases than those given by check_squares_zi would be good.  */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gmp.h"
#include "gmp-impl.h"
#include "tests.h"


#ifdef _LONG_LONG_LIMB
#define LL(l,ll)  ll
#else
#define LL(l,ll)  l
#endif


int option_pari = 0;


unsigned long
mpz_mod4 (mpz_srcptr z)
{
  mpz_t          m;
  unsigned long  ret;

  mpz_init (m);
  mpz_fdiv_r_2exp (m, z, 2);
  ret = mpz_get_ui (m);
  mpz_clear (m);
  return ret;
}

int
mpz_fits_ulimb_p (mpz_srcptr z)
{
  return (SIZ(z) == 1 || SIZ(z) == 0);
}

mp_limb_t
mpz_get_ulimb (mpz_srcptr z)
{
  if (SIZ(z) == 0)
    return 0;
  else
    return PTR(z)[0];
}


void
try_base (mp_limb_t a, mp_limb_t b, int answer)
{
  int  got;

  if ((b & 1) == 0 || b == 1 || a > b)
    return;

  got = mpn_jacobi_base (a, b, 0);
  if (got != answer)
    {
      printf (LL("mpn_jacobi_base (%lu, %lu) is %d should be %d\n",
		 "mpn_jacobi_base (%llu, %llu) is %d should be %d\n"),
	      a, b, got, answer);
      abort ();
    }
}


void
try_zi_ui (mpz_srcptr a, unsigned long b, int answer)
{
  int  got;

  got = mpz_kronecker_ui (a, b);
  if (got != answer)
    {
      printf ("mpz_kronecker_ui (");
      mpz_out_str (stdout, 10, a);
      printf (", %lu) is %d should be %d\n", b, got, answer);
      abort ();
    }
}


void
try_zi_si (mpz_srcptr a, long b, int answer)
{
  int  got;

  got = mpz_kronecker_si (a, b);
  if (got != answer)
    {
      printf ("mpz_kronecker_si (");
      mpz_out_str (stdout, 10, a);
      printf (", %ld) is %d should be %d\n", b, got, answer);
      abort ();
    }
}


void
try_ui_zi (unsigned long a, mpz_srcptr b, int answer)
{
  int  got;

  got = mpz_ui_kronecker (a, b);
  if (got != answer)
    {
      printf ("mpz_ui_kronecker (%lu, ", a);
      mpz_out_str (stdout, 10, b);
      printf (") is %d should be %d\n", got, answer);
      abort ();
    }
}


void
try_si_zi (long a, mpz_srcptr b, int answer)
{
  int  got;

  got = mpz_si_kronecker (a, b);
  if (got != answer)
    {
      printf ("mpz_si_kronecker (%ld, ", a);
      mpz_out_str (stdout, 10, b);
      printf (") is %d should be %d\n", got, answer);
      abort ();
    }
}


/* Don't bother checking mpz_jacobi, since it only differs for b even, and
   we don't have an actual expected answer for it.  tests/devel/try.c does
   some checks though.  */
void
try_zi_zi (mpz_srcptr a, mpz_srcptr b, int answer)
{
  int  got;

  got = mpz_kronecker (a, b);
  if (got != answer)
    {
      printf ("mpz_kronecker (");
      mpz_out_str (stdout, 10, a);
      printf (", ");
      mpz_out_str (stdout, 10, b);
      printf (") is %d should be %d\n", got, answer);
      abort ();
    }
}


void
try_pari (mpz_srcptr a, mpz_srcptr b, int answer)
{
  printf ("try(");
  mpz_out_str (stdout, 10, a);
  printf (",");
  mpz_out_str (stdout, 10, b);
  printf (",%d)\n", answer);
}


void
try_each (mpz_srcptr a, mpz_srcptr b, int answer)
{
  if (option_pari)
    {
      try_pari (a, b, answer);
      return;
    }

  if (mpz_fits_ulimb_p (a) && mpz_fits_ulimb_p (b))
    try_base (mpz_get_ulimb (a), mpz_get_ulimb (b), answer);

  if (mpz_fits_ulong_p (b))
    try_zi_ui (a, mpz_get_ui (b), answer);

  if (mpz_fits_slong_p (b))
    try_zi_si (a, mpz_get_si (b), answer);

  if (mpz_fits_ulong_p (a))
    try_ui_zi (mpz_get_ui (a), b, answer);

  if (mpz_fits_sint_p (a))
    try_si_zi (mpz_get_si (a), b, answer);

  try_zi_zi (a, b, answer);
}


/* Try (a/b) and (a/-b). */
void
try_pn (mpz_srcptr a, mpz_srcptr b_orig, int answer)
{
  mpz_t  b;

  mpz_init_set (b, b_orig);
  try_each (a, b, answer);

  mpz_neg (b, b);
  if (mpz_sgn (a) < 0)
    answer = -answer;

  try_each (a, b, answer);

  mpz_clear (b);
}


/* Try (a+k*p/b) for various k, using the fact (a/b) is periodic in a with
   period p.  For b>0, p=b if b!=2mod4 or p=4*b if b==2mod4. */

void
try_periodic_num (mpz_srcptr a_orig, mpz_srcptr b, int answer)
{
  mpz_t  a, a_period;
  int    i;

  if (mpz_sgn (b) <= 0)
    return;

  mpz_init_set (a, a_orig);
  mpz_init_set (a_period, b);
  if (mpz_mod4 (b) == 2)
    mpz_mul_ui (a_period, a_period, 4);

  /* don't bother with these tests if they're only going to produce
     even/even */
  if (mpz_even_p (a) && mpz_even_p (b) && mpz_even_p (a_period))
    goto done;

  for (i = 0; i < 6; i++)
    {
      mpz_add (a, a, a_period);
      try_pn (a, b, answer);
    }

  mpz_set (a, a_orig);
  for (i = 0; i < 6; i++)
    {
      mpz_sub (a, a, a_period);
      try_pn (a, b, answer);
    }

 done:
  mpz_clear (a);
  mpz_clear (a_period);
}


/* Try (a/b+k*p) for various k, using the fact (a/b) is periodic in b of
   period p.

			       period p
	   a==0,1mod4             a
	   a==2mod4              4*a
	   a==3mod4 and b odd    4*a
	   a==3mod4 and b even   8*a

   In Henri Cohen's book the period is given as 4*a for all a==2,3mod4, but
   a counterexample would seem to be (3/2)=-1 which with (3/14)=+1 doesn't
   have period 4*a (but rather 8*a with (3/26)=-1).  Maybe the plain 4*a is
   to be read as applying to a plain Jacobi symbol with b odd, rather than
   the Kronecker extension to b even. */

void
try_periodic_den (mpz_srcptr a, mpz_srcptr b_orig, int answer)
{
  mpz_t  b, b_period;
  int    i;

  if (mpz_sgn (a) == 0 || mpz_sgn (b_orig) == 0)
    return;

  mpz_init_set (b, b_orig);

  mpz_init_set (b_period, a);
  if (mpz_mod4 (a) == 3 && mpz_even_p (b))
    mpz_mul_ui (b_period, b_period, 8L);
  else if (mpz_mod4 (a) >= 2)
    mpz_mul_ui (b_period, b_period, 4L);

  /* don't bother with these tests if they're only going to produce
     even/even */
  if (mpz_even_p (a) && mpz_even_p (b) && mpz_even_p (b_period))
    goto done;

  for (i = 0; i < 6; i++)
    {
      mpz_add (b, b, b_period);
      try_pn (a, b, answer);
    }

  mpz_set (b, b_orig);
  for (i = 0; i < 6; i++)
    {
      mpz_sub (b, b, b_period);
      try_pn (a, b, answer);
    }

 done:
  mpz_clear (b);
  mpz_clear (b_period);
}


static const unsigned long  ktable[] = {
  0, 1, 2, 3, 4, 5, 6, 7,
  GMP_NUMB_BITS-1, GMP_NUMB_BITS, GMP_NUMB_BITS+1,
  2*GMP_NUMB_BITS-1, 2*GMP_NUMB_BITS, 2*GMP_NUMB_BITS+1,
  3*GMP_NUMB_BITS-1, 3*GMP_NUMB_BITS, 3*GMP_NUMB_BITS+1
};


/* Try (a/b*2^k) for various k. */
void
try_2den (mpz_srcptr a, mpz_srcptr b_orig, int answer)
{
  mpz_t  b;
  int    kindex;
  int    answer_a2, answer_k;
  unsigned long k;

  /* don't bother when b==0 */
  if (mpz_sgn (b_orig) == 0)
    return;

  mpz_init_set (b, b_orig);

  /* (a/2) is 0 if a even, 1 if a==1 or 7 mod 8, -1 if a==3 or 5 mod 8 */
  answer_a2 = (mpz_even_p (a) ? 0
	       : (((SIZ(a) >= 0 ? PTR(a)[0] : -PTR(a)[0]) + 2) & 7) < 4 ? 1
	       : -1);

  for (kindex = 0; kindex < numberof (ktable); kindex++)
    {
      k = ktable[kindex];

      /* answer_k = answer*(answer_a2^k) */
      answer_k = (answer_a2 == 0 && k != 0 ? 0
		  : (k & 1) == 1 && answer_a2 == -1 ? -answer
		  : answer);

      mpz_mul_2exp (b, b_orig, k);
      try_pn (a, b, answer_k);
    }

  mpz_clear (b);
}


/* Try (a*2^k/b) for various k.  If it happens mpz_ui_kronecker() gets (2/b)
   wrong it will show up as wrong answers demanded. */
void
try_2num (mpz_srcptr a_orig, mpz_srcptr b, int answer)
{
  mpz_t  a;
  int    kindex;
  int    answer_2b, answer_k;
  unsigned long  k;

  /* don't bother when a==0 */
  if (mpz_sgn (a_orig) == 0)
    return;

  mpz_init (a);

  /* (2/b) is 0 if b even, 1 if b==1 or 7 mod 8, -1 if b==3 or 5 mod 8 */
  answer_2b = (mpz_even_p (b) ? 0
	       : (((SIZ(b) >= 0 ? PTR(b)[0] : -PTR(b)[0]) + 2) & 7) < 4 ? 1
	       : -1);

  for (kindex = 0; kindex < numberof (ktable); kindex++)
    {
      k = ktable[kindex];

      /* answer_k = answer*(answer_2b^k) */
      answer_k = (answer_2b == 0 && k != 0 ? 0
		  : (k & 1) == 1 && answer_2b == -1 ? -answer
		  : answer);

	mpz_mul_2exp (a, a_orig, k);
      try_pn (a, b, answer_k);
    }

  mpz_clear (a);
}


/* The try_2num() and try_2den() routines don't in turn call
   try_periodic_num() and try_periodic_den() because it hugely increases the
   number of tests performed, without obviously increasing coverage.

   Useful extra derived cases can be added here. */

void
try_all (mpz_t a, mpz_t b, int answer)
{
  try_pn (a, b, answer);
  try_periodic_num (a, b, answer);
  try_periodic_den (a, b, answer);
  try_2num (a, b, answer);
  try_2den (a, b, answer);
}


void
check_data (void)
{
  static const struct {
    const char  *a;
    const char  *b;
    int         answer;

  } data[] = {

    /* Note that the various derived checks in try_all() reduce the cases
       that need to be given here.  */

    /* some zeros */
    {  "0",  "0", 0 },
    {  "0",  "2", 0 },
    {  "0",  "6", 0 },
    {  "5",  "0", 0 },
    { "24", "60", 0 },

    /* (a/1) = 1, any a
       In particular note (0/1)=1 so that (a/b)=(a mod b/b). */
    { "0", "1", 1 },
    { "1", "1", 1 },
    { "2", "1", 1 },
    { "3", "1", 1 },
    { "4", "1", 1 },
    { "5", "1", 1 },

    /* (0/b) = 0, b != 1 */
    { "0",  "3", 0 },
    { "0",  "5", 0 },
    { "0",  "7", 0 },
    { "0",  "9", 0 },
    { "0", "11", 0 },
    { "0", "13", 0 },
    { "0", "15", 0 },

    /* (1/b) = 1 */
    { "1",  "1", 1 },
    { "1",  "3", 1 },
    { "1",  "5", 1 },
    { "1",  "7", 1 },
    { "1",  "9", 1 },
    { "1", "11", 1 },

    /* (-1/b) = (-1)^((b-1)/2) which is -1 for b==3 mod 4 */
    { "-1",  "1",  1 },
    { "-1",  "3", -1 },
    { "-1",  "5",  1 },
    { "-1",  "7", -1 },
    { "-1",  "9",  1 },
    { "-1", "11", -1 },
    { "-1", "13",  1 },
    { "-1", "15", -1 },
    { "-1", "17",  1 },
    { "-1", "19", -1 },

    /* (2/b) = (-1)^((b^2-1)/8) which is -1 for b==3,5 mod 8.
       try_2num() will exercise multiple powers of 2 in the numerator.  */
    { "2",  "1",  1 },
    { "2",  "3", -1 },
    { "2",  "5", -1 },
    { "2",  "7",  1 },
    { "2",  "9",  1 },
    { "2", "11", -1 },
    { "2", "13", -1 },
    { "2", "15",  1 },
    { "2", "17",  1 },

    /* (-2/b) = (-1)^((b^2-1)/8)*(-1)^((b-1)/2) which is -1 for b==5,7mod8.
       try_2num() will exercise multiple powers of 2 in the numerator, which
       will test that the shift in mpz_si_kronecker() uses unsigned not
       signed.  */
    { "-2",  "1",  1 },
    { "-2",  "3",  1 },
    { "-2",  "5", -1 },
    { "-2",  "7", -1 },
    { "-2",  "9",  1 },
    { "-2", "11",  1 },
    { "-2", "13", -1 },
    { "-2", "15", -1 },
    { "-2", "17",  1 },

    /* (a/2)=(2/a).
       try_2den() will exercise multiple powers of 2 in the denominator. */
    {  "3",  "2", -1 },
    {  "5",  "2", -1 },
    {  "7",  "2",  1 },
    {  "9",  "2",  1 },
    {  "11", "2", -1 },

    /* Harriet Griffin, "Elementary Theory of Numbers", page 155, various
       examples.  */
    {   "2", "135",  1 },
    { "135",  "19", -1 },
    {   "2",  "19", -1 },
    {  "19", "135",  1 },
    { "173", "135",  1 },
    {  "38", "135",  1 },
    { "135", "173",  1 },
    { "173",   "5", -1 },
    {   "3",   "5", -1 },
    {   "5", "173", -1 },
    { "173",   "3", -1 },
    {   "2",   "3", -1 },
    {   "3", "173", -1 },
    { "253",  "21",  1 },
    {   "1",  "21",  1 },
    {  "21", "253",  1 },
    {  "21",  "11", -1 },
    {  "-1",  "11", -1 },

    /* Griffin page 147 */
    {  "-1",  "17",  1 },
    {   "2",  "17",  1 },
    {  "-2",  "17",  1 },
    {  "-1",  "89",  1 },
    {   "2",  "89",  1 },

    /* Griffin page 148 */
    {  "89",  "11",  1 },
    {   "1",  "11",  1 },
    {  "89",   "3", -1 },
    {   "2",   "3", -1 },
    {   "3",  "89", -1 },
    {  "11",  "89",  1 },
    {  "33",  "89", -1 },

    /* H. Davenport, "The Higher Arithmetic", page 65, the quadratic
       residues and non-residues mod 19.  */
    {  "1", "19",  1 },
    {  "4", "19",  1 },
    {  "5", "19",  1 },
    {  "6", "19",  1 },
    {  "7", "19",  1 },
    {  "9", "19",  1 },
    { "11", "19",  1 },
    { "16", "19",  1 },
    { "17", "19",  1 },
    {  "2", "19", -1 },
    {  "3", "19", -1 },
    {  "8", "19", -1 },
    { "10", "19", -1 },
    { "12", "19", -1 },
    { "13", "19", -1 },
    { "14", "19", -1 },
    { "15", "19", -1 },
    { "18", "19", -1 },

    /* Residues and non-residues mod 13 */
    {  "0",  "13",  0 },
    {  "1",  "13",  1 },
    {  "2",  "13", -1 },
    {  "3",  "13",  1 },
    {  "4",  "13",  1 },
    {  "5",  "13", -1 },
    {  "6",  "13", -1 },
    {  "7",  "13", -1 },
    {  "8",  "13", -1 },
    {  "9",  "13",  1 },
    { "10",  "13",  1 },
    { "11",  "13", -1 },
    { "12",  "13",  1 },

    /* various */
    {  "5",   "7", -1 },
    { "15",  "17",  1 },
    { "67",  "89",  1 },

    /* special values inducing a==b==1 at the end of jac_or_kron() */
    { "0x10000000000000000000000000000000000000000000000001",
      "0x10000000000000000000000000000000000000000000000003", 1 },
  };

  int    i;
  mpz_t  a, b;

  mpz_init (a);
  mpz_init (b);

  for (i = 0; i < numberof (data); i++)
    {
      mpz_set_str_or_abort (a, data[i].a, 0);
      mpz_set_str_or_abort (b, data[i].b, 0);
      try_all (a, b, data[i].answer);
    }

  mpz_clear (a);
  mpz_clear (b);
}


/* (a^2/b)=1 if gcd(a,b)=1, or (a^2/b)=0 if gcd(a,b)!=1.
   This includes when a=0 or b=0. */
void
check_squares_zi (void)
{
  gmp_randstate_ptr rands = RANDS;
  mpz_t  a, b, g;
  int    i, answer;
  mp_size_t size_range, an, bn;
  mpz_t bs;

  mpz_init (bs);
  mpz_init (a);
  mpz_init (b);
  mpz_init (g);

  for (i = 0; i < 50; i++)
    {
      mpz_urandomb (bs, rands, 32);
      size_range = mpz_get_ui (bs) % 10 + 2;

      mpz_urandomb (bs, rands, size_range);
      an = mpz_get_ui (bs);
      mpz_rrandomb (a, rands, an);

      mpz_urandomb (bs, rands, size_range);
      bn = mpz_get_ui (bs);
      mpz_rrandomb (b, rands, bn);

      mpz_gcd (g, a, b);
      if (mpz_cmp_ui (g, 1L) == 0)
	answer = 1;
      else
	answer = 0;

      mpz_mul (a, a, a);

      try_all (a, b, answer);
    }

  mpz_clear (bs);
  mpz_clear (a);
  mpz_clear (b);
  mpz_clear (g);
}


/* Check the handling of asize==0, make sure it isn't affected by the low
   limb. */
void
check_a_zero (void)
{
  mpz_t  a, b;

  mpz_init_set_ui (a, 0);
  mpz_init (b);

  mpz_set_ui (b, 1L);
  PTR(a)[0] = 0;
  try_all (a, b, 1);   /* (0/1)=1 */
  PTR(a)[0] = 1;
  try_all (a, b, 1);   /* (0/1)=1 */

  mpz_set_si (b, -1L);
  PTR(a)[0] = 0;
  try_all (a, b, 1);   /* (0/-1)=1 */
  PTR(a)[0] = 1;
  try_all (a, b, 1);   /* (0/-1)=1 */

  mpz_set_ui (b, 0);
  PTR(a)[0] = 0;
  try_all (a, b, 0);   /* (0/0)=0 */
  PTR(a)[0] = 1;
  try_all (a, b, 0);   /* (0/0)=0 */

  mpz_set_ui (b, 2);
  PTR(a)[0] = 0;
  try_all (a, b, 0);   /* (0/2)=0 */
  PTR(a)[0] = 1;
  try_all (a, b, 0);   /* (0/2)=0 */

  mpz_clear (a);
  mpz_clear (b);
}


int
main (int argc, char *argv[])
{
  tests_start ();

  if (argc >= 2 && strcmp (argv[1], "-p") == 0)
    {
      option_pari = 1;

      printf ("\
try(a,b,answer) =\n\
{\n\
  if (kronecker(a,b) != answer,\n\
    print(\"wrong at \", a, \",\", b,\n\
      \" expected \", answer,\n\
      \" pari says \", kronecker(a,b)))\n\
}\n");
    }

  check_data ();
  check_squares_zi ();
  check_a_zero ();

  tests_end ();
  exit (0);
}
